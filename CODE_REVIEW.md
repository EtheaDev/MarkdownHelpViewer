# Code Review — Markdown Help Viewer

**Data:** 1 settembre 2026
**Ambito:** codice proprietario del repository (`Source/`, `Packages/`, `Demo/`), ~8.500 righe.
**Escluso:** dipendenze vendored in `Ext/` (HTMLViewer, MarkdownProcessor, SVGIconImageList, StyledComponents, SynPDF, VCLStyleUtils, ISMultiLanguage).

> **Nota sui numeri di riga:** si riferiscono allo stato del codice *prima* delle correzioni del Gruppo A (vedi § Stato in fondo). Nei file già modificati le righe sono leggermente traslate.

---

## Sintesi

Il progetto è ben strutturato a livello macro (viewer / interfaccia help / componente), e le parti più recenti (`MDCodeHighlightEmitter.pas`, la gestione di `AllowUnsafe`, il commento sul dangling pointer dell'exporter SynEdit) sono di buona qualità e ben documentate.

I problemi si concentrano in tre aree:

1. **Bug funzionali reali** — una quindicina, quasi tutti con fix localizzato.
2. **Duplicazione pesante di codice** — fino a 4 copie divergenti della stessa funzione.
3. **Robustezza di IPC e rete** — validazione mancante su `WM_COPYDATA`, chiamate HTTP bloccanti sul main thread.

---

## 1. Bug funzionali

### 1.1 `LoadFromFile` non riconosce mai i file HTML — `MarkDownViewerComponents.pas:559`

```pascal
LExt := ExtractFileExt(AFileName);                                  // -> '.html'
LIsHTMLContent := SameText(LExt, 'HTML') or SameText(LExt, 'HTM');  // sempre False
```

Manca il punto: `ExtractFileExt` restituisce `.html`. Ogni file HTML caricato nel componente viene passato al processore Markdown e — con `AllowUnsafe=False` (default) — i tag vengono *escapati*, quindi l'utente vede il sorgente HTML anziché la pagina renderizzata.

**Fix:** confrontare con `'.html'`/`'.htm'`, o meglio riusare `IsFileNameWithExt`, che già esiste.

### 1.2 `LoadFromString` in modalità HTML cancella il contenuto — `MarkDownViewerComponents.pas:577-582`

```pascal
FHTMLContent.Text := AValue;   // -> HTMLContentChanged -> render (ok)
FMarkdownContent.Text := '';   // -> MDContentChanged   -> FHTMLContent := CSS + Process('')
```

`TStrings.SetTextStr` esegue sempre `BeginUpdate/Clear/EndUpdate`, quindi `OnChange` scatta **anche assegnando una stringa vuota**: `MDContentChanged` (riga 590) sovrascrive l'HTML appena caricato con il solo CSS.

Il percorso è oggi poco battuto per via del bug 1.1, ma `SetHTMLContent` e `LoadFromStream` ci passano.

**Fix:** invertire l'ordine delle assegnazioni e disattivare temporaneamente gli handler `OnChange`.

### 1.3 Doppio rendering ad ogni caricamento — `MarkDownViewerComponents.pas:569-585`

`LoadFromString` → `FMarkdownContent.Text` → `MDContentChanged` → `FHTMLContent.Text` → `HTMLContentChanged` → `RefreshViewer`, **e poi** `LoadFromString` chiama di nuovo `RefreshViewer`.

Ogni file viene parsato e renderizzato due volte: flicker visibile e costo raddoppiato su documenti grandi.

### 1.4 "Preserva posizione scroll" non funziona mai — `MarkDownViewerComponents.pas:599-609` e `MDHelpView.Main.pas:973-978`

```pascal
if AReloadImages then Self.Clear;     // azzera lo scroll
...
LOldPos := Self.VScrollBarPosition;   // legge 0
```

La posizione va letta **prima** di `Clear`. Effetto visibile: Ctrl+rotella (cambio dimensione font) e refresh riportano sempre in cima al documento.

### 1.5 Il pannello Indice mostra il documento principale — `MDHelpView.Main.pas:940-945`

```pascal
else
  Result := FHtmlContent;   // <- contenuto della pagina principale, non FHtmlIndexContent
```

`TransformMDToHTML` viene invocata anche con `FMdIndexContent` (`TransformTo(HtmlViewerIndex, ...)`, righe 1044 e 1420). Se l'indice è un file `.html`, `FMdIndexContent` è vuoto e la funzione restituisce il contenuto della pagina corrente.

**Fix:** passare esplicitamente il fallback HTML corretto invece di leggere un campo dell'istanza.

### 1.6 Ricerca del CSS su path malformato — `MDHelpView.Main.pas:874`

```pascal
TryLoadCSS(LWorkingFolder + ChangeFileExt(FCurrentFileName,'.css'))
```

`FCurrentFileName` è già assoluto, quindi si ottiene `C:\doc\C:\doc\file.css`. Il terzo tentativo di caricamento CSS (quello per-file) non riesce mai.

**Fix:** `ExtractFileName(ChangeFileExt(FCurrentFileName, '.css'))`.

### 1.7 Orientamento PDF mai persistito — `MDHelpView.Settings.pas:433` vs `:545`

Si scrive la chiave `Orientation` e si rilegge `PrintOrientation`. L'impostazione landscape/portrait si perde ad ogni riavvio.

### 1.8 Export HTML corrompe il testo — `MDHelpView.Main.pas:306-312`

```pascal
LHtmlContent := StringReplace(LHtmlContent, LExt, '.htm', [rfReplaceAll, rfIgnoreCase]);
```

La sostituzione agisce su **tutto il documento**, non solo sugli `href`: qualsiasi occorrenza di `.md` nel testo viene alterata. Inoltre l'ordine dell'array fa sì che `.mdown` diventi `.htmown`, perché `.md` viene sostituito per primo.

**Fix:** replace mirato sugli attributi `href="..."`, iterando dalle estensioni più lunghe alle più corte.

### 1.9 `SendWMCOPYToProcess` riporta successo anche quando fallisce — `MarkDownHelpViewer.pas:343-354`

`MyHwnd` può valere `0` (processo trovato ma nessuna finestra principale visibile): la `SendMessage` non fa nulla ma `Result := True`, quindi il fallback `ShellExecute` viene saltato e l'help non si apre.

**Fix:** verificare `MyHwnd <> 0` e usare il valore di ritorno della `SendMessage` (il ricevente però non imposta mai `Message.Result`, vedi 2.1). Mancano anche il `break` dopo il match e un `try/finally` sullo snapshot handle.

### 1.10 `GetHelpFile('')` restituisce sempre stringa vuota — `MarkDownHelpViewer.pas:574-593`

`Result` resta `''` quando `HelpKeyword` è vuoto. Conseguenze:

- `DisplayTopic` (riga 514) chiama `ShowMarkdownFile('')` → eccezione *File "" not found*.
- `GetHelpStrings` (riga 395) enumera la directory corrente invece della cartella di help.

**Fix:** assegnare `Result := LFileName` nel ramo "nessuna keyword".

### 1.11 Le query dell'help sollevano eccezioni — `MarkDownHelpViewer.pas:584, 606`

`UnderstandsKeyword`, `UnderstandsTopic` e `UnderstandsContext` chiamano `GetHelpFile`, che fa `raise Exception.Create(HELP_FILE_NOT_SET)` se `Application.HelpFile` è vuoto.

Premere F1 in un'applicazione che non usa questo help system produce un dialogo di errore invece di un semplice "non gestito". Le funzioni di interrogazione devono restituire `0` / `False`.

### 1.12 Registry: funzioni di *lettura* che *creano* la chiave — `MDHelpView.Registry.pas:81, 119`

```pascal
Result := Reg.OpenKey(RegPath, True);   // CanCreate = True in RegReadStr / RegReadInt
```

Effetto collaterale indesiderato, e fallimento su HKLM senza privilegi di amministratore.

**Fix:** `OpenKeyReadOnly`.

### 1.13 `IsWindows11` legge memoria non inizializzata — `MDHelpView.Registry.pas:151-171`

`VersionInfo` viene dichiarata, le si assegna solo `dwOSVersionInfoSize`, e poi si esegue `case VersionInfo.dwPlatformID of` senza mai chiamare `GetVersionEx`. Funziona per caso perché il ramo `else` è quello corretto.

Inoltre l'esito di `OpenKeyReadOnly` non è verificato prima di `ReadString`.

### 1.14 `GitHubAPI`: la classe di eccezione è un alias — `GitHubAPI.pas:41`

```pascal
ECheckNewVersionException = Exception;   // non è una sottoclasse
```

`on E: ECheckNewVersionException` in `AcceptNewSetup` (`MDHelpView.About.pas:161`) intercetta quindi **qualunque** eccezione, incluse le Access Violation, dentro un `while True` con dialogo Riprova.

**Fix:** `ECheckNewVersionException = class(Exception)`.

### 1.15 `GitHubAPI`: nil dereference — `GitHubAPI.pas:187` e `:299`

`FHTTPClient` è creato lazy solo in `InvokeGETAsString`: chiamare `DownloadLatestSetup` per primo produce un'AV. Idem `SetCustomHeaders`, che scrive su `FCustomHeaders` non ancora creata — ed è una property `published`, quindi anche lo streaming a design-time andrebbe in AV.

**Fix:** crearli nel costruttore.

Minore: `GetLatestVersionTag` (riga 213) fa `GetValue('tag_name').Value` senza controllo nil, e perde memoria se il JSON non è un oggetto.

### 1.16 Immagini remote: estensione sbagliata e file temporanei orfani — `MDHelpView.Resources.pas:262-269`

```pascal
if SameText(ExtractFileExt(LFileName),'svg') then            // manca il punto -> sempre False
...
LFileName := ChangeFileExt(TPath.GetTempFileName,'.svg');    // sempre .svg, per qualsiasi immagine
```

Tre problemi in sequenza:

- (a) gli SVG vengono letti come ANSI e corrotti;
- (b) il nome temporaneo è **sempre** `.svg`, e `ConvertImage` (riga 341) sceglie il decoder dall'estensione, quindi ogni PNG/JPEG scaricato finisce nel ramo SVG;
- (c) `TPath.GetTempFileName` **crea** un file che non viene mai cancellato, e nemmeno il `.svg` scritto subito dopo: accumulo di file temporanei ad ogni immagine remota.

Il passaggio del binario attraverso `TStringStream` / `DataString` non è comunque round-trip safe. Inoltre la ricorsione sui redirect (riga 292) non ha limite di profondità.

---

## 2. Robustezza e sicurezza

### 2.1 `WM_COPYDATA` senza validazione — `MDHelpView.Main.pas:1518-1539`

```pascal
r := PRecToPass(PCopyDataStruct(Message.LParam)^.lpData);
LFilePath := r^.FilePath;   // nessun controllo su dwData né su cbData
```

Qualunque processo può inviare un `WM_COPYDATA` con un buffer più piccolo di `SizeOf(THelpInfoToPass)` e provocare una lettura fuori dai limiti. Il mittente usa il magic `dwData := 3232` (`MarkDownHelpViewer.pas:347`) ma il ricevente non lo verifica.

**Fix:**

```pascal
if (p^.dwData = 3232) and (p^.cbData = SizeOf(THelpInfoToPass)) then ...
Message.Result := 1;   // oggi mai impostato
```

### 2.2 Il record IPC perde i caratteri non-ANSI — `MarkDownHelpViewer.pas:40-44`

`FilePath` e `FileName` sono `string[255]` (ShortString): la conversione Unicode→ANSI è lossy — non a caso il file apre con `{$WARN IMPLICIT_STRING_CAST_LOSS OFF}`. Per un prodotto localizzato in 6 lingue, un path con caratteri cirillici o accentati fuori codepage arriva corrotto.

**Fix:** `array[0..259] of WideChar`.

Nota: il path viene passato **già quotato** da `ShowMarkdownFile:449`, e le virgolette vengono rimosse dall'altra parte con `StripQuotes`. Funziona, ma è fragile e non intenzionale.

### 2.3 `ShellExecute` su URL arbitrari dal documento — `Main.pas:781`, `Resources.pas:200`, `MarkDownViewerComponents.pas:921`

Un link in un `.md` può puntare a un eseguibile locale o a un path UNC e viene aperto con un solo click, senza conferma. Per un help viewer che apre file di terzi conviene filtrare gli schemi consentiti (`http`, `https`, `mailto`) e chiedere conferma per il resto.

### 2.4 `Application.ProcessMessages` durante il layout HTML — `MDHelpView.Resources.pas:146`

La callback `OnImageRequest` è invocata da dentro il parsing/layout di `THtmlViewer`: pompare la coda messaggi lì dentro permette all'utente di avviare un nuovo caricamento mentre il viewer è a metà layout — scenario classico di Access Violation. Il flag `FStopImageRequest` non protegge dalla rientranza.

### 2.5 Possibile dereference di puntatore nullo in finalization — `MarkDownHelpViewer.pas:655`

```pascal
if Assigned(Markdown_HelpViewer.HelpManager) then
```

`Markdown_HelpViewer` viene azzerato dal distruttore (riga 570): se l'interfaccia è già stata rilasciata, qui si dereferenzia `nil`.

**Fix:** `if Assigned(Markdown_HelpViewer) and Assigned(Markdown_HelpViewer.HelpManager)`.

### 2.6 Check nuova versione bloccante sul main thread — `MDHelpView.Main.pas:1075` → `About.pas:146`

La chiamata HTTP a GitHub parte da `ActionListUpdate` (handler di *idle* delle action), su main thread, senza timeout configurato su `THTTPClient` e con un loop di retry modale. All'avvio, con rete lenta o proxy, la UI resta congelata.

**Fix:** spostare in un thread/task con timeout esplicito, fuori dall'update delle action (che oltretutto viene eseguito di continuo).

### 2.7 Eccezioni ingoiate in blocco

`MarkDownViewerComponents.pas:887`, `Resources.pas:461`, e tutte e cinque le funzioni di `Registry.pas`: `except ... end` senza logging. Un'immagine che non si carica o una chiave di registro non leggibile diventano problemi silenziosi e non diagnosticabili. Almeno un log condizionale in DEBUG.

### 2.8 Overflow potenziale in `PNG4TransparentBitMap` — `MarkDownViewerComponents.pas:807-809`

Nel ramo `A = 0`: `Round(BmpRGBA[X].B * 255)` può valere fino a 65025 assegnato a un `byte` → range error con `{$R+}`. Il ramo dovrebbe copiare il canale o azzerarlo.

### 2.9 Handle troncati su Win64 — `MarkDownHelpViewer.pas:286, 299, 309`

`EnumWindowsProc(Wnd: DWORD)` e `FindMainWindow(...): DWORD`: gli `HWND` sono pointer-sized su Win64. In pratica Windows garantisce che gli handle stiano in 32 bit, quindi non esplode, ma la firma corretta è `HWND`.

---

## 3. Duplicazione e architettura

Il problema strutturale principale: **la stessa logica esiste in 2-4 copie divergenti.**

| Logica | Copie |
|---|---|
| `ConvertImage` (rescaling / PNG alpha) | `MarkDownViewerComponents.pas:761`, `Resources.pas:306`, `ImageCollection.pas:230` |
| `getStreamData` (download immagini) | `Resources.pas:214`, `ImageCollection.pas:152` |
| `FileWithExtExists` | `MarkDownHelpViewer.pas:199`, `MarkDownViewerComponents.pas:232` |
| `FindHelpFile` | `MarkDownHelpViewer.pas:142`, `MarkDownViewerComponents.pas:972` (con commento *"if changing this function, change also…"*) |
| `GetFileMasks` | `MarkDownHelpViewer.pas:231`, più una copia locale in `MarkDownViewerRegister.pas:110` |
| Array `AMarkdownFileExt` / `AHTMLFileExt` | 3 copie, **già divergenti**: 7 estensioni in `MarkDownHelpViewer.pas:639`, 9 (con `.txt` / `.text`) in `MarkDownViewerComponents.pas:1021` e in `MarkDownViewerRegister.pas:213` |
| Estrazione versione da VERSIONINFO | `MDHelpView.Misc.pas:152`, `MDHelpView.About.pas:113` |
| Classe HTML→PDF | `TvmHtmlToPdf` e `TvmHtmlToPdfGDI`, ~95% identiche; la prima non è usata da nessuno |

Osservazioni collegate:

- **`Source/MDHelpView.ImageCollection.pas` (351 righe) è codice morto**: non è referenziata da `MDHelpViewer.dpr` né da alcuna unit. È un clone datato di `MDHelpView.Resources.pas`. Da eliminare.
- **L'applicazione non usa il proprio componente**: `MDHelpView.Main.pas` lavora con `THtmlViewer` grezzo e riscrive `TransformMDToHTML`, `ShowMarkdownAsHTML`, la gestione dell'emitter e delle immagini — cioè tutto ciò che `TMarkdownViewer` già fa. È il motivo per cui i bug 1.3 e 1.4 esistono in doppia copia: correggerne uno non corregge l'altro. Far usare al viewer il proprio componente sarebbe anche il miglior test di dogfooding.
- ~~`TvmHtmlToPdf.Execute` (`vmHtmlToPdf.pas:245-320`) **non ha try/finally**: se qualcosa solleva nel loop, la `TList` e tutti i `TMetafile` restano allocati. La variante GDI, quella effettivamente usata, è invece corretta.~~ **Risolto rimuovendo l'intera classe** (vedi § Stato): non era istanziata né qui né in MarkDownShellExtensions.
- Estrarre unit comuni (es. `MDHelpView.FileTypes.pas` e `MDHelpView.ImageUtils.pas`) risolverebbe la gran parte di quanto sopra.

---

## 4. Performance

- **`TryLoadTextFile` è O(n²)** — `MarkDownViewerComponents.pas:321`: `Result := Result + LStreamReader.ReadLine + sLineBreak` in loop. Su file grandi, o sulla ricerca full-text che legge *tutti* i file della cartella (`Main.pas:531`), il costo esplode. Usare `TStringBuilder` o `ReadToEnd`.
- **Il fallback ANSI usa un'eccezione come flusso di controllo** — stessa funzione, riga 327. Funziona (`TStreamReader` solleva davvero `EEncodingError` su UTF-8 non valido: verificato), ma un file ANSI viene letto due volte, una per fallire e una per il fallback. Meglio decidere la codifica in anticipo (BOM, altrimenti validazione UTF-8 sui byte già in memoria).
- **Nessuna cache immagini**: ogni refresh riscarica le immagini remote e rielabora quelle locali.
- `UpdateCaption` (`Main.pas:1511`) rilegge il VERSIONINFO dell'exe ad ogni caricamento file.
- `ActionListUpdate` esegue `UpdateGui` (che tocca `Margins`, `Visible`, `Action` di più controlli) ad ogni ciclo di idle.

---

## 5. Minori e manutenzione

- ~~**Deriva di versione su 4 fronti**: exe `2.5.4` (`MDHelpViewer.dproj:73`), README `2.4.2`, verbo design-time `PROJECT_VER = '2.3.7'` (`MarkDownViewerRegister.pas:88`), tutti i package `2.3.2.0`. Il numero mostrato all'utente nell'IDE è indietro di due minor.~~ **Risolto: tutto allineato a `2.5.5`** (vedi § Stato). Resta indietro solo il changelog del README, che va aggiornato a mano.
- **`MDSyntaxHighlight.inc` non esiste**: i commenti in `MDCodeHighlightEmitter.pas:40` e `MarkDownViewerComponents.pas:370` lo citano come punto di controllo, ma `MD_SYNTAX_HIGHLIGHTING` è definito solo in `MDHelpViewer.dproj:77`. Chi installa i package non ha syntax highlighting e non ha un modo documentato per abilitarlo.
- `SetCssStyle` (`:613`) e `SetProcessorDialect` (`:697`) non rigenerano l'HTML: cambiarli a runtime non ha effetto fino al caricamento successivo.
- `TabStop` (`:708`) è ridichiarata e il setter scrive solo sul campo, mai su `inherited TabStop`: la property published non ha effetto sul comportamento del controllo.
- `Lines` è asimmetrica: `GetLines` restituisce `FHTMLContent` (`:470`) mentre `ReadLines` / `DefineProperties` scrivono in `FMarkdownContent` (`:419-432`).
- `SetWindowLeft` / `SetWindowTop` (`Settings.pas:492-502`) scartano valori `<= 0`: una finestra su monitor secondario a sinistra non è memorizzabile. E `UpdateWindowPos` non fa clamping ai monitor attivi (finestra fuori schermo se cambia la configurazione video).
- Impostazioni lette/scritte ma mai usate: `ActivePageIndex`, `CurrentFileName`, `CurrentIndexFileName`; parametro `AEditorVisible` inutilizzato in `UpdateSettings` (`Settings.pas:504`); costanti `EDITOPTION_*` residui del progetto gemello con editor.
- `acSearchExecute` (`Main.pas:540`) usa un'eccezione per comunicare "nessun risultato": flusso normale gestito come errore.
- `GetIndexOfWorkingFolder` (`Main.pas:1228`) è una funzione "Get" che modifica `FileListBox.Mask`.
- `GetFileNamesWithExtensions` (`MarkDownHelpViewer.pas:277`) usa `AnsiPos` case-sensitive: un `README.MD` non viene trovato; ed essendo una ricerca di sottostringa, `.md` matcha anche dentro `.mdown`.
- `FileWithExtExists` accede a `AFileExtensions[0]` senza verificare che l'array non sia vuoto.
- `MidasLib` e `Vcl.DBCtrls` linkati in un help viewer (`MDHelpViewer.dpr:30`): dipendenze inutili.
- Metodi vuoti da rimuovere: `TMainForm.SetBounds` (`:1135`), `TMainForm.Loaded` (`:1115`), `TMarkdownViewerComponentEditor.Edit`, `THookControlActionLink.IsHelpContextLinked`, `GetWindowsDirectory` (`Misc.pas:86`, mai usata).
- **Encoding sorgenti misto**: `MDHelpView.Main.pas` ha BOM UTF-8 e "Nicolò" corretto, mentre `MarkDownHelpViewer.pas`, `MDHelpView.Misc.pas` e `MarkDownViewerRegister.pas` contengono caratteri accentati corrotti ("Nicol?", "? Ethea S.r.l."); quest'ultimo compare in un verbo del component editor, quindi **visibile nell'IDE dell'utente**.

---

## Priorità suggerite

### Gruppo A — da correggere subito
Bug visibili all'utente, fix di poche righe.

| # | Descrizione | File |
|---|---|---|
| 1.1 | Estensione HTML mai riconosciuta | `MarkDownViewerComponents.pas:559` |
| 1.4 | Posizione scroll non preservata | `MarkDownViewerComponents.pas:604`, `Main.pas:978` |
| 1.6 | Path CSS malformato | `Main.pas:874` |
| 1.7 | Orientamento PDF non persistito | `Settings.pas:433,545` |
| 1.9 | Falso successo IPC | `MarkDownHelpViewer.pas:354` |
| 1.10 | `GetHelpFile('')` vuoto | `MarkDownHelpViewer.pas:574-593` |
| 1.11 | Query help che sollevano eccezioni | `MarkDownHelpViewer.pas:584,606` |
| 1.12 | Registry aperto in scrittura per leggere | `Registry.pas:81,119` |
| 1.14 | Alias di eccezione | `GitHubAPI.pas:41` |

### Gruppo B — a breve
Robustezza.

- 2.1 validazione `WM_COPYDATA`
- 1.16 file temporanei ed estensione immagini remote
- 1.15 nil dereference in GitHubAPI
- 2.6 check versione bloccante
- 2.5 nil dereference in finalization

### Gruppo C — refactor a medio termine

- Eliminare `MDHelpView.ImageCollection.pas`.
- Unificare estensioni, `FindHelpFile` e `ConvertImage` in unit condivise.
- Far usare al main form il componente `TMarkdownViewer`.
- Allineare i numeri di versione.

---

## Stato

### Corretto (1 settembre 2026)

Compilato con successo: applicazione Win64 e Win32 Release, package `Packages\D13\MarkDownViewer.dproj` (cioè anche con `MD_SYNTAX_HIGHLIGHTING` disattivato). Nessun costrutto oltre XE6 introdotto nelle unit condivise (`Source\Components\`, `Source\AppInterface\`).

| # | Intervento | File |
|---|---|---|
| 1.1 | Confronto dell'estensione con `AHTMLFileExt` (con il punto) | `MarkDownViewerComponents.pas` — `LoadFromFile` |
| 1.2 | `OnChange` sospeso durante l'assegnazione dei due contenuti | `MarkDownViewerComponents.pas` — `LoadFromString` |
| 1.3 | Un solo `RefreshViewer` per caricamento | `MarkDownViewerComponents.pas` — `LoadFromString` |
| — | `OnChange` sospeso anche durante `ReadLines` (una trasformazione markdown per ogni riga letta dal DFM) | `MarkDownViewerComponents.pas` — `ReadLines` |
| 1.4 | Posizione di scroll letta prima di `Clear` | `MarkDownViewerComponents.pas` — `RefreshViewer`; `MDHelpView.Main.pas` — `ShowMarkdownAsHTML` |
| 1.6 | `ExtractFileName` sul nome del CSS per-file | `MDHelpView.Main.pas` — `Load` |
| 1.7 | Chiave INI allineata a `PrintOrientation` | `MDHelpView.Settings.pas` — `WriteSettings` |
| 1.9 | Invio solo con `MyHwnd <> 0`, `Break` dopo l'invio, `try/finally` sullo snapshot handle, controllo di `INVALID_HANDLE_VALUE` | `MarkDownHelpViewer.pas` — `SendWMCOPYToProcess` |
| 1.10 | `Result := LFileName` quando non c'è keyword | `MarkDownHelpViewer.pas` — `GetHelpFile` |
| 1.11 | Nessuna eccezione dai metodi di interrogazione; l'errore è sollevato da `ShowMarkdownFile` | `MarkDownHelpViewer.pas` |
| 1.12 | `OpenKeyReadOnly` nelle funzioni di lettura | `MDHelpView.Registry.pas` |
| 1.14 | `ECheckNewVersionException = class(Exception)`; gli errori di trasporto vengono incapsulati in `InvokeGETAsString` | `GitHubAPI.pas` |
| 2.1 | Validazione di `dwData` e `cbData`, `Message.Result := 1`, messaggi estranei ignorati in silenzio | `MDHelpView.Main.pas` — `WMCopyData` |

Note sugli interventi:

- **1.1 richiedeva 1.2.** Finché l'estensione non veniva riconosciuta, il ramo `IsHTMLContent = True` era di fatto morto. Correggendo solo 1.1, i file HTML sarebbero passati per quel ramo e il bug 1.2 li avrebbe resi *vuoti* invece che mostrati come sorgente: peggio di prima. I due fix vanno insieme.
- **1.9 non usa il valore di ritorno di `SendMessage`.** Sarebbe la verifica corretta, ma i viewer già installati non impostano `Message.Result`: un client aggiornato li considererebbe irraggiungibili e aprirebbe una seconda istanza via `ShellExecute`. `Message.Result := 1` è stato aggiunto ora nel ricevente, così la verifica potrà essere attivata in una versione futura.
- **2.1 è stato incluso** perché tocca lo stesso metodo di 1.9 ed è retrocompatibile: la firma `MD_HELP_COPYDATA_ID` (ex letterale `3232`) e la dimensione del record sono le stesse usate dai client già distribuiti. Il letterale è ora una costante pubblica in `MarkDownHelpViewer.pas`, condivisa fra mittente e ricevente.

### Corretto — Gruppo B (1 settembre 2026)

| # | Intervento | File |
|---|---|---|
| 1.15 | `FCustomHeaders` creata nel costruttore (è una property `published`); nuovo `EnsureClient` chiamato anche da `DownloadLatestSetup`; `DestroyClient` non distrugge più gli header dell'utente; `GetLatestVersionTag` controlla `tag_name` e libera il JSON su ogni percorso | `GitHubAPI.pas` |
| 2.6 | Timeout espliciti su `THTTPClient` (5 s connessione, 15 s risposta; 10 min per il download del setup) | `GitHubAPI.pas` |
| 2.6 | Nuova `CheckNewSetupAsync`: il controllo automatico all'avvio gira in `TTask`, il callback torna sul main thread solo se c'è davvero una nuova versione | `MDHelpView.About.pas`, `MDHelpView.Main.pas` |
| 1.16 | `getStreamData` riscritta: nessun file temporaneo, decoder scelto dall'estensione dell'URL realmente scaricato, redirect manuali limitati a 5, corpo letto come testo solo per riconoscere le pagine "moved"/"not found" | `MDHelpView.Resources.pas` |
| 1.16 | `LoadFileContent` usa il valore restituito da `getStreamData` invece di assegnare `FStream` incondizionatamente | `MDHelpView.Resources.pas` |
| 2.5 | `Assigned(Markdown_HelpViewer)` prima di dereferenziarlo in `finalization` | `MarkDownHelpViewer.pas` |

Note sugli interventi:

- **I timeout non sono uniformi.** Un `ResponseTimeout` di 15 s è giusto per il controllo di versione (poche centinaia di byte di JSON) ma interromperebbe il download di un setup su linea lenta: `DownloadLatestSetup` lo alza a 10 minuti prima della GET.
- **`AcceptNewSetup` è rimasta invariata** ed è ancora bloccante con dialogo di retry. È corretto così: serve il controllo *manuale* dal box Informazioni, dove è l'utente ad averlo chiesto ed è giusto che un errore venga mostrato. Il controllo automatico all'avvio ora usa `CheckNewSetupAsync` e in caso di errore tace.
- **La correzione 1.14 ha reso possibile 2.6.** Finché `ECheckNewVersionException` era un alias di `Exception`, il `try/except` del task avrebbe avuto lo stesso comportamento ma per la ragione sbagliata; ora l'incapsulamento degli errori di trasporto fatto in `InvokeGETAsString` rende esplicito che cosa viene ingoiato.
- **Il file temporaneo di `getStreamData` non veniva mai riletto.** `ConvertImage` lavora su `FStream`, del nome file usa solo l'estensione: la scrittura su disco era puro spreco, e il nome forzato a `.svg` faceva scegliere il decoder SVG per ogni immagine remota (quindi PNG e JPEG remoti non venivano mostrati).

**Verifica funzionale:** aperto `README.md` (che contiene un badge remoto da shields.io) con l'eseguibile Win64 ricompilato. Nessun file temporaneo creato — prima della correzione ogni immagine remota ne lasciava due; in `%TEMP%` se ne erano accumulati 106 `.svg`. Chiusura dell'applicazione pulita (`WM_CLOSE`, nessun blocco in uscita col task di controllo versione attivo).

### Corretto — terzo giro (1 settembre 2026)

Include due bug del paragrafo 1 che erano rimasti fuori da entrambi i raggruppamenti.

| # | Intervento | File |
|---|---|---|
| 1.5 | `TransformMDToHTML` e `TransformTo` ricevono il contenuto HTML di fallback come parametro invece di leggere il campo `FHtmlContent` | `MDHelpView.Main.pas` |
| 1.8 | Nuova `ReplaceMarkdownLinks`: sostituzione solo a fine `href` (estensione seguita da `"` o da `#`), estensioni più lunghe per prime | `MDHelpView.Main.pas` |
| 1.13 | `IsWindows11` non legge più un record `TOSVersionInfo` mai inizializzato; esito di `OpenKeyReadOnly` verificato | `MDHelpView.Registry.pas` |
| 2.2 | Nuovo messaggio Unicode (`MD_HELP_COPYDATA_ID_W` + `THelpInfoToPassW`) con fallback automatico al messaggio ANSI legacy | `MarkDownHelpViewer.pas`, `MDHelpView.Main.pas` |
| 2.3 | `OpenExternalLink`: apertura diretta solo per `http`, `https`, `mailto`; conferma per tutto il resto. Nuovo messaggio `CONFIRM_OPEN_LINK` | `MDHelpView.Main.pas`, `MDHelpView.Messages.pas` |
| — | Le ancore interne alla pagina (`#sezione`) sono lasciate al viewer: finivano in `ShellExecute` con `Handled := True`, quindi non funzionavano | `MDHelpView.Main.pas` |
| 2.4 | Guardia di rientranza `FRendering` in `ShowMarkdownAsHTML` | `MDHelpView.Main.pas` |
| 2.7 | `OutputDebugString` in build DEBUG per le eccezioni ingoiate (registry, immagini, contenuti) | `MDHelpView.Registry.pas`, `MDHelpView.Resources.pas`, `MarkDownViewerComponents.pas` |
| 2.8 | Pixel completamente trasparente: canali azzerati invece di `Round(canale * 255)`, che poteva arrivare a 65025 | `MarkDownViewerComponents.pas` |
| 2.9 | `EnumWindowsProc`, `FindMainWindow`, `GetHWndByPID` tipizzate `HWND` | `MarkDownHelpViewer.pas` |
| §4 | `TryLoadTextFile`: lettura in un solo passaggio (era quadratica) e riconoscimento reale della codifica — BOM, altrimenti validazione UTF-8, altrimenti ANSI | `MarkDownViewerComponents.pas` |
| §4 | Versione dell'eseguibile letta una sola volta invece che a ogni `UpdateCaption` | `MDHelpView.Main.pas` |
| §5 | Posizione finestra: coordinate negative ammesse (monitor a sinistra/sopra) e riporto al centro se la posizione salvata non è più su alcuno schermo | `MDHelpView.Settings.pas`, `MDHelpView.Main.pas` |
| §5 | "Nessuna corrispondenza" nella ricerca: messaggio informativo invece di un'eccezione | `MDHelpView.Main.pas` |
| §5 | `GetFileNamesWithExtensions`: confronto esatto dell'estensione e case-insensitive (`README.MD` ora viene trovato; `.md` non matcha più dentro `*.mdown`) | `MarkDownHelpViewer.pas` |
| §5 | `FileWithExtExists`: guardia su array di estensioni vuoto (entrambe le copie) | `MarkDownHelpViewer.pas`, `MarkDownViewerComponents.pas` |
| §5 | Rimossa `MDHelpView.ImageCollection.pas` + `.dfm` (351 righe di codice morto, `svn delete`) | — |
| §5 | Rimossi `TMainForm.SetBounds`, `TMainForm.Loaded`, `GetWindowsDirectory`, `resourcestring STextNotFound` | `MDHelpView.Main.pas`, `MDHelpView.Misc.pas` |
| §5 | Rimossi `MidasLib` e `Vcl.DBCtrls`: nessuna dipendenza DB nel viewer. **Eseguibile Win64 più piccolo di ~435 KB** | `MDHelpViewer.dpr`, `MDHelpView.Main.pas` |
| §5 | `PROJECT_VER` allineato a `2.5.4` (era `2.3.7`, ed è visibile nell'IDE) | `MarkDownViewerRegister.pas` |

Note sugli interventi:

- **Il messaggio IPC Unicode è retrocompatibile in entrambe le direzioni.** Il client invia prima il messaggio Unicode: un viewer che lo riconosce risponde 1, un viewer più vecchio lascia il risultato a 0 e il client ripiega sul messaggio ANSI legacy. Un client vecchio con un viewer nuovo continua a funzionare, perché il ricevente accetta ancora la firma `3232`. È il motivo per cui `Message.Result := 1` era stato introdotto nel giro precedente.
- **`Application.ProcessMessages` è stata mantenuta.** Toglierla eliminerebbe l'unica cosa che rende reattiva la UI durante il caricamento delle immagini, e con essa l'annullamento con ESC. La guardia `FRendering` chiude invece il percorso davvero pericoloso: un secondo rendering avviato mentre il primo è a metà layout.
- **Rettifica su `TryLoadTextFile`.** In una prima stesura avevo scritto che il fallback `EEncodingError` non scattava mai e che i file ANSI venivano caricati pieni di U+FFFD: **è sbagliato**, verificato compilando ed eseguendo la vecchia implementazione su un file ANSI. `TStreamReader` con `TEncoding.UTF8` solleva davvero `EEncodingError`, e il fallback funzionava. La riscrittura resta valida per gli altri due motivi — concatenazione quadratica e doppia lettura del file — e produce lo stesso risultato: verificato su UTF-8 senza BOM e su ANSI.
- **`ShellExecute` nel componente non è stato toccato.** In `TCustomMarkdownViewer.HotSpotClickHandled` l'applicazione ospite può già intercettare tutto con `OnURLClicked` o disattivare `AutoLoadOnHotSpotClick`: aggiungere un dialogo di conferma dentro un componente VCL cambierebbe il comportamento a chi lo usa. L'irrigidimento riguarda il viewer autonomo, che è quello che apre file di terze parti.

**Verifica funzionale**

- `TryLoadTextFile` provata sulla funzione realmente compilata, tramite un programma console che usa l'unit del componente:
  - UTF-8 senza BOM → `perché città più è ©` e `Привет` decodificati correttamente (U+00E9, U+00E0, U+00F9, U+00E8, U+00A9, U+041F…U+0442);
  - ANSI → stessi caratteri accentati decodificati correttamente (prima sarebbero stati U+FFFD).
- Viewer Win64 ricompilato: aperto `README.md`, processo stabile per 12 secondi, nessun file temporaneo creato, chiusura pulita con `WM_CLOSE`.

### Versione allineata a 2.5.5 (1 settembre 2026)

Incremento di patch, coerente con un rilascio di sole correzioni.

| Dove | Prima | Dopo |
|---|---|---|
| `Source/MDHelpViewer.dproj` (`FileVersion` e `VerInfo_*`) | 2.5.4.0 | 2.5.5.0 |
| `Setup/MarkDownHelpViewerSetup.iss` (`MyAppVersion`) | 2.5.4 | 2.5.5 |
| `Packages/MarkDownViewerRegister.pas` (`PROJECT_VER`, visibile nell'IDE) | 2.5.4 | 2.5.5 |
| 9 × `Packages/*/MarkDownViewer.dproj` | 2.3.2.0 | 2.5.5.0 |
| 9 × `Packages/*/dclMarkDownViewer.dproj` | 2.0.0.0 (D13: anche 1.0.0.0) | 2.5.5.0 |

Nei `dclMarkDownViewer.dproj` mancavano del tutto gli elementi `VerInfo_MinorVer` e `VerInfo_Release`: sono stati aggiunti, altrimenti il *fixed-file-info* binario sarebbe rimasto `2.0.0.0` anche con la stringa `FileVersion` corretta.

Verificato sui binari prodotti: `MDHelpViewer.exe` Win64 e Win32 riportano `2.5.5.0` sia nell'informazione binaria sia nella stringa.

### Gruppo C — pulizia interna (1 settembre 2026)

| Intervento | Effetto |
|---|---|
| Rimossa la classe `TvmHtmlToPdf` da `vmHtmlToPdf.pas` | Mai istanziata, né qui né in MarkDownShellExtensions, e copia al ~95% di `TvmHtmlToPdfGDI`. Con essa sparisce anche il suo `Execute` privo di `try/finally`. **476 → 244 righe** |

La modifica è stata applicata in modo identico anche a MarkDownShellExtensions: quel file è byte-identico fra i due repository, e finché la condivisione resta rimandata va tenuto allineato a mano. Verificato dopo l'intervento: i due file coincidono ancora esattamente.

Compilano app Win64 e Win32.

### Da fare

Non affrontati, con la motivazione:

- **Gruppo C — unit condivisa per estensioni / `FindHelpFile` / `ConvertImage`.** Va aggiunta alla clausola `Contains` di 9 package (DXE6 → D13): è un refactor da fare deliberatamente, non di passaggio.
- **Gruppo C — il main form che usa `TMarkdownViewer`.** Comporta modifiche al DFM e lo spostamento di gran parte della logica del form. È l'intervento che ripaga di più (elimina la doppia manutenzione che ha generato 1.3 e 1.4 in due copie) ma è anche il più invasivo, e senza suite di test va pianificato a parte.
- **Cache delle immagini** (§4): è sviluppo di una funzionalità nuova, non una correzione.
- **Costo di `ActionListUpdate`/`UpdateGui` a ogni idle** (§4): richiede rilevamento delle variazioni, beneficio modesto.
- **Changelog del README**: l'ultima voce è `2.4.2`, quindi mancano le note di rilascio da `2.5.0` in poi. Il contenuto è una scelta editoriale, non l'ho inventato.
- **Encoding misto dei sorgenti** (§5): `MarkDownHelpViewer.pas`, `MDHelpView.Misc.pas`, `MarkDownViewerRegister.pas` e `MDHelpView.Messages.pas` hanno caratteri accentati corrotti. Va fatta una conversione a UTF-8 con BOM in un commit dedicato, separato da modifiche funzionali, per non rendere illeggibile il diff.
- **`GetIndexOfWorkingFolder`** (§5) resta una funzione "Get" con effetto collaterale su `FileListBox.Mask`: separare le due cose cambierebbe l'ordine delle operazioni, meglio con un test davanti.

---

## Regressione del check versione asincrono — 2 settembre 2026

**Sintomo:** Access Violation in `System._UStrAsg` aprendo la form About dal
controllo automatico di nuova versione.

```
System._UStrAsg(???,???)
GitHubAPI.TGitHubHttpClient.DownloadLatestSetup('...Setup.exe',(...HttpClientReceiveData,$38766A0),'')
uAbout.TFrmAbout.DownloadAndInstallSetup
uAbout.TFrmAbout.FormActivate($38766A0)
...
Vcl.Forms.TCustomForm.ShowModal
uAbout.ShowAboutForm(..., True)
EditorMainForm.TfrmMain.ActionListUpdate$ActRec.$0$Body('v3.4.0','v3.4.1')
uAbout.CheckNewSetupAsync$ActRec.$0$Body$ActRec.$0$0$Body
System.Classes.CheckSynchronize(???)
```

**Causa.** La variabile globale `_HttpClient` veniva creata **solo** dentro
`AcceptNewSetup`, cioè il percorso del controllo **manuale**:

```pascal
if not Assigned(_HttpClient) then
  _HttpClient := TGitHubHttpClient.Create(nil);
Result := _HttpClient.IsNewVersionAvailable(...);
```

mentre `DownloadAndInstallSetup` la usava senza verificarla. Finché il controllo
era sincrono la sequenza era sempre "controlla poi scarica", quindi il client
esisteva per effetto collaterale. Spostando il controllo automatico in un task
di background — che per non condividere stato fra thread usa un client locale
suo — nessuno crea più `_HttpClient`: la form About aperta direttamente dal
callback arriva a `DownloadLatestSetup` con `Self = nil`, e la prima
assegnazione di campo (`FSetupFileName := ASetupFileName`) scrive a un offset
da `nil`. Da qui il frame `System._UStrAsg`.

Il terzo argomento mostrato come `''` nel trace è il parametro `out
ADownloadedFileName`, non la causa: è vuoto perché la funzione non era ancora
arrivata ad assegnarlo.

**Secondo difetto sullo stesso percorso.** Anche creando il client, un'istanza
appena costruita ha `FGitHubProjectURL` vuoto, e `DownloadLatestSetup` lo
verificava solo con un `Assert` **sul nome del file**, non sull'URL. Il
risultato non sarebbe stato un errore ma la URL malformata
`/releases/latest/download/<setup>`.

**Terzo difetto: il download fallito si ritentava in loop.** `FormActivate`
avvia il download quando `FStopDownload` è `False`, e `OnActivate` scatta a
ogni riattivazione della form. `UpdateGUI(False)`, chiamata nel `finally`,
rimetteva la didascalia del pulsante ma **non** il flag: chiuso il dialogo
d'errore, la form si riattivava e il download ripartiva.

### Correzioni

| Dove | Modifica |
|---|---|
| form About (3 progetti) | Nuova `function GitHubClient: TGitHubHttpClient` che crea il client alla prima chiamata **e** ne imposta sempre `GitHubProjectURL`. Usata sia da `AcceptNewSetup` sia da `DownloadAndInstallSetup` |
| form About (3 progetti) | `UpdateGUI(False)` rimette `FStopDownload := True`, coerentemente con la didascalia che torna a "check for updates" |
| `GitHubAPI.pas` (3 copie) | I due `Assert` sui parametri obbligatori diventano controlli veri con `ECheckNewVersionException` e messaggi propri (`ERR_SETUP_FILENAME_MISSING`, `ERR_PROJECT_URL_MISSING`): un `Assert` può essere escluso dalla compilazione, e sull'URL vuoto non c'era alcun controllo |

La regressione era presente in tutti e tre i progetti che hanno ricevuto
`CheckNewSetupAsync` — MarkDownHelpViewer, MarkDownShellExtensions e
SVGShellExtensions — e si manifesta solo quando una versione più recente esiste
davvero, per questo non era emersa prima.

### Osservazioni non modificate

- Il download parte da `FormActivate` e gira quindi dentro la closure accodata
  da `TThread.Queue`, con una form modale annidata. Funziona, ma un pulsante
  esplicito o un `PostMessage` alla form sarebbe più lineare.
- `DownloadAndInstallSetup` confronta la dimensione scaricata con
  `FExpectedSize`, che viene valorizzato dal primo callback `OnReceiveData` con
  `AContentLength`. Se il server rispondesse senza `Content-Length` il
  confronto fallirebbe sempre e un download riuscito sarebbe segnalato come
  errore. GitHub lo invia, quindi in pratica non si verifica.

### Regex delle versioni allineata a kittoX

`DecodeVersion` usava `'^v(\d+)\.(\d+)\.(\d+)$'`, che impone il prefisso `v`
e vieta qualunque suffisso. La copia in `kittoX/Kide/Source/GitHubAPI.pas` era
gia' stata resa piu' tollerante, e ora le quattro copie sono identiche:

```pascal
// Optional 'v'/'V' prefix and an ignored trailing suffix (e.g. a pre-release
// tag like 'beta' in 'V4.0.18beta'): only the leading N.N.N is significant.
Regex := TRegEx.Create('^v?(\d+)\.(\d+)\.(\d+)', [roIgnoreCase]);
```

Con la versione precedente un tag come `3.4.2`, `V4.0.18beta` o `v3.4.2-rc1`
faceva sollevare `ECheckNewVersionException`: nel controllo automatico
l'eccezione viene inghiottita in silenzio e l'aggiornamento non viene **mai**
proposto, nel controllo manuale l'utente vede il dialogo di errore con
"retry". Il rischio riguarda solo il tag che arriva da GitHub, perche'
`GetCurrentVersion` produce sempre `vN.N.N`.

Comportamento verificato compilando un confronto fra le due regex
(`dcc64` 37.0):

| tag | vecchia | nuova |
|---|---|---|
| `v3.4.2`, `V3.4.2` | 3.4.2 | 3.4.2 |
| `v3.4.10` | 3.4.10 | 3.4.10 |
| `v10.20.30` | 10.20.30 | 10.20.30 |
| `3.4.2` | eccezione | **3.4.2** |
| `V4.0.18beta` | eccezione | **4.0.18** |
| `v3.4.2-rc1` | eccezione | **3.4.2** |
| `v2.5` | eccezione | eccezione |
| `release-3.4.2` | eccezione | eccezione |
| `''`, `vX.Y.Z` | eccezione | eccezione |

Nessun tag che prima veniva accettato cambia risultato, e togliere l'ancora
`$` non intacca la greediness di `\d+`: `v3.4.10` resta `3.4.10` e non
`3.4.1`. L'ancora `^` continua a respingere i prefissi non numerici come
`release-3.4.2`.

Effetto collaterale da tenere presente: un tag di pre-release viene ora
interpretato come la versione finale corrispondente, quindi `v3.4.3-rc1`
sarebbe proposto come `3.4.3`. In pratica non capita perche' l'endpoint
`releases/latest` di GitHub esclude le pre-release.

---

## Regressione: Runtime error 217 all'avvio su macchina senza Delphi — 21 settembre 2026

**Sintomo:** in un ambiente UAT (Windows 10 Pro, nessun RAD Studio installato)
l'apertura di un file `.md` con `MDHelpViewer.exe` 2.5.6 a 32 bit termina con

```
Runtime error 217 at 0121864D
```

Nessuna finestra viene mostrata. Lo stesso eseguibile, sugli stessi file,
funziona sulla macchina di sviluppo. La differenza non è il sistema operativo
— sospetto iniziale, perché lo sviluppo è su Windows 11 e la UAT su Windows 10
— ma la presenza di RAD Studio.

**Causa.** `MDHelpView.Main.pas` esegue lavoro vero nella propria
`initialization`:

```pascal
initialization
  FViewerSettings := TViewerSettings.CreateSettings;
  RegisterTrxProperties(FViewerSettings.GUILanguage);
```

`RegisterTrxProperties` chiama `InitTrxSupport`, che in
`Ext\ISMultiLanguage\src\Engine\CBMultiLanguage.pas:665-679` apre sempre un
dataset, qualunque sia la lingua dell'interfaccia:

```pascal
CDSTrxPropList := TCDSTrxPropList.Create(nil);
CDSTrxPropList.FileName := TrxRepositoryPath + CDSTrxFileName;
CDSTrxPropList.OpenCDS; //Apre o crea il file
```

`TCDSTrxPropList` discende da `TTrxClientDataSet = class(TClientDataSet)`
(`CBMultiLanguage.pas:168`): serve Midas. Nel giro di pulizia del 1 settembre
`MidasLib` è stata rimossa da `MDHelpViewer.dpr` insieme a `Vcl.DBCtrls`, sul
presupposto che il viewer non avesse dipendenze database: vero per il codice
proprietario, falso per il motore di traduzione. I «~435 KB in meno»
sull'eseguibile registrati allora erano esattamente Midas linkata staticamente
(`midas.dll` pesa 434 KB).

Senza `MidasLib`, `OpenCDS` cerca una `midas.dll` esterna. Su una macchina di
sviluppo la trova sempre — la installa RAD Studio sia nella propria cartella
`bin` sia nella directory di sistema — quindi la dipendenza resta invisibile:
il processo in esecuzione mostra `midas.dll` caricata da
`Embarcadero\Studio\37.0\bin`. Su una macchina pulita l'eccezione parte dentro
l'`initialization`, dove nessun `try/except` può intercettarla, e il RTL la
riporta solo come Runtime error 217.

**Verificato in UAT:** copiando `midas.dll` a 32 bit accanto all'eseguibile, la
2.5.6 riparte senza altre modifiche.

### Correzione

| Dove | Modifica |
|---|---|
| `Source/MDHelpViewer.dpr` | `MidasLib` reinserita in testa alla `uses`, prima di `CBMultiLanguage` e di `MDHelpView.Main`: va inizializzata prima che l'`initialization` di quest'ultima apra il ClientDataSet. Il commento in loco spiega perché la dipendenza non si vede in sviluppo, così la unit non viene rimossa di nuovo come «inutilizzata» |

L'eseguibile torna autosufficiente e cresce di ~435 KB; il Setup non deve
deployare `midas.dll`.

### Osservazioni non modificate

- **Il `.dpr` non ha alcun `try/except`.** Qualunque eccezione
  nell'`initialization` delle unit, o nelle `Application.CreateForm` del blocco
  principale, continua a presentarsi come Runtime error 217 senza indicazione
  della causa. Un `try/except` che conservasse il messaggio per mostrarlo prima
  di uscire era stato scritto e poi rimosso: la scelta è di non introdurre un
  percorso di avvio alternativo ora che la causa nota è tolta alla radice.
- **Le versioni 2.5.5 e 2.5.6 già distribuite non partono su macchina senza
  RAD Studio.** Il difetto è stato introdotto in 2.5.5 e corretto in 2.5.7.
- **Gli altri progetti Ethea non sono interessati:** MarkdownShellExtensions,
  SVGShellExtensions, SKIAShellExtensions e DelphiShellExtensions non usano
  CBMultiLanguage né `TClientDataSet`. Il Demo
  (`Demo/Source/MarkDownHelpViewerDemo.dpr`) ha già `MidasLib`.
- **Da tenere presente per il futuro:** questa dipendenza non compare in
  nessuna `uses` del codice proprietario, arriva da una libreria vendored.
  Prima di rimuovere una unit apparentemente inutilizzata come `MidasLib`
  conviene provare l'eseguibile su una macchina senza Delphi: è l'unico
  ambiente in cui il difetto si manifesta.

### Versione allineata a 2.5.7 (21 settembre 2026)

| Dove | Prima | Dopo |
|---|---|---|
| `Source/MDHelpViewer.dproj` (`FileVersion` e `VerInfo_Release`) | 2.5.6.0 | 2.5.7.0 |
| `Setup/MarkDownHelpViewerSetup.iss` (`MyAppVersion`) | 2.5.6 | 2.5.7 |
| `Packages/MarkDownViewerRegister.pas` (`PROJECT_VER`, visibile nell'IDE) | 2.5.6 | 2.5.7 |
| 9 × `Packages/*/MarkDownViewer.dproj` | 2.5.6.0 | 2.5.7.0 |
| 9 × `Packages/*/dclMarkDownViewer.dproj` | 2.5.6.0 | 2.5.7.0 |

Aggiornate anche le note di rilascio del `README.md`, con l'avviso che le
versioni 2.5.5 e 2.5.6 non sono utilizzabili su macchina senza RAD Studio.
