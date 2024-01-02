{******************************************************************************}
{                                                                              }
{       CBMultiLanguage                                                        }
{       (Localization engine)                                                  }
{                                                                              }
{       Copyright (c) 2005-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/CBMultiLanguage                            }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit CBMultiLanguageVCL;

interface

uses
  System.Classes
  , System.SysUtils
  , CBMultiLanguage
  ;

type
  TCDSTranslatorVCL = class(TCDSTranslator)
  private
    function ClearShortcut(const Caption: string): string;
  protected
    function ReadTrxProperty(Component: TComponent;
      const PropertyName: string; out PropertyValue: string): boolean; override;
    procedure WriteTrxProperty(Component: TComponent;
      const PropertyName, TranslatedValue: string); override;
  end;

  TFormTranslatorVCL = class(TFormTranslator)
  protected
    function CreateInternalCDSTranslator(const FileName : string;
      Language : TAppLanguage) : TCDSTranslator; override;
  end;

implementation

uses
  System.TypInfo
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.ComCtrls
  , Vcl.DbGrids
  , Vcl.Menus
  , Vcl.ActnMan
  , Vcl.ActnMenus
  , Vcl.CategoryButtons
  , Vcl.WinXCtrls
  , Vcl.ButtonGroup
  ;

function GetMenuActionBar(ActionBars : TActionBars) : TActionBarItem;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to ActionBars.Count -1 do
  begin
    if TActionBarItem(ActionBars.Items[i]).ActionBar is TCustomActionMenuBar then
    begin
      Result := TActionBarItem(ActionBars.Items[i]);
      break;
    end;
  end;
end;

{ TCDSTranslatorVCL }

function TCDSTranslatorVCL.ClearShortcut(const Caption: string): string;
begin
  //Pulisco la caption dallo shortcut
  Result := StringReplace(Caption, '&', '', [rfReplaceAll]);
end;

function TCDSTranslatorVCL.ReadTrxProperty(Component: TComponent;
  const PropertyName: string; out PropertyValue: string): boolean;
var
  Strings : TStrings;
  BoundLabel : TCustomLabel;
  ListColumns : TListColumns;
  StatusPanels : TStatusPanels;
  DbGridColumns : TDBGridColumns;
  i : integer;
  ActionBars : TActionBars;
  MenuActionBar : TActionBarItem;
  ButtonCategories: TButtonCategories;
  ButtonGroupItems: TGrpButtonItems;
  StateCaptions: TToggleSwitchStateCaptions;
begin
  Result := True;
  case TTrxKind(GetCDSTrxPropList.FieldTrxKind.AsInteger) of
    txString :
    begin
      PropertyValue := GetStrProp(Component,PropertyName);
    end;
    txStrings :
    begin
      Strings := GetObjectProp(Component,PropertyName,TStrings) as TStrings;
      if Assigned(Strings) then
        PropertyValue := Strings.Text
      else
        Result := False;
    end;
    txBoundLabel :
    begin
      BoundLabel := GetObjectProp(Component,PropertyName,TCustomLabel) as TCustomLabel;
      if Assigned(BoundLabel) then
        PropertyValue := BoundLabel.Caption
      else
        Result := False;
    end;
    txListView :
    begin
      ListColumns := GetObjectProp(Component,PropertyName,TListColumns) as TListColumns;
      if Assigned(ListColumns) then
      begin
        PropertyValue := '';
        for i := 0 to ListColumns.Count -1 do
          PropertyValue := PropertyValue + ListColumns.Items[i].Caption+sLineBreak;
      end
      else
        Result := False;
    end;
    txStatusPanel :
    begin
      StatusPanels := TStatusPanels(GetObjectProp(Component,PropertyName,TStatusPanels));
      if Assigned(StatusPanels) then
      begin
        PropertyValue := '';
        for i := 0 to StatusPanels.Count -1 do
          PropertyValue := PropertyValue + StatusPanels.Items[i].Text+sLineBreak;
      end
      else
        Result := False;
    end;
    txDbGrid :
    begin
      DbGridColumns := TDbGridColumns(GetObjectProp(Component,PropertyName,TDbGridColumns));
      if Assigned(DbGridColumns) then
      begin
        PropertyValue := '';
        for i := 0 to DbGridColumns.Count -1 do
          PropertyValue := PropertyValue + DbGridColumns.Items[i].Title.Caption+sLineBreak;
      end
      else
        Result := False;
    end;
    txButtonGroup :
    begin
      ButtonGroupItems := TGrpButtonItems(GetObjectProp(Component,PropertyName,TGrpButtonItems));
      if Assigned(ButtonGroupItems) then
      begin
        PropertyValue := '';
        for i := 0 to ButtonGroupItems.Count -1 do
          PropertyValue := PropertyValue + ClearShortcut(ButtonGroupItems.Items[i].Caption)+sLineBreak;
      end
      else
        Result := False;
    end;
    txButtonCategories :
    begin
      ButtonCategories := TButtonCategories(GetObjectProp(Component,PropertyName,TButtonCategories));
      if Assigned(ButtonCategories) then
      begin
        PropertyValue := '';
        for i := 0 to ButtonCategories.Count -1 do
          PropertyValue := PropertyValue + ClearShortcut(ButtonCategories.Items[i].Caption)+sLineBreak;
      end
      else
        Result := False;
    end;
    txToggleSwitch :
    begin
      StateCaptions := TToggleSwitchStateCaptions(GetObjectProp(Component,PropertyName,TToggleSwitchStateCaptions));
      if Assigned(StateCaptions) then
      begin
        PropertyValue := StateCaptions.CaptionOn+sLineBreak+StateCaptions.CaptionOff;
      end
      else
        Result := False;
    end;
    txActionBars :
    begin
      ActionBars := TActionBars(GetObjectProp(Component,PropertyName,TActionBars));
      MenuActionBar := GetMenuActionBar(ActionBars);
      if MenuActionBar <> nil then
      begin
        PropertyValue := '';
        for i := 0 to MenuActionBar.Items.Count -1 do
          PropertyValue := PropertyValue + ClearShortcut(MenuActionBar.Items[i].Caption)+sLineBreak;
      end
      else
        Result := False;
    end;
  else
    Result := False;
  end;
end;

procedure TCDSTranslatorVCL.WriteTrxProperty(Component: TComponent;
  const PropertyName, TranslatedValue: string);
var
  Strings : TStrings;
  BoundLabel : TCustomLabel;
  ListColumns : TListColumns;
  StatusPanels : TStatusPanels;
  DbGridColumns : TDBGridColumns;
  i : integer;
  ActionBars : TActionBars;
  MenuActionBar : TActionBarItem;
  ButtonCategories: TButtonCategories;
  ButtonGroupItems: TGrpButtonItems;
  StateCaptions: TToggleSwitchStateCaptions;
begin
  case TTrxKind(GetCDSTrxPropList.FieldTrxKind.AsInteger) of
    txString :
    begin
      SetStrProp(Component,PropertyName,TranslatedValue);
    end;
    txStrings :
    begin
      Strings := TStrings(GetObjectProp(Component,PropertyName,TStrings));
      if Assigned(Strings) then
        Strings.Text := TranslatedValue;
    end;
    txBoundLabel :
    begin
      BoundLabel := TCustomLabel(GetObjectProp(Component,PropertyName,TCustomLabel));
      if Assigned(BoundLabel) then
        BoundLabel.Caption := TranslatedValue;
    end;
    txListView :
    begin
      ListColumns := TListColumns(GetObjectProp(Component,PropertyName,TListColumns));
      if Assigned(ListColumns) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to Strings.Count -1 do
            if i < ListColumns.Count then
              ListColumns[i].Caption := Strings[i];
        Finally
          Strings.Free;
        End;
      end;
    end;
    txStatusPanel :
    begin
      StatusPanels := TStatusPanels(GetObjectProp(Component,PropertyName,TStatusPanels));
      if Assigned(StatusPanels) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to Strings.Count -1 do
            if i < StatusPanels.Count then
              StatusPanels[i].Text := Strings[i];
        Finally
          Strings.Free;
        End;
      end;
    end;
    txDbGrid :
    begin
      DbGridColumns := TDbGridColumns(GetObjectProp(Component,PropertyName,TDbGridColumns));
      if Assigned(DbGridColumns) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to DbGridColumns.Count -1 do
          begin
            if i < Strings.Count then
            begin
              DbGridColumns[i].Title.Caption := Strings[i];
            end;
          end;
        Finally
          Strings.Free;
        End;
      end;
    end;
    txButtonCategories :
    begin
      ButtonCategories := TButtonCategories(GetObjectProp(Component,PropertyName,TButtonCategories));
      if Assigned(ButtonCategories) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to ButtonCategories.Count -1 do
          begin
            if i < Strings.Count then
            begin
              ButtonCategories.Items[i].Caption := Strings[i];
            end;
          end;
        Finally
          Strings.Free;
        End;
      end;
    end;
    txButtonGroup :
    begin
      ButtonGroupItems := TGrpButtonItems(GetObjectProp(Component,PropertyName,TGrpButtonItems));
      if Assigned(ButtonGroupItems) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to ButtonGroupItems.Count -1 do
          begin
            if i < Strings.Count then
            begin
              ButtonGroupItems.Items[i].Caption := Strings[i];
            end;
          end;
        Finally
          Strings.Free;
        End;
      end;
    end;
    txToggleSwitch :
    begin
      StateCaptions := TToggleSwitchStateCaptions(GetObjectProp(Component,PropertyName,TToggleSwitchStateCaptions));
      if Assigned(StateCaptions) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          if Strings.Count > 0 then
            StateCaptions.CaptionOn := Strings[0];
          if Strings.Count > 1 then
            StateCaptions.CaptionOff := Strings[1];
        Finally
          Strings.Free;
        End;
      end;
    end;
    txActionBars :
    begin
      ActionBars := TActionBars(GetObjectProp(Component,PropertyName,TActionBars));
      if Assigned(ActionBars) then
        MenuActionBar := GetMenuActionBar(ActionBars)
      else
        MenuActionBar := nil;
      if Assigned(MenuActionBar) then
      begin
        Strings := TStringList.Create;
        Try
          Strings.Text := TranslatedValue;
          for i := 0 to MenuActionBar.Items.Count -1 do
          begin
            if Strings.Count > i then
              MenuActionBar.Items[i].Caption := Strings[i]
            else
              MenuActionBar.Items[i].Caption := '';
          end;    
        Finally
          Strings.Free;
        End;
      end;
    end;
  end;
end;

{ TFormTranslatorVCL }

function TFormTranslatorVCL.CreateInternalCDSTranslator(
  const FileName: string; Language: TAppLanguage): TCDSTranslator;
begin
  Result := TCDSTranslatorVCL.CreateCDSTranslator(self,FileName,TranslateLang);
end;

end.
