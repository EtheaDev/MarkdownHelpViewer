unit MarkdownUnicodeUtils;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils;

{$IFNDEF FPC}
type
  UnicodeChar = char;
{$ENDIF}

function unicodeChars(s : String) : TArray<UnicodeChar>;

implementation

{$IFDEF FPC}
uses
  LazUTF8;
{$ENDIF}

{$IFDEF FPC}

function unicodeChars(s : String) : TArray<UnicodeChar>;
var
  i, c, l, cl : integer;
  ch : UnicodeChar;
  p: PChar;
begin
  l := length(s);
  SetLength(result, l); // maximum possible length
  i := 0;
  c := 1;
  p := @s[1];
  while l > 0 do
  begin
    ch := UnicodeChar(UTF8CodepointToUnicode(p, cl));
    result[i] := ch;
    inc(i);
    dec(l, cl);
    inc(p, cl);
  end;
  SetLength(result, i);
end;

{$ELSE}

function unicodeChars(s : String) : TArray<UnicodeChar>;
var
  i : Integer;
begin
  SetLength(result, length(s));
  for i := 1 to length(s) do
    result[i-1] := s[i];
end;

{$ENDIF}

end.

