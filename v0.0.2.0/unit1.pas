(*
  This Demo show you possible ways to call InputQueryEx correct.
*)

unit Unit1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  InpQryEx; // <- this unit must be added

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Counter: Integer;
  IQE: TInputQueryEx; // <- this is our new object
begin
  // Create an instance with Parent-Form as Argument
  IQE := TInputQueryEx.Create(Form1);
  // awake Labels with life, describe them
  IQE.TextLabels := TStringArray(['hello', 'world', 'i am', 'dynamic', 'done with', 'freepascal' + sLineBreak + 'and' + sLineBreak + 'codetyphon']);
  // that is the most basic setup to get started
  // all what follow is optional or auto-set
  // (just to explain possibilities)
  //
  // Window Caption Icon
  IQE.IconCaption := Form1.Icon;
  // Window Caption Text
  IQE.Caption := 'Test Caption';
  // do not draw [X] close button
  IQE.SupressExit := False;
  // Big Title Icon
  IQE.IconTitle := Form1.Icon;
  // Big Title Text
  IQE.Title := 'Test Title';
  // Turn Icon support off
  IQE.IconSupress := False;
  // Set Edit Box default text (should be same entry count as textlabels)
  IQE.TextEdits := TStringArray(['default #0', 'testing', '42', '<>', '@', '#EOF']);
  // background color
  IQE.Color := Form1.Color;
  // font color
  IQE.ColorFont := Form1.Font.Color;
  // button caption
  IQE.ButtonCaption := '&Do it!';
  // font color override for invert
  IQE.UseInverted := True;
  // Do something with Result
  if IQE.Execute then
    begin
      Memo1.Clear;
      for Counter := Low(IQE.Results) to High(IQE.Results) do
        Memo1.Lines.Add(IQE.Results[Counter]);
    end;
  // Free up memory
  IQE.Free;
end;

end.

