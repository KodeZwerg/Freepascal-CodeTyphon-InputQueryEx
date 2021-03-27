unit Unit1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

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

function InputQueryEx(const AColor: TColor; const AIcon: TIcon; const ACaption, ATitle: String; const ALabels: Array of String): TStringArray;
  function SwapColor(AColor: TColor): TColor; inline;
  begin
    Result := RGBToColor(Blue(AColor), Green(AColor), Red(AColor));
  end;
var
  Dlg: TForm;
  DlgLblArr: Array of TLabel;
  DlgEdtArr: Array of TEdit;
  DlgBvlArr: Array of TBevel;
  DlgLbl: TLabel;
  DlgBvl: TBevel;
  DlgPnl: TPanel;
  DlgImg: TImage;
  DlgBtn: TBitBtn;
  Counter: Integer;
begin
  if Length(ALabels) = 0 then
    Exit;
  Result := nil;
  Dlg := TForm.Create(Application.MainForm);
  try
    // setup dialog basics
    Dlg.Caption := ACaption;
    Dlg.FormStyle := fsStayOnTop;
    Dlg.Position := poOwnerFormCenter;
    Dlg.BorderStyle := bsSingle;
    Dlg.BorderIcons := [biSystemMenu];
    Dlg.Icon := AIcon;
    Dlg.Color := AColor;

    // add panel to hold icon and description
    DlgPnl := TPanel.Create(Dlg);
    DlgPnl.Parent := Dlg;
    DlgPnl.Align := alTop;
    DlgPnl.Caption := '';
    DlgPnl.Height := 40;
    DlgPnl.BorderSpacing.Around := 5;
    DlgPnl.BorderSpacing.Top := 10;
    DlgPnl.BevelOuter := bvNone;
    DlgPnl.Color := AColor;
    DlgPnl.Top := 0;

    // add icon into panel
    DlgImg := TImage.Create(Dlg);
    DlgImg.Parent := DlgPnl;
    DlgImg.Align := alLeft;
    DlgImg.Width := AIcon.Width;
    DlgImg.Picture.Icon := AIcon;
    DlgImg.BorderSpacing.Left := 10;

    // add label into panel
    DlgLbl := TLabel.Create(Dlg);
    DlgLbl.Parent := DlgPnl;
    DlgLbl.Align := alClient;
    DlgLbl.Caption := ATitle;
    DlgLbl.Alignment := taCenter;
    DlgLbl.BorderSpacing.Around := 10;
    DlgLbl.Font.Style := [fsBold];
    DlgLbl.Color := AColor;
    DlgLbl.Font.Color := SwapColor(AColor);

    // add a line to seperate view
    DlgBvl := TBevel.Create(Dlg);
    DlgBvl.Parent := Dlg;
    DlgBvl.Align := alTop;
    DlgBvl.Shape := bsBottomLine;
    DlgBvl.BorderSpacing.Left := 10;
    DlgBvl.BorderSpacing.Right := 10;
    DlgBvl.Height := 2;
    DlgBvl.Top := 1;
    DlgBvl.Color := SwapColor(AColor);

    SetLength(DlgLblArr, Length(ALabels) - 1);
    SetLength(DlgEdtArr, Length(ALabels) - 1);
    SetLength(DlgBvlArr, Length(ALabels) - 1);
    Counter := 0;
    for Counter := Low(ALabels) to High(ALabels) do
    begin
      // add a label to dialog
      DlgLblArr[Counter] := TLabel.Create(Dlg);
      DlgLblArr[Counter].Parent := Dlg;
      DlgLblArr[Counter].Parent := Dlg;
      DlgLblArr[Counter].Caption := ALabels[Counter];
      DlgLblArr[Counter].Align := alTop;
      DlgLblArr[Counter].Alignment := taCenter;
      DlgLblArr[Counter].BorderSpacing.Around := 10;
      DlgLblArr[Counter].Color := AColor;
      DlgLblArr[Counter].Font.Color := SwapColor(AColor);
      DlgLblArr[Counter].Top := (3 * Counter) + 3;

      // add a edit box to dialog
      DlgEdtArr[Counter] := TEdit.Create(Dlg);
      DlgEdtArr[Counter].Parent := Dlg;
      DlgEdtArr[Counter].Align := alTop;
      DlgEdtArr[Counter].Alignment := taCenter;
      DlgEdtArr[Counter].Text := '';
      DlgEdtArr[Counter].BorderSpacing.Around := 10;
      DlgEdtArr[Counter].Color := clWhite;
      DlgEdtArr[Counter].Font.Color := clBlack;
      DlgEdtArr[Counter].Top := (3 * Counter) + 4;

      // add a line to seperate view
      DlgBvlArr[Counter] := TBevel.Create(Dlg);
      DlgBvlArr[Counter].Parent := Dlg;
      DlgBvlArr[Counter].Align := alTop;
      DlgBvlArr[Counter].Shape := bsBottomLine;
      DlgBvlArr[Counter].BorderSpacing.Left := 30;
      DlgBvlArr[Counter].BorderSpacing.Right := 30;
      DlgBvlArr[Counter].Height := 2;
      DlgBvlArr[Counter].Color := SwapColor(AColor);
      DlgBvlArr[Counter].Top := (3 * Counter) + 5;
    end;

    // add a button to dialog
    DlgBtn := TBitBtn.Create(Dlg);
    DlgBtn.Parent := Dlg;
    DlgBtn.Align := alTop;
    DlgBtn.ModalResult := mrOk;
    DlgBtn.Kind := bkOK;
    DlgBtn.Caption := '&Confirm';
    DlgBtn.BorderSpacing.Around := 10;
    DlgBtn.BorderSpacing.Left := 40;
    DlgBtn.BorderSpacing.Right := 40;
    DlgBtn.BorderSpacing.Top := 5;
    DlgBtn.Top := High(Integer);

    // show dialog
    Dlg.AutoSize := True;
    DlgBtn.ModalResult := Dlg.ShowModal;

    // take results outside of dialog if button was pressed
    if DlgBtn.ModalResult = mrOK then
    begin
      SetLength(Result, Length(ALabels));
      for Counter := Low(ALabels) to High(ALabels) do
        Result[Counter] := DlgEdtArr[Counter].Text;
    end;

  finally
    // free things up
    DlgPnl.Free;
    DlgImg.Free;
    DlgLbl.Free;
    DlgBvl.Free;
    DlgBtn.Free;
    for Counter := Low(ALabels) to High(ALabels) do
    begin
      DlgLblArr[Counter].Free;
      DlgEdtArr[Counter].Free;
      DlgBvlArr[Counter].Free;
    end;
    Dlg.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Results: TStringArray;
  Counter: Integer;
begin
  Memo1.Clear;
  Results := InputQueryEx(clBtnFace, Application.MainForm.Icon, 'Test Caption', 'Test Description', ['hello', 'world', 'i am', 'dynamic', 'done with', 'freepascal']);
  for Counter := Low(Results) to High(Results) do
    Memo1.Lines.Add(Results[Counter]);
end;

end.

