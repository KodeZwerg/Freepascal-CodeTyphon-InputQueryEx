(*

Project: InputQueryEx
Version: 0.0.2.0
Authors: KodeZwerg, NiteHawk
Copyright: (c) by KodeZwerg
Language: ObjectPascal
Compiler: Freepascal
IDE: CodeTyphon
CrossPlatform: Tested successful on Windows and Linux.
License: Unlicense License
Limits: I offer you full source, feel free to fit to your needs or contribute!
 You are permitted to use in any form you like, Freeware/Commercial whatever.
 A *thank you* would be nice ;-)
Bugs: Can be send to GitHub, thanks!

Description:
  With this object you can dynamic create anytime a nicer InputQuery.
  Fully autosized, you do not need to care,
  it offer customized Icon usage for Caption and in Window,
  Caption text and a big intro header,
  in theory supports every possible way how you could request something,
  even different types can be requested,
  you just need to add example into label text
  everything will be returned as a stringarray where you need to create
  own parser to support your requested information at fixed index position.
  (hint: use constant sLineBreak for multiline labels.)

Download: https://github.com/KodeZwerg/Freepascal-CodeTyphon-InputQueryEx
Contact: https://discord.gg/afM3S425u8

History:
first public release
  initiator KodeZwerg posted an example demo which dynamically generates a window

refactored non public by NiteHawk
  internal "array ofs" is now own class
  implemented "Getter" for new workflow

second public release
  adopted NiteHawks code and transported the function to a TInputQueryEx() class to play with.
  see demo for a short usage.
  ( instead using endless long command lines to call method,
    all handy as properties ;-) )


planned:
  learn how-to make this a compoment to be dropped on formular.
  i guess to have it as a real designer component i need to learn and rewrite all again ;-)
  (editors for dynamic arrays...)
*)
unit InpQryEx;

{$mode ObjFPC}

interface

uses
  Graphics, SysUtils, Dialogs,
  Classes, Forms, Controls, StdCtrls, ExtCtrls, Buttons;

type
  TInputQueryEx = class(TComponent)
  strict private
    fSupressExit: Boolean;
    fCaption: string;
    fTitle: string;
    fButtonCaption: string;
    fIconSupress: Boolean;
    fColor: TColor;
    fUseInverted: Boolean;
    fColorFont: TColor;
    fIconCaption: TIcon;
    fIconTitle: TIcon;
    fTextLabels: TStringArray;
    fTextEdits: TStringArray;
    fResults: TStringArray;
    fOwner: TForm;
  protected
    function InputQueryEx: Boolean;
  private
    procedure SetTextLabels(const aValues: TStringArray);
    procedure SetTextEdits(const aValues: TStringArray);
  public
    constructor Create(const AOwner: TForm); overload;
//    destructor Destroy; overload;
    function Execute: Boolean;  overload;
  public
    property SupressExit: Boolean read fSupressExit write fSupressExit;
    property Caption: string read fCaption write fCaption;
    property IconCaption: TIcon read fIconCaption write fIconCaption;
    property Title: string read fTitle write fTitle;
    property IconSupress: Boolean read fIconSupress write fIconSupress;
    property IconTitle: TIcon read fIconTitle write fIconTitle;
    property Color: TColor read fColor write fColor;
    property UseInverted: Boolean read fUseInverted write fUseInverted;
    property ColorFont: TColor read fColorFont write fColorFont;
    property TextLabels: TStringArray read fTextLabels write SetTextLabels;
    property TextEdits: TStringArray read fTextEdits write SetTextEdits;
    property ButtonCaption: string read fButtonCaption write fButtonCaption;
    property Results: TStringArray read fResults;
  end;

implementation

type
  // child panel that combines label, edit field and separator
  TCustomizedDialogPanel = class(TCustomPanel)
  private
    FLabel: TLabel;
    FEdit: TEdit;
    FBevel: TBevel;
  public
    constructor Create(AOwner: TComponent; ACaption: string; AEdit: string); reintroduce;
    function GetText: string;
  end;


{ TCustomizedDialogPanel }

constructor TCustomizedDialogPanel.Create(AOwner: TComponent; ACaption: string; AEdit: string);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;

  // add label
  FLabel := TLabel.Create(Self);
  FLabel.Caption := ACaption;
  FLabel.Align := alTop;
  FLabel.Alignment := taCenter;
  FLabel.BorderSpacing.Around := 10;
  FLabel.Parent := Self;

  // add edit field
  FEdit := TEdit.Create(Self);
  FEdit.Align := alTop;
  FEdit.Alignment := taCenter;
  FEdit.BorderSpacing.Around := 10;
  FEdit.Color := clWhite;
  FEdit.Font.Color := clBlack;
  FEdit.Top := FLabel.Top + FLabel.Height;
  FEdit.Text := AEdit;
  FEdit.Parent := Self;

  // add bevel (separator)
  FBevel := TBevel.Create(Self);
  FBevel.Align := alBottom;
  FBevel.Shape := bsBottomLine;
  FBevel.BorderSpacing.Left := 30;
  FBevel.BorderSpacing.Right := 30;
  FBevel.Height := 2;
  FBevel.Parent := Self;

  AutoSize := True;
end;

function TCustomizedDialogPanel.GetText: string;
begin
  Result := FEdit.Text; // retrieve text from edit control
end;

{ InputQueryEx }

function TInputQueryEx.InputQueryEx: Boolean;
  function SwapColor(AColor: TColor): TColor; inline;
  begin
    Result := RGBToColor(Blue(AColor), Green(AColor), Red(AColor));
  end;

var
  Dlg: TForm;
  DlgLbl: TLabel;
  DlgBvl: TBevel;
  DlgPnl: TPanel;
  DlgImg: TImage;
  DlgPanels: array of TCustomizedDialogPanel;
  DlgBtn: TBitBtn;
  Count, i: Integer;
begin
  Result := False;
  Count := -1;
  if fTextLabels <> nil then
    Count := Length(fTextLabels);
  if Count <= 0 then
    Exit(False);

  if fOwner <> nil then
    Dlg := TForm.Create(fOwner)
    else
    Dlg := TForm.Create(nil);
  try
    // setup dialog basics
    Dlg.Caption := fCaption;
    Dlg.FormStyle := fsStayOnTop;
    Dlg.Position := poOwnerFormCenter;
    if not fIconSupress then
      Dlg.BorderStyle := bsSingle
      else
      Dlg.BorderStyle := bsDialog;
    if fSupressExit then
      Dlg.BorderIcons := []
      else
      Dlg.BorderIcons := [biSystemMenu];
    if not fIconSupress then
      Dlg.Icon := fIconCaption;
    Dlg.Color := fColor;

    // add panel to hold icon and description
    DlgPnl := TPanel.Create(Dlg);
    DlgPnl.Parent := Dlg;
    DlgPnl.Align := alTop;
    DlgPnl.Caption := '';
    DlgPnl.Height := 40;
    DlgPnl.BorderSpacing.Around := 5;
    DlgPnl.BorderSpacing.Top := 10;
    DlgPnl.BevelOuter := bvNone;
    DlgPnl.Color := fColor;
    DlgPnl.Top := 0;

    // add icon into panel
    DlgImg := TImage.Create(Dlg);
    if not fIconSupress then
      begin
        DlgImg.Parent := DlgPnl;
        DlgImg.Align := alLeft;
        DlgImg.Width := fIconTitle.Width;
        DlgImg.Picture.Icon := fIconTitle;
        DlgImg.BorderSpacing.Left := 10;
      end;

    // add label into panel
    DlgLbl := TLabel.Create(Dlg);
    DlgLbl.Parent := DlgPnl;
    DlgLbl.Align := alClient;
    DlgLbl.Caption := fTitle;
    DlgLbl.Alignment := taCenter;
    DlgLbl.BorderSpacing.Around := 10;
    DlgLbl.Font.Style := [fsBold];
    DlgLbl.Transparent := True;
    DlgLbl.Color := fColor;
    if fUseInverted then
      DlgLbl.Font.Color := SwapColor(fColor)
      else
      DlgLbl.Font.Color := fColorFont;

    // add a line to seperate view
    DlgBvl := TBevel.Create(Dlg);
    DlgBvl.Parent := Dlg;
    DlgBvl.Align := alTop;
    DlgBvl.Shape := bsBottomLine;
    DlgBvl.BorderSpacing.Left := 10;
    DlgBvl.BorderSpacing.Right := 10;
    DlgBvl.Height := 2;
    DlgBvl.Top := 1;
    if fUseInverted then
      DlgBvl.Color := SwapColor(fColor)
      else
      DlgBvl.Color := fColorFont;

    // create and add subpanels, one for each label passed in ALabels
    SetLength(DlgPanels, Count - 1);
    for i := 0 to pred(Count) do begin
      if ((fTextEdits <> nil) and (Length(fTextEdits) >= i)) then
        DlgPanels[i] := TCustomizedDialogPanel.Create(Dlg, fTextLabels[i], fTextEdits[i])
        else
        DlgPanels[i] := TCustomizedDialogPanel.Create(Dlg, fTextLabels[i], '');
      if fUseInverted then
        DlgPanels[i].Font.Color := SwapColor(fColor)
        else
        DlgPanels[i].Font.Color := fColorFont;
      DlgPanels[i].Align := alTop;
      DlgPanels[i].Top := 5 + i;
      DlgPanels[i].Parent := Dlg;
    end;

    // add a button to dialog
    DlgBtn := TBitBtn.Create(Dlg);
    DlgBtn.Parent := Dlg;
    DlgBtn.Align := alTop;
    DlgBtn.ModalResult := mrOK;
    DlgBtn.Kind := bkOK;
    DlgBtn.Caption := fButtonCaption;
    DlgBtn.BorderSpacing.Bottom := 10;
    DlgBtn.BorderSpacing.Left := 40;
    DlgBtn.BorderSpacing.Right := 40;
    DlgBtn.BorderSpacing.Top := 5;
    DlgBtn.Top := High(Integer);

    // show dialog
    Dlg.AutoSize := True;
    Result := (DlgBtn.ModalResult = Dlg.ShowModal);

    // take results outside of dialog if button was pressed
    if DlgBtn.ModalResult = mrOK then
      begin
        SetLength(fResults, Count);
        for i := 0 to pred(Count) do
          fResults[i] := DlgPanels[i].GetText;
      end;

  finally
    // free things up
    DlgImg.Free;
    DlgLbl.Free;
    DlgPnl.Free;
    DlgBvl.Free;
    for i := 0 to pred(Count) do DlgPanels[i].Free;
    DlgBtn.Free;
    Dlg.Free;
  end;
end;


constructor TInputQueryEx.Create(const AOwner: TForm);
begin
  inherited Create(AOwner);
  // setup defaults
  fOwner := AOwner;
  fSupressExit := False;
  fCaption := '';
  fTitle := '';
  fButtonCaption := '&Confirm';
  fIconSupress := False;
  fColor := TColor(clBtnFace);
  fUseInverted := True;
  fColorFont := TColor(clBlack);
  fTextLabels := nil;
  fTextEdits := nil;
  fResults := nil;
  fIconCaption := TIcon.Create;
  fIconTitle := TIcon.Create;
  // reintroduce to automagical fill basics
  if fOwner <> nil then
    begin
      if fOwner.Caption <> '' then
        begin
          fCaption := fOwner.Caption;
          fTitle := fOwner.Caption;
        end;
      fColor := fOwner.Color;
      fColorFont := fOwner.Font.Color;
      if fOwner.Icon <> nil then
        begin
          fIconCaption := fOwner.Icon;
          fIconTitle := fOwner.Icon;
        end;
    end;
end;

procedure TInputQueryEx.SetTextLabels(const aValues: TStringArray);
var
  i: Integer;
begin
  SetLength(fTextLabels, Length(aValues));
  for i := Low(aValues) to High(aValues) do
    fTextLabels[i] := aValues[i];
end;

procedure TInputQueryEx.SetTextEdits(const aValues: TStringArray);
var
  i: Integer;
begin
  SetLength(fTextEdits, Length(aValues));
  for i := Low(aValues) to High(aValues) do
    fTextEdits[i] := aValues[i];
end;

function TInputQueryEx.Execute: Boolean;
begin
  Result := InputQueryEx;
end;

{
destructor TInputQueryEx.Destroy; overload;
begin
  fIconCaption.Free;
  fIconTitle.Free;
  Inherited Destroy;
end;
}

end.

end.
