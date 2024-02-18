unit SettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, LCLTranslator, Spin, Registry;

type
  TOkEvent = procedure of object;
  TAbortEvent = procedure of object;
  TColorChangeEvent = procedure(AColor: TColor) of object;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    AbortButton: TBitBtn;
    ApplicationGroupBox: TGroupBox;
    BrightListRadioButton: TRadioButton;
    BrightnessControlLabel: TLabel;
    BrightnessPercentLabel: TLabel;
    BrightnessTrackBar: TTrackBar;
    BrightSettingsRadioButton: TRadioButton;
    ClearOnCloseCheckBox: TCheckBox;
    ColorButton: TColorButton;
    DspBox: TGroupBox;
    DspTypeCombo: TComboBox;
    IconGroupBox: TGroupBox;
    IdleLevelLabel: TLabel;
    IdlePercentageLabel: TLabel;
    IdleTrackBar: TTrackBar;
    IfCfgCombo: TComboBox;
    IfCfgLabel: TLabel;
    IfLabel: TLabel;
    ImageList1: TImageList;
    ImageList2: TImageList;
    InterfaceCombo: TComboBox;
    LangDeButton: TBitBtn;
    LangEnButton: TBitBtn;
    LangItButton: TBitBtn;
    LanguageGroupBox: TGroupBox;
    OkButton: TBitBtn;
    OnlyOnIdleBox: TCheckBox;
    OptionsBox: TGroupBox;
    Panel1: TPanel;
    PreviewColorLabel: TLabel;
    ResLabel: TLabel;
    ResXSpinEdit: TSpinEdit;
    ResYSpinEdit: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    StartMinimizedCheckBox: TCheckBox;
    TypeLabel: TLabel;
    XLabel: TLabel;
    procedure AbortButtonClick(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure ColorButtonColorChanged(Sender: TObject);
    procedure DspTypeComboChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTrackBarChange(Sender: TObject);
    procedure LangDeButtonClick(Sender: TObject);
    procedure LangEnButtonClick(Sender: TObject);
    procedure LangItButtonClick(Sender: TObject);
    procedure LanguageGroupBoxResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
  protected
    FOkEvent: TOkEvent;
    FAbortEvent: TAbortEvent;
    FColorChangeEvent: TColorChangeEvent;
    FLanguage: string;
    FIconIndex: Integer;
  public
    property IconIndex: Integer read FIconIndex write FIconIndex;
    property Language: string read FLanguage write FLanguage;
    property OnOkPressed: TOkEvent read FOkEvent write FOkEvent;
    property OnAbortPressed: TAbortEvent read FAbortEvent write FAbortEvent;
    property OnColorChange: TColorChangeEvent read FColorChangeEvent write FColorChangeEvent;

    function GetSerialPortNames: TStringList;

  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.lfm}

{ TConfigForm }


procedure TConfigForm.LanguageGroupBoxResize(Sender: TObject);
begin
  LanguageGroupBox.ChildSizing.ControlsPerLine:= LanguageGroupBox.Width div (LangDeButton.Width + LanguageGroupBox.ChildSizing.HorizontalSpacing);
end;

procedure TConfigForm.OkButtonClick(Sender: TObject);
begin
  if (Assigned(FOkEvent)) then
    FOkEvent;
  Close;
end;

procedure TConfigForm.SpeedButton1Click(Sender: TObject);
begin
 FIconIndex:= 0;
end;

procedure TConfigForm.SpeedButton2Click(Sender: TObject);
begin
  FIconIndex:= 1;
end;

procedure TConfigForm.SpeedButton3Click(Sender: TObject);
begin
  FIconIndex:= 2;
end;

procedure TConfigForm.SpeedButton4Click(Sender: TObject);
begin
  FIconIndex:= 3;
end;

procedure TConfigForm.SpeedButton5Click(Sender: TObject);
begin
  FIconIndex:= 4;
end;

procedure TConfigForm.AbortButtonClick(Sender: TObject);
begin
  if (Assigned(FAbortEvent)) then
    FAbortEvent;
  Close;
end;

procedure TConfigForm.BrightnessTrackBarChange(Sender: TObject);
begin
  BrightnessPercentLabel.Caption:= IntToStr(BrightnessTrackBar.Position) + '%';
end;

procedure TConfigForm.ColorButtonColorChanged(Sender: TObject);
begin
  if (Assigned(FColorChangeEvent)) then
    FColorChangeEvent(ColorButton.ButtonColor);
end;

procedure TConfigForm.DspTypeComboChange(Sender: TObject);
begin
  if (0 = DspTypeCombo.ItemIndex) then begin
    // NONE display has been selected
    InterfaceCombo.Enabled:= False;
    IfCfgCombo.Enabled:= False;
  end else begin
    InterfaceCombo.Enabled:= True;
    IfCfgCombo.Enabled:= True;
  end;

end;

procedure TConfigForm.FormShow(Sender: TObject);
var
  Ports: TStringList;
begin
  Ports := GetSerialPortNames;
  try
    InterfaceCombo.Items.Assign(Ports); // fill ComboBox with detected ports
    (*
    if (Ports.Count > 0) then
      InterfaceCombo.ItemIndex:= 0;
    *)
  finally
    Ports.Free;
  end;
  DspTypeComboChange(Self);
end;

procedure TConfigForm.IdleTrackBarChange(Sender: TObject);
begin
  IdlePercentageLabel.Caption:= IntTosTr(IdleTrackBar.Position) + '%';
end;

function TConfigForm.GetSerialPortNames: TStringList;
var
  Reg: TRegistry;
  AValue: UnicodeString;
begin
  Result:= TStringList.Create;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    begin
      for AValue in Reg.GetValueNames do begin
        if (rdString = Reg.GetDataType(AValue)) then
          Result.Add(UTF8Encode(Reg.ReadString(AValue)));
      end;
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
end;

procedure TConfigForm.LangDeButtonClick(Sender: TObject);
begin
  FLanguage:= 'de';
  SetDefaultLang(FLanguage);
end;

procedure TConfigForm.LangEnButtonClick(Sender: TObject);
begin
  FLanguage:= 'en';
  SetDefaultLang(FLanguage);
end;

procedure TConfigForm.LangItButtonClick(Sender: TObject);
begin
  FLanguage:= 'it';
  SetDefaultLang(FLanguage);
end;

end.

