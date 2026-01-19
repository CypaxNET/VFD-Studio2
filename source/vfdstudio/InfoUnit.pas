unit InfoUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, ExtCtrls, StdCtrls, LCLTranslator,
  Buttons, HtmlView, HtmlGlobals, lclintf, Classes;
type

  { TInfoForm }

  TInfoForm = class(TForm)
    HtmlViewer1: THtmlViewer;
    InfoOkButton: TBitBtn;
    Image1: TImage;
    Memo1: TMemo;
    Shape1: TShape;
    procedure HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
    procedure Image1Click(Sender: TObject);
    procedure InfoOkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private

  public

  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.InfoOkButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TInfoForm.HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
begin
  OpenURL(SRC);
end;

procedure TInfoForm.Image1Click(Sender: TObject);
begin

end;

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  HtmlViewer1.LoadFromString(Memo1.Text);
end;

procedure TInfoForm.Memo1Change(Sender: TObject);
begin

end;

end.
