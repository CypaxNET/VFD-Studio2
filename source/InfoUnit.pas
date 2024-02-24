unit InfoUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, ExtCtrls, StdCtrls, LCLTranslator,
  Buttons, HtmlView, HTMLUn2, HtmlGlobals, lclintf;
type

  { TInfoForm }

  TInfoForm = class(TForm)
    Bevel1: TBevel;
    HtmlViewer1: THtmlViewer;
    InfoOkButton: TBitBtn;
    Image1: TImage;
    Memo1: TMemo;
    procedure HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
    procedure InfoOkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  HtmlViewer1.LoadFromString(Memo1.Text);
end;

end.
