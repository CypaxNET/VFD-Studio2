unit InfoUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    Bevel1: TBevel;
    BitBtn1: TBitBtn;
    Image1: TImage;
    Memo1: TMemo;
    procedure Bevel1ChangeBounds(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private

  public

  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure TInfoForm.Bevel1ChangeBounds(Sender: TObject);
begin

end;

procedure TInfoForm.Memo1Change(Sender: TObject);
begin

end;

end.

