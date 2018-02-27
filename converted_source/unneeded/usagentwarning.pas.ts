// unit usagentwarning

from conversion_common import *
import usdomain
import delphi_compatability


export class TAgentWarningForm {
    Label1: TLabel = new TLabel()
    dontShowThisDialogAgain: TCheckBox = new TCheckBox()
    okButton: TButton = new TButton()
    Label2: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Label4: TLabel = new TLabel()
    TAgentWarningForm.prototype = new TForm()
    TAgentWarningForm.prototype.constructor = TAgentWarningForm
    
    //$R *.DFM
    okButtonClick(Sender: TObject): void {
        usdomain.domain.options.suppressAgentNotPresentWarning = this.dontShowThisDialogAgain.Checked
        this.ModalResult = mrOK
    }
    
}

