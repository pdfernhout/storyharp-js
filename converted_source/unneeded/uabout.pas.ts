// unit uabout
// initial comment
// before interface and uses comment

from conversion_common import *
import ufilesupport
import uregister
import usdomain
import delphi_compatability

// var
let UnregisteredAboutForm: TUnregisteredAboutForm



export class TUnregisteredAboutForm {
    close: TButton = new TButton()
    registerIt: TButton = new TButton()
    noDistributeLabel: TLabel = new TLabel()
    readLicense: TButton = new TButton()
    TimePanel: TPanel = new TPanel()
    hoursLabel: TLabel = new TLabel()
    cancel: TButton = new TButton()
    whyRegister: TButton = new TButton()
    timeWarningLabel: TLabel = new TLabel()
    Label1: TLabel = new TLabel()
    Image1: TImage = new TImage()
    Label2: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Label4: TLabel = new TLabel()
    Label5: TLabel = new TLabel()
    versionLabel: TLabel = new TLabel()
    Label7: TLabel = new TLabel()
    Label8: TLabel = new TLabel()
    Label9: TLabel = new TLabel()
    registeredToLabel: TLabel = new TLabel()
    TUnregisteredAboutForm.prototype = new TForm()
    TUnregisteredAboutForm.prototype.constructor = TUnregisteredAboutForm
    
    //$R *.DFM
    initializeWithWhetherClosingProgram(closingProgram: boolean): void {
        if (usdomain.domain.playerOnly) {
            this.ClientHeight = this.TimePanel.Top - 1
            this.Caption = "Thank you for using the StoryHarp player!"
            this.registeredToLabel.Caption = "Player-only mode"
            this.whyRegister.Visible = false
            this.registerIt.Visible = false
            this.readLicense.Top = this.registerIt.Top
        }
        if (closingProgram) {
            this.close.Caption = "Quit"
        } else {
            this.close.Caption = "Close"
        }
        this.cancel.Visible = closingProgram
    }
    
    FormActivate(Sender: TObject): void {
        let timeBetween: TDateTime
        let smallHours: byte
        let minutes: byte
        let seconds: byte
        let milliseconds: byte
        let randomNumber: int
        let hours: int
        let i: int
        
        if (usdomain.domain.playerOnly) {
            return
        }
        this.ActiveControl = this.registerIt
        UNRESOLVED.randomize
        for (i = 0; i <= 100; i++) {
            UNRESOLVED.random
        }
        randomNumber = UNRESOLVED.random(2)
        switch (randomNumber) {
            case 0:
                this.close.Top = 4
                this.registerIt.Top = this.close.Top + this.close.Height + 3
                break
            case 1:
                this.registerIt.Top = 4
                this.close.Top = this.registerIt.Top + this.registerIt.Height + 3
                break
        this.hoursLabel.Caption = "You have been using StoryHarp for "
        timeBetween = usdomain.max((UNRESOLVED.Now - usdomain.domain.startTimeThisSession), 0) + usdomain.domain.accumulatedUnregisteredTime
        UNRESOLVED.DecodeTime(timeBetween, smallHours, minutes, seconds, milliseconds)
        hours = smallHours
        if (timeBetween >= 1.0) {
            hours = hours + trunc(timeBetween) * 24
        }
        if ((minutes < 1) && (hours < 1)) {
            // hoursLabel.caption := hoursLabel.caption + 'more than 24 hours.'
            this.hoursLabel.Caption = this.hoursLabel.Caption + "less than one minute."
        } else if ((minutes === 1) && (hours < 1)) {
            this.hoursLabel.Caption = this.hoursLabel.Caption + "one minute."
        } else if (hours < 1) {
            this.hoursLabel.Caption = this.hoursLabel.Caption + IntToStr(minutes) + " minutes."
        } else if (hours === 1) {
            this.hoursLabel.Caption = this.hoursLabel.Caption + IntToStr(hours) + " hour and " + IntToStr(minutes) + " minutes."
        } else {
            this.hoursLabel.Caption = this.hoursLabel.Caption + IntToStr(hours) + " hours and " + IntToStr(minutes) + " minutes."
        }
        if (hours >= 24.0) {
            this.timeWarningLabel.Font.Color = delphi_compatability.clGreen
            this.timeWarningLabel.Font.Style = {UNRESOLVED.fsBold, }
        }
    }
    
    closeClick(Sender: TObject): void {
        this.ModalResult = mrOK
    }
    
    registerItClick(Sender: TObject): void {
        if (uregister.RegistrationForm.ShowModal() === mrOK) {
            this.ModalResult = mrCancel
        }
    }
    
    hoursLabelClick(Sender: TObject): void {
        //var
        //  hours, minutes: smallint;
        //  hourString: string;
        // only for cfk testing -- remove later
        //
        //  if inputQuery('Testing', 'Enter number of hours', hourString) then
        //    hours := strToInt(hourString);
        //  if inputQuery('Testing', 'Enter number of minutes', hourString) then
        //    minutes := strToInt(hourString);
        //
        //  hoursLabel.caption := 'You have been evaluating StoryHarp for ';
        //  if hours >= 24 then
        //    hoursLabel.caption := hoursLabel.caption + 'more than 24 hours.'
        //  else if (minutes < 1) and (hours < 1) then
        //    hoursLabel.caption := hoursLabel.caption + 'less than one minute.'
        //  else if (minutes = 1) and (hours < 1) then
        //    hoursLabel.caption := hoursLabel.caption + 'one minute.'
        //  else if hours < 1 then
        //    hoursLabel.caption := hoursLabel.caption + intToStr(minutes) + ' minutes.'
        //  else if hours = 1 then
        //    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hour and ' + intToStr(minutes) + ' minutes.'
        //  else
        //    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hours and ' + intToStr(minutes) + ' minutes.';
        //  
    }
    
    readLicenseClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("License")
    }
    
    whyRegisterClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Why_register?")
    }
    
    FormCreate(Sender: TObject): void {
        this.versionLabel.Caption = ufilesupport.gVersionName
    }
    
}

// almost final comment
// very final comment
