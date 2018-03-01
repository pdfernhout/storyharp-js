import * as m from "mithril"

enum ContextWizardPages {
    kStartPage = 0,
    kContextsPage = 1,
    kDescriptionPage = 2,
    kFinishPage = 3,
}

export class ContextWizardView {
    domain: any
    wizardPage = ContextWizardPages.kStartPage

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    cancelClick() { console.log("cancelClick") }

    goBackClick() { this.wizardPage = Math.max(ContextWizardPages.kStartPage, this.wizardPage - 1) }

    goNextClick() { 
        if (this.wizardPage === ContextWizardPages.kFinishPage) {
            alert("Finished!!!")
        }
        this.wizardPage = Math.min(ContextWizardPages.kFinishPage, this.wizardPage + 1)
    }

    helpButtonClick() { console.log("helpButtonClick") }

    /*
    FormActivate(Sender: TObject): void {
        this.contextStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.commandStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.replyStartPageImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
        this.commandImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
    }
    
    initialize(): boolean {
        let result = false
        this.generated = false
        this.notebook.PageIndex = kStartPage
        this.NewContextsMemo.Text = ""
        result = true
        return result
    }
    
    goBackClick(Sender: TObject): void {
        if (this.notebook.PageIndex > kStartPage) {
            this.notebook.PageIndex = this.notebook.PageIndex - 1
        }
    }
    
    goNextClick(Sender: TObject): void {
        if (this.notebook.PageIndex === kFinishPage) {
            this.generateRules()
        } else if (this.notebook.PageIndex === kContextsPage) {
            if ((trim(this.NewContextsMemo.Text) === "")) {
                ShowMessage("You must enter one or more contexts to proceed.")
                return
            }
        } else if (this.notebook.PageIndex === kDescriptionPage) {
            if ((trim(this.DescribeEdit.Text) === "")) {
                ShowMessage("You must enter a command to be used to describe these contexts.")
                return
            }
        }
        if (this.notebook.PageIndex < kFinishPage) {
            this.notebook.PageIndex = this.notebook.PageIndex + 1
        }
    }
    
    notebookPageChanged(Sender: TObject): void {
        this.goBack.Enabled = this.notebook.PageIndex > kStartPage
        if (this.notebook.PageIndex !== kFinishPage) {
            this.goNext.Caption = "&Next >>"
        } else {
            this.goNext.Caption = "&Finish"
        }
    }
    
    cancelClick(Sender: TObject): void {
        this.ModalResult = mrCancel
    }
    
    generateRules(): void {
        let stream: TStringStream
        let line: string
        let previousContext: string
        let contextName: string
        let contextDescription: string
        let character: string
        let pipeRead: boolean
        let newRule: TSRule
        let position: TPoint
        let newRulesCommand: TSNewRulesCommand
        
        uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
        position = usruleeditorform.RuleEditorForm.goodPosition()
        stream = delphi_compatability.TStringStream().Create(this.NewContextsMemo.Text)
        previousContext = ""
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "new context wizard"
        try {
            character = stream.ReadString(1)
            while (character !== "") {
                line = ""
                contextName = ""
                contextDescription = ""
                pipeRead = false
                while (!((character === Character(13)) || (character === Character(10)) || (character === ""))) {
                    line = line + character
                    if (pipeRead) {
                        contextDescription = contextDescription + character
                    } else if (character === "|") {
                        pipeRead = true
                    } else {
                        contextName = contextName + character
                    }
                    character = stream.ReadString(1)
                }
                if (trim(contextName) !== "") {
                    if ((trim(this.DescribeEdit.Text) !== "")) {
                        //ShowMessage('name: ' + contextName);
                        //ShowMessage('description: ' + contextDescription);
                        newRule = usdomain.domain.world.newRule()
                        newRulesCommand.addRule(newRule)
                        usruleeditorform.RuleEditorForm.lastChoice = newRule
                        newRule.position = position
                        newRule.setContext(trim(contextName))
                        newRule.setCommand(trim(this.DescribeEdit.Text))
                        if (trim(contextDescription) !== "") {
                            newRule.setReply(trim(contextDescription))
                        } else {
                            newRule.setReply("There is nothing of interest here.")
                        }
                    }
                    previousContext = trim(contextName)
                    position.Y = position.Y + 60
                }
                while ((character === Character(13)) || (character === Character(10))) {
                    character = stream.ReadString(1)
                }
            }
        } finally {
            stream.free
        }
        if (newRulesCommand.rules.Count > 0) {
            usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        } else {
            newRulesCommand.free
        }
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.adjustScrollBars()
        this.ModalResult = mrOK
        this.generated = true
    }
    
    //
    //cave|You are in a big cave.
    //forest|You are in a lively forest.
    //spring|You are standing near a burbling spring.
    //
    //
    //well house|You are in a well house for a small spring.
    //grate|You are standing above a grate.
    //forest|You are wandering around in dense forest.
    //glade|You are in a forest glade.
    //stream|You are walking along a dry stream bed.
    //
    FormCloseQuery(Sender: TObject, CanClose: boolean): void {
        let result: byte
        
        if (this.generated) {
            return CanClose
        }
        if (trim(this.NewContextsMemo.Text) !== "") {
            result = MessageDialog("You are about to close this wizard" + chr(13) + "and lose any work done with it." + chr(13) + chr(13) + "Is this OK?", mtConfirmation, {mbYes, mbNo, }, 0)
            if (result !== mrYes) {
                CanClose = false
                return CanClose
            }
            uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
        }
        return CanClose
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Making_new_rules_using_the_new_contexts_wizard")
    }
    */

    view() {
        function caption(text: string) { return text }
        
        return m(".ContextWizardView",
            {
            },
            m("button.helpButton.TButton",
                {
                    onclick: () => this.helpButtonClick(),
                },
                caption("&Help"),
            ),
            m("button.goBack.TButton",
                {
                    onclick: () => this.goBackClick(),
                },
                caption("<< &Back"),
            ),
            m("button.goNext.TButton",
                {
                    onclick: () => this.goNextClick(),
                },
                this.wizardPage === ContextWizardPages.kFinishPage ? "Finish" : caption("&Next >>"),
            ),
            m("button.cancel.TButton",
                {
                    onclick: () => this.cancelClick(),
                },
                caption("&Cancel"),
            ),
            m("TNotebook.notebook.TNotebook",
                {
                },
                this.wizardPage !== ContextWizardPages.kStartPage ? [] : m("TPage.page1.TPage",
                    {
                    },
                    m("h1", "Start"),
                    m("img.Image1.TImage",
                        {
                        },
                    ),
                    m("div.Label1.TLabel",
                        {
                        },
                        "Welcome to the New Contexts Wizard!",
                    ),
                    m("div.Label2.TLabel",
                        {
                        },
                        "This wizard will help you quickly create a set of new rules based on contexts you enter.",
                    ),
                    m("img.commandStartPageImage.TImage",
                        {
                        },
                    ),
                    m("div.Label10.TLabel",
                        {
                        },
                        "A command is what you say to the computer.",
                    ),
                    m("img.contextStartPageImage.TImage",
                        {
                        },
                    ),
                    m("div.Label8.TLabel",
                        {
                        },
                        "A context is the single most important requirement for the user of a command, usually a physical location.",
                    ),
                    m("div.Label7.TLabel",
                        {
                        },
                        "You can enter a descriptive reply for each new context you enter here. The descriptive replies will be accessed with a common command such as \"look\".",
                    ),
                    m("img.replyStartPageImage.TImage",
                        {
                        },
                    ),
                    m("div.Label9.TLabel",
                        {
                        },
                        "A reply is what the computer says after you say a command.",
                    ),
                    m("div.Label3.TLabel",
                        {
                        },
                        "Click the Next button to begin.",
                    ),
                    m("div.Label26.TLabel",
                        {
                        },
                        "You can click Cancel at any time to close the wizard without making any new rules.",
                    ),
                ),
                this.wizardPage !== ContextWizardPages.kContextsPage ? [] : m("TPage.page2.TPage",
                    {
                    },
                    m("h1", "Enter Contexts"),
                    m("img.Image3.TImage",
                        {
                        },
                    ),
                    m("div.Label5.TLabel",
                        {
                        },
                        "Enter or paste the contexts you want to create in the area below, separating each context from its descriptive reply by a pipe bar. For example, \"house | You are in a house\".",
                    ),
                    m("div.Label21.TLabel",
                        {
                        },
                        "Descriptions are optional. It's okay to wrap entries on more than one line. Use carriage returns to separate entries.",
                    ),
                    m("div.Label22.TLabel",
                        {
                        },
                        "Context | Descriptive Reply",
                    ),
                    m("textarea.NewContextsMemo.TMemo",
                        {
                        },
                    ),
                    m("div.Label6.TLabel",
                        {
                        },
                        " When you are finished entering contexts, click Next.",
                    ),
                ),
                this.wizardPage !== ContextWizardPages.kDescriptionPage ? [] : m("TPage.page3.TPage",
                    {
                    },
                    m("h1", "Generate Descriptions"),
                    m("img.DescribeImage.TImage",
                        {
                        },
                    ),
                    m("div.DescribeLabel.TLabel",
                        {
                        },
                        " What command should the user to say to access these descriptive replies?",
                    ),
                    m("input.DescribeEdit.TEdit",
                        {
                        },
                    ),
                    m("img.commandImage.TImage",
                        {
                        },
                    ),
                    m("div.DescribeLabelExtra.TLabel",
                        {
                        },
                        "Some generic examples are:",
                    ),
                    m("div.Label15.TLabel",
                        {
                        },
                        "\"look\", \"listen\", \"smell\", \"feel\", \"taste\", and \"sense\".",
                    ),
                    m("div.Label16.TLabel",
                        {
                        },
                        "You should stick with \"look\" unless you are doing something special. You can change individual commands later (in the editor) to deal with specific situations.",
                    ),
                    m("div.Label4.TLabel",
                        {
                        },
                        "If you have not entered a description for a context, the wizard will add a default description of 'There is nothing of interest here.' ",
                    ),
                ),
                this.wizardPage !== ContextWizardPages.kFinishPage ? [] : m("TPage.page4.TPage",
                    {
                    },
                    m("h1", "Finish"),
                    m("img.Image2.TImage",
                        {
                        },
                    ),
                    m("div.Label18.TLabel",
                        {
                        },
                        "Congratulations!",
                    ),
                    m("div.Label13.TLabel",
                        {
                        },
                        "You have completed the information the wizard needs to generate a new set of rules based on your the contexts and descriptions you have entered.",
                    ),
                    m("div.Label12.TLabel",
                        {
                        },
                        "The text you entered here will also be saved in the log file (even if you cancel using the wizard).",
                    ),
                    m("div.Label14.TLabel",
                        {
                        },
                        "Click Finish to create the new rules and close the wizard.",
                    ),
                    m("div.Label11.TLabel",
                        {
                        },
                        "After you finish the wizard, you can choose Undo from the Edit menu to remove your new rules.",
                    ),
                    m("div.Label19.TLabel",
                        {
                        },
                        "Click Back to review your choices. Click Cancel to close the wizard without making any new rules.",
                    ),
                ),
            ),
        )
    }
}
