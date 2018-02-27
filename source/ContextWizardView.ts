import * as m from "mithril"

export class ContextWizardView {
    domain: any
    wizardPage = 1

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    cancelClick() { console.log("cancelClick") }

    goBackClick() { this.wizardPage = Math.max(1, this.wizardPage - 1) }

    goNextClick() { 
        if (this.wizardPage === 4) {
            alert("Finished!!!")
        }
        this.wizardPage = Math.min(4, this.wizardPage + 1)
    }

    helpButtonClick() { console.log("helpButtonClick") }

    view() {
        function caption(text: string) { return text }
        
        return m("ContextWizardForm.TContextWizardForm",
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
                this.wizardPage === 4 ? "Finish" : caption("&Next >>"),
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
                this.wizardPage !== 1 ? [] : m("TPage.page1.TPage",
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
                this.wizardPage !== 2 ? [] : m("TPage.page2.TPage",
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
                this.wizardPage !== 3 ? [] : m("TPage.page3.TPage",
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
                this.wizardPage !== 4 ? [] : m("TPage.page4.TPage",
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
