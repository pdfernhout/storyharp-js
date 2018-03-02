import * as m from "mithril"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"

const exampleOne =`
cave|You are in a big cave.
forest|You are in a lively forest.
spring|You are standing near a burbling spring.`.trim()

const exampleTwo = `    
well house | You are in a well house for a small spring.
grate | You are standing above a grate.
forest | You are wandering around in dense forest.
glade | You are in a forest glade.
stream | You are walking along a dry stream bed.`.trim()

const defaultCommand = "look"
const defaultReply = "There is nothing of interest here."

export class ContextWizardView {
    domain: any

    newContextsTextToParse: string = "";
    newContextsTextToParseError: string = "";
    newContextsTextToParseLastGenerated: string = "";

    commandPhrase = defaultCommand;
    commandPhraseError: string = "";
    commandPhraseLastGenerated: string = "";

    wasGenerateRulesPressed = false

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    checkInputForErrors() {
        if (!this.newContextsTextToParse.trim()) {
            this.newContextsTextToParseError = "You must enter one or more contexts to generate rules."
        } else {
            this.newContextsTextToParseError = ""
        }
        if (!this.commandPhrase.trim()) {
            this.commandPhraseError = "You must enter a command to be used to describe these contexts."
        } else {
            this.commandPhraseError = ""
        }
        return this.newContextsTextToParseError || this.commandPhraseError
    }
    
    generateRules(): void {
        console.log("generateRules")

        this.wasGenerateRulesPressed = true
        if (this.checkInputForErrors()) return

        const commandPhrase = this.commandPhrase.trim()

        const world: TWorld = this.domain.world
        const ruleEditorForm = this.domain.ruleEditorForm
        
        // TODO: save text to log
        // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)

        const newRulesCommand = new TSNewRulesCommand(world, ruleEditorForm)
        newRulesCommand.creator = "new context wizard"

        const lines = this.newContextsTextToParse.split(/\r\n|\r|\n/)

        for (let line of lines) {
            line = line.trim()
            if (!line) continue
            console.log("line", line)

            const pipeBarLocation = line.indexOf("|")
            let context
            let reply
            if (pipeBarLocation === -1) {
                context = line
                reply = defaultReply
            } else {
                context = line.substring(0, pipeBarLocation).trim() || ("missing context " + Math.random())
                reply =  line.substring(pipeBarLocation + 1).trim() || defaultReply
            }

            const position: TPoint = ruleEditorForm.goodPosition()

            const newRule = world.newRule()
            newRule.position = position
            newRule.setContext(context)
            newRule.setCommand(commandPhrase)
            newRule.setReply(reply)
            newRule.selected = true

            newRulesCommand.addRule(newRule)
            ruleEditorForm.lastChoice = newRule
            this.domain.editedRule = newRule
        }

        if (newRulesCommand.rules.length > 0) {
            this.domain.worldCommandList.doCommand(newRulesCommand)
            // ruleEditorForm.updateForRuleChange()
            // ruleEditorForm.adjustScrollBars()
            const plural = newRulesCommand.rules.length === 1 ? "" : "s"
            alert("A total of " + newRulesCommand.rules.length + " new rule" + plural + " were generated")
        } else {
            alert("No rules were generated")
            return
        }

        this.newContextsTextToParseLastGenerated = this.newContextsTextToParse
        this.commandPhraseLastGenerated = this.commandPhrase
        this.newContextsTextToParse = ""
        this.wasGenerateRulesPressed = false
    }
    
    // TODO:
    // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
    // Application.HelpJump("Making_new_rules_using_the_new_contexts_wizard")

    // the good place | You're in the good place

    view() {
        function caption(text: string) { return text }
        
        return m(".ContextWizardView",
            {
            },
            m("div",
                "Welcome to the New Contexts Wizard!",
                "This wizard will help you quickly create a set of new rules based on contexts you enter.",
                "A command is what you say to the computer.",
                "A context is the single most important requirement for the user of a command, usually a physical location.",
                "A reply is what the computer says after you say a command.",

                "You can enter a descriptive reply for each new context you enter here.",
                "The descriptive replies will be accessed with a common command such as \"look\".",

                // "You can click Cancel at any time to close the wizard without making any new rules.",

                m("h1", "Enter Contexts"),

                "Enter or paste the contexts you want to create in the area below,",
                "separating each context from its descriptive reply by a pipe bar.",
                "For example, \"house | You are in a house\".",
 
                "Descriptions are optional. It's okay to wrap entries on more than one line.",
                "Use carriage returns to separate entries.",

                "Context | Descriptive Reply",

                m("textarea" + (this.newContextsTextToParseError ? ".bg-yellow" : ""),
                    {
                        value: this.newContextsTextToParse,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.newContextsTextToParse = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.newContextsTextToParseError ? m("div", this.newContextsTextToParseError) : [],

                m("h1", "Generate Descriptions"),

                "What command should the user to say to access these descriptive replies?",
                m("input" + (this.commandPhraseError ? ".bg-yellow" : ""),
                    {
                        value: this.commandPhrase,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.commandPhrase = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.commandPhraseError ? m("div", this.commandPhraseError) : []

                "Some generic examples are:",

                "\"look\", \"listen\", \"smell\", \"feel\", \"taste\", and \"sense\".",

                "You should stick with \"look\" unless you are doing something special.",
                "You can change individual commands later (in the editor) to deal with specific situations.",


                "If you have not entered a description for a context, the wizard will add a default description of 'There is nothing of interest here.' ",

                "Congratulations!",

                "You have completed the information the wizard needs to generate a new set of rules based on your the contexts and descriptions you have entered.",

                "The text you entered here will also be saved in the log file (even if you cancel using the wizard).",

                "Click Finish to create the new rules and close the wizard.",

                m("button", {
                    onclick: () => this.generateRules()
                }, "Generate rules"),

                "After you finish the wizard, you can choose Undo from the Edit menu to remove your new rules.",

                "Click Cancel to close the wizard without making any new rules.",
            ),
        )
    }
}
