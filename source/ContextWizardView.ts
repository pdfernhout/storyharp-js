import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"
import { TSDomain } from "./TSDomain"
import { TQuickFillComboBox } from "./TQuickFillComboBox"

const exampleOne =`
cave|You are in a big cave.
forest|You are in a lively forest.
spring|You are standing near a burbling spring.`.trim()

const exampleWithNineRules = `    
well house | You are in a well house for a small spring.
grate | You are standing above a grate.
forest | You are wandering around in dense forest.
forest | It is getting dark and hard to see.
mossy clearing
glade | You are in a forest glade.

tree tops

path by rock|you are on a path in the forest besides a big rock
stream | You are walking along a dry stream bed.`.trim()

/*
the good place | You're in the good place
the bad place | You're in the  bad place
mindy st. claire's house | You're in a medium place at Mindy St. Claire's house.
*/

const defaultCommand = "look"
const defaultReply = "There is nothing of interest here."

export interface ContextWizardData {
    newContextsTextToParse: string
    commandPhrase: string
}

export function newContextWizardData(): ContextWizardData {
    return {
        newContextsTextToParse: "",
        commandPhrase: defaultCommand,
    }
}


export class ContextWizardView {
    domain: TSDomain
    contextWizardData: ContextWizardData

    newContextsTextToParseError: string = ""
    newContextsTextToParseLastGenerated: string = ""

    commandPhraseError: string = ""
    commandPhraseLastGenerated: string = ""

    wasGenerateRulesPressed = false

    showHelp = true

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.contextWizardData = this.domain.contextWizardData
    }
    
    checkInputForErrors() {
        if (!this.contextWizardData.newContextsTextToParse.trim()) {
            this.newContextsTextToParseError = "You must enter one or more contexts to generate rules."
        } else {
            this.newContextsTextToParseError = ""
        }
        if (!this.contextWizardData.commandPhrase.trim()) {
            this.commandPhraseError = "You must enter a command to be used to describe these contexts."
        } else {
            this.commandPhraseError = ""
        }
        return this.newContextsTextToParseError || this.commandPhraseError
    }
    
    generateRules(): void {
        console.log("generateRules")

        this.wasGenerateRulesPressed = true
        if (this.checkInputForErrors()) {
            setTimeout(() => alert("Please fix the highlighted issues and try again."), 50)
            return
        }

        const commandPhrase = this.contextWizardData.commandPhrase.trim()

        const world: TWorld = this.domain.world
        const ruleEditorForm = this.domain.ruleEditorForm
        
        // TODO: save text to log
        // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)

        const newRulesCommand = new TSNewRulesCommand(this.domain)
        newRulesCommand.creator = "new context wizard"

        const lines = this.contextWizardData.newContextsTextToParse.split(/\r\n|\r|\n/)

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

            const newRule = world.newRule()
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
            const plural = newRulesCommand.rules.length === 1 ? "" : "s"
            alert("A total of " + newRulesCommand.rules.length + " new rule" + plural + " were generated")
        } else {
            alert("No rules were generated")
            return
        }

        this.newContextsTextToParseLastGenerated = this.contextWizardData.newContextsTextToParse
        this.commandPhraseLastGenerated = this.contextWizardData.commandPhrase
        // this.newContextsTextToParse = ""
        this.wasGenerateRulesPressed = false
    }
    
    // TODO:
    // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
    // Application.HelpJump("Making_new_rules_using_the_new_contexts_wizard")

    // the good place | You're in the good place

    view() {
        function caption(text: string) { return text }
        const showHelp = this.showHelp
        function help(...args: string[]) {
            return showHelp ? m("p", ...args) : []
        }
        
        return m(".ContextWizardView.h-100.overflow-auto",
            {
            },
            m("div",
                m("h2", "New Contexts Wizard"),

                m("div", {onclick: () => this.showHelp = !this.showHelp }, "Show help", expander(showHelp, "", "(Click to close help)")),

                help("This wizard will create new rules defining contexts and replies to a common command like \"look\"."),

                help("A command (", Glyph.command, ") is what you say to the computer."),
                help("A context (", Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."),
                help("A reply (", Glyph.reply, ") is what the computer says after you say a command."),

                help("You can enter a descriptive reply for each new context. ",
                "The descriptive replies will be accessed with a common command such as \"look\"."),

                m("h3", "Enter Contexts"),

                help("Enter or paste the contexts you want to create in the area below, ",
                "separating each context from its descriptive reply by a pipe bar."),
                help("For example, \"house | You are in a house\"."),
 
                help("Use carriage returns to separate entries -- one entry per line. Blank lines will be ignored."),
                help("Replies are optional. It's okay if long replies wrap around in the editor as long as they do not have a carriage return in them."),

                help("If do not enter descriptive reply for a context, the wizard will add a default description of \"" + defaultReply + "\""),

                help("You can also use the same context more than once and later add special requirements (", Glyph.requirements, ") to some of the extra rules"),

                help("Here is an example showing a mix of different entries which generated nine rules:"),
                showHelp ? m("pre.ba.bw2.pa1.ml2.mr2", exampleWithNineRules) : [],

                m("div.ma2", "Context (", Glyph.context, ")", m("span.ml2.mr2.f4.b", "|"), "Descriptive Reply (", Glyph.reply, ")"),

                m("textarea.ml2" + (this.newContextsTextToParseError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.contextWizardData.newContextsTextToParse,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.contextWizardData.newContextsTextToParse = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [],

                m("h3", "Generate Descriptions"),

                m("p", "What command (", Glyph.command, ") should the user to say to access these descriptive replies?"),
                m(TQuickFillComboBox,
                    <any>{
                        extraStyling: (this.commandPhraseError ? ".ml2.bg-yellow" : ".ml2"),
                        value: this.contextWizardData.commandPhrase,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.contextWizardData.commandPhrase = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        items: this.domain.world.getCommandNames(),
                    },
                ),

                this.commandPhraseError ? m("div.i.bg-yellow", this.commandPhraseError) : [],

                help("Some generic examples are: \"look\", \"listen\", \"smell\", \"feel\", \"taste\", and \"sense\"."),

                help("You should stick with \"look\" unless you are doing something special. ",
                "You can change individual commands later (in the editor) to deal with specific situations."),

                m("p", "Click the \"Generate Rules\" button to create the new rules."),
                
                m("div.ml2", 
                    m("button", {
                        onclick: () => this.generateRules()
                    }, "Generate rules"),
                ),

                help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."),
                help("The new rules will also initally be selected in the rules table."),
                // TODO: use or remove: help("The text you entered here to generate rules will also be saved in the log file if you need to recover it later."),

                m("div.ml2.mt2", 
                    m("button", {
                        onclick: () => {
                            if (!confirm("Are you sure you want to clear the Context Wizard form?")) return
                            this.domain.contextWizardData = newContextWizardData()
                            this.contextWizardData = this.domain.contextWizardData
                        }
                    }, "Clear Context Wizard form"),
                ),
            ),
        )
    }
}
