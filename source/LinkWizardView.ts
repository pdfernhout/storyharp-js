import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"
import { TSDomain } from "./TSDomain"

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

export class LinkWizardView {
    domain: TSDomain

    newContextsTextToParse: string = ""
    newContextsTextToParseError: string = ""
    newContextsTextToParseLastGenerated: string = ""

    commandPhrase = defaultCommand
    commandPhraseError: string = ""
    commandPhraseLastGenerated: string = ""

    wasGenerateRulesPressed = false

    showHelp = true

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
        if (this.checkInputForErrors()) {
            setTimeout(() => alert("Please fix the highlighted issues and try again."), 50)
            return
        }

        const commandPhrase = this.commandPhrase.trim()

        const world: TWorld = this.domain.world
        const ruleEditorForm = this.domain.ruleEditorForm
        
        // TODO: save text to log
        // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)

        const newRulesCommand = new TSNewRulesCommand(this.domain)
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

        this.newContextsTextToParseLastGenerated = this.newContextsTextToParse
        this.commandPhraseLastGenerated = this.commandPhrase
        // this.newContextsTextToParse = ""
        this.wasGenerateRulesPressed = false
    }
    
    // TODO:
    // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
    // Application.HelpJump("Making_new_rules_using_the_new_moves_wizard")

    view() {
        function caption(text: string) { return text }
        const showHelp = this.showHelp
        function help(...args: string[]) {
            return showHelp ? m("p", ...args) : []
        }
        
        return m(".LinkWizardForm.h-100.overflow-auto",
            {
            },
            m("div",
                m("h2", "New Moves Wizard"),

                m("div", {onclick: () => this.showHelp = !this.showHelp }, "Show help", expander(showHelp, "", "(Click to close help)")),

                help("This wizard will link up two contexts by creating rules with commands to move between them."),

                help("A command (", Glyph.command, ") is what you say to the computer."),
                help("A context (", Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."),
                help("A reply (", Glyph.reply, ") is what the computer says after you say a command."),

                help("You can enter a reply for each command."),

                m("h3", "Contexts"),

                help("Choose two contexts to move between. The order doesn't matter."),
 
                // TODO: FirstContextBox

                // TODO: SecondContextBox

                help("You can also choose these two contexts by selecting them in the Map (Shift-click to select the second context) before you open the wizard."),

                ///////////////////////////////////////

                m("h3", "Forward"),

                m("p", "What command (", Glyph.command, ") should the user say to move from: ", TODO:ForwardLabel),
                m("input.ml2" + (this.commandPhraseError ? ".bg-yellow" : ""),
                    {
                        value: this.commandPhrase,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.commandPhrase = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command?"),

                m("textarea.ml2" + (this.newContextsTextToParseError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.newContextsTextToParse,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.newContextsTextToParse = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [],

                help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),

                ///////////////////////////////////////

                m("h3", "Backward"),

                m("p", "What command (", Glyph.command, ") should the user say to move from: ", TODO:BackwardLabel),
                m("input.ml2" + (this.commandPhraseError ? ".bg-yellow" : ""),
                    {
                        value: this.commandPhrase,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.commandPhrase = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command?"),

                m("textarea.ml2" + (this.newContextsTextToParseError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.newContextsTextToParse,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.newContextsTextToParse = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [],

                help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),

                //////////////////////

                m("h3", "Generate Rules"),

                // forwardSummary  "nether regions -> go to elevator -> elevator",

                // backwardSummary  "elevator -> go to nether regions -> nether regions",

                m("p", "You have completed the information the wizard needs to generate two new rules to link the two contexts you have chosen."),
                
                m("p", "Click the \"Generate Rules\" button to create the new rules."),
                
                m("div.ml2", 
                    m("button", {
                        onclick: () => this.generateRules()
                    }, "Generate rules"),
                ),

                help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."),
                help("The new rules will also initally be selected in the rules table."),
                help("The text you entered here to generate rules will also be saved in the log file if you need to recover it later."),
            ),
        )
    }
}
