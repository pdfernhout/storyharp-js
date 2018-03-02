import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"

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

const defaultReply = "There is nothing of interest here."

export class CommandWizardView {
    domain: any

    newContextsTextToParse: string = "";
    newContextsTextToParseError: string = "";
    newContextsTextToParseLastGenerated: string = "";

    commandPhrase = "";
    commandPhraseError: string = "";
    commandPhraseLastGenerated: string = "";

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
        const showHelp = this.showHelp
        function help(...args: string[]) {
            return showHelp ? m("p", ...args) : []
        }
        
        return m(".CommandWizardView.h-100.overflow-auto",
            {
            },
            m("div",
                m("h2", "New Commands Wizard"),

                m("div", {onclick: () => this.showHelp = !this.showHelp }, "Show help", expander(showHelp, "", "(Click to close help)")),

                help("This wizard will create a set of new rules based on one context and a list of commands you enter."),
                help("You can enter a reply for each command."),
                
                help("A command (", Glyph.command, ") is what you say to the computer."),
                help("A context (", Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."),
                help("A reply (", Glyph.reply, ") is what the computer says after you say a command."),

                help("You can also link your new commands in a sequence so that only one command is available at any time. ",
                "This is done by generating requirements and changes."),

                help("A requirement (", Glyph.requirements, ") is variable state necessary for a command to be available. Variables can be true or false."),
                help("A change (", Glyph.changes, ") is a new variable state resulting from a command."),

                help("You can add extra requirements or changes later using the editor."),

                m("h3", "Context"),

                m("p", "What context (", Glyph.context, ") do you want your new commands to use?"),
                // TODO: Drop down or scrolling list of existing contexts
                m("input.ml2" + (this.commandPhraseError ? ".bg-yellow" : ""),
                    {
                        value: this.commandPhrase,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.commandPhrase = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.commandPhraseError ? m("div.i.bg-yellow", this.commandPhraseError) : [],

                help("Some generic examples of a context are: \"cave\", \"forest\", and \"inside house\"."),

                m("h3", "Commands"),

                // TODO: "New commands for: context",

                help("Enter or paste the commands you want to create in the area below, ",
                "separating each command from its reply by a pipe bar."),
                help("For example, \"open the door | You open the rusty door.\"."),
 
                help("Use carriage returns to separate entries -- one entry per line. Blank lines will be ignored."),
                help("Replies are optional. It's okay if long replies wrap around in the editor as long as they do not have a carriage return in them."),

                help("If do not enter descriptive reply for a context, the wizard will add a default description of \"" + defaultReply + "\""),

                help("You can also use the same context more than once and later add special requirements (", Glyph.requirements, ") to some of the extra rules"),

                help("Here is an example showing a mix of different entries which generated nine rules:"),
                showHelp ? m("pre.ba.bw2.pa1.ml2.mr2", exampleWithNineRules) : [],

                m("div.ma2", "Command (", Glyph.command, ")", m("span.ml2.mr2.f4.b", "|"), "Reply (", Glyph.reply, ")"),

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

                m("h3", "Sequence"),

                m("div", "Do you want to link your new commands in sequence, so that each becomes available only after the previous one is said?"),

                m("input[type=checkbox]", {
                    value: "TODO",
                    onclick: () => alert("Unfinished")
                }),
                m("span", "Yes, link the commands in a sequence."),

                help("Sequences are useful when the user has to do several things in a certain order, ",
                "such as steps in an assembly process or parts of a conversation."),

                help("Creating sequences is an advanced topic; see the help system for details."),

                m("p", " What prefix do you want to use for the requirements that create the sequence?"),

                // TODO
                m("input"),
                help("Examples are: \"talking to sailor\", \"in boarding house\". By default the prefix is the same as the context."),

                // TODO img.sequenceEndArrow.TImage
                m("p", "When the last command has been said,"),

                // TODO -- fix variable
                m("input[type=radio].endSequenceLoopToFirst.TRadioButton",
                    {
                    },
                ),
                "loop to the first command in the sequence",
                m("br"),
                m("input[type=radio].endSequenceLeaveLastCommand.TRadioButton",
                    {
                    },
                    
                ),
                "leave the last command available",
                m("br"),
                m("input[type=radio].endSequenceRemoveTheLastCommand.TRadioButton",
                    {
                    },
                ),
                "remove the last command",
                m("br"),

                m("h3", "Generate Rules"),

                m("p", "Click the \"Generate Rules\" button to create the new rules and clear the wizard."),
                
                m("div.ml2", 
                    m("button", {
                        onclick: () => this.generateRules()
                    }, "Generate rules"),
                ),

                help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."),
                help("The new rules will also initally be selected in the rules table."),
                help("The command and reply text you entered here to generate rules will also be saved in the log file if you need to recover it later."),
            ),
        )
    }
}
