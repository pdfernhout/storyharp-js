import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"

// the good place
const exampleWithFiveRules = `
tell Shawn you are angry | Shawn rolls up his cocoon.
tell Shawn you are happy| Shawn rolls up his cocoon.

   tell Shawn he is wrong |You are sent to the bad place.
    
spit at Shawn
throw a cactus at Shawn | The cactus bounces off Shawn's quickly-rolled-up cocoon.
`.trim()

// the grue place
const exampleSequenceWithFourRules = `    
talk to the grue | The grue won't listen.
talk to the grue | The grue seems to be getting very agitated.
talk to the grue | The grue seems about to fly into a rage.
talk to the grue | The grue devours you (except your bones of course).
`.trim()

const defaultReply = "Nothing happens."

// TODO Application.HelpJump("Making_new_rules_using_the_new_commands_wizard")

enum EndSequence {
    noSelection = "noSelection",
    loopToFirst = "loopToFirst",
    leaveLastCommand = "leaveLastCommand",
    removeLastCommand = "removeLastCommand"
}

export class CommandWizardView {
    domain: TSDomain

    newCommandsTextToParse: string = "";
    newCommandsTextToParseError: string = "";
    newCommandsTextToParseLastGenerated: string = "";

    contextName = "";
    contextNameError: string = "";
    contextNameLastGenerated: string = "";

    prefix = "";
    prefixError: string = "";
    prefixLastGenerated: string = "";

    endSequence = EndSequence.noSelection;
    endSequenceError: string = "";
    endSequenceGenerated: string = "";

    doSequence = false

    wasGenerateRulesPressed = false

    showHelp = true

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    checkInputForErrors() {
        if (!this.newCommandsTextToParse.trim()) {
            this.newCommandsTextToParseError = "You must enter one or more commands to generate rules."
        } else {
            this.newCommandsTextToParseError = ""
        }
        if (!this.contextName.trim()) {
            this.contextNameError = "You must enter a context to be used with these commands."
        } else {
            this.contextNameError = ""
        }
        if (this.doSequence) {
            if (!this.prefix.trim()) {
                this.prefixError = "You must enter a prefix."
            } else {
                this.prefixError = ""
            }
            if (this.endSequence === EndSequence.noSelection) {
                this.endSequenceError = "You must select an option to end the sequence."
            } else {
                this.endSequenceError = ""
            }
        } else {
            this.prefixError = ""
            this.endSequenceError = ""
        }
        return this.newCommandsTextToParseError || this.contextNameError || this.prefixError || this.endSequenceError
    }
    
    generateRules(): void {
        console.log("generateRules", this.endSequence)

        this.wasGenerateRulesPressed = true
        if (this.checkInputForErrors()) {
            setTimeout(() => alert("Please fix the highlighted issues and try again."), 50)
            return
        }

        const contextName = this.contextName.trim()
        const prefix = this.prefix.trim()

        const world: TWorld = this.domain.world
        const ruleEditorForm = this.domain.ruleEditorForm

        // TODO: save text to log
        // uschangelog.ChangeLogForm.addToLog(this.newCommandsMemo.Text)

        const newRulesCommand = new TSNewRulesCommand(world, ruleEditorForm)
        newRulesCommand.creator = "command sequence wizard"

        const lines = this.newCommandsTextToParse.split(/\r\n|\r|\n/)

        let index = 1
        let newRule: TSRule | null = null

        for (let line of lines) {
            line = line.trim()
            if (!line) continue
            console.log("line", line)

            const pipeBarLocation = line.indexOf("|")
            let command
            let reply
            if (pipeBarLocation === -1) {
                command = line
                reply = defaultReply
            } else {
                command = line.substring(0, pipeBarLocation).trim() || ("missing command " + Math.random())
                reply =  line.substring(pipeBarLocation + 1).trim() || defaultReply
            }

            const position: TPoint = ruleEditorForm.goodPosition()

            newRule = world.newRule()
            newRule.position = position
            newRule.setContext(this.contextName)
            newRule.setCommand(command)
            newRule.setReply(reply)
            newRule.selected = true

            newRulesCommand.addRule(newRule)
            ruleEditorForm.lastChoice = newRule
            this.domain.editedRule = newRule

            if (this.doSequence && (prefix !== "")) {
                let requirements
                let changes
                // Add zero at end of index to provide room for adding things in between liek BASIC line numbers
                if (index === 1) {
                    requirements = "~" + prefix + " started"
                    changes = prefix + " started & " + prefix + " " + index + "0"
                } else {
                    requirements = prefix + " " + (index - 1) + "0"
                    changes = "~" + prefix + " " + (index - 1) + "0 & " + prefix + " " + index + "0"
                }
                newRule.setRequirements(requirements)
                newRule.setChanges(changes)
            }

            index += 1

            // TODO: use or remove
            // position.Y = position.Y + 60
        }

        if (this.doSequence && (prefix !== "") && (newRule !== null) && (index > 2)) {
            // cleanup for last rule
            let changes = newRule.changesString
            if (this.endSequence === EndSequence.loopToFirst) {
                changes = "~" + prefix + " " + (index - 2) + "0 & " + "~" + prefix + " started"
            } else if (this.endSequence === EndSequence.leaveLastCommand) {
                changes = ""
            } else if (this.endSequence === EndSequence.removeLastCommand) {
                changes = "~" + prefix + " " + (index - 2) + "0"
            } else {
                throw new Error("unexpected case")
            }
            newRule.setChanges(changes)
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

        this.newCommandsTextToParseLastGenerated = this.newCommandsTextToParse
        this.contextNameLastGenerated = this.contextName
        this.prefixLastGenerated = this.prefix
        this.endSequenceGenerated = this.endSequence
        // this.newCommandsTextToParse = ""
        this.wasGenerateRulesPressed = false
    }
    
    // TODO use or remove:

    // usdomain.domain.world.addContextsToListBox(this.ContextBox)
    
    // "You must create at least one context before using the command wizard."
    
    // this.lastFloodedContextPrefix = ""
    // goNextClick(Sender: TObject): void {
    // } else if (this.notebook.PageIndex === kContextPage) {
    // this.lastFloodedContextPrefix = this.ContextBox.Items[this.ContextBox.ItemIndex]
    
    // usdomain.domain.world.variables
    // if (this.ContextBox.Items[j] === variable.phrase) {

    view() {
        function caption(text: string) { return text }
        const showHelp = this.showHelp
        function help(...args: string[]) {
            return showHelp ? m("p", ...args) : []
        }
        const doSequence = this.doSequence

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
                m("input.ml2" + (this.contextNameError ? ".bg-yellow" : ""),
                    {
                        value: this.contextName,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.contextName = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.contextNameError ? m("div.i.bg-yellow", this.contextNameError) : [],

                help("Some generic examples of a context are: \"cave\", \"forest\", and \"inside house\"."),

                m("h3", "Commands"),

                help("Enter or paste the commands you want to create in the area below, ",
                "separating each command from its reply by a pipe bar."),
                help("For example, \"open the door | You open the rusty door.\"."),
 
                help("Use carriage returns to separate entries -- one entry per line. Blank lines will be ignored."),
                help("Replies are optional. It's okay if long replies wrap around in the editor as long as they do not have a carriage return in them."),

                help("If do not enter descriptive reply for a context, the wizard will add a default description of \"" + defaultReply + "\""),

                help("You can also use the same context more than once and later add special requirements (", Glyph.requirements, ") to some of the extra rules"),

                help("Here is an example of a sequence showing a mix of different entries which generates five rules:"),
                showHelp ? m("pre.ba.bw2.pa1.ml2.mr2", exampleWithFiveRules) : [],

                m("div.ma2", "Command (", Glyph.command, ")", m("span.ml2.mr2.f4.b", "|"), "Reply (", Glyph.reply, ")"),

                m("div.b", "New commands ", this.contextName ? "for: " + this.contextName : ""),
                m("textarea.ml2" + (this.newCommandsTextToParseError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.newCommandsTextToParse,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.newCommandsTextToParse = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.newCommandsTextToParseError ? m("div.i.bg-yellow", this.newCommandsTextToParseError) : [],

                m("h3", "Sequence"),

                help("Sequences are useful when the user has to do several things in a certain order, ",
                "such as steps in an assembly process or parts of a conversation."),

                m("div", "Do you want to link your new commands in sequence, so that each becomes available only after the previous one is said?"),

                m("div.ma2",
                    m("input[type=checkbox].ml1", {
                        checked: doSequence || undefined,
                        onchange: (event: { target: HTMLInputElement }) => { 
                            this.doSequence = event.target.checked 
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    }),
                    m("span.ml1", "Yes, link the commands in a sequence."),
                ),

                m("div.ml2" + (doSequence ? "" : ".gray"),

                    help("Here is an example of a sequence (see below) which could generate four rules for a \"grue pit\" context each using the same command:"),
                    showHelp ? m("pre.ba.bw2.pa1.ml2.mr2", exampleSequenceWithFourRules) : [],

                    // help("Creating sequences is an advanced topic; see the help system for details."),

                    m("p", " What prefix do you want to use for the requirements that create the sequence?"),

                    m("input", {
                        value: this.prefix,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.prefix = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        disabled: !doSequence || null,
                    }),
                    this.prefixError ? m("div.i.bg-yellow", this.prefixError) : [],

                    help("Examples are: \"talking to sailor\", \"in boarding house\". By default the prefix is the same as the context."),

                    // TODO img.sequenceEndArrow.TImage
                    m("p", "When the last command has been said,"),

                    m("input[type=radio]",
                        {
                            name: "endSequence",
                            value: EndSequence.loopToFirst,
                            checked: this.endSequence === EndSequence.loopToFirst,
                            onchange: () => { 
                                this.endSequence = EndSequence.loopToFirst
                                if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                            },
                            disabled: !doSequence || null,
                        },
                    ),
                    "loop to the first command in the sequence",
                    m("br"),
                    m("input[type=radio]",
                        {
                            name: "endSequence",
                            value: EndSequence.leaveLastCommand,
                            checked: this.endSequence === EndSequence.leaveLastCommand,
                            onchange: () => { 
                                this.endSequence = EndSequence.leaveLastCommand 
                                if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                            },
                            disabled: !doSequence || null,
                        },
                        
                    ),
                    "leave the last command available",
                    m("br"),
                    m("input[type=radio]",
                        {
                            name: "endSequence",
                            value: EndSequence.removeLastCommand,
                            checked: this.endSequence === EndSequence.removeLastCommand,
                            onchange: () => { 
                                this.endSequence = EndSequence.removeLastCommand 
                                if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                            },
                            disabled: !doSequence || null,
                        },
                    ),
                    "remove the last command",
                    m("br"),

                    this.endSequenceError ? m("div.i.bg-yellow", this.endSequenceError) : [],
                ),

                m("h3", "Generate Rules"),

                m("p", "Click the \"Generate Rules\" button to create the new rules."),
                
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
