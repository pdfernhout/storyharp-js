import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"
import { TSDomain } from "./TSDomain"
import { TSRule } from "./TSRule";

/* TODO use or remove:
        if (this.FirstContextBox.Items.Count < 2) {
            ShowMessage("You must create at least two contexts before using the link wizard.")
            return result
        }
*/

/* Example command and reply for house and yard:

house : break through wall

    The wall crumbles and you find yourself outside. Someone is going to have to do lot of work to do to fix this.

yard : burrow inside

   You dig a tunnel and find yourself inside the house.

*/

export class LinkWizardView {
    domain: TSDomain

    firstContext: string = ""
    firstContextError: string = ""
    firstContextLastGenerated: string = ""

    firstCommand: string = ""
    firstCommandError: string = ""
    firstCommandLastGenerated: string = ""

    firstReply: string = ""
    firstReplyError: string = ""
    firstReplyLastGenerated: string = ""

    secondContext: string = ""
    secondContextError: string = ""
    secondContextGenerated: string = ""

    secondCommand: string = ""
    secondCommandError: string = ""
    secondCommandLastGenerated: string = ""

    secondReply: string = ""
    secondReplyError: string = ""
    secondReplyLastGenerated: string = ""

    wasGenerateRulesPressed = false

    showHelp = true

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.initializeContexts()
    }

    initializeContexts() {
        const world: TWorld = this.domain.world

        for (let i = 0; i < world.variables.length; i++) {
            const variable = world.variables[i]
            if (variable.selected) {
                if (!this.firstContext) {
                    this.firstContext = variable.phrase
                } else {
                    this.secondContext = variable.phrase
                    break
                }
            }
        }
    }

    checkInputForErrors() {
        if (this.firstCommand.trim() === this.secondContext.trim()) {
            this.firstContextError = "The two contexts must have different names."
            this.secondContextError = "The two contexts must have different names."
        }

        if (!this.firstContext.trim()) {
            this.firstContextError = "Both contexts must be entered to proceed."
        } else {
            this.firstContextError = ""
        }

        if (!this.secondContext.trim()) {
            this.secondContextError = "Both contexts must be entered to proceed."
        } else {
            this.secondContextError = ""
        }

        if (!this.firstCommand.trim() && this.firstReply.trim()) {
            this.firstCommandError = "You must enter a command phrase if you enter a reply."
        } else {
            this.firstCommandError = ""
        }

        if (!this.secondCommand.trim() && this.secondReply.trim()) {
            this.secondCommandError = "You must enter a command phrase if you enter a reply."
        } else {
            this.secondCommandError = ""
        }

        if (!this.firstCommand.trim() && !this.secondCommand.trim()) {
            this.firstCommandError = "You must enter at least one command to generate a link."
            this.secondCommandError = "You must enter at least one command to generate a link."

        } else {
            this.firstCommandError = ""
            this.secondCommandError = ""
        }

        return this.firstContextError
            || this.firstCommandError
            || this.firstReplyError
            || this.secondContextError
            || this.secondCommandError
            || this.secondReplyError
    }

    makeLink(firstContext: string, secondContext: string, command: string, reply: string): TSRule | null {
        let newRule: TSRule | null = null
        if (firstContext === "") {
            return newRule
        }

        const world: TWorld = this.domain.world
        const ruleEditorForm = this.domain.ruleEditorForm
        
        newRule = world.newRule()
        ruleEditorForm.lastChoice = newRule

        newRule.setContext(firstContext)

        newRule.setCommand(command)

        if (reply !== "") {
            newRule.setReply(reply)
        } else {
            newRule.setReply("You " + command + ".")
        }

        newRule.setMove(secondContext)

        newRule.position.X = (newRule.context.position.X + newRule.move.position.X) / 2
        newRule.position.Y = (newRule.context.position.Y + newRule.move.position.Y) / 2

        // determine offset above or below midpoint between two contexts depending on direction of move
        const dx = newRule.context.position.X - newRule.move.position.X
        const dy = newRule.context.position.Y - newRule.move.position.Y
        if (Math.abs(dy) >= Math.abs(dx)) {
            if (dy >= 0) {
                newRule.position.X = newRule.position.X - 100
            } else {
                newRule.position.X = newRule.position.X + 100
            }
        } else {
            if (dx < 0) {
                newRule.position.Y = newRule.position.Y - 30
            } else {
                newRule.position.Y = newRule.position.Y + 30
            }
        }

        newRule.selected = true

        return newRule
    }
    
    generateRules(): void {
        console.log("generateRules")

        this.wasGenerateRulesPressed = true
        if (this.checkInputForErrors()) {
            setTimeout(() => alert("Please fix the highlighted issues and try again."), 50)
            return
        }

        // TODO: save text to log
        // uschangelog.ChangeLogForm.addToLog(this.ForwardMemo.Text)
        // uschangelog.ChangeLogForm.addToLog(this.BackwardMemo.Text)

        const newRulesCommand = new TSNewRulesCommand(this.domain)
        newRulesCommand.creator = "link wizard"

        const newRule1 = this.makeLink(this.firstContext.trim(), this.secondContext.trim(), this.firstCommand.trim(), this.firstReply.trim())
        if (newRule1 !== null) {
            newRulesCommand.addRule(newRule1)
        }

        const newRule2 = this.makeLink(this.secondContext.trim(), this.firstContext.trim(), this.secondCommand.trim(), this.secondReply.trim())
        if (newRule2 !== null) {
            newRulesCommand.addRule(newRule2)
        }

        if (newRulesCommand.rules.length > 0) {
            this.domain.worldCommandList.doCommand(newRulesCommand)
            const plural = newRulesCommand.rules.length === 1 ? "" : "s"
            alert("A total of " + newRulesCommand.rules.length + " new rule" + plural + " were generated")
        } else {
            alert("No rules were generated")
            return
        }

        this.firstContextLastGenerated = this.firstContext
        this.firstCommandLastGenerated = this.firstCommand
        this.firstReplyLastGenerated = this.firstReply
        this.secondContextGenerated = this.secondContext
        this.secondCommandLastGenerated = this.secondCommand
        this.secondReplyLastGenerated = this.secondReply

        // TODO: How to clear this out? Maybe add clear button?
        // this.newContextsTextToParse = ""

        this.wasGenerateRulesPressed = false
    }
    
    // TODO:
    // uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
    // Application.HelpJump("Making_new_rules_using_the_new_moves_wizard")

    /* TODO: enabling/disabling and arrow glyphs
        ForwardEditChange(Sender: TObject): void {
            let haveText: boolean
            
            haveText = this.ForwardEdit.Text !== ""
            this.forwardReplyArrow.Visible = haveText
            this.forwardReplyLabel.Enabled = haveText
            this.forwardReplyImage.Visible = haveText
            this.ForwardMemo.Visible = haveText
            this.forwardReplyNote.Enabled = haveText
        }
        
        BackwardEditChange(Sender: TObject): void {
            let haveText: boolean
            
            haveText = this.BackwardEdit.Text !== ""
            this.backwardReplyArrow.Visible = haveText
            this.backwardReplyLabel.Enabled = haveText
            this.backwardReplyImage.Visible = haveText
            this.BackwardMemo.Visible = haveText
            this.backwardReplyNote.Enabled = haveText
        }
    */

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
                m("h2", "New Links Wizard"),

                m("div", {onclick: () => this.showHelp = !this.showHelp }, "Show help", expander(showHelp, "", "(Click to close help)")),

                help("This wizard will link up two contexts by creating rules with commands to move between them."),

                help("A command (", Glyph.command, ") is what you say to the computer."),
                help("A context (", Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."),
                help("A reply (", Glyph.reply, ") is what the computer says after you say a command."),

                help("You can enter a reply for each command."),

                ///////////////////////////////////////

                m("h3", "Contexts"),

                help("Choose two contexts to move between."), //  The order doesn't matter."),
 
                // TODO: FirstContextBox

                m("p", "First context (", Glyph.context, "):"),

                m("input.ml2" + (this.firstContextError ? ".bg-yellow" : ""),
                    {
                        value: this.firstContext,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.firstContext = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.firstContextError ? m("div.i.bg-yellow", this.firstContextError) : [],

                // TODO: SecondContextBox

                m("p", "Second context (", Glyph.context, "):"),

                m("input.ml2" + (this.secondContextError ? ".bg-yellow" : ""),
                    {
                        value: this.secondContext,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.secondContext = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.secondContextError ? m("div.i.bg-yellow", this.secondContextError) : [],


                help("You can also choose these two contexts by selecting them in the Map (Shift-click to select the second context) before you open the wizard."),

                ///////////////////////////////////////

                m("h3", "Forward"),

                m("p", "What command (", Glyph.command, ") should the user say to move from ", (this.firstContext || "the first context") + ":"),
                m("input.ml2" + (this.firstCommandError ? ".bg-yellow" : ""),
                    {
                        value: this.firstCommand,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.firstCommand = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.firstCommandError ? m("div.i.bg-yellow", this.firstCommandError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command from ", m("i", (this.firstContext || "the first context")), "?"),

                m("textarea.ml2" + (this.firstReplyError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.firstReply,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.firstReply = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.firstReplyError ? m("div.i.bg-yellow", this.firstReplyError) : [],

                help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),

                ///////////////////////////////////////

                m("h3", "Backward"),

                m("p", "What command (", Glyph.command, ") should the user say to move from ", (this.secondContext || "the second context") + ":"),
                m("input.ml2" + (this.secondCommandError ? ".bg-yellow" : ""),
                    {
                        value: this.secondCommand,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.secondCommand = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),
                this.secondCommandError ? m("div.i.bg-yellow", this.secondCommandError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command from ", m("i", (this.secondContext || "the second context")), "?"),

                m("textarea.ml2" + (this.secondReplyError ? ".bg-yellow" : ""),
                    {
                        rows: 10,
                        cols: 60,
                        value: this.secondReply,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.secondReply = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        }
                    },
                ),

                this.secondReplyError ? m("div.i.bg-yellow", this.secondReplyError) : [],

                help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),

                ///////////////////////////////////////

                m("h3", "Generate Rules"),

                m("div.commandsToGenerate",
                 
                    (this.firstCommand || this.secondCommand) 
                        ? m("p", "Commands to generate:")
                        : [],

                    m("div.ml3",
                        (!this.firstCommand)
                            ? []
                            : m("p", this.firstContext + "  ---  " + this.firstCommand + "  -->  " + this.secondContext),

                        (!this.secondCommand)
                            ? []
                            : m("p", this.secondContext + "  ---  " + this.secondCommand + "  -->  " + this.firstContext),
                    )
                ),
    
                // m("p", "You have completed the information the wizard needs to generate new rules to link the two contexts you have chosen."),
                
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
