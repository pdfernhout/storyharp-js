import * as m from "mithril"
import { expander } from "./common"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TWorld } from "./TWorld"
import { Glyph } from "./VariablesView"
import { TSDomain } from "./TSDomain"
import { TSRule } from "./TSRule"
import { TQuickFillComboBox } from "./TQuickFillComboBox"
import { toast } from "./ToastView"
import { modalConfirm } from "./ModalInputView"

// Previous help: "Making_new_rules_using_the_new_moves_wizard"

/* Example command and reply for house and yard:

house : break through wall

    The wall crumbles and you find yourself outside. Someone is going to have to do lot of work to do to fix this.

yard : burrow inside

   You dig a tunnel and find yourself inside the house.

*/

export interface LinkWizardData {
    firstContext: string
    firstCommand: string
    firstReply: string
    secondContext: string
    secondCommand: string
    secondReply: string
}

export function newLinkWizardData(): LinkWizardData {
    return {
        firstContext: "",
        firstCommand: "",
        firstReply: "",
        secondContext: "",
        secondCommand: "",
        secondReply: "",
    }
}

// TODO: Figure out how to jump to newly non-disabled reply when leave command combobox before it

export class LinkWizardView {
    domain: TSDomain
    linkWizardData: LinkWizardData

    firstContextError: string = ""
    firstCommandError: string = ""
    firstReplyError: string = ""
    secondContextError: string = ""
    secondCommandError: string = ""
    secondReplyError: string = ""

    wasGenerateRulesPressed = false

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.linkWizardData = this.domain.linkWizardData
        this.initializeContexts()
    }

    initializeContexts() {
        const world: TWorld = this.domain.world

        for (let i = 0; i < world.variables.length; i++) {
            const variable = world.variables[i]
            if (variable.selected) {
                if (!this.linkWizardData.firstContext) {
                    this.linkWizardData.firstContext = variable.phrase
                } else {
                    this.linkWizardData.secondContext = variable.phrase
                    break
                }
            }
        }
    }

    checkInputForErrors() {
        this.firstContextError = ""
        this.firstCommandError = ""
        this.firstReplyError = ""
        this.secondContextError = ""
        this.secondCommandError = ""
        this.secondReplyError = ""

        if (this.linkWizardData.firstContext.trim() === this.linkWizardData.secondContext.trim()) {
            this.firstContextError = "The two contexts must have different names."
            this.secondContextError = "The two contexts must have different names."
        }

        if (!this.linkWizardData.firstContext.trim()) {
            this.firstContextError = "Both contexts must be entered to proceed."
        }

        if (!this.linkWizardData.secondContext.trim()) {
            this.secondContextError = "Both contexts must be entered to proceed."
        }

        if (!this.linkWizardData.firstCommand.trim() && this.linkWizardData.firstReply.trim()) {
            this.firstCommandError = "You must enter a command phrase if you enter a reply."
        }

        if (!this.linkWizardData.secondCommand.trim() && this.linkWizardData.secondReply.trim()) {
            this.secondCommandError = "You must enter a command phrase if you enter a reply."
        }

        if (!this.linkWizardData.firstCommand.trim() && !this.linkWizardData.secondCommand.trim()) {
            this.firstCommandError = "You must enter at least one command to generate a link."
            this.secondCommandError = "You must enter at least one command to generate a link."
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
        if (!command || !firstContext || !secondContext) {
            return null
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
        this.wasGenerateRulesPressed = true
        if (this.checkInputForErrors()) {
            toast("Please fix the highlighted issues and try again.")
            return
        }

        this.domain.addToLog("--- link wizard")
        this.domain.addToLog(this.linkWizardData.firstContext)
        this.domain.addToLog(this.linkWizardData.firstCommand)
        this.domain.addToLog(this.linkWizardData.firstReply)
        this.domain.addToLog(this.linkWizardData.secondContext)
        this.domain.addToLog(this.linkWizardData.secondCommand)
        this.domain.addToLog(this.linkWizardData.secondReply)

        const newRulesCommand = new TSNewRulesCommand(this.domain)
        newRulesCommand.creator = "link wizard"

        const newRule1 = this.makeLink(
            this.linkWizardData.firstContext.trim(),
            this.linkWizardData.secondContext.trim(),
            this.linkWizardData.firstCommand.trim(),
            this.linkWizardData.firstReply.trim()
        )
        if (newRule1 !== null) {
            newRulesCommand.addRule(newRule1)
        }

        const newRule2 = this.makeLink(
            this.linkWizardData.secondContext.trim(),
            this.linkWizardData.firstContext.trim(),
            this.linkWizardData.secondCommand.trim(),
            this.linkWizardData.secondReply.trim()
        )
        if (newRule2 !== null) {
            newRulesCommand.addRule(newRule2)
        }

        if (newRulesCommand.rules.length > 0) {
            this.domain.worldCommandList.doCommand(newRulesCommand)
            const plural = newRulesCommand.rules.length === 1 ? "" : "s"
            toast("A total of " + newRulesCommand.rules.length + " new rule" + plural + " were generated")
        } else {
            toast("No rules were generated")
            return
        }

        this.wasGenerateRulesPressed = false
    }
    
    view() {
        function caption(text: string) { return text }
        const showHelp = this.domain.showWizardHelp
        function help(...args: string[]) {
            return showHelp ? m("p", ...args) : []
        }
        
        return m(".LinkWizardView.h-100.w-100.overflow-auto",
            {
            },
            m("div",
                m("h2", "New Links Wizard"),

                m("div", {onclick: () => this.domain.showWizardHelp = !this.domain.showWizardHelp }, "Show wizard help", expander(showHelp, "", "(Click to close help)")),

                help("This wizard will link up two contexts by creating rules with commands to move between them."),

                help("A command (", Glyph.command, ") is what you say to the computer."),
                help("A context (", Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."),
                help("A reply (", Glyph.reply, ") is what the computer says after you say a command."),

                help("You can enter a reply for each command."),

                ///////////////////////////////////////

                m("h3", "Contexts"),

                help("Choose two contexts to move between."), //  The order doesn't matter."),
 
                // First Context

                m("p", "First context (", Glyph.context, "):"),

                m(TQuickFillComboBox,
                    <any>{
                        extraStyling: (this.firstContextError ? ".ml2.bg-yellow" : ".ml2"),
                        value: this.linkWizardData.firstContext,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.linkWizardData.firstContext = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        items: this.domain.world.getContextNames(),
                    },
                ),
                this.firstContextError ? m("div.i.bg-yellow", this.firstContextError) : [],

                // Second Context

                m("p", "Second context (", Glyph.context, "):"),

                m(TQuickFillComboBox,
                    <any>{
                        extraStyling: (this.secondContextError ? ".ml2.bg-yellow" : ".ml2"),
                        value: this.linkWizardData.secondContext,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.linkWizardData.secondContext = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        items: this.domain.world.getContextNames(),
                    },
                ),
                this.secondContextError ? m("div.i.bg-yellow", this.secondContextError) : [],


                help("You can also choose these two contexts by selecting them in the Map (Control-click to select the second context) before you open the wizard."),

                ///////////////////////////////////////

                m("h3", "Forward ➞"),

                m("p", "What command (", Glyph.command, ") should the user say to move from ", m("i", (this.linkWizardData.firstContext || "the first context")), " to ", m("i", (this.linkWizardData.secondContext || "the second context")), ":"),
                m(TQuickFillComboBox,
                    <any>{
                        extraStyling: (this.firstCommandError ? ".ml2.bg-yellow" : ".ml2"),
                        value: this.linkWizardData.firstCommand,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.linkWizardData.firstCommand = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        items: this.domain.world.getCommandNames(),
                    },
                ),
                this.firstCommandError ? m("div.i.bg-yellow", this.firstCommandError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("div.ml2" + (this.linkWizardData.firstCommand ? "" : ".gray"),

                    m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command from ", m("i", (this.linkWizardData.firstContext || "the first context")), "?"),

                    m("textarea.ml2" + (this.firstReplyError ? ".bg-yellow" : ""),
                        {
                            rows: 10,
                            cols: 60,
                            value: this.linkWizardData.firstReply,
                            oninput: (event: { target: HTMLInputElement }) => {
                                this.linkWizardData.firstReply = event.target.value
                                if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                            },
                            disabled: !this.linkWizardData.firstCommand || null,
                        },
                    ),

                    this.firstReplyError ? m("div.i.bg-yellow", this.firstReplyError) : [],

                    help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),
                
                ),

                ///////////////////////////////////////

                m("h3", "Backward ↩"),

                m("p", "What command (", Glyph.command, ") should the user say to move from ", m("i", (this.linkWizardData.secondContext || "the second context")), (this.linkWizardData.firstCommand ? " back" : []), " to ", m("i", (this.linkWizardData.firstContext || "the first context")), ":"),
                m(TQuickFillComboBox,
                    <any>{
                        extraStyling: (this.secondCommandError ? ".ml2.bg-yellow" : ".ml2"),
                        value: this.linkWizardData.secondCommand,
                        onchange: (event: { target: HTMLInputElement }) => {
                            this.linkWizardData.secondCommand = event.target.value
                            if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                        },
                        items: this.domain.world.getCommandNames(),
                    },
                ),
                this.secondCommandError ? m("div.i.bg-yellow", this.secondCommandError) : [],

                help("Leave this blank if you don't want to move this way. Examples are \"move forward\", \"go east\", \"leap up\", \"enter the building\", and \"activate the transporter\"."),

                m("div.ml2" + (this.linkWizardData.secondCommand ? "" : ".gray"),

                m("p", "What should the computer reply (", Glyph.reply, ") after the user says the move command from ", m("i", (this.linkWizardData.secondContext || "the second context")), "?"),

                    m("textarea.ml2" + (this.secondReplyError ? ".bg-yellow" : ""),
                        {
                            rows: 10,
                            cols: 60,
                            value: this.linkWizardData.secondReply,
                            oninput: (event: { target: HTMLInputElement }) => {
                                this.linkWizardData.secondReply = event.target.value
                                if (this.wasGenerateRulesPressed) this.checkInputForErrors()
                            },
                            disabled: !this.linkWizardData.secondCommand || null,
                        },
                    ),

                    this.secondReplyError ? m("div.i.bg-yellow", this.secondReplyError) : [],

                    help("Leave this blank to get a default reply of \"You\" plus the command phrase. For example, for \"go east\" the default would be \"You go east\"."),

                ),
                ///////////////////////////////////////

                m("h3", "Generate Rules"),

                m("div.commandsToGenerate",
                 
                    (this.linkWizardData.firstCommand || this.linkWizardData.secondCommand) 
                        ? m("p", "Commands to generate:")
                        : [],

                    m("div.ml3",
                        (!this.linkWizardData.firstCommand)
                            ? []
                            : m("p", this.linkWizardData.firstContext + "  ---  " + this.linkWizardData.firstCommand + "  -->  " + this.linkWizardData.secondContext),

                        (!this.linkWizardData.secondCommand)
                            ? []
                            : m("p", this.linkWizardData.secondContext + "  ---  " + this.linkWizardData.secondCommand + "  -->  " + this.linkWizardData.firstContext),
                    )
                ),
    
                m("p", "Click the \"Generate Rules\" button to create the new rules."),
                
                m("div.ml2", 
                    m("button", {
                        onclick: () => this.generateRules()
                    }, "Generate rules"),
                ),

                help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."),
                help("The new rules will also initally be selected in the rules table."),
                help("The text you entered here to generate rules will also be saved in the log file if you need to recover it later."),

                m("div.ml2.mt2.mb3", 
                    m("button", {
                        onclick: () => {
                            modalConfirm("Are you sure you want to clear the Link Wizard form?").then(value => {
                                if (!value) return
                                this.domain.linkWizardData = newLinkWizardData()
                                this.linkWizardData = this.domain.linkWizardData
                                this.firstContextError = ""
                                this.firstCommandError = ""
                                this.firstReplyError = ""
                                this.secondContextError = ""
                                this.secondCommandError = ""
                                this.secondReplyError = ""
                                this.wasGenerateRulesPressed = false
                            })
                        }
                    }, "Clear Link Wizard form"),
                ),
            ),
        )
    }
}
