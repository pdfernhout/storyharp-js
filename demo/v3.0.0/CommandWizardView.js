define(["require", "exports", "mithril", "./common", "./TSNewRulesCommand", "./VariablesView", "./TQuickFillComboBox", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, TSNewRulesCommand_1, VariablesView_1, TQuickFillComboBox_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const exampleWithFiveRules = `
tell Shawn you are angry | Shawn rolls up his cocoon.
tell Shawn you are happy| Shawn rolls up his cocoon.

   tell Shawn he is wrong |You are sent to the bad place.
    
spit at Shawn
throw a cactus at Shawn | It bounces off Shawn's quickly-rolled-up cocoon.
`.trim();
    const exampleSequenceWithFourRules = `    
talk to the grue | The grue won't listen.
talk to the grue | The grue seems to be getting very agitated.
talk to the grue | The grue seems about to fly into a rage.
talk to the grue | The grue devours you (except your bones of course).
`.trim();
    const defaultReply = "Nothing happens.";
    var EndSequence;
    (function (EndSequence) {
        EndSequence["noSelection"] = "noSelection";
        EndSequence["loopToFirst"] = "loopToFirst";
        EndSequence["leaveLastCommand"] = "leaveLastCommand";
        EndSequence["removeLastCommand"] = "removeLastCommand";
    })(EndSequence || (EndSequence = {}));
    function newCommandWizardData() {
        return {
            newCommandsTextToParse: "",
            contextName: "",
            prefix: "",
            endSequence: EndSequence.noSelection,
            doSequence: false,
        };
    }
    exports.newCommandWizardData = newCommandWizardData;
    class CommandWizardView {
        constructor(vnode) {
            this.newCommandsTextToParseError = "";
            this.contextNameError = "";
            this.prefixError = "";
            this.endSequenceError = "";
            this.wasGenerateRulesPressed = false;
            this.domain = vnode.attrs.domain;
            this.commandWizardData = this.domain.commandWizardData;
        }
        checkInputForErrors() {
            if (!this.commandWizardData.newCommandsTextToParse.trim()) {
                this.newCommandsTextToParseError = "You must enter one or more commands to generate rules.";
            }
            else {
                this.newCommandsTextToParseError = "";
            }
            if (!this.commandWizardData.contextName.trim()) {
                this.contextNameError = "You must enter a context to be used with these commands.";
            }
            else {
                this.contextNameError = "";
            }
            if (this.commandWizardData.doSequence) {
                if (!this.commandWizardData.prefix.trim()) {
                    this.prefixError = "You must enter a prefix.";
                }
                else {
                    this.prefixError = "";
                }
                if (this.commandWizardData.endSequence === EndSequence.noSelection) {
                    this.endSequenceError = "You must select an option to end the sequence.";
                }
                else {
                    this.endSequenceError = "";
                }
            }
            else {
                this.prefixError = "";
                this.endSequenceError = "";
            }
            return this.newCommandsTextToParseError || this.contextNameError || this.prefixError || this.endSequenceError;
        }
        generateRules() {
            this.wasGenerateRulesPressed = true;
            if (this.checkInputForErrors()) {
                ToastView_1.toast("Please fix the highlighted issues and try again.");
                return;
            }
            const contextName = this.commandWizardData.contextName.trim();
            const prefix = this.commandWizardData.prefix.trim();
            const world = this.domain.world;
            const ruleEditorForm = this.domain.ruleEditorForm;
            this.domain.addToLog("--- command wizard");
            this.domain.addToLog(this.commandWizardData.contextName);
            this.domain.addToLog(this.commandWizardData.prefix);
            this.domain.addToLog(this.commandWizardData.newCommandsTextToParse);
            const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
            newRulesCommand.creator = "command sequence wizard";
            const lines = this.commandWizardData.newCommandsTextToParse.split(/\r\n|\r|\n/);
            let index = 1;
            let newRule = null;
            for (let line of lines) {
                line = line.trim();
                if (!line)
                    continue;
                const pipeBarLocation = line.indexOf("|");
                let command;
                let reply;
                if (pipeBarLocation === -1) {
                    command = line;
                    reply = defaultReply;
                }
                else {
                    command = line.substring(0, pipeBarLocation).trim() || ("missing command " + Math.random());
                    reply = line.substring(pipeBarLocation + 1).trim() || defaultReply;
                }
                newRule = world.newRule();
                newRule.setContext(this.commandWizardData.contextName);
                newRule.setCommand(command);
                newRule.setReply(reply);
                newRule.selected = true;
                newRulesCommand.addRule(newRule);
                ruleEditorForm.lastChoice = newRule;
                this.domain.editRule(newRule);
                if (this.commandWizardData.doSequence && (prefix !== "")) {
                    let requirements;
                    let changes;
                    if (index === 1) {
                        requirements = "~" + prefix + " started";
                        changes = prefix + " started & " + prefix + " " + index + "0";
                    }
                    else {
                        requirements = prefix + " " + (index - 1) + "0";
                        changes = "~" + prefix + " " + (index - 1) + "0 & " + prefix + " " + index + "0";
                    }
                    newRule.setRequirements(requirements);
                    newRule.setChanges(changes);
                }
                index += 1;
            }
            if (this.commandWizardData.doSequence && (prefix !== "") && (newRule !== null) && (index > 2)) {
                let changes = newRule.changesString;
                if (this.commandWizardData.endSequence === EndSequence.loopToFirst) {
                    changes = "~" + prefix + " " + (index - 2) + "0 & " + "~" + prefix + " started";
                }
                else if (this.commandWizardData.endSequence === EndSequence.leaveLastCommand) {
                    changes = "";
                }
                else if (this.commandWizardData.endSequence === EndSequence.removeLastCommand) {
                    changes = "~" + prefix + " " + (index - 2) + "0";
                }
                else {
                    throw new Error("unexpected case");
                }
                newRule.setChanges(changes);
            }
            if (newRulesCommand.rules.length > 0) {
                this.domain.worldCommandList.doCommand(newRulesCommand);
                const plural = newRulesCommand.rules.length === 1 ? "" : "s";
                ToastView_1.toast("A total of " + newRulesCommand.rules.length + " new rule" + plural + " were generated");
            }
            else {
                ToastView_1.toast("No rules were generated");
                return;
            }
            this.wasGenerateRulesPressed = false;
        }
        view() {
            function caption(text) { return text; }
            const showHelp = this.domain.showWizardHelp;
            function help(...args) {
                return showHelp ? m("p", ...args) : [];
            }
            const doSequence = this.commandWizardData.doSequence;
            return m(".CommandWizardView.h-100.w-100.overflow-auto", {}, m("div", m("h2", "New Commands Wizard"), m("div", { onclick: () => this.domain.showWizardHelp = !this.domain.showWizardHelp }, "Show wizard help", common_1.expander(showHelp, "", "(Click to close help)")), help("This wizard will create a set of new rules based on one context and a list of commands you enter."), help("You can enter a reply for each command."), help("A command (", VariablesView_1.Glyph.command, ") is what you say to the computer."), help("A context (", VariablesView_1.Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."), help("A reply (", VariablesView_1.Glyph.reply, ") is what the computer says after you say a command."), help("You can also link your new commands in a sequence so that only one command is available at any time. ", "This is done by generating requirements and changes."), help("A requirement (", VariablesView_1.Glyph.requirements, ") is variable state necessary for a command to be available. Variables can be true or false."), help("A change (", VariablesView_1.Glyph.changes, ") is a new variable state resulting from a command."), help("You can add extra requirements or changes later using the editor."), m("h3", "Context"), m("p", "What context (", VariablesView_1.Glyph.context, ") do you want your new commands to use?"), m(TQuickFillComboBox_1.TQuickFillComboBox, {
                extraStyling: (this.contextNameError ? ".ml2.bg-yellow" : ".ml2"),
                value: this.commandWizardData.contextName,
                onchange: (event) => {
                    this.commandWizardData.contextName = event.target.value;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                items: this.domain.world.getContextNames(),
            }), this.contextNameError ? m("div.i.bg-yellow", this.contextNameError) : [], help("Some generic examples of a context are: \"cave\", \"forest\", and \"inside house\"."), m("h3", "Commands"), help("Enter or paste the commands you want to create in the area below, ", "separating each command from its reply by a pipe bar."), help("For example, \"open the door | You open the rusty door.\"."), help("Use carriage returns to separate entries -- one entry per line. Blank lines will be ignored."), help("Replies are optional. It's okay if long replies wrap around in the editor as long as they do not have a carriage return in them."), help("If do not enter descriptive reply for a context, the wizard will add a default description of \"" + defaultReply + "\""), help("You can also use the same context more than once and later add special requirements (", VariablesView_1.Glyph.requirements, ") to some of the extra rules"), help("Here is an example of a sequence (intentionally showing inconsistent input styles) which generates five rules:"), showHelp ? m("pre.dib.ba.bw2.pa1.ml2.mr2", exampleWithFiveRules) : [], m("div.ma2", "Command (", VariablesView_1.Glyph.command, ")", m("span.ml2.mr2.f4.b", "|"), "Reply (", VariablesView_1.Glyph.reply, ")"), m("div.b", "New commands ", this.commandWizardData.contextName ? "for: " + this.commandWizardData.contextName : ""), m("textarea.ml2" + (this.newCommandsTextToParseError ? ".bg-yellow" : ""), {
                rows: 10,
                cols: 60,
                value: this.commandWizardData.newCommandsTextToParse,
                oninput: (event) => {
                    this.commandWizardData.newCommandsTextToParse = event.target.value;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                }
            }), this.newCommandsTextToParseError ? m("div.i.bg-yellow", this.newCommandsTextToParseError) : [], m("h3", "Sequence"), help("Sequences are useful when the user has to do several things in a certain order, ", "such as steps in an assembly process or parts of a conversation."), m("div", "Do you want to link your new commands in sequence, so that each becomes available only after the previous one is said?"), m("div.ma2", m("label.dib.ml1", m("input[type=checkbox]", {
                checked: doSequence || undefined,
                onchange: (event) => {
                    this.commandWizardData.doSequence = event.target.checked;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                }
            }), m("span.ml1", "Yes, link the commands in a sequence."))), m("div.ml2" + (doSequence ? "" : ".gray"), help("Here is an example of a sequence (see below) which could generate four rules for a \"grue pit\" context each using the same command:"), showHelp ? m("pre.dib.ba.bw2.pa1.ml2.mr2", exampleSequenceWithFourRules) : [], m("p", " What prefix do you want to use for the requirements that create the sequence?"), m("input", {
                value: this.commandWizardData.prefix,
                oninput: (event) => {
                    this.commandWizardData.prefix = event.target.value;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                disabled: !doSequence || null,
            }), this.prefixError ? m("div.i.bg-yellow", this.prefixError) : [], help("Examples are: \"talking to sailor\", \"in boarding house\". By default the prefix is the same as the context."), m("p", "When the last command has been said,"), m("input[type=radio]", {
                name: "endSequence",
                value: EndSequence.loopToFirst,
                checked: this.commandWizardData.endSequence === EndSequence.loopToFirst,
                onchange: () => {
                    this.commandWizardData.endSequence = EndSequence.loopToFirst;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                disabled: !doSequence || null,
            }), "loop to the first command in the sequence", m("br"), m("input[type=radio]", {
                name: "endSequence",
                value: EndSequence.leaveLastCommand,
                checked: this.commandWizardData.endSequence === EndSequence.leaveLastCommand,
                onchange: () => {
                    this.commandWizardData.endSequence = EndSequence.leaveLastCommand;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                disabled: !doSequence || null,
            }), "leave the last command available", m("br"), m("input[type=radio]", {
                name: "endSequence",
                value: EndSequence.removeLastCommand,
                checked: this.commandWizardData.endSequence === EndSequence.removeLastCommand,
                onchange: () => {
                    this.commandWizardData.endSequence = EndSequence.removeLastCommand;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                disabled: !doSequence || null,
            }), "remove the last command", m("br"), this.endSequenceError ? m("div.i.bg-yellow", this.endSequenceError) : []), m("h3", "Generate Rules"), m("p", "Click the \"Generate Rules\" button to create the new rules."), m("div.ml2", m("button", {
                onclick: () => this.generateRules()
            }, "Generate rules")), help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."), help("The new rules will also initally be selected in the rules table."), help("The command and reply text you entered here to generate rules will also be saved in the log file if you need to recover it later."), m("div.ml2.mt2.mb3", m("button", {
                onclick: () => {
                    ModalInputView_1.modalConfirm("Are you sure you want to clear the Command Wizard form?").then(value => {
                        if (!value)
                            return;
                        this.domain.commandWizardData = newCommandWizardData();
                        this.commandWizardData = this.domain.commandWizardData;
                        this.newCommandsTextToParseError = "";
                        this.contextNameError = "";
                        this.prefixError = "";
                        this.endSequenceError = "";
                        this.wasGenerateRulesPressed = false;
                    });
                }
            }, "Clear Command Wizard form"))));
        }
    }
    exports.CommandWizardView = CommandWizardView;
});
