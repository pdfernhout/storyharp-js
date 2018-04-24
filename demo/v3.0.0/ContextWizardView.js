define(["require", "exports", "mithril", "./common", "./TSNewRulesCommand", "./VariablesView", "./TQuickFillComboBox", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, TSNewRulesCommand_1, VariablesView_1, TQuickFillComboBox_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const exampleOne = `
cave|You are in a big cave.
forest|You are in a lively forest.
spring|You are standing near a burbling spring.`.trim();
    const exampleWithNineRules = `    
well house | You are in a well house for a small spring.
grate | You are standing above a grate.
forest | You are wandering around in dense forest.
forest | It is getting dark and hard to see.
mossy clearing
glade | You are in a forest glade.

tree tops

path by rock|you are on a path in the forest besides a big rock
stream | You are walking along a dry stream bed.`.trim();
    const defaultCommand = "look";
    const defaultReply = "There is nothing of interest here.";
    function newContextWizardData() {
        return {
            newContextsTextToParse: "",
            commandPhrase: defaultCommand,
        };
    }
    exports.newContextWizardData = newContextWizardData;
    class ContextWizardView {
        constructor(vnode) {
            this.newContextsTextToParseError = "";
            this.commandPhraseError = "";
            this.wasGenerateRulesPressed = false;
            this.domain = vnode.attrs.domain;
            this.contextWizardData = this.domain.contextWizardData;
        }
        checkInputForErrors() {
            if (!this.contextWizardData.newContextsTextToParse.trim()) {
                this.newContextsTextToParseError = "You must enter one or more contexts to generate rules.";
            }
            else {
                this.newContextsTextToParseError = "";
            }
            if (!this.contextWizardData.commandPhrase.trim()) {
                this.commandPhraseError = "You must enter a command to be used to describe these contexts.";
            }
            else {
                this.commandPhraseError = "";
            }
            return this.newContextsTextToParseError || this.commandPhraseError;
        }
        generateRules() {
            this.wasGenerateRulesPressed = true;
            if (this.checkInputForErrors()) {
                ToastView_1.toast("Please fix the highlighted issues and try again.");
                return;
            }
            const commandPhrase = this.contextWizardData.commandPhrase.trim();
            const world = this.domain.world;
            const ruleEditorForm = this.domain.ruleEditorForm;
            this.domain.addToLog("--- context wizard");
            this.domain.addToLog(this.contextWizardData.commandPhrase);
            this.domain.addToLog(this.contextWizardData.newContextsTextToParse);
            const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
            newRulesCommand.creator = "new context wizard";
            const lines = this.contextWizardData.newContextsTextToParse.split(/\r\n|\r|\n/);
            for (let line of lines) {
                line = line.trim();
                if (!line)
                    continue;
                const pipeBarLocation = line.indexOf("|");
                let context;
                let reply;
                if (pipeBarLocation === -1) {
                    context = line;
                    reply = defaultReply;
                }
                else {
                    context = line.substring(0, pipeBarLocation).trim() || ("missing context " + Math.random());
                    reply = line.substring(pipeBarLocation + 1).trim() || defaultReply;
                }
                const newRule = world.newRule();
                newRule.setContext(context);
                newRule.setCommand(commandPhrase);
                newRule.setReply(reply);
                newRule.selected = true;
                newRulesCommand.addRule(newRule);
                ruleEditorForm.lastChoice = newRule;
                this.domain.editRule(newRule);
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
            return m(".ContextWizardView.h-100.w-100.overflow-auto", {}, m("div", m("h2", "New Contexts Wizard"), m("div", { onclick: () => this.domain.showWizardHelp = !this.domain.showWizardHelp }, "Show wizard help", common_1.expander(showHelp, "", "(Click to close help)")), help("This wizard will create new rules defining contexts and replies to a common command like \"look\"."), help("A command (", VariablesView_1.Glyph.command, ") is what you say to the computer."), help("A context (", VariablesView_1.Glyph.context, ") is the single most important requirement to make a command available -- usually a physical location."), help("A reply (", VariablesView_1.Glyph.reply, ") is what the computer says after you say a command."), help("You can enter a descriptive reply for each new context. ", "The descriptive replies will be accessed with a common command such as \"look\"."), m("h3", "Enter Contexts"), help("Enter or paste the contexts you want to create in the area below, ", "separating each context from its descriptive reply by a pipe bar."), help("For example, \"house | You are in a house\"."), help("Use carriage returns to separate entries -- one entry per line. Blank lines will be ignored."), help("Replies are optional. It's okay if long replies wrap around in the editor as long as they do not have a carriage return in them."), help("If do not enter descriptive reply for a context, the wizard will add a default description of \"" + defaultReply + "\""), help("You can also use the same context more than once and later add special requirements (", VariablesView_1.Glyph.requirements, ") to some of the extra rules"), help("Here is an example showing a mix of different entries which generated nine rules:"), showHelp ? m("pre.dib.ba.bw2.pa1.ml2.mr2", exampleWithNineRules) : [], m("div.ma2", "Context (", VariablesView_1.Glyph.context, ")", m("span.ml2.mr2.f4.b", "|"), "Descriptive Reply (", VariablesView_1.Glyph.reply, ")"), m("textarea.ml2" + (this.newContextsTextToParseError ? ".bg-yellow" : ""), {
                rows: 10,
                cols: 60,
                value: this.contextWizardData.newContextsTextToParse,
                oninput: (event) => {
                    this.contextWizardData.newContextsTextToParse = event.target.value;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                }
            }), this.newContextsTextToParseError ? m("div.i.bg-yellow", this.newContextsTextToParseError) : [], m("h3", "Generate Descriptions"), m("p", "What command (", VariablesView_1.Glyph.command, ") should the user to say to access these descriptive replies?"), m(TQuickFillComboBox_1.TQuickFillComboBox, {
                extraStyling: (this.commandPhraseError ? ".ml2.bg-yellow" : ".ml2"),
                value: this.contextWizardData.commandPhrase,
                onchange: (event) => {
                    this.contextWizardData.commandPhrase = event.target.value;
                    if (this.wasGenerateRulesPressed)
                        this.checkInputForErrors();
                },
                items: this.domain.world.getCommandNames(),
            }), this.commandPhraseError ? m("div.i.bg-yellow", this.commandPhraseError) : [], help("Some generic examples are: \"look\", \"listen\", \"smell\", \"feel\", \"taste\", and \"sense\"."), help("You should stick with \"look\" unless you are doing something special. ", "You can change individual commands later (in the editor) to deal with specific situations."), m("p", "Click the \"Generate Rules\" button to create the new rules."), m("div.ml2", m("button", {
                onclick: () => this.generateRules()
            }, "Generate rules")), help("After you have generated new rules, if you change your mind, you can choose Undo from the Edit menu to remove your new rules."), help("The new rules will also initally be selected in the rules table."), help("The text you entered here to generate rules will also be saved in the log file if you need to recover it later."), m("div.ml2.mt2.mb3", m("button", {
                onclick: () => {
                    ModalInputView_1.modalConfirm("Are you sure you want to clear the Context Wizard form?").then(value => {
                        if (!value)
                            return;
                        this.domain.contextWizardData = newContextWizardData();
                        this.contextWizardData = this.domain.contextWizardData;
                        this.newContextsTextToParseError = "";
                        this.commandPhraseError = "";
                        this.wasGenerateRulesPressed = false;
                    });
                }
            }, "Clear Context Wizard form"))));
        }
    }
    exports.ContextWizardView = ContextWizardView;
});
