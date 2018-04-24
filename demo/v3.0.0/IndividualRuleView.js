define(["require", "exports", "mithril", "./common", "./TSRule", "./TSNewRulesCommand", "./TSDomain", "./TQuickFillComboBox", "./VariablesView", "./TSLogicListBox", "./ConsoleForm", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, TSRule_1, TSNewRulesCommand_1, TSDomain_1, TQuickFillComboBox_1, VariablesView_1, TSLogicListBox_1, ConsoleForm_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const kPlaySoundMacroStart = "sound ";
    const kPlayMusicMacroStart = "music ";
    const kShowPictureMacroStart = "picture ";
    function insertTextAtCursor(element, textToInsert) {
        let newText;
        if (element.selectionStart || element.selectionStart === 0) {
            var startPos = element.selectionStart;
            var endPos = element.selectionEnd;
            newText = element.value.substring(0, startPos) + textToInsert + element.value.substring(endPos, element.value.length);
            element.value = newText;
            element.selectionStart = startPos + textToInsert.length;
            element.selectionEnd = startPos + textToInsert.length;
        }
        else {
            newText = element.value + textToInsert;
            element.value = newText;
        }
        return newText;
    }
    class IndividualRuleView {
        constructor(vnode) {
            this.expanded = true;
            this.testPictures = [];
            this.cachedRule = null;
            this.cachedInitialValue = "";
            this.cachedReply = "";
            this.domain = vnode.attrs.domain;
            this.ruleEditorForm = vnode.attrs.ruleEditorForm;
        }
        RuleNewClick() {
            const world = this.domain.world;
            const rule = this.domain.editedRule;
            const worldCommandList = this.domain.worldCommandList;
            const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
            const newRule = world.newRule();
            newRulesCommand.addRule(newRule);
            const variable = world.firstSelectedVariable();
            if (variable !== null) {
                newRule.setContext(variable.phrase);
            }
            else if (rule !== null) {
                newRule.setContext(rule.context.phrase);
            }
            world.deselectAllExcept(newRule);
            newRule.selected = true;
            worldCommandList.doCommand(newRulesCommand);
            this.domain.editRule(newRule);
            this.expanded = true;
        }
        RuleDuplicateClick() {
            const world = this.domain.world;
            const rule = this.domain.editedRule;
            const worldCommandList = this.domain.worldCommandList;
            if (rule === null) {
                return;
            }
            const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
            newRulesCommand.creator = "duplicating";
            const newRule = world.newRule();
            newRulesCommand.addRule(newRule);
            newRule.setContext(rule.context.phrase);
            newRule.setCommand(rule.command.phrase);
            newRule.setReply(rule.reply);
            newRule.setMove(rule.move.phrase);
            newRule.setRequirements(rule.requirementsString);
            newRule.setChanges(rule.changesString);
            world.deselectAllExcept(newRule);
            newRule.selected = true;
            worldCommandList.doCommand(newRulesCommand);
            this.domain.editRule(newRule);
        }
        RuleDeleteClick() {
            const rule = this.domain.editedRule;
            const worldCommandList = this.domain.worldCommandList;
            if ((rule !== null) && (rule.selected)) {
                this.domain.editRule(null);
            }
            worldCommandList.deleteSelectedRules();
            this.domain.ruleEditorForm.previousChoice = null;
            this.domain.ruleEditorForm.lastChoice = null;
        }
        MoveDownClick() {
            const worldCommandList = this.domain.worldCommandList;
            worldCommandList.lowerSelectedRules();
        }
        MoveUpClick() {
            const worldCommandList = this.domain.worldCommandList;
            worldCommandList.raiseSelectedRules();
        }
        SpeedButtonClick(field, selectedVariable = null) {
            this.domain.currentEditorView = "browser";
            this.domain.setOrganizeByField(field, selectedVariable);
        }
        insertSoundClick() {
            ModalInputView_1.modalPrompt("Sound file URL?").then(url => {
                if (!url)
                    return;
                const textToInsert = " {" + kPlaySoundMacroStart + url + "} ";
                const rule = this.domain.editedRule;
                if (rule) {
                    const newText = insertTextAtCursor(this.replyTextArea, textToInsert);
                    rule.setReply(newText);
                }
            });
        }
        insertMusicClick() {
            ModalInputView_1.modalPrompt("Music file URL?").then(url => {
                if (!url)
                    return;
                const textToInsert = " {" + kPlayMusicMacroStart + url + "} ";
                const rule = this.domain.editedRule;
                if (rule) {
                    const newText = insertTextAtCursor(this.replyTextArea, textToInsert);
                    rule.setReply(newText);
                }
            });
        }
        insertImageClick() {
            ModalInputView_1.modalPrompt("Image file URL?").then(url => {
                if (!url)
                    return;
                const textToInsert = " {" + kShowPictureMacroStart + url + "} ";
                const rule = this.domain.editedRule;
                if (rule) {
                    const newText = insertTextAtCursor(this.replyTextArea, textToInsert);
                    rule.setReply(newText);
                }
            });
        }
        view() {
            const world = this.domain.world;
            const worldCommandList = this.domain.worldCommandList;
            const rule = this.domain.editedRule;
            if (!rule) {
                this.cachedReply = "";
            }
            else if (rule !== this.cachedRule || this.cachedInitialValue !== rule.reply) {
                this.cachedInitialValue = rule.reply;
                this.cachedReply = rule.reply;
                this.cachedRule = rule;
            }
            const requirements = [];
            const changes = [];
            if (rule) {
                rule.compile(rule.decompile(rule.requirements), requirements);
                rule.compile(rule.decompile(rule.changes), changes);
            }
            function InsertMusicButtonClick() { console.log("InsertMusicButtonClick"); }
            function contextChange(event) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleContext, event.target.value);
            }
            function commandChange(event) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleCommand, event.target.value);
            }
            const replyInput = (event) => {
                if (!rule)
                    throw new Error("Rule must be defined first");
                this.cachedReply = event.target.value;
            };
            function replyChange(event) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleReply, event.target.value);
            }
            function moveChange(event) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleMove, event.target.value);
            }
            function requirementsChange(newRequirements) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                const value = rule.decompile(newRequirements);
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleRequirements, value);
            }
            function changesChange(newChanges) {
                if (!rule)
                    throw new Error("Rule must be defined first");
                const value = rule.decompile(newChanges);
                worldCommandList.ruleFieldChange(rule, TSRule_1.TSRuleField.kRuleChanges, value);
            }
            const testReply = () => {
                if (!rule)
                    throw new Error("Rule must be defined first");
                const textWithMacros = rule.reply;
                const isSpeaking = window.speechSynthesis && window.speechSynthesis.speaking;
                this.domain.speechSystem.haltSpeechAndSoundAndMusic();
                this.testPictures = [];
                if (!isSpeaking) {
                    if (window.speechSynthesis) {
                        this.domain.speechSystem.sayTextWithMacros(textWithMacros);
                    }
                    else {
                        ToastView_1.toast("Text-to-Speech system unavailable to say:\n" + textWithMacros);
                    }
                    const segments = ConsoleForm_1.parseTextWithMacros(textWithMacros);
                    for (let segment of segments) {
                        if (segment.type === ConsoleForm_1.SegmentType.showPicture) {
                            const url = segment.text;
                            this.testPictures.push(url);
                        }
                    }
                }
            };
            return m(".IndividualRuleView.ba.bg-light-gray.w-100.pa1", this.testPictures.map(url => {
                return m("div.nowrap.mw6", {
                    onclick: () => {
                        const index = this.testPictures.indexOf(url);
                        if (index >= 0)
                            this.testPictures.splice(index, 1);
                    }
                }, m("img.ml3", {
                    src: TSDomain_1.fixupPath(this.domain, url),
                }), m("span.v-top", "X"));
            }), m("div.PanelButtonBar.TPanel.ma1", {}, m("span", {
                onclick: () => this.expanded = !this.expanded
            }, "Rule Viewer " + common_1.expander(this.expanded)), m("span.dib.w2.RuleNumberLabel.TLabel", {
                title: "The index of the edited rule in the table",
            }, rule ? "#" + (world.rules.indexOf(rule) + 1) : ""), m("button.NewRuleButton.TSpeedButton.ml1", {
                onclick: () => this.RuleNewClick(),
                title: "Make a new rule",
            }, "New"), m("button.DuplicateRuleButton.TSpeedButton.ml1", {
                onclick: () => this.RuleDuplicateClick(),
                disabled: !rule,
                title: "Duplicate the rule showing in the rule editor panel",
            }, "Duplicate"), m("button.DeleteRuleButton.TSpeedButton.ml1", {
                onclick: () => this.RuleDeleteClick(),
                disabled: !rule,
                title: "Delete all selected rules",
            }, "Delete"), m("button.MoveUpButton.TSpeedButton.ml1", {
                onclick: () => this.MoveUpClick(),
                disabled: !rule,
                title: "Raise all selected rules",
            }, "Raise"), m("button.MoveDownButton.TSpeedButton.ml1", {
                onclick: () => this.MoveDownClick(),
                disabled: !rule,
                title: "Lower all selected rules",
            }, "Lower"), (this.expanded && this.domain.editedRule) ? [
                m("button.insertSound.TSpeedButton.ml1", {
                    onclick: () => this.insertSoundClick(),
                    disabled: !rule,
                    title: "Insert a sound into a reply",
                }, "+Sound"),
                m("button.InsertMusicButton.TSpeedButton.ml1", {
                    onclick: () => this.insertMusicClick(),
                    disabled: !rule,
                    title: "Insert music into a reply",
                }, "+Music"),
                m("button.InsertImageButton.TSpeedButton.ml1", {
                    onclick: () => this.insertImageClick(),
                    disabled: !rule,
                    title: "Insert picture image into a reply",
                }, "+Picture"),
            ] : [], m("button.ml4.w3", {
                onclick: () => this.ruleEditorForm.search(),
                title: "Search for a rule containing some text"
            }, "Search")), !this.expanded ? [] : !rule ? ["Please select a rule to edit it"] : [
                m("div.Rule", m(".Context.mt1", m("button.ContextSpeedButton.pt1.w-10rem.w4.mr1", {
                    onclick: () => this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleContext),
                    title: "Browse all rules with this context",
                }, VariablesView_1.Glyph.context + " Context"), m(TQuickFillComboBox_1.TQuickFillComboBox, {
                    extraStyling: ".ContextEdit",
                    style: { width: "35rem", },
                    value: rule.context.phrase,
                    onchange: contextChange,
                    items: this.domain.world.getContextNames(),
                })), m(".Requirements.mt1", m("button.RequirementsSpeedButton.pt1.w-10rem.mr1.v-top", {
                    onclick: () => this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleRequirements),
                    title: "Browse all rules with the selected requirement",
                }, VariablesView_1.Glyph.requirements + " Requirements"), m(TSLogicListBox_1.TSLogicListBox, {
                    style: { width: "35rem", },
                    selections: requirements,
                    items: world.getVariableNames(),
                    world: world,
                    onchange: requirementsChange,
                    onselect: (variable) => {
                        this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleRequirements, variable);
                    }
                })), m(".Command.mt1", m("button.CommandSpeedButton.pt1.w-10rem.mr1", {
                    onclick: () => this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleCommand),
                    title: "Browse all rules with this command",
                }, VariablesView_1.Glyph.command + " Command"), m(TQuickFillComboBox_1.TQuickFillComboBox, {
                    extraStyling: ".CommandEdit",
                    style: { width: "35rem", },
                    value: rule.command.phrase,
                    onchange: commandChange,
                    items: this.domain.world.getCommandNames(),
                })), m(".Reply.flex.items-center.mt1", m("button.ReplySpeedButton.pt1.w-10rem.mr1", {
                    onclick: testReply,
                    title: "Test saying the reply.\nClick when speaking to stop.",
                }, VariablesView_1.Glyph.reply + " Reply"), m("textarea.ReplyMemo.TMemo", {
                    style: {
                        width: "35rem",
                        height: "5em",
                    },
                    oncreate: (vnode) => {
                        this.replyTextArea = (vnode.dom);
                    },
                    value: this.cachedReply,
                    oninput: replyInput,
                    onchange: replyChange,
                })), m(".Move.mt1", m("button.MoveSpeedButton.pt1.w-10rem.mr1", {
                    onclick: () => this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleMove),
                    title: "Browse all rules with this move",
                }, VariablesView_1.Glyph.move + " Move"), m(TQuickFillComboBox_1.TQuickFillComboBox, {
                    extraStyling: ".MoveEdit",
                    style: { width: "35rem", },
                    value: rule.move.phrase,
                    onchange: moveChange,
                    items: this.domain.world.getContextNames(),
                }))),
                m(".Changes.mt1", m("button.ChangesSpeedButton.pt1.w-10rem.mr1.v-top", {
                    onclick: () => this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleChanges),
                    title: "Browse all rules with the selected change",
                }, VariablesView_1.Glyph.changes + " Changes"), m(TSLogicListBox_1.TSLogicListBox, {
                    style: { width: "35rem", },
                    selections: changes,
                    items: world.getVariableNames(),
                    world: world,
                    onchange: changesChange,
                    onselect: (variable) => {
                        this.SpeedButtonClick(TSRule_1.TSRuleField.kRuleChanges, variable);
                    },
                })),
            ]);
        }
    }
    exports.IndividualRuleView = IndividualRuleView;
});
