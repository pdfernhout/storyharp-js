define(["require", "exports", "mithril", "./common", "./VariablesView", "./TSDomain", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, VariablesView_1, TSDomain_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const firstRiddleAnswer = "say an answer for a riddle";
    function availableCommands(world, showRiddleAnswers = false) {
        const result = [];
        for (let rule of world.rules) {
            if (rule.available) {
                if (rule.command.phrase === "") {
                    continue;
                }
                let command = rule.command.phrase;
                if (!showRiddleAnswers && rule.command.phrase.startsWith("$")) {
                    command = firstRiddleAnswer;
                }
                if (result.indexOf(command) === -1) {
                    result.push(command);
                }
            }
        }
        return result;
    }
    function doCommand(domain, commandPhrase) {
        if (window.speechSynthesis && window.speechSynthesis.speaking)
            window.speechSynthesis.cancel();
        if (commandPhrase === firstRiddleAnswer) {
            ModalInputView_1.modalPrompt("Please enter the answer to a riddle. [case-sensitive]", "").then(answer => {
                if (!answer)
                    return;
                commandPhrase = "answer " + answer;
                doCommandContinued(domain, commandPhrase);
            });
        }
        else {
            doCommandContinued(domain, commandPhrase);
        }
    }
    exports.doCommand = doCommand;
    function doCommandContinued(domain, commandPhrase) {
        let commandPhraseModified;
        if ((availableCommands(domain.world, true).indexOf(commandPhrase) === -1)) {
            commandPhraseModified = "$" + commandPhrase;
        }
        else {
            commandPhraseModified = commandPhrase;
        }
        if ((availableCommands(domain.world, true).indexOf(commandPhraseModified) === -1)) {
            if (commandPhrase.startsWith("$")) {
                commandPhrase = commandPhrase.substring(1);
            }
            domain.consoleForm.addLineToTranscript("> " + commandPhrase, common_1.Color.clBlue);
            domain.consoleForm.addLineToTranscript("That accomplishes nothing.", common_1.Color.clBlack);
            return;
        }
        domain.sessionCommandList.doCommandPhrase(commandPhraseModified);
        if (domain.updateEditorAfterCommandDone) {
            domain.editRule(domain.ruleEditorForm.lastCommand, common_1.ScrollIntoViewDirection.kFromTop, true);
        }
    }
    function viewChoices(domain, scrollCallback) {
        const commands = availableCommands(domain.world);
        return m("div", m("hr"), [
            commands.sort().map(command => m("button.ma2.dark-blue.hover-blue", {
                key: command,
                onclick: () => {
                    doCommand(domain, command);
                    scrollCallback();
                },
            }, command)),
        ]);
    }
    function color(color) {
        switch (color) {
            case common_1.Color.clBlue: return ".blue";
            case common_1.Color.clGreen: return ".green";
            case common_1.Color.clBlack: return ".black";
            case common_1.Color.clRed: return ".red";
            default: return "";
        }
    }
    var SegmentType;
    (function (SegmentType) {
        SegmentType[SegmentType["speakText"] = 0] = "speakText";
        SegmentType[SegmentType["sayOptionsMacroInForce"] = 1] = "sayOptionsMacroInForce";
        SegmentType[SegmentType["showPicture"] = 2] = "showPicture";
        SegmentType[SegmentType["speakSound"] = 3] = "speakSound";
        SegmentType[SegmentType["speakMusic"] = 4] = "speakMusic";
    })(SegmentType = exports.SegmentType || (exports.SegmentType = {}));
    function parseTextWithMacros(aString) {
        const result = [];
        let remaining = aString;
        const wholeLength = aString.length;
        while (remaining.length > 0) {
            const startPosition = remaining.indexOf("{");
            if (startPosition !== -1) {
                const toSay = remaining.substring(0, startPosition);
                if (toSay.trim() !== "") {
                    result.push({ type: SegmentType.speakText, text: toSay });
                }
                remaining = remaining.substring(startPosition + 1);
            }
            else {
                if (remaining.trim() !== "") {
                    result.push({ type: SegmentType.speakText, text: remaining });
                }
                return result;
            }
            const endPosition = remaining.indexOf("}");
            if (endPosition === -1) {
                console.log("Error == unmatched braces");
                result.push({ type: SegmentType.speakText, text: remaining });
                return result;
            }
            const macro = remaining.substring(0, endPosition).trim();
            remaining = remaining.substring(endPosition + 1);
            if (macro.startsWith("options")) {
                result.push({ type: SegmentType.sayOptionsMacroInForce, text: "" });
            }
            else if (macro.startsWith("picture")) {
                result.push({ type: SegmentType.showPicture, text: macro.substring("picture".length).trim() });
            }
            else if (macro.startsWith("music")) {
                result.push({ type: SegmentType.speakMusic, text: macro.substring("music".length).trim() });
            }
            else if (macro.startsWith("sound")) {
                result.push({ type: SegmentType.speakSound, text: macro.substring("sound".length).trim() });
            }
            else {
                result.push({ type: SegmentType.speakSound, text: macro.trim() });
            }
        }
        return result;
    }
    exports.parseTextWithMacros = parseTextWithMacros;
    function viewTranscriptItem(item) {
        const domain = this;
        const segments = parseTextWithMacros(item.text);
        return m("div.mw6" + color(item.color), {
            key: item.uuid,
        }, segments.map(segment => {
            switch (segment.type) {
                case SegmentType.speakText:
                    if (item.color === common_1.Color.clBlue)
                        return m("div.ma2", segment.text);
                    return m("div.ma1.ml3", segment.text);
                case SegmentType.sayOptionsMacroInForce:
                    return [];
                case SegmentType.showPicture:
                    if (domain.speechSystem.optionPicture && TSDomain_1.isMediaOK(segment.text)) {
                        return m("div", m("img.ml3", {
                            src: TSDomain_1.fixupPath(domain, segment.text),
                        }));
                    }
                    else {
                        return [];
                    }
                case SegmentType.speakSound:
                    return [];
                case SegmentType.speakMusic:
                    return [];
                default:
                    throw new Error("unexpected segment type: " + JSON.stringify(segment));
            }
        }));
    }
    function resetConsole(domain) {
        ModalInputView_1.modalConfirm("Are you sure you want to restart playing the world?").then(value => {
            if (!value)
                return;
            domain.speechSystem.haltSpeechAndSoundAndMusic();
            domain.newSession();
            startSession(domain);
        });
    }
    function startSession(domain) {
        if (domain.world.rules.length > 0) {
            domain.consoleForm.doCommand(domain, domain.world.rules[0].command.phrase);
        }
        else {
            ToastView_1.toast("This world has no rules yet and so can't be started.");
        }
    }
    class ConsoleForm {
        constructor(vnode) {
            this.domain = vnode.attrs.domain;
        }
        scrollEndOfTranscriptIntoView() {
            if (this.transcriptDiv) {
                const transcriptDiv = this.transcriptDiv;
                setTimeout(() => {
                    transcriptDiv.scrollTop = transcriptDiv.scrollHeight;
                }, 0);
            }
        }
        view() {
            const domain = this.domain;
            return m("div.ConsoleForm.h-100.w-100.flex.flex-column", m("div.ml2.mb2.flex-none", m("button.ml1.mt1", {
                title: "Reset current world",
                onclick: () => resetConsole(domain)
            }, "Restart session"), m("div.dib.ml3", m("button.mt1.w3", {
                disabled: !domain.sessionCommandList.isUndoEnabled() || domain.sessionChangeCount <= 1,
                onclick: () => {
                    domain.sessionCommandList.undoLast();
                    this.scrollEndOfTranscriptIntoView();
                    domain.speechSystem.haltSpeechAndSoundAndMusic();
                },
                title: "Undo " + domain.sessionCommandList.undoDescription()
            }, "Undo"), m("button.ml1.mt1.w3", {
                disabled: !domain.sessionCommandList.isRedoEnabled(),
                onclick: () => {
                    domain.sessionCommandList.redoLast();
                    this.scrollEndOfTranscriptIntoView();
                },
                title: "Redo " + domain.sessionCommandList.redoDescription()
            }, "Redo")), m("div.dib.ml3", m("label.dib.mt1", m("input[type=checkbox]", {
                checked: domain.speechSystem.optionSound || undefined,
                onchange: (event) => {
                    domain.speechSystem.optionSound = event.target.checked;
                    if (!domain.speechSystem.optionSound) {
                        domain.speechSystem.haltSpeechAndSoundAndMusic();
                    }
                }
            }), "sound"), m("label.dib.ml2.mt1", m("input[type=checkbox]", {
                checked: domain.speechSystem.optionSpeech || undefined,
                onchange: (event) => {
                    domain.speechSystem.optionSpeech = event.target.checked;
                    if (!domain.speechSystem.optionSpeech) {
                        domain.speechSystem.haltSpeechAndSoundAndMusic();
                    }
                }
            }), "speech"), m("label.dib.ml2.mt1", m("input[type=checkbox]", {
                checked: domain.speechSystem.optionPicture || undefined,
                onchange: (event) => {
                    domain.speechSystem.optionPicture = event.target.checked;
                }
            }), "pictures"))), (!domain.sessionChangeCount && domain.transcript.length <= 1)
                ? m("div.flex-none", m("button.ml5.mt5.w5.blue", {
                    onclick: () => startSession(domain)
                }, m("span.f2", "Start ▶")))
                : [
                    m("div.flex-auto.overflow-auto.flex.flex-column-reverse", {
                        oncreate: (vnode) => {
                            this.transcriptDiv = (vnode.dom);
                        },
                    }, domain.transcript.map(viewTranscriptItem.bind(domain))),
                    (!domain.sessionChangeCount && domain.transcript.length <= 1) ? [] : m("div.flex-none", viewChoices(domain, this.scrollEndOfTranscriptIntoView.bind(this))),
                ], m(VariablesView_1.VariablesView, { domain }));
        }
    }
    exports.ConsoleForm = ConsoleForm;
});
