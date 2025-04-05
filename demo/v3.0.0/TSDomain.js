var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
define(["require", "exports", "mithril", "./TWorld", "./TSCommandList", "./TSRule", "./common", "./KfCommand", "./LinkWizardView", "./ContextWizardView", "./CommandWizardView", "./TSMapView", "./ConsoleForm", "./TPoint", "./LoggingView", "./ToastView"], function (require, exports, m, TWorld_1, TSCommandList_1, TSRule_1, common_1, KfCommand_1, LinkWizardView_1, ContextWizardView_1, CommandWizardView_1, TSMapView_1, ConsoleForm_1, TPoint_1, LoggingView_1, ToastView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const kUnsavedWorldFileName = "untitled";
    const kUnsavedSessionFileName = "untitled";
    const kWorldExtension = "wld";
    const kSessionExtension = "ses";
    let musicPlayed = null;
    let soundPlayed = null;
    function isMediaOK(text) {
        const result = text.startsWith("http")
            || text.startsWith("/")
            || text.startsWith("media/");
        if (!result)
            console.log("Not loading media for:", text);
        return result;
    }
    exports.isMediaOK = isMediaOK;
    function fixupPath(domain, text) {
        if (text.startsWith("media/")) {
            return domain.dataPath + text;
        }
        return text;
    }
    exports.fixupPath = fixupPath;
    function speakText(text) {
        const synth = window.speechSynthesis;
        if (synth) {
            const sentences = (text + ".").match(/[^\.!\?]+[\.!\?]+/g);
            if (sentences && sentences.length) {
                const lastItem = sentences[sentences.length - 1];
                sentences[sentences.length - 1] = lastItem.substring(0, lastItem.length - 1);
                for (let sentence of sentences) {
                    if (!sentence.trim())
                        continue;
                    const utterance = new SpeechSynthesisUtterance(sentence);
                    synth.speak(utterance);
                }
            }
        }
        return Boolean(synth);
    }
    exports.speakText = speakText;
    let nextTranscriptLineUUID = 1;
    class TSApplication {
        constructor() {
            this.sessionChangeCount = 0;
            this.worldChangeCount = 0;
            this.isWorldFileLoaded = false;
            this.editedRule = null;
            this.lastSingleRuleIndex = 0;
            this.activeForm = "console";
            this.currentEditorView = "table";
            this.currentEditorWizard = "context";
            this.transcript = [];
            this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension;
            this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension;
            this.showCommandPrefixInMap = false;
            this.showCommandsInMap = true;
            this.updateEditorAfterCommandDone = true;
            this.showWizardHelp = true;
            this.browseBy = TSRule_1.TSRuleField.kRuleContext;
            this.pendingTableScroll = null;
            this.pendingMapScroll = false;
            this.pendingBrowserScroll = false;
            this.dataPath = "./data/";
            this.addToLog = LoggingView_1.addToLog;
            this.world = new TWorld_1.TWorld(this.goodPosition.bind(this));
            this.sessionCommandList = new TSCommandList_1.TSCommandList(this);
            this.sessionCommandList.setNewUndoLimit(1000);
            this.sessionCommandList.notifyProcedure = this.sessionCommandChangedNotification.bind(this);
            this.worldCommandList = new TSCommandList_1.TSCommandList(this);
            this.worldCommandList.setNewUndoLimit(1000);
            this.worldCommandList.notifyProcedure = this.worldCommandChangedNotification.bind(this);
            this.linkWizardData = LinkWizardView_1.newLinkWizardData();
            this.contextWizardData = ContextWizardView_1.newContextWizardData();
            this.commandWizardData = CommandWizardView_1.newCommandWizardData();
            this.mapViewState = TSMapView_1.newMapViewState();
            this.consoleForm = {
                addLineToTranscript: (text, color) => {
                    this.transcript.unshift({ uuid: nextTranscriptLineUUID++, text, color });
                },
                scrollTranscriptEndIntoView: () => null,
                doCommand: ConsoleForm_1.doCommand
            };
            this.ruleEditorForm = {
                selectEditorField: (fieldIndex) => null,
                scrollGridSelectionsIntoView: (direction) => {
                    this.pendingTableScroll = {
                        rule: null,
                        direction,
                    };
                },
                lastChoice: null,
                previousChoice: null,
                lastCommand: null
            };
            this.speechSystem = {
                optionSpeech: true,
                optionSound: true,
                optionPicture: true,
                lastSaidTextWithMacros: "TEST",
                stripMacros: (text) => text,
                sayTextWithMacros: (text) => {
                    const segments = ConsoleForm_1.parseTextWithMacros(text);
                    for (let segment of segments) {
                        if (segment.type === ConsoleForm_1.SegmentType.speakSound) {
                            if (soundPlayed) {
                                soundPlayed.pause();
                                soundPlayed = null;
                            }
                            if (this.speechSystem.optionSound && segment.text && isMediaOK(segment.text)) {
                                soundPlayed = new Audio(fixupPath(this, segment.text));
                                soundPlayed.play();
                            }
                        }
                        else if (segment.type === ConsoleForm_1.SegmentType.speakMusic) {
                            if (musicPlayed) {
                                musicPlayed.pause();
                                musicPlayed = null;
                            }
                            if (this.speechSystem.optionSound && segment.text && isMediaOK(segment.text)) {
                                musicPlayed = new Audio(fixupPath(this, segment.text));
                                musicPlayed.play();
                            }
                        }
                        else if (segment.type === ConsoleForm_1.SegmentType.speakText) {
                            if (this.speechSystem.optionSpeech) {
                                speakText(segment.text);
                            }
                        }
                        else {
                        }
                    }
                },
                listenForAvailableCommands: () => null,
                checkForSayOptionsMacro: () => null,
                speakText: (text) => null,
                haltSpeechAndSoundAndMusic: () => {
                    if (window.speechSynthesis && window.speechSynthesis.speaking)
                        window.speechSynthesis.cancel();
                    if (musicPlayed)
                        musicPlayed.pause();
                    if (soundPlayed)
                        soundPlayed.pause();
                },
            };
        }
        editRule(rule, scrollDirection = common_1.ScrollIntoViewDirection.kFromTop, force = false) {
            this.editedRule = rule;
            if (rule) {
                if (force || this.currentEditorView !== "table") {
                    this.pendingTableScroll = {
                        rule: rule,
                        direction: scrollDirection,
                    };
                }
                if (force || this.currentEditorView !== "browser") {
                    this.pendingBrowserScroll = true;
                }
                this.pendingMapScroll = true;
            }
        }
        setOrganizeByField(newValue, selectedVariable) {
            this.browseBy = newValue;
            this.pendingBrowserScrollSelectedVariable = selectedVariable;
            this.pendingBrowserScroll = true;
        }
        loadWorldFromServerData(fileName) {
            return __awaiter(this, void 0, void 0, function* () {
                if (!this.demoConfig) {
                    this.demoConfig = (yield m.request(this.dataPath + "demoConfig.json")
                        .catch(error => {
                        console.log("error loading demoConfig.json", error);
                        ToastView_1.toast("Something went wrong loading demoConfig.json from the server");
                        m.redraw();
                        const result = { demoWorldFiles: [] };
                        return result;
                    }));
                }
                const worldContent = yield m.request(this.dataPath + fileName + ".wld", { deserialize: (text) => text })
                    .catch(error => {
                    console.log("error loading a world file", fileName, error);
                    ToastView_1.toast("Something went wrong loading the world file \"" + fileName + "\" from the server");
                    m.redraw();
                    return "";
                });
                if (!worldContent)
                    return false;
                this.world.resetVariablesAndRules();
                const loaded = this.world.loadWorldFromFileContents(worldContent);
                if (!loaded)
                    throw new Error("Failed to load");
                this.updateForNewOrLoadedWorld(fileName, true);
                m.redraw();
                return true;
            });
        }
        goodPosition() {
            let result = new TPoint_1.TPoint();
            if (this.ruleEditorForm.lastChoice !== null) {
                if (this.ruleEditorForm.previousChoice !== null) {
                    result = new TPoint_1.TPoint((this.ruleEditorForm.previousChoice.position.X + this.ruleEditorForm.lastChoice.position.X) / 2, (this.ruleEditorForm.previousChoice.position.Y + this.ruleEditorForm.lastChoice.position.Y) / 2 + 30);
                }
                else {
                    result = new TPoint_1.TPoint(this.ruleEditorForm.lastChoice.position.X, this.ruleEditorForm.lastChoice.position.Y + 30);
                }
            }
            else {
                result = new TPoint_1.TPoint(Math.round(this.mapViewState.viewportSize.X / 2 - this.mapViewState.scroll.X), Math.round(this.mapViewState.viewportSize.Y / 2 - this.mapViewState.scroll.Y));
            }
            result.X = result.X + Math.round(Math.random() * 200) - 100;
            result.Y = result.Y + Math.round(Math.random() * 200) - 100;
            return result;
        }
        updateForNewOrLoadedWorld(fileName, isWorldFileLoaded) {
            this.addToLog("--- world change: " + fileName);
            this.speechSystem.haltSpeechAndSoundAndMusic();
            this.worldCommandList.clear();
            this.resetWorldChangeCount();
            if (fileName) {
                this.worldFileName = fileName;
            }
            else {
                this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension;
            }
            this.isWorldFileLoaded = isWorldFileLoaded;
            this.lastSingleRuleIndex = 0;
            this.ruleEditorForm.lastChoice = null;
            this.ruleEditorForm.previousChoice = null;
            const ruleToEdit = this.world.rules.length ? this.world.rules[0] : null;
            this.editRule(ruleToEdit);
            if (ruleToEdit) {
                ruleToEdit.selected = true;
            }
            this.newSession();
            this.mapViewState.scroll.X = 0;
            this.mapViewState.scroll.Y = 0;
        }
        newSession() {
            this.world.newSession();
            this.sessionCommandList.clear();
            this.transcript.length = 0;
            this.consoleForm.addLineToTranscript("Starting: " + common_1.makeFileNameWithoutWldExtension(this.worldFileName), common_1.Color.clGreen);
            this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension;
            this.sessionChangeCount = 0;
        }
        isSessionFileChanged() {
            return this.sessionChangeCount !== 0;
        }
        sessionCommandChangedNotification(command, state) {
            switch (state) {
                case KfCommand_1.KfCommandChangeType.commandDone:
                    this.sessionChangeCount += 1;
                    break;
                case KfCommand_1.KfCommandChangeType.commandUndone:
                    this.sessionChangeCount -= 1;
                    break;
                default:
                    throw new Error("sessionCommandChangedNotification: unexpected case");
            }
        }
        worldCommandChangedNotification(command, state) {
            switch (state) {
                case KfCommand_1.KfCommandChangeType.commandDone:
                    this.worldChangeDone();
                    break;
                case KfCommand_1.KfCommandChangeType.commandUndone:
                    this.worldChangeUndone();
                    break;
                default:
                    throw new Error("worldCommandChangedNotification: unexpected case");
            }
        }
        isWorldFileChanged() {
            return this.worldChangeCount !== 0;
        }
        resetWorldChangeCount() {
            this.worldChangeCount = 0;
        }
        worldChangeDone() {
            this.worldChangeCount += 1;
        }
        worldChangeUndone() {
            this.worldChangeCount -= 1;
        }
    }
    exports.TSApplication = TSApplication;
});
