var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
define(["require", "exports", "mithril", "./common", "./TWorld", "./FileUtils", "./TSJavaScriptWriter", "./TSNewRulesCommand", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, TWorld_1, FileUtils_1, TSJavaScriptWriter_1, TSNewRulesCommand_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    function saveWorldToLocalFile(domain) {
        const world = domain.world;
        const fileName = common_1.makeFileNameWithoutWldExtension(domain.worldFileName);
        FileUtils_1.FileUtils.saveToFile(fileName, world.saveWorldToFileContents(TWorld_1.ExportRulesOption.kSaveAllRules), ".wld", (fileName) => {
            domain.worldFileName = common_1.makeFileNameWithWldExtension(fileName);
            domain.resetWorldChangeCount();
            domain.worldCommandList.clear();
            m.redraw();
        });
    }
    function exportSelectedRulesToLocalFile(domain) {
        const world = domain.world;
        const fileName = common_1.makeFileNameWithoutWldExtension(domain.worldFileName);
        FileUtils_1.FileUtils.saveToFile(fileName, world.saveWorldToFileContents(TWorld_1.ExportRulesOption.kSaveOnlySelectedRules), ".wld", (fileName) => {
            m.redraw();
        });
    }
    function confirmUnsavedChangesLoss(domain) {
        if (domain.isWorldFileChanged()) {
            return ModalInputView_1.modalConfirm("You have unsaved changes to the current world which will be lost; proceed anyway?");
        }
        return Promise.resolve("OK");
    }
    exports.confirmUnsavedChangesLoss = confirmUnsavedChangesLoss;
    function loadWorldFromLocalFile(domain) {
        confirmUnsavedChangesLoss(domain).then(value => {
            if (!value)
                return;
            const world = domain.world;
            FileUtils_1.FileUtils.loadFromFile(false, (fileName, contents) => {
                world.resetVariablesAndRules();
                const loaded = world.loadWorldFromFileContents(contents);
                domain.addToLog("--- Read: " + fileName + (loaded ? " OK" : " Failed"));
                if (!loaded)
                    ToastView_1.toast("Something went wrong loading file: " + fileName);
                domain.updateForNewOrLoadedWorld(fileName, true);
                m.redraw();
            });
        });
    }
    exports.loadWorldFromLocalFile = loadWorldFromLocalFile;
    function newWorld(domain) {
        confirmUnsavedChangesLoss(domain).then(value => {
            if (!value)
                return;
            ModalInputView_1.modalPrompt("What would you like to call your new world?").then(fileName => {
                if (!fileName)
                    return;
                domain.world.resetVariablesAndRules();
                domain.updateForNewOrLoadedWorld(common_1.makeFileNameWithWldExtension(fileName), false);
            });
        });
    }
    function mergeWorldFromLocalFile(domain) {
        const world = domain.world;
        const oldRuleCount = world.rules.length;
        const oldVariablesCount = world.variables.length;
        FileUtils_1.FileUtils.loadFromFile(false, (fileName, contents) => {
            const loaded = world.loadWorldFromFileContents(contents);
            domain.addToLog("--- Load for merge: " + fileName + (loaded ? " OK" : " Failed"));
            if (!loaded) {
                ToastView_1.toast("Something went wrong merging file: " + fileName);
                world.rules.length = oldRuleCount;
                world.variables.length = oldVariablesCount;
                m.redraw();
                return;
            }
            const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(domain);
            newRulesCommand.creator = "merging " + common_1.makeFileNameWithoutWldExtension(fileName);
            world.deselectAllExcept(null);
            for (let i = oldRuleCount; i < world.rules.length; i++) {
                const rule = world.rules[i];
                newRulesCommand.addRule(rule);
                rule.selected = true;
            }
            for (let i = oldVariablesCount; i < world.variables.length; i++) {
                const variable = world.variables[i];
                variable.selected = true;
            }
            domain.worldCommandList.doCommand(newRulesCommand);
            if (world.rules.length > 0) {
                domain.editRule(world.rules[world.rules.length - 1]);
            }
            else {
                domain.editRule(null);
            }
            m.redraw();
        });
    }
    function generateHTML(domain) {
        return __awaiter(this, void 0, void 0, function* () {
            const writer = new TSJavaScriptWriter_1.TSJavaScriptWriter();
            const programText = writer.writeJavaScriptProgram(domain.world);
            if (!programText) {
                ToastView_1.toast("Some rules and contexts must be defined first");
                return;
            }
            const template = yield m.request("resources/template.html", { deserialize: (text) => text });
            const htmlFile = template
                .replace(/\/\/ START REPLACE[\s\S]*\/\/ END REPLACE/gm, programText)
                .replace("StoryHarp 3.0 Player", common_1.makeFileNameWithoutWldExtension(domain.worldFileName));
            const fileName = common_1.makeFileNameWithoutWldExtension(domain.worldFileName);
            FileUtils_1.FileUtils.saveToFile(fileName, htmlFile, ".html", (fileName) => {
                console.log("written", fileName);
                domain.addToLog("--- Wrote: " + fileName);
                m.redraw();
            });
        });
    }
    class FileForm {
        constructor(vnode) {
            this.domain = vnode.attrs.domain;
        }
        view() {
            const domain = this.domain;
            return m("div.FileForm", m("p"), m("button.ml3", { title: "Make a new world", onclick: () => newWorld(domain) }, "Make a new world"), m("p"), m("button.ml3", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load a world from local filesystem"), m("p"), m("button.ml3", { title: "Merge in another world file", onclick: () => mergeWorldFromLocalFile(domain) }, "Merge in another world from local filesystem"), m("p"), m("button.ml3", { title: "Save a world file", onclick: () => saveWorldToLocalFile(domain) }, "Save a world to local filesystem"), m("p"), m("button.ml3", { title: "Export selected rules to a world file", onclick: () => exportSelectedRulesToLocalFile(domain) }, "Export only selected rules to local filesystem"), m("p"), m("button.ml3", { title: "Generate a standalone HTML file for this world", onclick: () => generateHTML(domain) }, "Generate a standalone HTML file for this world"));
        }
    }
    exports.FileForm = FileForm;
});
