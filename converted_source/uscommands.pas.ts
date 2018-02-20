// unit uscommands

from conversion_common import *
import uschangelog
import usruleeditorform
import usspeech
import usconsoleform
import usdomain
import usvariablecommands
import usfocuscommands
import usworld
import ucommand
import delphi_compatability

const uscommands = uscommands || {}


//TEditChangeCommand = class(KfCommand)
//  	entryField: TObject;
//    oldSelectionInformation: TSelectionInformation;
//    newSelectionInformation: TSelectionInformation;
//		closed: boolean;
//		procedure recordChangesMadeToEdit;
//		constructor createWithEdit(Sender: TObject;  const lastSelectionInformation: TSelectionInformation);
// 		procedure doCommand; override;
//		procedure undoCommand; override;
//		procedure redoCommand; override;
//		function description: string; override;
//		end;
//
//  	entryField: TObject;
//    newSelectionInformation: TSelectionInformation;
//		constructor createWithRule(Sender: TObject;  const lastSelectionInformation: TSelectionInformation);
//   
export class TSRuleFieldChange {
    rule: TSRule = new TSRule()
    field: int = 0
    oldValue: string = ""
    newValue: string = ""
    TSRuleFieldChange.prototype = new KfCommand()
    TSRuleFieldChange.prototype.constructor = TSRuleFieldChange
    
    // TSRuleFieldChange ------------------------------------------
    createWithRule(rule: TSRule, field: int, newValue: string): void {
        this.create()
        this.rule = rule
        this.field = field
        this.oldValue = rule.getTextForField(field)
        this.newValue = newValue
    }
    
    updateEditorForChange(): void {
        usruleeditorform.RuleEditorForm.rule = this.rule
        usruleeditorform.RuleEditorForm.loadAllRuleFields()
        if ((this.field === usworld.kRuleContext) || (this.field === usworld.kRuleMove)) {
            usconsoleform.ConsoleForm.locationCacheValid = false
        }
        if (this.field === usworld.kRuleRequirements) {
            //wrapper entries will get freed if list box - so reset them
            usruleeditorform.RuleEditorForm.fillListBox(usruleeditorform.RuleEditorForm.RequirementsListBox, this.rule.requirements)
        } else if (this.field === usworld.kRuleChanges) {
            usruleeditorform.RuleEditorForm.fillListBox(usruleeditorform.RuleEditorForm.ChangesListBox, this.rule.changes)
        }
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        usruleeditorform.RuleEditorForm.MapPaintBoxChanged()
        // could optimize to only do in certain cases
        usruleeditorform.RuleEditorForm.SecondListBox.Invalidate()
        if (usruleeditorform.RuleEditorForm.organizeByField === this.field) {
            // could optimize to only do when browser visible
            usruleeditorform.RuleEditorForm.setOrganizeByField(usruleeditorform.RuleEditorForm.organizeByField)
        }
    }
    
    doCommand(): void {
        usdomain.domain.world.lastVariableCreated = ""
        this.rule.setTextForField(this.field, this.newValue)
        if (this.field !== usworld.kRuleReply) {
            // log changes
            uschangelog.ChangeLogForm.addToLog(usdomain.domain.world.lastVariableCreated)
        } else {
            uschangelog.ChangeLogForm.addToLog(this.newValue)
        }
        this.updateEditorForChange()
        KfCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        this.rule.setTextForField(this.field, this.oldValue)
        this.updateEditorForChange()
        usruleeditorform.RuleEditorForm.selectEditorField(this.field)
        KfCommand.prototype.undoCommand.call(this)
    }
    
    redoCommand(): void {
        this.rule.setTextForField(this.field, this.newValue)
        this.updateEditorForChange()
        usruleeditorform.RuleEditorForm.selectEditorField(this.field)
        KfCommand.prototype.doCommand.call(this)
    }
    
    description(): string {
        let result = ""
        //  result := 'rule ' + IntToStr(domain.world.rules.indexOf(rule) + 1) + ' change of ' + TSRule.headerForField(field);
        result = "Change " + usworld.TSRule.headerForField(this.field) + " For Rule " + IntToStr(usdomain.domain.world.rules.IndexOf(this.rule) + 1)
        return result
    }
    
}

export class TSNewRulesCommand {
    rules: TList = new TList()
    creator: string = ""
    TSNewRulesCommand.prototype = new KfCommand()
    TSNewRulesCommand.prototype.constructor = TSNewRulesCommand
    
    // TSNewRulesCommand ------------------------------------------
    create(): void {
        this.rules = delphi_compatability.TList().Create()
    }
    
    destroy(): void {
        let i: int
        
        if (!this.done) {
            if (this.rules !== null) {
                for (i = 0; i <= this.rules.Count - 1; i++) {
                    UNRESOLVED.TObject(this.rules[i]).free
                }
            }
        }
        this.rules.free
        this.rules = null
    }
    
    addRule(rule: TSRule): void {
        this.rules.Add(rule)
    }
    
    doCommand(): void {
        //already added at start
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
    }
    
    undoCommand(): void {
        let i: int
        let rule: TSRule
        
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = usworld.TSRule(this.rules[i])
            usdomain.domain.world.rules.Remove(rule)
            rule.selected = false
            rule.removeUseages()
        }
        KfCommand.prototype.undoCommand.call(this)
        if (this.rules.IndexOf(usruleeditorform.RuleEditorForm.rule) >= 0) {
            usruleeditorform.RuleEditorForm.editRule(null)
        }
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    redoCommand(): void {
        let i: int
        let rule: TSRule
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = usworld.TSRule(this.rules[i])
            rule.selected = true
            usdomain.domain.world.rules.Add(rule)
            rule.addUseages()
        }
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        //if rules.count > 0 then
        //  RuleEditorForm.editRule(rules[rules.count - 1]);
    }
    
    description(): string {
        let result = ""
        if (this.rules.Count > 1) {
            result = "new rules"
        } else if (this.rules.Count === 1) {
            result = "new rule"
        } else {
            result = "new rule"
        }
        if (this.creator !== "") {
            result = result + " from " + this.creator
        }
        if (this.creator === "duplicating") {
            result = "duplicate rule"
        }
        return result
    }
    
}

export class TSDeleteRulesCommand {
    ruleWrappers: TList = new TList()
    TSDeleteRulesCommand.prototype = new KfCommand()
    TSDeleteRulesCommand.prototype.constructor = TSDeleteRulesCommand
    
    // TSDeleteRulesCommand ------------------------------------------
    create(): void {
        this.ruleWrappers = delphi_compatability.TList().Create()
    }
    
    destroy(): void {
        let i: int
        let rule: TSRule
        let wrapper: TSIndexChangeRuleWrapper
        
        if (this.ruleWrappers !== null) {
            for (i = 0; i <= this.ruleWrappers.Count - 1; i++) {
                wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
                rule = wrapper.rule
                if (this.done) {
                    rule.free
                }
                wrapper.free
            }
        }
        this.ruleWrappers.free
        this.ruleWrappers = null
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        let wrapper: TSIndexChangeRuleWrapper
        
        wrapper = usworld.TSIndexChangeRuleWrapper().createWithRuleNewIndex(rule, newIndex)
        this.ruleWrappers.Add(wrapper)
    }
    
    doCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        for (i = this.ruleWrappers.Count - 1; i >= 0; i--) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            if ((wrapper.rule === usruleeditorform.RuleEditorForm.rule)) {
                usruleeditorform.RuleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    undoCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = 0; i <= this.ruleWrappers.Count - 1; i++) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            wrapper.rule.addUseages()
            wrapper.undoChange()
            wrapper.rule.selected = true
        }
        if (this.ruleWrappers.Count > 0) {
            usruleeditorform.RuleEditorForm.editRule(usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[0]).rule)
        }
        KfCommand.prototype.undoCommand.call(this)
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
    }
    
    redoCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = this.ruleWrappers.Count - 1; i >= 0; i--) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            if ((wrapper.rule === usruleeditorform.RuleEditorForm.rule)) {
                usruleeditorform.RuleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    description(): string {
        let result = ""
        if (this.ruleWrappers.Count > 1) {
            result = "delete rules"
        } else if (this.ruleWrappers.Count === 1) {
            result = "delete rule"
        } else {
            result = "delete rule"
        }
        return result
    }
    
}

export class TSMoveRulesCommand {
    ruleWrappers: TList = new TList()
    action: string = ""
    TSMoveRulesCommand.prototype = new KfCommand()
    TSMoveRulesCommand.prototype.constructor = TSMoveRulesCommand
    
    // TSMoveRulesCommand ------------------------------------------
    create(): void {
        this.ruleWrappers = delphi_compatability.TList().Create()
    }
    
    destroy(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        if (this.ruleWrappers !== null) {
            for (i = 0; i <= this.ruleWrappers.Count - 1; i++) {
                wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
                wrapper.free
            }
        }
        this.ruleWrappers.free
        this.ruleWrappers = null
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        let wrapper: TSIndexChangeRuleWrapper
        
        wrapper = usworld.TSIndexChangeRuleWrapper().createWithRuleNewIndex(rule, newIndex)
        this.ruleWrappers.Add(wrapper)
    }
    
    doCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        for (i = 0; i <= this.ruleWrappers.Count - 1; i++) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            wrapper.doChange()
        }
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
        } else {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        }
        usruleeditorform.RuleEditorForm.updateRuleNumberLabel()
    }
    
    undoCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = this.ruleWrappers.Count - 1; i >= 0; i--) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            wrapper.rule.selected = true
            wrapper.undoChange()
        }
        KfCommand.prototype.undoCommand.call(this)
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        } else {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
        }
        usruleeditorform.RuleEditorForm.updateRuleNumberLabel()
    }
    
    redoCommand(): void {
        let i: int
        let wrapper: TSIndexChangeRuleWrapper
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = 0; i <= this.ruleWrappers.Count - 1; i++) {
            wrapper = usworld.TSIndexChangeRuleWrapper(this.ruleWrappers[i])
            wrapper.rule.selected = true
            wrapper.doChange()
        }
        KfCommand.prototype.doCommand.call(this)
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
        } else {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        }
        usruleeditorform.RuleEditorForm.updateRuleNumberLabel()
    }
    
    description(): string {
        let result = ""
        if (this.ruleWrappers.Count > 1) {
            result = "rules"
        } else if (this.ruleWrappers.Count === 1) {
            result = "rule"
        } else {
            result = "rule"
        }
        if (this.action !== "") {
            result = this.action + " " + result
        } else {
            result = "move " + result
        }
        return result
    }
    
}

export class TSMapDragCommand {
    dragRecords: TList = new TList()
    notifyProcedure: TCommandEvent = new TCommandEvent()
    TSMapDragCommand.prototype = new KfCommand()
    TSMapDragCommand.prototype.constructor = TSMapDragCommand
    
    // TSMapDragCommand ------------------------------------------
    create(): void {
        this.dragRecords = delphi_compatability.TList().Create()
        usdomain.domain.world.addDragRecordsToList(this.dragRecords)
    }
    
    destroy(): void {
        let i: int
        
        if (this.dragRecords !== null) {
            for (i = 0; i <= this.dragRecords.Count - 1; i++) {
                UNRESOLVED.TObject(this.dragRecords[i]).free
            }
        }
        this.dragRecords.free
        this.dragRecords = null
    }
    
    doCommand(): void {
        let i: int
        
        for (i = 0; i <= this.dragRecords.Count - 1; i++) {
            usworld.TSDragRecord(this.dragRecords[i]).doDrag()
        }
        if (delphi_compatability.Assigned(this.notifyProcedure)) {
            this.notifyProcedure(this, ucommand.KfCommandChangeType.commandDone)
        }
        KfCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        let i: int
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = 0; i <= this.dragRecords.Count - 1; i++) {
            usworld.TSDragRecord(this.dragRecords[i]).draggedNode.selected = true
            usworld.TSDragRecord(this.dragRecords[i]).undoDrag()
        }
        if (delphi_compatability.Assigned(this.notifyProcedure)) {
            this.notifyProcedure(this, ucommand.KfCommandChangeType.commandUndone)
        }
        KfCommand.prototype.undoCommand.call(this)
    }
    
    redoCommand(): void {
        let i: int
        
        usdomain.domain.world.deselectAllExcept(null)
        for (i = 0; i <= this.dragRecords.Count - 1; i++) {
            usworld.TSDragRecord(this.dragRecords[i]).draggedNode.selected = true
            usworld.TSDragRecord(this.dragRecords[i]).doDrag()
        }
        if (delphi_compatability.Assigned(this.notifyProcedure)) {
            this.notifyProcedure(this, ucommand.KfCommandChangeType.commandDone)
        }
        KfCommand.prototype.doCommand.call(this)
    }
    
    description(): string {
        let result = ""
        if (this.dragRecords.Count > 1) {
            result = "Drag nodes"
        } else if (this.dragRecords.Count === 1) {
            result = "Drag " + usworld.TSDragRecord(this.dragRecords[0]).draggedNode.displayName()
        } else {
            result = "Drag"
        }
        return result
    }
    
    TrackMouse(aTrackPhase: TrackPhase, anchorPoint: TPoint, previousPoint: TPoint, nextPoint: TPoint, mouseDidMove: boolean, rightButtonDown: boolean): KfCommand {
        let result = new KfCommand()
        let i: int
        let delta: TPoint
        
        result = this
        switch (aTrackPhase) {
            case ucommand.TrackPhase.trackPress:
                if (this.dragRecords.Count === 0) {
                    result = null
                    this.free
                    return result
                }
                break
            case ucommand.TrackPhase.trackMove:
                if (mouseDidMove) {
                    delta = Point(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    for (i = 0; i <= this.dragRecords.Count - 1; i++) {
                        usworld.TSDragRecord(this.dragRecords[i]).offset(delta)
                    }
                    if (delphi_compatability.Assigned(this.notifyProcedure)) {
                        this.notifyProcedure(this, ucommand.KfCommandChangeType.commandDone)
                    }
                }
                break
            case ucommand.TrackPhase.trackRelease:
                if (!mouseDidMove) {
                    if ((usworld.TSDragRecord(this.dragRecords[0]).draggedNode.position.X !== usworld.TSDragRecord(this.dragRecords[0]).originalLocation.X) || (usworld.TSDragRecord(this.dragRecords[0]).draggedNode.position.Y !== usworld.TSDragRecord(this.dragRecords[0]).originalLocation.Y)) {
                        for (i = 0; i <= this.dragRecords.Count - 1; i++) {
                            usworld.TSDragRecord(this.dragRecords[i]).undoDrag()
                        }
                        if (delphi_compatability.Assigned(this.notifyProcedure)) {
                            this.notifyProcedure(this, ucommand.KfCommandChangeType.commandDone)
                        }
                    }
                    result = null
                    this.free
                } else {
                    delta = Point(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    if ((delta.X !== 0) || (delta.Y !== 0)) {
                        for (i = 0; i <= this.dragRecords.Count - 1; i++) {
                            usworld.TSDragRecord(this.dragRecords[i]).offset(delta)
                        }
                        if (delphi_compatability.Assigned(this.notifyProcedure)) {
                            this.notifyProcedure(this, ucommand.KfCommandChangeType.commandDone)
                        }
                    }
                }
                break
        return result
    }
    
}

export class TSCommandList {
    TSCommandList.prototype = new KfCommandList()
    TSCommandList.prototype.constructor = TSCommandList
    
    // ----------------------------- TSCommandList -------------------------------
    toggleVariable(variable: TSVariable): TSToggleVariableCommand {
        let result = new TSToggleVariableCommand()
        result = usvariablecommands.TSToggleVariableCommand().createWithVariable(variable)
        this.doCommand(result)
        return result
    }
    
    moveFocus(newFocus: TSVariable): TSMoveFocusCommand {
        let result = new TSMoveFocusCommand()
        result = usvariablecommands.TSMoveFocusCommand().createWithNewFocus(newFocus)
        this.doCommand(result)
        return result
    }
    
    doCommandPhrase(commandPhrase: string): TSDoCommandPhrase {
        let result = new TSDoCommandPhrase()
        result = usvariablecommands.TSDoCommandPhrase().createWithCommandPhrase(commandPhrase)
        this.doCommand(result)
        return result
    }
    
    ruleFieldChange(rule: TSRule, field: int, newValue: string): TSRuleFieldChange {
        let result = new TSRuleFieldChange()
        let newContextOrMove: TSVariable
        
        if ((field === usworld.kRuleContext) || (field === usworld.kRuleMove)) {
            if (UNRESOLVED.Pos("new context ", rule.getTextForField(field)) === 1) {
                if (usdomain.domain.world.findVariable(newValue) === null) {
                    newContextOrMove = usdomain.domain.world.findOrCreateVariable(newValue, false)
                    newContextOrMove.position = rule.context.position
                }
            }
        }
        result = TSRuleFieldChange().createWithRule(rule, field, newValue)
        this.doCommand(result)
        return result
    }
    
}

