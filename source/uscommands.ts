// unit uscommands

import { int, arrayRemove} from "./common"
import { KfCommand, TrackPhase, KfCommandChangeType, TCommandEvent } from "./KfCommand"
import { TSRule, TSRuleField } from "./TSRule"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TPoint } from "./TPoint"
import { TSDragRecord } from "./TSDragRecord"
import { TWorld } from "./TWorld";

// TODO: FIX THIS
const usconsoleform: any = {};
const usruleeditorform: any = {};
const uschangelog: any = {};

export class TSRuleFieldChange extends KfCommand {
    world: TWorld
    rule: TSRule
    field: int
    oldValue: string
    newValue: string

    constructor(world: TWorld, rule: TSRule, field: int, newValue: string) {
        super()
        this.world = world
        this.rule = rule
        this.field = field
        this.oldValue = rule.getTextForField(field)
        this.newValue = newValue
    }
    
    updateEditorForChange(): void {
        usruleeditorform.RuleEditorForm.rule = this.rule
        usruleeditorform.RuleEditorForm.loadAllRuleFields()
        if ((this.field === TSRuleField.kRuleContext) || (this.field === TSRuleField.kRuleMove)) {
            usconsoleform.ConsoleForm.locationCacheValid = false
        }
        if (this.field === TSRuleField.kRuleRequirements) {
            //wrapper entries will get freed if list box - so reset them
            usruleeditorform.RuleEditorForm.fillListBox(usruleeditorform.RuleEditorForm.RequirementsListBox, this.rule.requirements)
        } else if (this.field === TSRuleField.kRuleChanges) {
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
        this.world.lastVariableCreated = ""
        this.rule.setTextForField(this.field, this.newValue)
        if (this.field !== TSRuleField.kRuleReply) {
            // log changes
            uschangelog.ChangeLogForm.addToLog(this.world.lastVariableCreated)
        } else {
            uschangelog.ChangeLogForm.addToLog(this.newValue)
        }
        this.updateEditorForChange()
        super.doCommand()
    }
    
    undoCommand(): void {
        this.rule.setTextForField(this.field, this.oldValue)
        this.updateEditorForChange()
        usruleeditorform.RuleEditorForm.selectEditorField(this.field)
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.rule.setTextForField(this.field, this.newValue)
        this.updateEditorForChange()
        usruleeditorform.RuleEditorForm.selectEditorField(this.field)
        super.doCommand()
    }
    
    description(): string {
        let result = ""
        //  result := 'rule ' + IntToStr(domain.world.rules.indexOf(rule) + 1) + ' change of ' + TSRule.headerForField(field);
        result = "Change " + TSRule.headerForField(this.field) + " For Rule " + (this.world.rules.indexOf(this.rule) + 1)
        return result
    }
    
}

export class TSNewRulesCommand extends KfCommand {
    world: TWorld
    rules: TSRule[] = []
    creator: string = ""

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    addRule(rule: TSRule): void {
        this.rules.push(rule)
    }
    
    doCommand(): void {
        //already added at start
        super.doCommand()
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
    }
    
    undoCommand(): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            arrayRemove(this.world.rules, rule)
            rule.selected = false
            rule.removeUseages()
        }
        super.undoCommand()
        if (this.rules.indexOf(usruleeditorform.RuleEditorForm.rule) >= 0) {
            usruleeditorform.RuleEditorForm.editRule(null)
        }
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            rule.selected = true
            this.world.rules.push(rule)
            rule.addUseages()
        }
        super.doCommand()
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        //if rules.count > 0 then
        //  RuleEditorForm.editRule(rules[rules.count - 1]);
    }
    
    description(): string {
        let result = ""
        if (this.rules.length > 1) {
            result = "new rules"
        } else if (this.rules.length === 1) {
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

export class TSDeleteRulesCommand extends KfCommand {
    world: TWorld
    ruleWrappers: TSIndexChangeRuleWrapper[] = []

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        const wrapper: TSIndexChangeRuleWrapper = new TSIndexChangeRuleWrapper(rule, newIndex)
        this.ruleWrappers.push(wrapper)
    }
    
    doCommand(): void {
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if ((wrapper.rule === usruleeditorform.RuleEditorForm.rule)) {
                usruleeditorform.RuleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    undoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.addUseages()
            wrapper.undoChange()
            wrapper.rule.selected = true
        }
        if (this.ruleWrappers.length > 0) {
            usruleeditorform.RuleEditorForm.editRule(this.ruleWrappers[0].rule)
        }
        super.undoCommand()
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if ((wrapper.rule === usruleeditorform.RuleEditorForm.rule)) {
                usruleeditorform.RuleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
        usruleeditorform.RuleEditorForm.updateForRuleChange()
    }
    
    description(): string {
        let result = ""
        if (this.ruleWrappers.length > 1) {
            result = "delete rules"
        } else if (this.ruleWrappers.length === 1) {
            result = "delete rule"
        } else {
            result = "delete rule"
        }
        return result
    }
    
}

export class TSMoveRulesCommand extends KfCommand {
    world: TWorld
    ruleWrappers: TSIndexChangeRuleWrapper[] = []
    action: string = ""

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        const wrapper: TSIndexChangeRuleWrapper = new TSIndexChangeRuleWrapper(rule, newIndex)
        this.ruleWrappers.push(wrapper)
    }
    
    doCommand(): void {
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.doChange()
        }
        super.doCommand()
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
        } else {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        }
        usruleeditorform.RuleEditorForm.updateRuleNumberLabel()
    }
    
    undoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.undoChange()
        }
        super.undoCommand()
        usruleeditorform.RuleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromBottom)
        } else {
            usruleeditorform.RuleEditorForm.scrollGridSelectionsIntoView(usruleeditorform.kFromTop)
        }
        usruleeditorform.RuleEditorForm.updateRuleNumberLabel()
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.doChange()
        }
        super.doCommand()
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
        if (this.ruleWrappers.length > 1) {
            result = "rules"
        } else if (this.ruleWrappers.length === 1) {
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

export class TSMapDragCommand extends KfCommand {
    world: TWorld
    dragRecords: TSDragRecord[] = []
    notifyProcedure: TCommandEvent
    
    constructor(world: TWorld) {
        super()
        this.world = world
        this.world.addDragRecordsToList(this.dragRecords)
    }
    
    doCommand(): void {
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].doDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandDone)
        }
        super.doCommand()
    }
    
    undoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].undoDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandUndone)
        }
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].doDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandDone)
        }
        super.doCommand()
    }
    
    description(): string {
        let result = ""
        if (this.dragRecords.length > 1) {
            result = "Drag nodes"
        } else if (this.dragRecords.length === 1) {
            result = "Drag " + this.dragRecords[0].draggedNode.displayName()
        } else {
            result = "Drag"
        }
        return result
    }
    
    trackMouse(aTrackPhase: TrackPhase, anchorPoint: TPoint, previousPoint: TPoint, nextPoint: TPoint, mouseDidMove: boolean, rightButtonDown: boolean): KfCommand | null {
        let result: KfCommand | null

        result = this
        switch (aTrackPhase) {
            case TrackPhase.trackPress:
                if (this.dragRecords.length === 0) {
                    result = null
                }
                break
            case TrackPhase.trackMove:
                if (mouseDidMove) {
                    const delta = new TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    for (let i = 0; i < this.dragRecords.length; i++) {
                        this.dragRecords[i].offset(delta)
                    }
                    if (this.notifyProcedure) {
                        this.notifyProcedure(this, KfCommandChangeType.commandDone)
                    }
                }
                break
            case TrackPhase.trackRelease:
                if (!mouseDidMove) {
                    if ((this.dragRecords[0].draggedNode.position.X !== this.dragRecords[0].originalLocation.X) || (this.dragRecords[0].draggedNode.position.Y !== this.dragRecords[0].originalLocation.Y)) {
                        for (let i = 0; i < this.dragRecords.length; i++) {
                            this.dragRecords[i].undoDrag()
                        }
                        if (this.notifyProcedure) {
                            this.notifyProcedure(this, KfCommandChangeType.commandDone)
                        }
                    }
                    result = null
                } else {
                    const delta = new TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    if ((delta.X !== 0) || (delta.Y !== 0)) {
                        for (let i = 0; i < this.dragRecords.length; i++) {
                            this.dragRecords[i].offset(delta)
                        }
                        if (this.notifyProcedure) {
                            this.notifyProcedure(this, KfCommandChangeType.commandDone)
                        }
                    }
                }
                break
        }
        return result
    }
    
}
