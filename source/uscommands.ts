// unit uscommands

import { int, arrayRemove} from "./common"
import { KfCommand, TrackPhase, KfCommandChangeType, TCommandEvent } from "./KfCommand"
import { TSRule, TSRuleField } from "./TSRule"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TPoint } from "./TPoint"
import { TSDragRecord } from "./TSDragRecord"
import { TWorld } from "./TWorld";

// TODO: Fix these as imports
import { RuleEditorForm, ChangeLogForm, ConsoleForm, ScrollIntoViewDirection } from "./fixTypes"

export class TSRuleFieldChangeCommand extends KfCommand {
    world: TWorld
    ruleEditorForm: RuleEditorForm
    changeLogForm: ChangeLogForm
    consoleForm: ConsoleForm
    rule: TSRule
    field: int
    oldValue: string
    newValue: string

    constructor(world: TWorld, ruleEditorForm: RuleEditorForm, changeLogForm: ChangeLogForm, consoleForm: ConsoleForm, rule: TSRule, field: int, newValue: string) {
        super()
        this.world = world
        this.ruleEditorForm = ruleEditorForm
        this.changeLogForm = changeLogForm
        this.consoleForm = consoleForm
        this.rule = rule
        this.field = field
        this.oldValue = rule.getTextForField(field)
        this.newValue = newValue
    }
    
    updateEditorForChange(): void {
        this.ruleEditorForm.rule = this.rule
        this.ruleEditorForm.loadAllRuleFields()
        // TODO: This locationCacheValid field probably can be removed along with consoleForm after finish refactoring
        if ((this.field === TSRuleField.kRuleContext) || (this.field === TSRuleField.kRuleMove)) {
            this.consoleForm.locationCacheValid = false
        }
        if (this.field === TSRuleField.kRuleRequirements) {
            //wrapper entries will get freed if list box - so reset them
            this.ruleEditorForm.fillListBox(this.ruleEditorForm.RequirementsListBox, this.rule.requirements)
        } else if (this.field === TSRuleField.kRuleChanges) {
            this.ruleEditorForm.fillListBox(this.ruleEditorForm.ChangesListBox, this.rule.changes)
        }
        this.ruleEditorForm.RuleGrid.Invalidate()
        this.ruleEditorForm.MapPaintBoxChanged()
        // could optimize to only do in certain cases
        this.ruleEditorForm.SecondListBox.Invalidate()
        if (this.ruleEditorForm.organizeByField === this.field) {
            // could optimize to only do when browser visible
            this.ruleEditorForm.setOrganizeByField(this.ruleEditorForm.organizeByField)
        }
    }
    
    doCommand(): void {
        this.world.lastVariableCreated = ""
        this.rule.setTextForField(this.field, this.newValue)
        if (this.field !== TSRuleField.kRuleReply) {
            // log changes
            this.changeLogForm.addToLog(this.world.lastVariableCreated)
        } else {
            this.changeLogForm.addToLog(this.newValue)
        }
        this.updateEditorForChange()
        super.doCommand()
    }
    
    undoCommand(): void {
        this.rule.setTextForField(this.field, this.oldValue)
        this.updateEditorForChange()
        this.ruleEditorForm.selectEditorField(this.field)
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.rule.setTextForField(this.field, this.newValue)
        this.updateEditorForChange()
        this.ruleEditorForm.selectEditorField(this.field)
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
    ruleEditorForm: RuleEditorForm
    rules: TSRule[] = []
    creator: string = ""

    constructor(world: TWorld, ruleEditorForm: RuleEditorForm) {
        super()
        this.world = world
        this.ruleEditorForm = ruleEditorForm 
    }
    
    addRule(rule: TSRule): void {
        this.rules.push(rule)
    }
    
    doCommand(): void {
        //already added at start
        super.doCommand()
        this.ruleEditorForm.updateForRuleChange()
        this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
    }
    
    undoCommand(): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            arrayRemove(this.world.rules, rule)
            rule.selected = false
            rule.removeUseages()
        }
        super.undoCommand()
        if (this.rules.indexOf(this.ruleEditorForm.rule) >= 0) {
            this.ruleEditorForm.editRule(null)
        }
        this.ruleEditorForm.updateForRuleChange()
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
        this.ruleEditorForm.updateForRuleChange()
        this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
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
    ruleEditorForm: RuleEditorForm
    ruleWrappers: TSIndexChangeRuleWrapper[] = []

    constructor(world: TWorld, ruleEditorForm: RuleEditorForm) {
        super()
        this.world = world
        this.ruleEditorForm = ruleEditorForm
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        const wrapper: TSIndexChangeRuleWrapper = new TSIndexChangeRuleWrapper(rule, newIndex)
        this.ruleWrappers.push(wrapper)
    }
    
    doCommand(): void {
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if ((wrapper.rule === this.ruleEditorForm.rule)) {
                this.ruleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
        this.ruleEditorForm.updateForRuleChange()
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
            this.ruleEditorForm.editRule(this.ruleWrappers[0].rule)
        }
        super.undoCommand()
        this.ruleEditorForm.updateForRuleChange()
        this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if ((wrapper.rule === this.ruleEditorForm.rule)) {
                this.ruleEditorForm.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
        this.ruleEditorForm.updateForRuleChange()
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
    ruleEditorForm: RuleEditorForm
    ruleWrappers: TSIndexChangeRuleWrapper[] = []
    action: string = ""

    constructor(world: TWorld, ruleEditorForm: RuleEditorForm) {
        super()
        this.world = world
        this.ruleEditorForm = ruleEditorForm
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
        this.ruleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        } else {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        }
        this.ruleEditorForm.updateRuleNumberLabel()
    }
    
    undoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.undoChange()
        }
        super.undoCommand()
        this.ruleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        } else {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        }
        this.ruleEditorForm.updateRuleNumberLabel()
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.doChange()
        }
        super.doCommand()
        this.ruleEditorForm.RuleGrid.Invalidate()
        if (this.action === "raise") {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        } else {
            this.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        }
        this.ruleEditorForm.updateRuleNumberLabel()
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
