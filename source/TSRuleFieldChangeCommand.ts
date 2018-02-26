import { int } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"

// TODO: Fix these imports
import { RuleEditorForm, ChangeLogForm, ConsoleForm } from "./fixTypes"

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
        /* TODO: Maybe most or all of this is no longer needed with Mithril? Perhaps the map invalidation is still needed?
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
        */
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
