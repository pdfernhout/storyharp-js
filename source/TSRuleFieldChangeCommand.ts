import { int, ScrollIntoViewDirection } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"
import { TSDomain } from "./TSDomain"

export class TSRuleFieldChangeCommand extends KfCommand {
    domain: TSDomain
    rule: TSRule
    field: TSRuleField
    oldValue: string
    newValue: string

    constructor(domain: TSDomain, rule: TSRule, field: TSRuleField, newValue: string) {
        super()
        this.domain = domain
        this.rule = rule
        this.field = field
        this.oldValue = rule.getTextForField(field)
        this.newValue = newValue
    }
    
    updateEditorForChange(): void {
        this.domain.editRule(this.rule, ScrollIntoViewDirection.kFromTop, true)
    }
    
    doCommand(): void {
        this.domain.world.lastVariableCreated = ""
        this.rule.setTextForField(this.field, this.newValue)

        // log changes
        this.domain.addToLog("--- edit rule #" + (this.domain.world.rules.indexOf(this.rule) + 1) + " " + TSRuleField[this.field].substring(5))
        this.domain.addToLog(this.rule.getTextForField(this.field))
        
        this.updateEditorForChange()
        super.doCommand()
    }
    
    undoCommand(): void {
        this.rule.setTextForField(this.field, this.oldValue)
        this.updateEditorForChange()
        this.domain.ruleEditorForm.selectEditorField(this.field)
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.rule.setTextForField(this.field, this.newValue)
        this.updateEditorForChange()
        this.domain.ruleEditorForm.selectEditorField(this.field)
        super.doCommand()
    }
    
    description(): string {
        let result = ""
        //  result := 'rule ' + IntToStr(domain.world.rules.indexOf(rule) + 1) + ' change of ' + TSRule.headerForField(field)
        result = "Change " + TSRule.headerForField(this.field) + " For Rule " + (this.domain.world.rules.indexOf(this.rule) + 1)
        return result
    }
    
}
