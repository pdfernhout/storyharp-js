import { arrayRemove } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"

// TODO: Fix these imports
import { RuleEditorForm, ScrollIntoViewDirection } from "./fixTypes"

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
