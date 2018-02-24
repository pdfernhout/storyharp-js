import { int } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TSRule } from "./TSRule"

// TODO: Fix these imports
import { RuleEditorForm, ScrollIntoViewDirection } from "./fixTypes"

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
