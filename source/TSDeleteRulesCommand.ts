import { int, ScrollIntoViewDirection } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"

export class TSDeleteRulesCommand extends KfCommand {
    domain: TSDomain
    ruleWrappers: TSIndexChangeRuleWrapper[] = []

    constructor(domain: TSDomain) {
        super()
        this.domain = domain
    }
    
    addRule(rule: TSRule, newIndex: int): void {
        const wrapper: TSIndexChangeRuleWrapper = new TSIndexChangeRuleWrapper(rule, newIndex)
        this.ruleWrappers.push(wrapper)
    }
    
    doCommand(): void {
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if (wrapper.rule === this.domain.editedRule) {
                this.domain.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
    }
    
    undoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.addUseages()
            wrapper.undoChange()
            wrapper.rule.selected = true
        }
        if (this.ruleWrappers.length > 0) {
            this.domain.editRule(this.ruleWrappers[0].rule)
        }
        super.undoCommand()
        this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
    }
    
    redoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            if ((wrapper.rule === this.domain.editedRule)) {
                this.domain.editRule(null)
            }
            wrapper.rule.removeUseages()
            wrapper.doChange()
        }
        super.doCommand()
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
