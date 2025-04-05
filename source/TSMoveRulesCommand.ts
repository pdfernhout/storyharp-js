import { int, ScrollIntoViewDirection } from "./common"
import { KfCommand } from "./KfCommand"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"

export class TSMoveRulesCommand extends KfCommand {
    domain: TSDomain
    ruleWrappers: TSIndexChangeRuleWrapper[] = []
    action: string = ""

    constructor(domain: TSDomain) {
        super()
        this.domain = domain
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
        if (this.action === "raise") {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        } else {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        }
    }
    
    undoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.undoChange()
        }
        super.undoCommand()
        if (this.action === "raise") {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        } else {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        }
    }
    
    redoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = 0; i < this.ruleWrappers.length; i++) {
            const wrapper: TSIndexChangeRuleWrapper = this.ruleWrappers[i]
            wrapper.rule.selected = true
            wrapper.doChange()
        }
        super.doCommand()
        if (this.action === "raise") {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromTop)
        } else {
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        }
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
