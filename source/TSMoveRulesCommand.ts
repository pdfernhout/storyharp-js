import { int } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSIndexChangeRuleWrapper } from "./TSIndexChangeRuleWrapper"
import { TSRule } from "./TSRule"

// TODO: Fix these imports
import { RuleEditorForm, ScrollIntoViewDirection } from "./fixTypes"

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
