import { arrayRemove, ScrollIntoViewDirection } from "./common"
import { KfCommand } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"

export class TSNewRulesCommand extends KfCommand {
    domain: TSDomain
    rules: TSRule[] = []
    creator: string = ""

    constructor(domain: TSDomain) {
        super()
        this.domain = domain
    }
    
    addRule(rule: TSRule): void {
        this.rules.push(rule)
    }
    
    doCommand(): void {
        //already added at start
        super.doCommand()
        this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
    }
    
    undoCommand(): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            arrayRemove(this.domain.world.rules, rule)
            rule.selected = false
            rule.removeUseages()
        }
        super.undoCommand()
        // TODO: A little uncertain about this extra guard check added for TS for a null current editedRule
        if (this.domain.editedRule) {
            if (this.rules.indexOf(this.domain.editedRule) >= 0) {
                this.domain.editRule(null)
            }
        }
    }
    
    redoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            rule.selected = true
            this.domain.world.rules.push(rule)
            rule.addUseages()
        }
        super.doCommand()
        this.domain.ruleEditorForm.scrollGridSelectionsIntoView(ScrollIntoViewDirection.kFromBottom)
        //if rules.count > 0 then
        //  RuleEditorForm.editRule(rules[rules.count - 1])
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
