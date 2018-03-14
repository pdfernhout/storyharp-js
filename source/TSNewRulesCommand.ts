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
        // already added rule at start
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
        if (this.rules.length > 0) {
            this.domain.editRule(this.rules[this.rules.length - 1], ScrollIntoViewDirection.kFromBottom)
        }
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
