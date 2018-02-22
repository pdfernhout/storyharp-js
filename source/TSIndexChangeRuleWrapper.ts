import { int } from "./common.js"
import { TSRule } from "./TSRule.js"

export class TSIndexChangeRuleWrapper {
    rule: TSRule
    oldIndex: int
    newIndex: int
    
    constructor(rule: TSRule, newIndex: int) {
        this.rule = rule
        this.oldIndex = rule.world.rules.indexOf(rule)
        this.newIndex = newIndex
    }
    
    doChange(): void {
        if (this.oldIndex === this.newIndex) {
            return
        }
        if (this.newIndex >= 0) {
            this.rule.world.rules.splice(this.oldIndex, 1)
            this.rule.world.rules.splice(this.newIndex, 0, this.rule)
        } else {
            this.rule.world.rules.splice(this.oldIndex, 1)
        }
    }
    
    undoChange(): void {
        if (this.oldIndex === this.newIndex) {
            return
        }
        if (this.newIndex >= 0) {
            this.rule.world.rules.splice(this.newIndex, 1)
            this.rule.world.rules.splice(this.oldIndex, 0, this.rule)
        } else {
            this.rule.world.rules.splice(this.oldIndex, 0, this.rule)
        }
    }
    
}
