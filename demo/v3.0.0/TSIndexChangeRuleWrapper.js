define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSIndexChangeRuleWrapper {
        constructor(rule, newIndex) {
            this.rule = rule;
            this.oldIndex = rule.world.rules.indexOf(rule);
            this.newIndex = newIndex;
        }
        doChange() {
            if (this.oldIndex === this.newIndex) {
                return;
            }
            if (this.newIndex >= 0) {
                this.rule.world.rules.splice(this.oldIndex, 1);
                this.rule.world.rules.splice(this.newIndex, 0, this.rule);
            }
            else {
                this.rule.world.rules.splice(this.oldIndex, 1);
            }
        }
        undoChange() {
            if (this.oldIndex === this.newIndex) {
                return;
            }
            if (this.newIndex >= 0) {
                this.rule.world.rules.splice(this.newIndex, 1);
                this.rule.world.rules.splice(this.oldIndex, 0, this.rule);
            }
            else {
                this.rule.world.rules.splice(this.oldIndex, 0, this.rule);
            }
        }
    }
    exports.TSIndexChangeRuleWrapper = TSIndexChangeRuleWrapper;
});
