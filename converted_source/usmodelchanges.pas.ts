// unit usmodelchanges

from conversion_common import *
import usruleeditorform
import usworld
import delphi_compatability

const usmodelchanges = usmodelchanges || {}


export class TModelChange {
    
    //////////////////////////////////// TModelChange /////////////////////////////////////
    doChange(): void {
        // subclasses should override
    }
    
    undoChange(): void {
        // subclasses should override
    }
    
    redoChange(): void {
        this.doChange()
        // sublasses may override and should call inherited doChange
    }
    
}

export class TRuleFieldChange {
    rule: TSRule = new TSRule()
    fieldType: int = 0
    oldText: string = ""
    newText: string = ""
    TRuleFieldChange.prototype = new TModelChange()
    TRuleFieldChange.prototype.constructor = TRuleFieldChange
    
    //////////////////////////////// TRuleFieldChange ////////////////////////////////////
    createWithRuleFieldTypeOldTextNewText(rule: TSRule, fieldType: int, newText: string): void {
        this.create
        this.rule = rule
        this.fieldType = fieldType
        if (rule !== null) {
            this.oldText = rule.getTextForField(fieldType)
        }
        this.newText = newText
    }
    
    doChange(): void {
        if (this.rule !== null) {
            this.rule.setTextForField(this.fieldType, this.newText)
        }
    }
    
    undoChange(): void {
        if (this.rule !== null) {
            this.rule.setTextForField(this.fieldType, this.oldText)
        }
        //	RuleEditorForm.updateForFieldChange(fieldType);
    }
    
    redoChange(): void {
        if (this.rule !== null) {
            this.rule.setTextForField(this.fieldType, this.newText)
        }
        //	RuleEditorForm.updateForFieldChange(fieldType);
    }
    
}

