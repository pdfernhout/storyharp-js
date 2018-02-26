import { int } from "./common"
import { TSDraggableObject } from "./TSDraggableObject"
import { TSVariable, TSVariableState } from "./TSVariable"
import { TSChangedVariableWrapper } from "./TSChangedVariableWrapper"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper"
import { TWorld } from "./TWorld"

// TODO: This should become an enum
// need to be sequence from zero
export enum TSRuleField {
    kRuleContext = 0, 
    kRuleCommand = 1,
    kRuleReply = 2,
    kRuleMove = 3,
    kRuleRequirements = 4,
    kRuleChanges = 5,
    kLastRuleField = 5
}

export class TSRule extends TSDraggableObject {
    // TODO FIX: Needs to track: usdomain.domain.options.showCommandPrefixInMap
    static showCommandPrefixInMap: boolean = false;
    world: TWorld
    context: TSVariable
    requirements: TSDesiredStateVariableWrapper[] = []
    command: TSVariable
    reply: string = ""
    move: TSVariable
    changes: TSDesiredStateVariableWrapper[] = []
    available: boolean = false
    requirementsString: string = ""
    changesString: string = ""
    // TODO: useagesRemoved may be no longer needed
    useagesRemoved: boolean = false
 
    displayName(): string {
        let result = ""
        if (TSRule.showCommandPrefixInMap) {
            result = result + "> "
        }
        result = result + this.command.phrase
        return result
    }
    
    addUseages(): void {  
        this.context.contextUseages += 1
        for (let i = 0; i < this.requirements.length; i++) {
            this.requirements[i].variable.requirementsUseages += 1
        }
        this.command.commandUseages += 1
        this.move.moveUseages += 1
        for (let i = 0; i < this.changes.length; i++) {
            this.changes[i].variable.changesUseages += 1
        }
        this.useagesRemoved = false
    }
    
    removeUseages(): void {
        this.context.contextUseages -= 1
        for (let i = 0; i < this.requirements.length; i++) {
            this.requirements[i].variable.requirementsUseages -= 1
        }
        this.command.commandUseages -= 1
        this.move.moveUseages -= 1
        for (let i = 0; i < this.changes.length; i++) {
            this.changes[i].variable.changesUseages -= 1
        }
        this.useagesRemoved = true
    }
    
    setContext(aString: string): void {
        if (this.context) {
            this.context.contextUseages -= 1
        }
        // TODO: Seems like this and the other "setXYZ" methods will leak memory
        // if the world holds onto variables which no longer have references
        this.context = this.world.findOrCreateVariable(aString, false)
        this.context.contextUseages += 1
    }
    
    setRequirements(aString: string): void { 
        this.requirementsString = aString
        for (let i = 0; i < this.requirements.length; i++) {
            this.requirements[i].variable.requirementsUseages -= 1
        }
        this.requirements.length = 0
        this.compile(aString, this.requirements)
        for (let i = 0; i < this.requirements.length; i++) {
            this.requirements[i].variable.requirementsUseages += 1
        }
    }
    
    setCommand(aString: string): void {
        if (this.command) {
            this.command.commandUseages -= 1
        }
        this.command = this.world.findOrCreateVariable(aString, false)
        this.command.commandUseages += 1
    }
    
    setReply(aString: string): void {
        const safeString = aString.replace("\r", " ").replace("\n", " ")
        this.reply = safeString
    }
    
    setMove(aString: string): void {
        if (this.move) {
            this.move.moveUseages -= 1
        }
        this.move = this.world.findOrCreateVariable(aString, false)
        this.move.moveUseages += 1
    }
    
    setChanges(aString: string): void {
        this.changesString = aString
        for (let i = 0; i < this.changes.length; i++) {
            this.changes[i].variable.changesUseages -= 1
        }
        this.changes.length = 0
        this.compile(aString, this.changes)
        for (let i = 0; i < this.changes.length; i++) {
            this.changes[i].variable.changesUseages += 1
        }
    }
    
    compile(aString: string, list: TSDesiredStateVariableWrapper[]): void {
        let remaining = aString.trim()
        while (remaining.length > 0) {
            let phrase: string
            let rest: string
            const position = remaining.indexOf("&")
            if (position >= 0) {
                phrase = remaining.substring(0, position)
                rest = remaining.substring(position + 1)
            } else {
                phrase = remaining
                rest = ""
            }
            phrase = phrase.trim()

            let desiredState: TSVariableState
            if (phrase.indexOf("~") === 0) {
                desiredState = TSVariableState.kAbsent
            } else {
                desiredState = TSVariableState.kPresent
            }
            if (desiredState === TSVariableState.kAbsent) {
                phrase = phrase.substring(1)
            }

            const variable: TSVariable = this.world.findOrCreateVariable(phrase, false)
            const wrapper: TSDesiredStateVariableWrapper = new TSDesiredStateVariableWrapper(variable, desiredState)
            list.push(wrapper)

            remaining = rest.trim()
        }
    }
    
    decompile(list: TSDesiredStateVariableWrapper[]): string {
        let result = ""
        for (let i = 0; i < list.length; i++) {
            const wrapper: TSDesiredStateVariableWrapper = list[i]
            const item = wrapper.leader() + wrapper.variable.phrase
            if (result !== "") {
                result = result + " & " + item
            } else {
                result = item
            }
        }
        return result
    }
    
    decompileRequirements(): string {
        return this.decompile(this.requirements)
    }
    
    decompileChanges(): string {
        return this.decompile(this.changes)
    }
    
    updateAvailable(): void {
        // assuming all field set up correctly and not null or undefined
        this.available = false
        if (this.context === this.world.emptyEntry) {
            //for now - really should do anyway - assuming unfinished
            return
        }
        if (this.context.state !== TSVariableState.kPresent) {
            return
        }
        for (let i = 0; i < this.requirements.length; i++) {
            const wrapper: TSDesiredStateVariableWrapper = this.requirements[i]
            if (wrapper.variable.getState() !== wrapper.desiredState) {
                return
            }
        }
        this.available = true
    }
    
    recordReplyMoveChanges(changedVariablesList: TSChangedVariableWrapper[], totalReply: string): {totalReply: string, contextToFocusTo: TSVariable | null} {
        let contextToFocusTo = null

        if ((totalReply !== "") && (this.reply !== "")) {
            totalReply = totalReply + " "
        }
        totalReply = totalReply + this.reply
        if (this.move !== this.world.emptyEntry) {
            contextToFocusTo = this.move
        }
        for (let i = 0; i < this.changes.length; i++) {
            const desiredStateWrapper: TSDesiredStateVariableWrapper = this.changes[i]
            const changedVariableWrapper: TSChangedVariableWrapper = new TSChangedVariableWrapper(desiredStateWrapper.variable, desiredStateWrapper.desiredState)
            changedVariablesList.push(changedVariableWrapper)
        }
        return { totalReply, contextToFocusTo }
    }
    
    setTextForField(col: int, text: string): void {
        switch (col) {
            case TSRuleField.kRuleContext:
                this.setContext(text)
                break
            case TSRuleField.kRuleCommand:
                this.setCommand(text)
                break
            case TSRuleField.kRuleReply:
                this.setReply(text)
                break
            case TSRuleField.kRuleMove:
                this.setMove(text)
                break
            case TSRuleField.kRuleRequirements:
                this.setRequirements(text)
                break
            case TSRuleField.kRuleChanges:
                this.setChanges(text)
                break
            default:
                throw new Error("Unexpected case")
        }
    }
    
    getTextForField(col: int): string {
        let result = ""
        switch (col) {
            case TSRuleField.kRuleContext:
                result = this.context.phrase
                break
            case TSRuleField.kRuleCommand:
                result = this.command.phrase
                break
            case TSRuleField.kRuleReply:
                result = this.reply
                break
            case TSRuleField.kRuleMove:
                result = this.move.phrase
                break
            case TSRuleField.kRuleRequirements:
                result = this.requirementsString
                break
            case TSRuleField.kRuleChanges:
                result = this.changesString
                break
            default:
                throw new Error("Unexpected case")
        }
        return result
    }
    
    static headerForField(col: int): string {
        let result = ""
        switch (col) {
            case TSRuleField.kRuleContext:
                result = "Context"
                break
            case TSRuleField.kRuleCommand:
                result = "Command"
                break
            case TSRuleField.kRuleReply:
                result = "Reply"
                break
            case TSRuleField.kRuleMove:
                result = "Move"
                break
            case TSRuleField.kRuleRequirements:
                result = "Requirements"
                break
            case TSRuleField.kRuleChanges:
                result = "Changes"
                break
            default:
                throw new Error("Unexpected case")
        }
        return result
    }
    
    usesVariableInList(variable: TSVariable, list: TSDesiredStateVariableWrapper[]): boolean {
        for (let i = 0; i < list.length; i++) {
            const wrapper: TSDesiredStateVariableWrapper = list[i]
            if (wrapper.variable === variable) {
                return true
            }
        }
        return false
    }
    
    usesVariableFor(variable: TSVariable, col: int): boolean {
        let result = false
        result = false
        switch (col) {
            case TSRuleField.kRuleContext:
                result = this.context === variable
                break
            case TSRuleField.kRuleCommand:
                result = this.command === variable
                break
            case TSRuleField.kRuleReply:
                // error
                console.log("Is this an error?")
                result = false
                break
            case TSRuleField.kRuleMove:
                result = this.move === variable
                break
            case TSRuleField.kRuleRequirements:
                result = this.usesVariableInList(variable, this.requirements)
                break
            case TSRuleField.kRuleChanges:
                result = this.usesVariableInList(variable, this.changes)
                break
            default:
                throw new Error("Unexpected case")
        }
        return result
    }
    
    variableInList(n: int, list: TSDesiredStateVariableWrapper[]): TSVariable {
        let result: TSVariable
        if (n < 0) {
            n = 0
        }
        if (n < list.length) {
            const wrapper: TSDesiredStateVariableWrapper = list[n]
            result = wrapper.variable
        } else {
            result = this.world.emptyEntry
        }
        return result
    }
    
    variableForFieldWithSelections(col: int, requirementsIndex: int, changesIndex: int): TSVariable {
        let result: TSVariable = this.world.emptyEntry
        switch (col) {
            case TSRuleField.kRuleContext:
                result = this.context
                break
            case TSRuleField.kRuleCommand:
                result = this.command
                break
            case TSRuleField.kRuleReply:
                // error
                console.log("Is this an error too?")
                result = this.world.emptyEntry
                break
            case TSRuleField.kRuleMove:
                result = this.move
                break
            case TSRuleField.kRuleRequirements:
                result = this.variableInList(requirementsIndex, this.requirements)
                break
            case TSRuleField.kRuleChanges:
                result = this.variableInList(changesIndex, this.changes)
                break
            default:
                throw new Error("Unexpected case")
        }
        return result
    }
    
    variableForField(col: int): TSVariable {
        const result: TSVariable = this.variableForFieldWithSelections(col, 0, 0)
        return result
    }
    
    setPosition(value: string): void {
        const [firstPart, secondPart, thirdPart] = value.split("|")
        super.setPosition(firstPart)
        this.context.setPosition(secondPart)
        this.move.setPosition(thirdPart)
    }
}
