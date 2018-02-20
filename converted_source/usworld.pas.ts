// unit usworld

from conversion_common import *
import usdomain
import uscommands
import ufilesupport
import usruleeditorform
import usconsoleform
import usmain
import delphi_compatability

const usworld = usworld || {}

// const
const kSaveAllRules = false
const kSaveOnlySelectedRules = true


export enum TSVariableState { kPresent, kAbsent }

TSVariableDisplayOptions = boolean[] /* 5 + 1 */
// const
const kRuleContext = 0
const kRuleCommand = 1
const kRuleReply = 2
const kRuleMove = 3
const kRuleRequirements = 4
const kRuleChanges = 5
const kLastRuleField = 5


function findCompleteWorldFileName(worldFileNameRead: string): string {
    let result = ""
    if (!FileExists(worldFileNameRead)) {
        result = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeWorld, worldFileNameRead, "Search for world file: " + worldFileNameRead, ufilesupport.kOtherExtNotOK)
    } else {
        result = worldFileNameRead
    }
    return result
}

function swapIntegers(a: int, b: int): void {
    let temp: int
    
    temp = a
    a = b
    b = temp
    return a, b
}


export class TSDraggableObject {
    position: TPoint = new TPoint()
    extent: TPoint = new TPoint()
    selected: boolean = false
    
    // need to be sequence from zero
    //////////////////// TObjectWithPosition ///////////////////////
    displayName(): string {
        let result = ""
        result = "Error - override needed"
        return result
    }
    
    setPosition(value: string): void {
        let firstNumber: string
        let secondNumber: string
        
        firstNumber = UNRESOLVED.Copy(value, 1, UNRESOLVED.pos(",", value) - 1)
        secondNumber = UNRESOLVED.Copy(value, len(firstNumber) + 2, len(value))
        //position.x := 0;
        //position.y := 0;
        try {
            this.position.X = StrToInt(firstNumber)
            this.position.Y = StrToInt(secondNumber)
        } catch (Exception e) {
            // pass
        }
    }
    
    bounds(): TRect {
        let result = new TRect()
        let topLeft: TPoint
        
        topLeft = Point(this.position.X - this.extent.X / 2, this.position.Y - this.extent.Y / 2)
        result = Rect(topLeft.X, topLeft.Y, topLeft.X + this.extent.X, topLeft.Y + this.extent.Y)
        return result
    }
    
    center(): TPoint {
        let result = new TPoint()
        result = this.position
        return result
    }
    
}

export class TSDragRecord {
    draggedNode: TSDraggableObject = new TSDraggableObject()
    originalLocation: TPoint = new TPoint()
    newLocation: TPoint = new TPoint()
    
    //////////////////// TSDragRecord ///////////////////////
    createWithNode(node: TSDraggableObject): void {
        this.create
        this.draggedNode = node
        this.originalLocation = this.draggedNode.position
        this.newLocation = this.originalLocation
    }
    
    doDrag(): void {
        this.draggedNode.position = this.newLocation
    }
    
    undoDrag(): void {
        this.draggedNode.position = this.originalLocation
    }
    
    offset(delta: TPoint): void {
        this.newLocation = Point(this.newLocation.X + delta.X, this.newLocation.Y + delta.Y)
        this.draggedNode.position = this.newLocation
    }
    
}

export class TSVariable {
    world: TWorld = new TWorld()
    phrase: string = ""
    state: TSVariableState = new TSVariableState()
    contextUseages: int = 0
    requirementsUseages: int = 0
    commandUseages: int = 0
    moveUseages: int = 0
    changesUseages: int = 0
    indexInVariables: int = 0
    TSVariable.prototype = new TSDraggableObject()
    TSVariable.prototype.constructor = TSVariable
    
    // for java creation
    //////////////////// TSVariable ///////////////////////
    displayName(): string {
        let result = ""
        result = this.phrase
        return result
    }
    
    setPhrase(aPhrase: string): void {
        this.phrase = aPhrase
    }
    
    setState(newState: TSVariableState): void {
        this.state = newState
    }
    
    getState(): TSVariableState {
        let result = new TSVariableState()
        result = this.state
        return result
    }
    
    hasUseagesForField(col: int): boolean {
        let result = false
        result = false
        switch (col) {
            case kRuleContext:
                result = (this.contextUseages > 0) || (this.moveUseages > 0)
                break
            case kRuleCommand:
                result = this.commandUseages > 0
                break
            case kRuleReply:
                result = false
                break
            case kRuleMove:
                result = this.moveUseages > 0
                break
            case kRuleRequirements:
                result = this.requirementsUseages > 0
                break
            case kRuleChanges:
                result = this.changesUseages > 0
                break
        return result
    }
    
    meetsDisplayOptions(displayOptions: TSVariableDisplayOptions): boolean {
        let result = false
        let i: int
        
        result = false
        for (i = 0; i <= 5; i++) {
            if (i === kRuleCommand) {
                //don't display commands for now - used to display rules
                continue
            }
            if (this.hasUseagesForField(i) && displayOptions[i]) {
                result = true
                return result
            }
        }
        return result
    }
    
}

export class TSDesiredStateVariableWrapper {
    variable: TSVariable = new TSVariable()
    desiredState: TSVariableState = new TSVariableState()
    
    /////////////////////////// TSDesiredStateVariableWrapperleader /////////////////////////////////////
    leader(): string {
        let result = ""
        if (this.desiredState === TSVariableState.kAbsent) {
            result = "~"
        } else {
            result = ""
        }
        return result
    }
    
    displayLeader(): string {
        let result = ""
        if (this.desiredState === TSVariableState.kAbsent) {
            result = "~"
        } else {
            result = "  "
        }
        return result
    }
    
    invertDesiredState(): void {
        if (this.desiredState === TSVariableState.kAbsent) {
            this.desiredState = TSVariableState.kPresent
        } else {
            this.desiredState = TSVariableState.kAbsent
        }
    }
    
    displayString(): string {
        let result = ""
        result = this.displayLeader() + this.variable.phrase
        return result
    }
    
}

export class TSChangedVariableWrapper {
    variable: TSVariable = new TSVariable()
    oldState: TSVariableState = new TSVariableState()
    newState: TSVariableState = new TSVariableState()
    
    /////////////////////////// TSChangedVariableWrapper /////////////////////////////////////
    createWithVariableNewState(variable: TSVariable, newState: TSVariableState): void {
        this.create
        this.variable = variable
        this.newState = newState
        this.oldState = variable.getState()
    }
    
    doChange(): void {
        this.variable.setState(this.newState)
    }
    
    undoChange(): void {
        this.variable.setState(this.oldState)
    }
    
}

export class TSRule {
    world: TWorld = new TWorld()
    context: TSVariable = new TSVariable()
    requirements: TList = new TList()
    command: TSVariable = new TSVariable()
    reply: string = ""
    move: TSVariable = new TSVariable()
    changes: TList = new TList()
    available: boolean = false
    requirementsString: string = ""
    changesString: string = ""
    useagesRemoved: boolean = false
    TSRule.prototype = new TSDraggableObject()
    TSRule.prototype.constructor = TSRule
    
    //////////////////// TSRule ////////////////////////
    displayName(): string {
        let result = ""
        result = ""
        if (usdomain.domain.options.showCommandPrefixInMap) {
            result = result + "> "
        }
        result = result + this.command.phrase
        return result
    }
    
    create(): void {
        this.requirements = delphi_compatability.TList().Create()
        this.changes = delphi_compatability.TList().Create()
    }
    
    addUseages(): void {
        let i: int
        
        this.context.contextUseages += 1
        for (i = 0; i <= this.requirements.Count - 1; i++) {
            TSDesiredStateVariableWrapper(this.requirements[i]).variable.requirementsUseages += 1
        }
        this.command.commandUseages += 1
        this.move.moveUseages += 1
        for (i = 0; i <= this.changes.Count - 1; i++) {
            TSDesiredStateVariableWrapper(this.changes[i]).variable.changesUseages += 1
        }
        this.useagesRemoved = false
    }
    
    removeUseages(): void {
        let i: int
        
        this.context.contextUseages -= 1
        for (i = 0; i <= this.requirements.Count - 1; i++) {
            TSDesiredStateVariableWrapper(this.requirements[i]).variable.requirementsUseages -= 1
        }
        this.command.commandUseages -= 1
        this.move.moveUseages -= 1
        for (i = 0; i <= this.changes.Count - 1; i++) {
            TSDesiredStateVariableWrapper(this.changes[i]).variable.changesUseages -= 1
        }
        this.useagesRemoved = true
    }
    
    destroy(): void {
        let i: int
        
        if (!this.useagesRemoved) {
            if (this.context !== null) {
                this.context.contextUseages -= 1
            }
            if (this.requirements !== null) {
                for (i = 0; i <= this.requirements.Count - 1; i++) {
                    TSDesiredStateVariableWrapper(this.requirements[i]).variable.requirementsUseages -= 1
                    TSDesiredStateVariableWrapper(this.requirements[i]).free
                }
            }
            if (this.command !== null) {
                this.command.commandUseages -= 1
            }
            if (this.move !== null) {
                this.move.moveUseages -= 1
            }
            if (this.changes !== null) {
                for (i = 0; i <= this.changes.Count - 1; i++) {
                    TSDesiredStateVariableWrapper(this.changes[i]).variable.changesUseages -= 1
                    TSDesiredStateVariableWrapper(this.changes[i]).free
                }
            }
        } else {
            if (this.requirements !== null) {
                for (i = 0; i <= this.requirements.Count - 1; i++) {
                    TSDesiredStateVariableWrapper(this.requirements[i]).free
                }
            }
            if (this.changes !== null) {
                for (i = 0; i <= this.changes.Count - 1; i++) {
                    TSDesiredStateVariableWrapper(this.changes[i]).free
                }
            }
        }
        this.requirements.free
        this.requirements = null
        this.changes.free
        this.changes = null
        TSDraggableObject.prototype.destroy.call(this)
    }
    
    setContext(aString: string): void {
        if (this.context !== null) {
            this.context.contextUseages -= 1
        }
        this.context = this.world.findOrCreateVariable(aString, false)
        this.context.contextUseages += 1
    }
    
    setRequirements(aString: string): void {
        let i: int
        
        this.requirementsString = aString
        if (this.requirements.Count > 0) {
            for (i = 0; i <= this.requirements.Count - 1; i++) {
                TSDesiredStateVariableWrapper(this.requirements[i]).variable.requirementsUseages -= 1
                TSDesiredStateVariableWrapper(this.requirements[i]).free
            }
        }
        this.requirements.Clear()
        this.compile(aString, this.requirements)
        if (this.requirements.Count > 0) {
            for (i = 0; i <= this.requirements.Count - 1; i++) {
                TSDesiredStateVariableWrapper(this.requirements[i]).variable.requirementsUseages += 1
            }
        }
    }
    
    setCommand(aString: string): void {
        if (this.command !== null) {
            this.command.commandUseages -= 1
        }
        this.command = this.world.findOrCreateVariable(aString, false)
        this.command.commandUseages += 1
    }
    
    setReply(aString: string): void {
        let i: int
        let safeString: string
        
        safeString = aString
        for (i = 1; i <= len(safeString); i++) {
            if (safeString[i] === chr(13)) {
                safeString[i] = " "
            }
            if (safeString[i] === chr(10)) {
                safeString[i] = " "
            }
        }
        this.reply = safeString
    }
    
    setMove(aString: string): void {
        if (this.move !== null) {
            this.move.moveUseages -= 1
        }
        this.move = this.world.findOrCreateVariable(aString, false)
        this.move.moveUseages += 1
    }
    
    setChanges(aString: string): void {
        let i: int
        
        this.changesString = aString
        if (this.changes.Count > 0) {
            for (i = 0; i <= this.changes.Count - 1; i++) {
                TSDesiredStateVariableWrapper(this.changes[i]).variable.changesUseages -= 1
                TSDesiredStateVariableWrapper(this.changes[i]).free
            }
        }
        this.changes.Clear()
        this.compile(aString, this.changes)
        if (this.changes.Count > 0) {
            for (i = 0; i <= this.changes.Count - 1; i++) {
                TSDesiredStateVariableWrapper(this.changes[i]).variable.changesUseages += 1
            }
        }
    }
    
    compile(aString: string, list: TList): void {
        let phrase: string
        let position: int
        let remaining: string
        let variable: TSVariable
        let desiredState: TSVariableState
        let wrapper: TSDesiredStateVariableWrapper
        
        remaining = trim(aString)
        while ((len(trim(remaining)) > 0)) {
            position = UNRESOLVED.pos("&", remaining)
            if (position > 0) {
                phrase = UNRESOLVED.Copy(remaining, 1, position - 1)
            } else {
                phrase = remaining
            }
            phrase = trim(phrase)
            if (UNRESOLVED.pos("~", phrase) === 1) {
                desiredState = TSVariableState.kAbsent
            } else {
                desiredState = TSVariableState.kPresent
            }
            if (desiredState === TSVariableState.kAbsent) {
                phrase = trim(UNRESOLVED.Copy(phrase, 2, len(phrase)))
            }
            variable = this.world.findOrCreateVariable(phrase, false)
            if (list === this.requirements) {
                // need to distinguish for context list
                wrapper = TSDesiredStateVariableWrapper.create
            } else {
                wrapper = TSDesiredStateVariableWrapper.create
            }
            wrapper.variable = variable
            wrapper.desiredState = desiredState
            list.Add(wrapper)
            if (position > 0) {
                remaining = UNRESOLVED.Copy(remaining, position + 1, len(remaining))
            } else {
                remaining = ""
            }
        }
    }
    
    decompile(list: TList): string {
        let result = ""
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        let item: string
        
        result = ""
        for (i = 0; i <= list.Count - 1; i++) {
            // OK to cast requirements as this
            wrapper = TSDesiredStateVariableWrapper(list[i])
            item = wrapper.leader() + wrapper.variable.phrase
            if (result !== "") {
                result = result + " & " + item
            } else {
                result = item
            }
        }
        return result
    }
    
    decompileRequirements(): string {
        let result = ""
        result = this.decompile(this.requirements)
        return result
    }
    
    decompileChanges(): string {
        let result = ""
        result = this.decompile(this.changes)
        return result
    }
    
    updateAvailable(): void {
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        
        // assuming all field set up correctly and not nil
        this.available = false
        if (this.context === this.world.emptyEntry) {
            //for now - really should do anyway - assuming unfinished
            return
        }
        if (this.context.state !== TSVariableState.kPresent) {
            return
        }
        i = 0
        while (i < this.requirements.Count) {
            wrapper = TSDesiredStateVariableWrapper(this.requirements.Items[i])
            if (wrapper.variable.getState() !== wrapper.desiredState) {
                return
            }
            i = i + 1
        }
        this.available = true
    }
    
    recordReplyMoveChanges(changedVariablesList: TList, totalReply: string, contextToFocusTo: TSVariable): void {
        raise "method recordReplyMoveChanges had assigned to var parameter contextToFocusTo not added to return -- fixup manually"
        let i: int
        let desiredStateWrapper: TSDesiredStateVariableWrapper
        let changedVariableWrapper: TSChangedVariableWrapper
        
        if ((totalReply !== "") && (this.reply !== "")) {
            totalReply = totalReply + " "
        }
        totalReply = totalReply + this.reply
        if (this.move !== this.world.emptyEntry) {
            contextToFocusTo = this.move
        }
        i = 0
        while (i < this.changes.Count) {
            desiredStateWrapper = TSDesiredStateVariableWrapper(this.changes[i])
            changedVariableWrapper = TSChangedVariableWrapper().createWithVariableNewState(desiredStateWrapper.variable, desiredStateWrapper.desiredState)
            changedVariablesList.Add(changedVariableWrapper)
            i = i + 1
        }
        return totalReply
    }
    
    setTextForField(col: int, text: string): void {
        switch (col) {
            case kRuleContext:
                this.setContext(text)
                break
            case kRuleCommand:
                this.setCommand(text)
                break
            case kRuleReply:
                this.setReply(text)
                break
            case kRuleMove:
                this.setMove(text)
                break
            case kRuleRequirements:
                this.setRequirements(text)
                break
            case kRuleChanges:
                this.setChanges(text)
                break
    }
    
    getTextForField(col: int): string {
        let result = ""
        switch (col) {
            case kRuleContext:
                result = this.context.phrase
                break
            case kRuleCommand:
                result = this.command.phrase
                break
            case kRuleReply:
                result = this.reply
                break
            case kRuleMove:
                result = this.move.phrase
                break
            case kRuleRequirements:
                result = this.requirementsString
                break
            case kRuleChanges:
                result = this.changesString
                break
        return result
    }
    
    headerForField(col: int): string {
        let result = ""
        switch (col) {
            case kRuleContext:
                result = "Context"
                break
            case kRuleCommand:
                result = "Command"
                break
            case kRuleReply:
                result = "Reply"
                break
            case kRuleMove:
                result = "Move"
                break
            case kRuleRequirements:
                result = "Requirements"
                break
            case kRuleChanges:
                result = "Changes"
                break
        return result
    }
    
    usesVariableInList(variable: TSVariable, list: TList): boolean {
        let result = false
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        
        result = true
        for (i = 0; i <= list.Count - 1; i++) {
            wrapper = TSDesiredStateVariableWrapper(list.Items[i])
            if (wrapper.variable === variable) {
                return result
            }
        }
        result = false
        return result
    }
    
    usesVariableFor(variable: TSVariable, col: int): boolean {
        let result = false
        result = false
        switch (col) {
            case kRuleContext:
                result = this.context === variable
                break
            case kRuleCommand:
                result = this.command === variable
                break
            case kRuleReply:
                // error
                result = false
                break
            case kRuleMove:
                result = this.move === variable
                break
            case kRuleRequirements:
                result = this.usesVariableInList(variable, this.requirements)
                break
            case kRuleChanges:
                result = this.usesVariableInList(variable, this.changes)
                break
        return result
    }
    
    variableInList(n: int, list: TList): TSVariable {
        let result = new TSVariable()
        let wrapper: TSDesiredStateVariableWrapper
        
        if (n < 0) {
            n = 0
        }
        if (list.Count > n) {
            wrapper = TSDesiredStateVariableWrapper(list.Items[n])
            result = wrapper.variable
        } else {
            result = this.world.emptyEntry
        }
        return result
    }
    
    variableForFieldWithSelections(col: int, requirementsIndex: int, changesIndex: int): TSVariable {
        let result = new TSVariable()
        result = this.world.emptyEntry
        switch (col) {
            case kRuleContext:
                result = this.context
                break
            case kRuleCommand:
                result = this.command
                break
            case kRuleReply:
                // error
                result = this.world.emptyEntry
                break
            case kRuleMove:
                result = this.move
                break
            case kRuleRequirements:
                result = this.variableInList(requirementsIndex, this.requirements)
                break
            case kRuleChanges:
                result = this.variableInList(changesIndex, this.changes)
                break
        return result
    }
    
    variableForField(col: int): TSVariable {
        let result = new TSVariable()
        result = this.variableForFieldWithSelections(col, 0, 0)
        return result
    }
    
    setPosition(value: string): void {
        let firstPart: string
        let secondPart: string
        let thirdPart: string
        let rest: string
        
        firstPart = UNRESOLVED.Copy(value, 1, UNRESOLVED.pos("|", value) - 1)
        rest = UNRESOLVED.Copy(value, len(firstPart) + 2, len(value))
        secondPart = UNRESOLVED.Copy(rest, 1, UNRESOLVED.pos("|", rest) - 1)
        thirdPart = UNRESOLVED.Copy(rest, len(secondPart) + 2, len(rest))
        TSDraggableObject.prototype.setPosition.call(this, firstPart)
        this.context.setPosition(secondPart)
        this.move.setPosition(thirdPart)
    }
    
}

export class TSIndexChangeRuleWrapper {
    rule: TSRule = new TSRule()
    oldIndex: int = 0
    newIndex: int = 0
    
    //////////////////// TSIndexChangeRuleWrapper ///////////////////////////
    createWithRuleNewIndex(rule: TSRule, newIndex: int): void {
        this.rule = rule
        this.oldIndex = rule.world.rules.IndexOf(rule)
        this.newIndex = newIndex
    }
    
    doChange(): void {
        if (this.oldIndex === this.newIndex) {
            return
        }
        if (this.newIndex >= 0) {
            this.rule.world.rules.Move(this.oldIndex, this.newIndex)
        } else {
            this.rule.world.rules.Delete(this.oldIndex)
        }
    }
    
    undoChange(): void {
        if (this.oldIndex === this.newIndex) {
            return
        }
        if (this.newIndex >= 0) {
            this.rule.world.rules.Move(this.newIndex, this.oldIndex)
        } else {
            this.rule.world.rules.Insert(this.oldIndex, this.rule)
        }
    }
    
}

export class TWorld {
    emptyEntry: TSVariable = new TSVariable()
    variables: TList = new TList()
    rules: TList = new TList()
    focus: TSVariable = new TSVariable()
    previousFocus: TSVariable = new TSVariable()
    firstCommandDoneForLastCommandPhrase: int = 0
    lastVariableCreated: string = ""
    
    //////////////////// TWorld ///////////////////////////
    Create(): void {
        this.variables = delphi_compatability.TList().Create()
        this.rules = delphi_compatability.TList().Create()
        this.emptyEntry = TSVariable.create
    }
    
    resetVariableValues(): void {
        let i: int
        
        if (this.variables !== null) {
            if (this.variables.Count > 0) {
                for (i = 0; i <= this.variables.Count - 1; i++) {
                    TSVariable(this.variables.Items[i]).state = TSVariableState.kAbsent
                }
            }
        }
    }
    
    resetVariablesAndRules(): void {
        let i: int
        
        if (this.rules !== null) {
            if (this.rules.Count > 0) {
                for (i = 0; i <= this.rules.Count - 1; i++) {
                    TSRule(this.rules.Items[i]).free
                }
            }
            this.rules.Clear()
        }
        if (this.variables !== null) {
            if (this.variables.Count > 0) {
                for (i = 0; i <= this.variables.Count - 1; i++) {
                    TSVariable(this.variables.Items[i]).free
                }
            }
            this.variables.Clear()
        }
    }
    
    Destroy(): void {
        this.resetVariablesAndRules()
        this.rules.free
        this.rules = null
        this.variables.free
        this.variables = null
        this.emptyEntry.free
        this.emptyEntry = null
        TObject.prototype.Destroy.call(this)
    }
    
    newRule(): TSRule {
        let result = new TSRule()
        result = TSRule().create()
        result.world = this
        result.context = this.emptyEntry
        result.command = this.emptyEntry
        result.move = this.emptyEntry
        if (usruleeditorform.RuleEditorForm !== null) {
            result.position = usruleeditorform.RuleEditorForm.goodPosition()
        }
        this.rules.Add(result)
        return result
    }
    
    findVariable(aString: string): TSVariable {
        let result = new TSVariable()
        let i: int
        
        result = null
        if (aString === "") {
            result = this.emptyEntry
            return result
        }
        if (this.variables.Count > 0) {
            for (i = 0; i <= this.variables.Count - 1; i++) {
                if (UNRESOLVED.AnsiCompareText(TSVariable(this.variables.Items[i]).phrase, aString) === 0) {
                    result = TSVariable(this.variables.Items[i])
                    if (TSVariable(this.variables.Items[i]).phrase !== aString) {
                        // take on the case of the last choice if not the same
                        TSVariable(this.variables.Items[i]).setPhrase(aString)
                    }
                    return result
                }
            }
        }
        return result
    }
    
    findOrCreateVariable(aString: string, madeByMacro: boolean): TSVariable {
        let result = new TSVariable()
        result = this.findVariable(trim(aString))
        if (result !== null) {
            return result
        }
        result = TSVariable.create
        result.world = this
        result.setPhrase(trim(aString))
        // directly set for now - otherwise circular error on startup...
        result.state = TSVariableState.kAbsent
        if (usruleeditorform.RuleEditorForm !== null) {
            result.position = usruleeditorform.RuleEditorForm.goodPosition()
        }
        this.variables.Add(result)
        this.lastVariableCreated = aString
        return result
    }
    
    setInitialFocus(): void {
        if (this.rules.Count > 0) {
            this.focus = TSRule(this.rules[0]).context
            this.previousFocus = TSRule(this.rules[0]).context
            this.focus.state = TSVariableState.kPresent
            this.updateAvailable()
        } else {
            this.focus = null
            this.previousFocus = null
        }
    }
    
    newWorld(): void {
        this.resetVariablesAndRules()
        this.focus = null
        this.previousFocus = null
    }
    
    loadWorldFromFile(name: string): boolean {
        let result = false
        let WorldFile: TextFile
        let value: string
        let rule: TSRule
        let count: int
        let header: string
        
        result = false
        usconsoleform.ConsoleForm.reportMode("Loading")
        AssignFile(WorldFile, name)
        Reset(WorldFile)
        try {
            // done by caller to allow merges
            //self.resetVariablesAndRules;
            count = 0
            // unfinished - need better error checking
            UNRESOLVED.readln(WorldFile, header)
            if ((header !== "; world file version 1.0") && (header !== "; StoryHarp world file version 1.3")) {
                ShowMessage("File header for world file is not correct")
                return result
            }
            while (!UNRESOLVED.eof(WorldFile)) {
                UNRESOLVED.readln(WorldFile, value)
                if ((value !== "") && (value[1] === ";")) {
                    continue
                }
                if (value !== "====================") {
                    return result
                }
                if (count === 0) {
                    // context
                    UNRESOLVED.readln(WorldFile, value)
                    // command
                    UNRESOLVED.readln(WorldFile, value)
                    // reply
                    UNRESOLVED.readln(WorldFile, value)
                    // move to
                    UNRESOLVED.readln(WorldFile, value)
                    // extra changes
                    UNRESOLVED.readln(WorldFile, value)
                    // extra requirements
                    UNRESOLVED.readln(WorldFile, value)
                    // map positions
                    UNRESOLVED.readln(WorldFile, value)
                } else {
                    rule = this.newRule()
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setContext(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setCommand(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setReply(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setMove(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setChanges(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setRequirements(trim(value))
                    UNRESOLVED.readln(WorldFile, value)
                    rule.setPosition(trim(value))
                }
                count = count + 1
            }
        } finally {
            CloseFile(WorldFile)
            usconsoleform.ConsoleForm.reportMode("Running")
        }
        result = true
        return result
    }
    
    saveWorldToFile(name: string, saveOnlySelectedRules: boolean): void {
        let i: int
        let WorldFile: TextFile
        let rule: TSRule
        
        AssignFile(WorldFile, name)
        Rewrite(WorldFile)
        usconsoleform.ConsoleForm.reportMode("Saving")
        try {
            // 1.0 had all lower case
            // 1.3 supports mixed case
            writeln(WorldFile, "; StoryHarp world file version 1.3")
            writeln(WorldFile, "====================")
            for (i = 0; i <= 5; i++) {
                writeln(WorldFile, TSRule.headerForField(i))
            }
            writeln(WorldFile, "map positions")
            for (i = 0; i <= this.rules.Count - 1; i++) {
                rule = TSRule(this.rules.Items[i])
                if (saveOnlySelectedRules && !rule.selected) {
                    continue
                }
                writeln(WorldFile, "====================")
                writeln(WorldFile, rule.context.phrase)
                writeln(WorldFile, rule.command.phrase)
                writeln(WorldFile, rule.reply)
                writeln(WorldFile, rule.move.phrase)
                writeln(WorldFile, rule.changesString)
                writeln(WorldFile, rule.requirementsString)
                writeln(WorldFile, rule.position.X, ",", rule.position.Y, "|", rule.context.position.X, ",", rule.context.position.Y, "|", rule.move.position.X, ",", rule.move.position.Y)
            }
            Flush(WorldFile)
        } finally {
            CloseFile(WorldFile)
            usconsoleform.ConsoleForm.reportMode("Running")
        }
    }
    
    newSession(): void {
        this.resetVariableValues()
        this.setInitialFocus()
    }
    
    loadSessionFromFile(name: string, worldFileName: string): boolean {
        let result = false
        let SessionFile: TextFile
        let variable: TSVariable
        let header: string
        let worldFileNameRead: string
        let variableNameRead: string
        let focusNameRead: string
        let previousFocusNameRead: string
        let completeWorldFileName: string
        
        result = false
        AssignFile(SessionFile, name)
        Reset(SessionFile)
        try {
            this.resetVariableValues()
            // unfinished - need better error checking
            UNRESOLVED.readln(SessionFile, header)
            if (header !== "; session file version 1.0") {
                ShowMessage("File header for session file is not correct")
                return result
            }
            UNRESOLVED.readln(SessionFile, header)
            if (header !== "============ Variables for world =================") {
                return result
            }
            UNRESOLVED.readln(SessionFile, worldFileNameRead)
            if (worldFileNameRead !== worldFileName) {
                completeWorldFileName = findCompleteWorldFileName(worldFileNameRead)
                if (completeWorldFileName !== "") {
                    usruleeditorform.RuleEditorForm.openWorldFile(completeWorldFileName)
                    // to counteract resetting session when load world
                    usdomain.domain.sessionFileName = name
                } else {
                    return result
                }
            }
            UNRESOLVED.readln(SessionFile, header)
            if (header !== "============ Focus ===============================") {
                return result
            }
            UNRESOLVED.readln(SessionFile, focusNameRead)
            UNRESOLVED.readln(SessionFile, previousFocusNameRead)
            UNRESOLVED.readln(SessionFile, header)
            if (header !== "============ Variables ===========================") {
                return result
            }
            while (!UNRESOLVED.eof(SessionFile)) {
                UNRESOLVED.readln(SessionFile, variableNameRead)
                variableNameRead = trim(variableNameRead)
                variable = this.findOrCreateVariable(variableNameRead, false)
                variable.state = TSVariableState.kPresent
            }
        } finally {
            CloseFile(SessionFile)
        }
        this.focus = this.findOrCreateVariable(focusNameRead, false)
        this.previousFocus = this.findOrCreateVariable(previousFocusNameRead, false)
        this.updateAvailable()
        result = true
        return result
    }
    
    saveSessionToFile(name: string, worldFileName: string): void {
        let i: int
        let SessionFile: TextFile
        let variable: TSVariable
        
        AssignFile(SessionFile, name)
        Rewrite(SessionFile)
        try {
            writeln(SessionFile, "; session file version 1.0")
            writeln(SessionFile, "============ Variables for world =================")
            writeln(SessionFile, worldFileName)
            writeln(SessionFile, "============ Focus ===============================")
            writeln(SessionFile, this.focus.phrase)
            writeln(SessionFile, this.previousFocus.phrase)
            writeln(SessionFile, "============ Variables ===========================")
            for (i = 0; i <= this.variables.Count - 1; i++) {
                variable = TSVariable(this.variables.Items[i])
                if (variable.state === TSVariableState.kPresent) {
                    writeln(SessionFile, variable.phrase)
                }
            }
            Flush(SessionFile)
        } finally {
            CloseFile(SessionFile)
        }
    }
    
    updateAvailable(): void {
        let rule: TSRule
        let i: int
        
        if (this.rules.Count > 0) {
            for (i = 0; i <= this.rules.Count - 1; i++) {
                rule = TSRule(this.rules.Items[i])
                rule.updateAvailable()
            }
        }
    }
    
    setFocusTo(contextToFocusOn: TSVariable): void {
        if (contextToFocusOn !== null) {
            this.previousFocus = this.focus
            this.previousFocus.setState(TSVariableState.kAbsent)
            this.focus = contextToFocusOn
            this.focus.setState(TSVariableState.kPresent)
        }
    }
    
    // retruns whether should redraw grid
    deselectAllExcept(exceptObject: TSDraggableObject): boolean {
        let result = false
        let rule: TSRule
        let variable: TSVariable
        let i: int
        
        result = false
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            if ((rule.selected) && (rule !== exceptObject)) {
                rule.selected = false
                result = true
            }
        }
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if ((variable.selected) && (variable !== exceptObject)) {
                variable.selected = false
                result = true
            }
        }
        return result
    }
    
    addDragRecordsToList(dragRecords: TList): void {
        let rule: TSRule
        let variable: TSVariable
        let i: int
        
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            if ((rule.selected)) {
                dragRecords.Add(TSDragRecord().createWithNode(rule))
            }
        }
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if ((variable.selected)) {
                dragRecords.Add(TSDragRecord().createWithNode(variable))
            }
        }
    }
    
    deleteSelectedRules(): void {
        let rule: TSRule
        let i: int
        let command: TSDeleteRulesCommand
        
        command = uscommands.TSDeleteRulesCommand().create()
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            if ((rule.selected)) {
                command.addRule(rule, -1)
            }
        }
        if (command.ruleWrappers.Count > 0) {
            usdomain.domain.worldCommandList.doCommand(command)
        } else {
            command.free
        }
    }
    
    raiseSelectedRules(): void {
        let rule: TSRule
        let higherRule: TSRule
        let i: int
        let command: TSMoveRulesCommand
        let moving: boolean
        
        command = uscommands.TSMoveRulesCommand().create()
        command.action = "raise"
        moving = false
        for (i = 1; i <= this.rules.Count - 1; i++) {
            //skip first
            rule = TSRule(this.rules[i])
            if (rule.selected) {
                if (!moving) {
                    higherRule = TSRule(this.rules[i - 1])
                    if (!higherRule.selected) {
                        moving = true
                    }
                }
                if (moving) {
                    command.addRule(rule, i - 1)
                }
            } else {
                moving = true
            }
        }
        if (command.ruleWrappers.Count > 0) {
            usdomain.domain.worldCommandList.doCommand(command)
        } else {
            command.free
        }
    }
    
    lowerSelectedRules(): void {
        let rule: TSRule
        let lowerRule: TSRule
        let i: int
        let command: TSMoveRulesCommand
        let moving: boolean
        
        command = uscommands.TSMoveRulesCommand().create()
        command.action = "lower"
        moving = false
        for (i = this.rules.Count - 2; i >= 0; i--) {
            //skip first
            rule = TSRule(this.rules[i])
            if (rule.selected) {
                if (!moving) {
                    lowerRule = TSRule(this.rules[i + 1])
                    if (!lowerRule.selected) {
                        moving = true
                    }
                }
                if (moving) {
                    command.addRule(rule, i + 1)
                }
            } else {
                moving = true
            }
        }
        if (command.ruleWrappers.Count > 0) {
            usdomain.domain.worldCommandList.doCommand(command)
        } else {
            command.free
        }
    }
    
    selectAvailable(): void {
        let rule: TSRule
        let i: int
        
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            rule.selected = rule.available
        }
    }
    
    firstAvailable(): TSRule {
        let result = new TSRule()
        let rule: TSRule
        let i: int
        
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            if (rule.available) {
                result = rule
                return result
            }
        }
        result = null
        return result
    }
    
    selectInRectangle(rect: TRect): void {
        let intersection: TRect
        let i: int
        let rule: TSRule
        let variable: TSVariable
        
        if (Rect.right < Rect.left) {
            Rect.left, Rect.right = swapIntegers(Rect.left, Rect.right)
        }
        if (Rect.bottom < Rect.top) {
            Rect.top, Rect.bottom = swapIntegers(Rect.top, Rect.bottom)
        }
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            delphi_compatability.IntersectRect(intersection, rule.bounds(), Rect)
            if (!delphi_compatability.IsRectEmpty(intersection)) {
                rule.selected = true
            }
        }
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            delphi_compatability.IntersectRect(intersection, variable.bounds(), Rect)
            if (!delphi_compatability.IsRectEmpty(intersection)) {
                variable.selected = true
            }
        }
    }
    
    firstSelectedVariable(): TSVariable {
        let result = new TSVariable()
        let i: int
        let variable: TSVariable
        
        result = null
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if (variable.selected) {
                result = variable
                return result
            }
        }
        return result
    }
    
    firstSelectedObject(): TSDraggableObject {
        let result = new TSDraggableObject()
        let i: int
        let variable: TSVariable
        let rule: TSRule
        
        result = null
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if (variable.selected) {
                result = variable
                return result
            }
        }
        for (i = 0; i <= this.rules.Count - 1; i++) {
            rule = TSRule(this.rules[i])
            if (rule.selected) {
                result = rule
                return result
            }
        }
        return result
    }
    
    addContextsToCombBox(comboBox: TComboBox): void {
        let i: int
        let variable: TSVariable
        
        comboBox.Clear()
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if (variable.contextUseages > 0) {
                comboBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    
    addContextsToListBox(listBox: TListBox): void {
        let i: int
        let variable: TSVariable
        
        listBox.Clear()
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            if (variable.contextUseages > 0) {
                listBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    
    boundsRect(): TRect {
        let result = new TRect()
        let node: TSDraggableObject
        let i: int
        
        result.Top = 0
        result.Bottom = 0
        result.Left = 0
        result.Right = 0
        for (i = 0; i <= this.variables.Count - 1; i++) {
            node = this.variables[i]
            if (result.Left > node.position.X) {
                result.Left = node.position.X
            }
            if (result.Right < node.position.X + node.extent.X) {
                result.Right = node.position.X + node.extent.X
            }
            if (result.Top > node.position.Y) {
                result.Top = node.position.Y
            }
            if (result.Bottom < node.position.Y + node.extent.Y) {
                result.Bottom = node.position.Y + node.extent.Y
            }
        }
        for (i = 0; i <= this.rules.Count - 1; i++) {
            node = this.rules[i]
            if (result.Left > node.position.X) {
                result.Left = node.position.X
            }
            if (result.Right < node.position.X + node.extent.X) {
                result.Right = node.position.X + node.extent.X
            }
            if (result.Top > node.position.Y) {
                result.Top = node.position.Y
            }
            if (result.Bottom < node.position.Y + node.extent.Y) {
                result.Bottom = node.position.Y + node.extent.Y
            }
        }
        return result
    }
    
    updateVariablesForIndexInVariables(): void {
        let i: int
        let variable: TSVariable
        
        for (i = 0; i <= this.variables.Count - 1; i++) {
            variable = TSVariable(this.variables[i])
            variable.indexInVariables = i
        }
    }
    
}

