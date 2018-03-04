import { int, compareTextIgnoreCase } from "./common"
import { TPoint } from "./TPoint"
import { TRect } from "./TRect"
import { TSVariable, TSVariableState } from "./TSVariable"
import { TSRule } from "./TSRule"
import { TSDragRecord } from "./TSDragRecord"
import { TSDraggableObject } from "./TSDraggableObject"

/* External dependencies that need to be defined in app:
  // These can be set at World creation
  TWorld instance goodPositionCallback
*/

/* TODO: FIX: this is used by loadSessionFromFile and needs to be rethought
function findCompleteWorldFileName(worldFileNameRead: string): string {
    let result = ""
    if (!FileExists(worldFileNameRead)) {
        result = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeWorld, worldFileNameRead, "Search for world file: " + worldFileNameRead, ufilesupport.kOtherExtNotOK)
    } else {
        result = worldFileNameRead
    }
    return result
}
*/

// TODO: Should be an exported enum
const kSaveAllRules = false
const kSaveOnlySelectedRules = true

function swapIntegers(a: int, b: int): int[] {
    return [b, a]
}

export class TWorld {
    // TODO: goodPosition needs to be set by user
    goodPositionCallback = function() { return new TPoint(0, 0) }

    emptyEntry: TSVariable = new TSVariable()
    variables: TSVariable[] = []
    rules: TSRule[] = []
    focus: TSVariable | null = null
    previousFocus: TSVariable | null = null
    firstCommandDoneForLastCommandPhrase: int = 0
    lastVariableCreated: string = ""
    
    resetVariableValues(): void {
        if (this.variables) {
            for (let i = 0; i < this.variables.length; i++) {
                this.variables[i].state = TSVariableState.kAbsent
            }
        }
    }
    
    resetVariablesAndRules(): void {
        if (this.rules) {
            this.rules.length = 0
        }
        if (this.variables) {
            this.variables.length = 0
        }
    }
    
    newRule(): TSRule {
        // TODO: Maybe make a constructor for TSRule?
        const result: TSRule = new TSRule()
        result.world = this
        result.context = this.emptyEntry
        result.command = this.emptyEntry
        result.move = this.emptyEntry
        result.position = this.goodPositionCallback()
        this.rules.push(result)
        return result
    }
    
    findVariable(aString: string): TSVariable | null {
        if (aString === "") {
            return this.emptyEntry
        }
        for (let i = 0; i < this.variables.length; i++) {
            if (compareTextIgnoreCase(this.variables[i].phrase, aString)) {
                const result: TSVariable = this.variables[i]
                if (result.phrase !== aString) {
                    // take on the case of the last choice if not the same
                    result.setPhrase(aString)
                }
                return result
            }
        }
        return null
    }
    
    findOrCreateVariable(aString: string, madeByMacro: boolean): TSVariable {
        const match = this.findVariable(aString.trim())
        if (match !== null) {
            return match
        }
        const result = new TSVariable()
        result.setPhrase(aString.trim())
        // directly set for now - otherwise circular error on startup...
        result.state = TSVariableState.kAbsent
        result.position = this.goodPositionCallback()
        this.variables.push(result)
        this.lastVariableCreated = aString
        return result
    }
    
    setInitialFocus(): void {
        if (this.rules.length > 0) {
            this.focus = this.rules[0].context
            this.previousFocus = this.rules[0].context
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
    
    /* TODO: IMPLEMENT: loadWorldFromFile(name: string) using file */

    loadWorldFromFileContents(contents: string): boolean {

        // Emulate file reader
        const lines = contents.split(/\r?\n/)
        if (lines.length && lines[lines.length - 1] === "") lines.pop()
        function readln(): string {
            const result = lines.shift()
            if (result !== undefined) return result
            throw new Error("Unexpected EOF loading file")
        }
        function eof() {
            return !lines.length
        }
        
        // done by caller to allow merges
        // this.resetVariablesAndRules()
        let count = 0
        // unfinished - need better error checking
        const header = readln()
        if ((header !== "; world file version 1.0") && (header !== "; StoryHarp world file version 1.3")) {
            throw new Error("File header for world file is not correct")
        }
        while (!eof()) {
            let value = readln()
            if ((value !== "") && (value[0] === ";")) {
                continue
            }
            if (value !== "====================") {
                return false
            }
            if (count === 0) {
                // context
                value = readln()
                // command
                value = readln()
                // reply
                value = readln()
                // move to
                value = readln()
                // extra changes
                value = readln()
                // extra requirements
                value = readln()
                // map positions
                value = readln()
            } else {
                const rule: TSRule = this.newRule()
                value = readln()
                rule.setContext(value.trim())
                value = readln()
                rule.setCommand(value.trim())
                value = readln()
                rule.setReply(value.trim())
                value = readln()
                rule.setMove(value.trim())
                value = readln()
                rule.setChanges(value.trim())
                value = readln()
                rule.setRequirements(value.trim())
                value = readln()
                rule.setPosition(value.trim())
            }
            count = count + 1
        }

        return true
    }

    /* TODO: IMPLEMENT: saveWorldToFile(name: string, saveOnlySelectedRules: boolean): void { */
    saveWorldToFileContents(saveOnlySelectedRules: boolean): string {
        // Emulate file writer
        const lines: string[] = []
        function writeln(...sections: (string | number)[]): void {
            lines.push(sections.join(""))
        }

        // 1.0 had all lower case
        // 1.3 supports mixed case
        writeln("; StoryHarp world file version 1.3")
        writeln("====================")
        for (let i = 0; i < 6; i++) {
            writeln(TSRule.headerForField(i))
        }
        writeln("map positions")
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if (saveOnlySelectedRules && !rule.selected) {
                continue
            }
            writeln("====================")
            writeln(rule.context.phrase)
            writeln(rule.command.phrase)
            writeln(rule.reply)
            writeln(rule.move.phrase)
            writeln(rule.changesString)
            writeln(rule.requirementsString)
            writeln(rule.position.X, ",", rule.position.Y, "|", rule.context.position.X, ",", rule.context.position.Y, "|", rule.move.position.X, ",", rule.move.position.Y)
        }

        return lines.join("\n") + "\n"
    }
    
    newSession(): void {
        this.resetVariableValues()
        this.setInitialFocus()
    }
    
    /* TODO: IMPLEMENT: loadSessionFromFile(name: string, worldFileName: string): boolean { */
    loadSessionFromFile(worldFileName: string, contents: string): boolean {
        let focusNameRead: string
        let previousFocusNameRead: string
        
        // Emulate file reader
        const lines = contents.split(/\r?\n/)
        if (lines.length && lines[lines.length - 1] === "") lines.pop()
        function readln(): string {
            const result = lines.shift()
            if (result !== undefined) return result
            throw new Error("Unexpected EOF loading file")
        }
        function eof() {
            return !lines.length
        }
        
        let header: string
        this.resetVariableValues()
        // unfinished - need better error checking
        header = readln()
        if (header !== "; session file version 1.0") {
            throw new Error("File header for session file is not correct")
        }
        header = readln()
        if (header !== "============ Variables for world =================") {
            return false
        }
        const worldFileNameRead = readln()
        if (worldFileNameRead !== worldFileName) {
            // TODO: FIX THIS!!!!
            throw new Error("worldFileNameRead !== worldFileName")
            /* TODO FIX
            const completeWorldFileName = findCompleteWorldFileName(worldFileNameRead)
            if (completeWorldFileName !== "") {
                usruleeditorform.RuleEditorForm.openWorldFile(completeWorldFileName)
                // to counteract resetting session when load world
                usdomain.domain.sessionFileName = name
            } else {
                return false
            }
            */
        }
        header = readln()
        if (header !== "============ Focus ===============================") {
            return false
        }
        focusNameRead = readln()
        previousFocusNameRead = readln()
        header = readln()
        if (header !== "============ Variables ===========================") {
            return false
        }
        while (!eof()) {
            const variableNameRead = readln().trim()
            const variable: TSVariable = this.findOrCreateVariable(variableNameRead, false)
            variable.state = TSVariableState.kPresent
        }

        this.focus = this.findOrCreateVariable(focusNameRead, false)
        this.previousFocus = this.findOrCreateVariable(previousFocusNameRead, false)
        this.updateAvailable()
        return true
    }
    
    /* TODO: IMPLEMENT: saveSessionToFile(name: string, worldFileName: string): void { */
    saveSessionToFileContents(worldFileName: string): string {
        // Emulate file writer
        const lines: string[] = []
        function writeln(...sections: (string | number)[]): void {
            lines.push(sections.join(""))
        }
        
        writeln("; session file version 1.0")
        writeln("============ Variables for world =================")
        writeln(worldFileName)
        writeln("============ Focus ===============================")
        writeln(this.focus ? this.focus.phrase : "")
        writeln(this.previousFocus ? this.previousFocus.phrase: "")
        writeln("============ Variables ===========================")
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.state === TSVariableState.kPresent) {
                writeln(variable.phrase)
            }
        }

        return lines.join("\n") + "\n"
    }
    
    updateAvailable(): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            rule.updateAvailable()
        }
    }
    
    setFocusTo(contextToFocusOn: TSVariable): void {
        if (contextToFocusOn !== null) {
            this.previousFocus = this.focus
            if (this.previousFocus) {
                this.previousFocus.setState(TSVariableState.kAbsent)
            }
            this.focus = contextToFocusOn
            this.focus.setState(TSVariableState.kPresent)
        }
    }
    
    // returns whether should redraw grid
    deselectAllExcept(exceptObject: TSDraggableObject | null): boolean {
        let result = false
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if ((rule.selected) && (rule !== exceptObject)) {
                rule.selected = false
                result = true
            }
        }
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if ((variable.selected) && (variable !== exceptObject)) {
                variable.selected = false
                result = true
            }
        }
        return result
    }
    
    addDragRecordsToList(dragRecords: TSDragRecord[]): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if ((rule.selected)) {
                dragRecords.push(new TSDragRecord(rule))
            }
        }
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if ((variable.selected)) {
                dragRecords.push(new TSDragRecord(variable))
            }
        }
    }
    
    selectAvailable(): void {
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            rule.selected = rule.available
        }
    }
    
    firstAvailable(): TSRule | null {
        let result = new TSRule()
        
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if (rule.available) {
                return rule
            }
        }

        return null
    }

    /* TODO: Remove if not needed
    firstSelectedRule(): TSRule | null {
        let result = new TSRule()
        
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if (rule.selected) {
                return rule
            }
        }

        return null
    }
    */
    
    selectInRectangle(rect: TRect): void {
        let intersection: TRect
        let rule: TSRule
        let variable: TSVariable
        
        // Make a copy of rect in case swap values
        rect = rect.copy()
        if (rect.Right < rect.Left) {
            [rect.Left, rect.Right] = swapIntegers(rect.Left, rect.Right)
        }
        if (rect.Bottom < rect.Top) {
            [rect.Top, rect.Bottom] = swapIntegers(rect.Top, rect.Bottom)
        }
        for (let i = 0; i < this.rules.length; i++) {
            const rule:TSRule = this.rules[i]
            if (rule.bounds().intersects(rect)) {
                rule.selected = true
            }
        }
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.bounds().intersects(rect)) {
                variable.selected = true
            }
        }
    }
    
    firstSelectedVariable(): TSVariable | null {
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.selected) {
                return variable
            }
        }
        return null
    }
    
    firstSelectedObject(): TSDraggableObject | null {
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.selected) {
                return variable
            }
        }
        for (let i = 0; i < this.rules.length; i++) {
            const rule: TSRule = this.rules[i]
            if (rule.selected) {
                return rule
            }
        }
        return null
    }
    
    /* TODO: Maybe move this code used in wizard? If it is still needed with Mithril?
    addContextsToCombBox(comboBox: TComboBox): void {
        comboBox.Clear()
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.contextUseages > 0) {
                comboBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    
    addContextsToListBox(listBox: TListBox): void {
        listBox.Clear()
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            if (variable.contextUseages > 0) {
                listBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    */
    
    boundsRect(): TRect {
        // It is OK to have left and right at zero because
        // this is not a true bounds rect but one including the origin
        let result = new TRect(0, 0, 0, 0)

        function addToBounds(rect: TRect) {
            if (result.Left > rect.Left) {
                result.Left = rect.Left
            }
            if (result.Right < rect.Right) {
                result.Right = rect.Right
            }
            if (result.Top > rect.Top) {
                result.Top = rect.Top
            }
            if (result.Bottom < rect.Bottom) {
                result.Bottom = rect.Bottom
            }
        }

        for (let i = 0; i < this.variables.length; i++) {
            const node = this.variables[i]
            addToBounds(node.bounds())
        }
        for (let i = 0; i < this.rules.length; i++) {
            const node = this.rules[i]
            addToBounds(node.bounds())
        }
        const inflate = 10
        return new TRect(result.Left - inflate, result.Top - inflate, result.Right + inflate, result.Bottom + inflate)
    }
    
    updateVariablesForIndexInVariables(): void {
        for (let i = 0; i < this.variables.length; i++) {
            const variable: TSVariable = this.variables[i]
            variable.indexInVariables = i
        }
    }
}
