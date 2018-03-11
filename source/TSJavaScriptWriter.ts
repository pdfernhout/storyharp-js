import { TSRule } from "./TSRule"
import { TSVariableState } from "./TSVariable"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper"
import { TSDomain } from "./TSDomain"

export class TSJavaScriptWriter {
    output: string[] = []
    
    writeln(line: string): void {
        this.output.push(line)
    }
    
    logicalStatementForRule(rule: TSRule): string {
        let result = "variableValue[" + rule.context.indexInVariables + "]"
        for (let i = 0; i < rule.requirements.length; i++) {
            result = result + " && "
            const wrapper: TSDesiredStateVariableWrapper = rule.requirements[i]
            if (wrapper.desiredState === TSVariableState.kAbsent) {
                result = result + "!"
            }
            result = result + "variableValue[" + wrapper.variable.indexInVariables + "]"
        }
        return result
    }
    
    writeChangesForRule(domain: TSDomain, rule: TSRule): string {
        let result = ""
        for (let i = 0; i < rule.changes.length; i++) {
            const wrapper: TSDesiredStateVariableWrapper = rule.changes[i]
            const varies: string = "" + wrapper.variable.indexInVariables
            if (wrapper.desiredState === TSVariableState.kAbsent) {
                this.writeln("      variableValue[" + varies + "] = false;")
            } else {
                this.writeln("      variableValue[" + varies + "] = true;")
            }
        }
        if (rule.move !== domain.world.emptyEntry) {
            const varies = "" + rule.move.indexInVariables
            this.writeln("      move(" + varies + ");")
        }
        return result
    }
    
    specialHandlingForReply(reply: string): string {
        let result = ""
        for (let i = 0; i < reply.length; i++) {
            if (reply[i] === "\"") {
                result = result + "\\"
            }
            result = result + reply[i]
        }
        return result
    }
    
    writeStoryFunctions(domain: TSDomain): void {
        let varies: string
        
        const firstRule: TSRule = domain.world.rules[0]
        varies = "" + domain.world.variables.length
        this.writeln("  int SHNumberVariables()")
        this.writeln("    {")
        this.writeln("    return " + varies + ";")
        this.writeln("    }")
        this.writeln("")
        varies = "" + domain.world.rules.length
        this.writeln("  int SHNumberRules()")
        this.writeln("    {")
        this.writeln("    return " + varies + ";")
        this.writeln("    }")
        this.writeln("")
        varies = "" + firstRule.context.indexInVariables
        this.writeln("  int SHFirstLocation()")
        this.writeln("    {")
        this.writeln("    return " + varies + ";")
        this.writeln("    }")
        this.writeln("")
        varies = "" + firstRule.command.indexInVariables
        this.writeln("  int SHFirstCommand()")
        this.writeln("    {")
        this.writeln("    return " + varies + ";")
        this.writeln("    }")
        this.writeln("")
        this.writeln("  void SHDefineVariables()")
        this.writeln("    {")
        for (let i = 0; i < domain.world.variables.length; i++) {
            varies = this.specialHandlingForReply(domain.world.variables[i].phrase)
            this.writeln("    variableName[" + i + "] = \"" + varies + "\";")
        }
        this.writeln("    }")
        this.writeln("")
        this.writeln("  void SHComputeSatisfiedRules()")
        this.writeln("    {")
        for (let i = 0; i < domain.world.rules.length; i++) {
            const rule: TSRule = domain.world.rules[i]
            this.writeln("    ruleSatisfied[" + i + "] = " + this.logicalStatementForRule(rule) + ";")
        }
        this.writeln("    }")
        this.writeln("")
        this.writeln("  void SHAddAvailableCommands()")
        this.writeln("    {")
        for (let i = 0; i < domain.world.rules.length; i++) {
            const rule: TSRule = domain.world.rules[i]
            varies = "" + rule.command.indexInVariables
            this.writeln("    if (ruleSatisfied[" + i + "]) addCommand(" + varies + ");")
        }
        this.writeln("    }")
        this.writeln("")
        this.writeln("  void SHDoCommand(int command)")
        this.writeln("    {")
        for (let i = 0; i < domain.world.rules.length; i++) {
            const rule: TSRule = domain.world.rules[i]
            varies = "" + rule.command.indexInVariables
            this.writeln("    if (command == " + varies + " && ruleSatisfied[" + i + "])")
            this.writeln("      {")
            this.writeln("      reply(\"" + this.specialHandlingForReply(rule.reply) + "\");")
            this.writeChangesForRule(domain, rule)
            this.writeln("      }")
        }
        this.writeln("    }")
        this.writeln("")
        this.writeln("  }")
    }
    
    writeJavaScriptProgram(domain: TSDomain, filename: string): void {
        if ((domain.world.rules.length < 1) || (domain.world.variables.length < 1)) {
            alert("Some rules and contexts must be defined first")
            return
        }
        // TODO: use or remove: load "Template.java"

        this.output = []
        domain.world.updateVariablesForIndexInVariables()
        /*TODO: Use or remove:
        while (!UNRESOLVED.eof(javaTemplate)) {
            UNRESOLVED.readln(javaTemplate, line)
            this.writeln(line)
        }
        */
        this.writeStoryFunctions(domain)
        console.log(this.output.join("/n") + "/n")
    }
    
}
