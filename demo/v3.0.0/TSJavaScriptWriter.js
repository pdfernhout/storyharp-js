define(["require", "exports", "./TSVariable"], function (require, exports, TSVariable_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSJavaScriptWriter {
        constructor() {
            this.output = [];
        }
        writeln(line) {
            this.output.push(line);
        }
        logicalStatementForRule(rule) {
            let result = "variableValue[" + rule.context.indexInVariables + "]";
            for (let i = 0; i < rule.requirements.length; i++) {
                result = result + " && ";
                const wrapper = rule.requirements[i];
                if (wrapper.desiredState === TSVariable_1.TSVariableState.kAbsent) {
                    result = result + "!";
                }
                result = result + "variableValue[" + wrapper.variable.indexInVariables + "]";
            }
            return result;
        }
        writeChangesForRule(world, rule) {
            let result = "";
            for (let i = 0; i < rule.changes.length; i++) {
                const wrapper = rule.changes[i];
                const varies = "" + wrapper.variable.indexInVariables;
                if (wrapper.desiredState === TSVariable_1.TSVariableState.kAbsent) {
                    this.writeln("            variableValue[" + varies + "] = false;");
                }
                else {
                    this.writeln("            variableValue[" + varies + "] = true;");
                }
            }
            if (rule.move !== world.emptyEntry) {
                const varies = "" + rule.move.indexInVariables;
                this.writeln("            move(" + varies + ");");
            }
            return result;
        }
        specialHandlingForReply(reply) {
            let result = "";
            for (let i = 0; i < reply.length; i++) {
                if (reply[i] === "\"") {
                    result = result + "\\";
                }
                result = result + reply[i];
            }
            return result;
        }
        writeStoryFunctions(world) {
            let varies;
            const firstRule = world.rules[0];
            varies = "" + world.variables.length;
            this.writeln("    function SHNumberVariables() {");
            this.writeln("        return " + varies + ";");
            this.writeln("    }");
            this.writeln("");
            varies = "" + world.rules.length;
            this.writeln("    function SHNumberRules() {");
            this.writeln("        return " + varies + ";");
            this.writeln("    }");
            this.writeln("");
            varies = "" + firstRule.context.indexInVariables;
            this.writeln("    function SHFirstLocation() {");
            this.writeln("        return " + varies + ";");
            this.writeln("    }");
            this.writeln("");
            varies = "" + firstRule.command.indexInVariables;
            this.writeln("    function SHFirstCommand() {");
            this.writeln("        return " + varies + ";");
            this.writeln("    }");
            this.writeln("");
            this.writeln("    function SHDefineVariables() {");
            for (let i = 0; i < world.variables.length; i++) {
                varies = this.specialHandlingForReply(world.variables[i].phrase);
                this.writeln("        variableName[" + i + "] = \"" + varies + "\";");
            }
            this.writeln("    }");
            this.writeln("");
            this.writeln("    function SHComputeSatisfiedRules() {");
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                this.writeln("        ruleSatisfied[" + i + "] = " + this.logicalStatementForRule(rule) + ";");
            }
            this.writeln("    }");
            this.writeln("");
            this.writeln("    function SHAddAvailableCommands() {");
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                varies = "" + rule.command.indexInVariables;
                this.writeln("        if (ruleSatisfied[" + i + "]) addCommand(" + varies + ");");
            }
            this.writeln("    }");
            this.writeln("");
            this.writeln("    function SHDoCommand(command) {");
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                varies = "" + rule.command.indexInVariables;
                this.writeln("        if (command == " + varies + " && ruleSatisfied[" + i + "]) {");
                this.writeln("            reply(\"" + this.specialHandlingForReply(rule.reply) + "\");");
                this.writeChangesForRule(world, rule);
                this.writeln("        }");
            }
            this.writeln("    }");
            this.writeln("");
        }
        writeJavaScriptProgram(world) {
            if ((world.rules.length < 1) || (world.variables.length < 1)) {
                return "";
            }
            this.output = [];
            world.updateVariablesForIndexInVariables();
            this.writeStoryFunctions(world);
            return this.output.join("\n") + "\n";
        }
    }
    exports.TSJavaScriptWriter = TSJavaScriptWriter;
});
