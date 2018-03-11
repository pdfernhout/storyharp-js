unit usjavawriter;

interface

uses Dialogs, SysUtils, Forms, USDomain, USWorld;

type

	TSJavaWriter = class
   	public
  	javaFile: TextFile;
    procedure java(line: string);
    procedure writeJavaProgram(filename: string);
		function logicalStatementForRule(rule: TSRule): string;
		function writeChangesForRule(rule: TSRule): string;
    function specialHandlingForReply(reply: string): string;
    procedure writeStoryFunctions;
    end;

implementation

procedure TSJavaWriter.java(line: string);
  begin
  writeln(javaFile, line);
  end;

function TSJavaWriter.logicalStatementForRule(rule: TSRule): string;
  var
  	i: integer;
    wrapper: TSDesiredStateVariableWrapper;
  begin
  result := 'variableValue[' + IntToStr(rule.context.indexInVariables) + ']';
  for i := 0 to rule.requirements.count - 1 do
    begin
    result := result + ' && ';
    wrapper := TSDesiredStateVariableWrapper(rule.requirements.items[i]);
    if wrapper.desiredState = kAbsent then
      result := result + '!';
    result := result + 'variableValue[' + IntToStr(wrapper.variable.indexInVariables) + ']';
    end;
  end;

function TSJavaWriter.writeChangesForRule(rule: TSRule): string;
  var
  	i: integer;
    wrapper: TSDesiredStateVariableWrapper;
    varies: string;
  begin
  for i := 0 to rule.changes.count - 1 do
    begin
    wrapper := TSDesiredStateVariableWrapper(rule.changes.items[i]);
  	varies := IntToStr(wrapper.variable.indexInVariables);
    if wrapper.desiredState = kAbsent then
java('      variableValue[' + varies + '] = false;')
		else
java('      variableValue[' + varies + '] = true;');
    end;
  if rule.move <> domain.world.emptyEntry then
    begin
varies := IntToStr(rule.move.indexInVariables);
java('      move(' + varies + ');');
    end;
  end;

function TSJavaWriter.specialHandlingForReply(reply: string): string;
  var i: integer;
  begin
  result := '';
  for i := 1 to length(reply) do
    begin
    if reply[i] = '"' then
    	result := result + '\';
    result := result + reply[i];
    end;
  end;

procedure TSJavaWriter.writeStoryFunctions;
  var
  	firstRule, rule: TSRule;
    varies: string;
    i: integer;
  begin
  firstRule := TSRule(domain.world.rules[0]);

varies := IntToStr(domain.world.variables.count);
java('  int SHNumberVariables()');
java('    {');
java('    return '+ varies +';');
java('    }');
java('');

varies := IntToStr(domain.world.rules.count);
java('  int SHNumberRules()');
java('    {');
java('    return '+ varies +';');
java('    }');
java('');

varies := IntToStr(firstRule.context.indexInVariables);
java('  int SHFirstLocation()');
java('    {');
java('    return '+ varies +';');
java('    }');
java('');

varies := IntToStr(firstRule.command.indexInVariables);
java('  int SHFirstCommand()');
java('    {');
java('    return '+ varies +';');
java('    }');
java('');

java('  void SHDefineVariables()');
java('    {');
  for i := 0 to domain.world.variables.count - 1 do
    begin
    varies := specialHandlingForReply(TSVariable(domain.world.variables[i]).phrase);
java('    variableName[' + IntToStr(i) + '] = "' + varies + '";');
    end;
java('    }');
java('');

java('  void SHComputeSatisfiedRules()');
java('    {');
  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
java('    ruleSatisfied[' + IntToStr(i) + '] = ' + logicalStatementForRule(rule) + ';');
    end;
java('    }');
java('');

java('  void SHAddAvailableCommands()');
java('    {');
  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
    varies := IntToStr(rule.command.indexInVariables);
java('    if (ruleSatisfied[' + IntToStr(i) + ']) addCommand(' + varies + ');');
    end;
java('    }');
java('');

java('  void SHDoCommand(int command)');
java('    {');
  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
    varies := IntToStr(rule.command.indexInVariables);
java('    if (command == ' + varies + ' && ruleSatisfied[' + IntToStr(i) +'])');
java('      {');
java('      reply("' + specialHandlingForReply(rule.reply) + '");');
    self.writeChangesForRule(rule);
java('      }');
    end;
java('    }');
java('');
java('  }');
  end;

procedure TSJavaWriter.writeJavaProgram(filename: string);
  var
    javaTemplate: TextFile;
    line: string;
	begin
  if (domain.world.rules.count < 1) or (domain.world.variables.count < 1) then
    begin
    ShowMessage('Some rules and contexts must be defined first');
    exit;
    end;
  AssignFile(javaTemplate, ExtractFilePath(Application.exeName) + 'Template.java');
  try
  Reset(javaTemplate);

  AssignFile(javaFile, filename);
  try
    Rewrite(javaFile);
    domain.world.updateVariablesForIndexInVariables;
    while not eof(javaTemplate) do
      begin
      readln(javaTemplate, line);
      java(line);
      end;
    writeStoryFunctions;
    Flush(javaFile);
  finally
  	try
 			CloseFile(javaFile);
  	except
      ShowMessage('Problem closing java file ' + filename);
  	end;
  end;
  finally
 			CloseFile(javaTemplate);
  end;
  end;

end.


