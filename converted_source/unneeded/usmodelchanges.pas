unit usmodelchanges;

interface

uses USWorld;

type

  TModelChange = class(TObject)
    procedure doChange; virtual;
    procedure undoChange; virtual;
    procedure redoChange; virtual;
    end;

  TRuleFieldChange = class(TModelChange)
    rule: TSRule;
    fieldType: integer;
    oldText, newText: string;
    constructor createWithRuleFieldTypeOldTextNewText(rule: TSRule; fieldType: integer; const newText: string);
    procedure doChange; override;
    procedure undoChange; override;
    procedure redoChange; override;
    end;

implementation

uses USRuleEditorForm;

//////////////////////////////////// TModelChange /////////////////////////////////////

procedure TModelChange.doChange;
  begin
  // subclasses should override
  end;

procedure TModelChange.undoChange;
  begin
  // subclasses should override
  end;

procedure TModelChange.redoChange;
  begin
  self.doChange;
  // sublasses may override and should call inherited doChange
  end;

//////////////////////////////// TRuleFieldChange ////////////////////////////////////

constructor TRuleFieldChange.createWithRuleFieldTypeOldTextNewText(rule: TSRule; fieldType: integer; const newText: string);
  begin
  self.create;
  self.rule := rule;
  self.fieldType := fieldType;
  if rule <> nil then
  	self.oldText := rule.getTextForField(fieldType);
  self.newText := newText;
  end;

procedure TRuleFieldChange.doChange;
  begin
  if rule <> nil then
  	rule.setTextForField(fieldType, newText);
  end;

procedure TRuleFieldChange.undoChange;
  begin
  if rule <> nil then
  	rule.setTextForField(fieldType, oldText);
//	RuleEditorForm.updateForFieldChange(fieldType);
  end;

procedure TRuleFieldChange.redoChange;
  begin
  if rule <> nil then
  	rule.setTextForField(fieldType, newText);
 //	RuleEditorForm.updateForFieldChange(fieldType);
  end;

end.
