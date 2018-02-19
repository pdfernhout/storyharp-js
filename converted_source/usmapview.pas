unit usmapview;

interface

uses Windows, Classes, Controls, Graphics, SysUtils, USWorld, UCommand;

const
  kDrawCommand = true; kDrawContext = false;

type

	TSMapView = class(TObject)
  	public
    scroll: TPoint;
		function nearestNode(location: TPoint; const displayOptions: TSVariableDisplayOptions): TSDraggableObject;
    procedure displayOn(canvas: TCanvas; const displayOptions: TSVariableDisplayOptions; lastChoice, previousChoice: TSDraggableObject);
		// procedure drawArrow(canvas: TCanvas; a, b: TPoint);
    procedure drawArrowToRect(canvas: TCanvas; origin: TPoint; destRect: TRect);
		procedure drawArrowhead(canvas: TCanvas; p1, p2: TPoint);
		procedure drawBogusArrow(canvas: TCanvas; a, b: TPoint);
		procedure drawArrowToRectEdge(canvas: TCanvas; origin: TPoint; destRect: TRect);
		procedure drawArrowFromRectEdgeToRectEdge(canvas: TCanvas; originRect: TRect; destRect: TRect);
		procedure drawLineFromRectEdgeToRectEdge(canvas: TCanvas; originRect: TRect; destRect: TRect);
		procedure drawCommandOrContext(canvas: TCanvas; text: string;
			bounds: TRect; position: TPoint; selected, focused, isCommand: boolean);
    end;

implementation

uses USDomain, USRuleEditorForm;

const
	arrowlength = 10;
	arrowwidth  =  4;

{ TSMapView ---------------------------------------- }

procedure TSMapView.drawBogusArrow(canvas: TCanvas; a, b: TPoint);
  var
  //midPoint1: TPoint;
  midPoint2: TPoint;
  oldColor: TColor;
  begin
  //midPoint1 := Point(a.x + (b.x - a.x) div 3, a.y + (b.y - a.y) div 3);
  midPoint2 := Point(a.x + (b.x - a.x) div 3, a.y + (b.y - a.y) div 3);
  //midPoint2 := Point(b.x - (b.x - a.x) mod 2, b.y - (b.y - a.y) div 5);
  canvas.pen.style := psSolid;
	canvas.moveTo(a.x + scroll.x, a.y + scroll.y);
	canvas.lineTo(b.x + scroll.x, b.y + scroll.y);
  oldColor := canvas.brush.color;
  //canvas.brush.color := clGreen;
	//canvas.Ellipse(midPoint1.x-4, midPoint1.y-4,midPoint1.x+4, midPoint1.y+4);
  canvas.brush.color := clBlue;
	canvas.Ellipse(midPoint2.x - 4 + scroll.x, midPoint2.y - 4 + scroll.y, midPoint2.x + 4 + scroll.x, midPoint2.y + 4 + scroll.y);
  canvas.brush.color := oldColor;
  end;

{needs to have rectangle with center of 0,0 and origin adjusted to that coordinate system}
function AdjustedIntersectionPointForLineAndRectangle(origin: TPoint; rect: TRect): TPoint;
  var slope: extended;
  begin
  if PtInRect(rect, origin) then
    begin
    result := Point(0,0);
    exit;
    end;
  if (rect.left = 0) or (rect.top = 0) or (rect.right = 0) or (rect.bottom = 0) then
    begin
    result:= Point(0,0);
    exit;
    end;
  {do zero cases to avoid divide by zero later}
  if origin.x = 0 then
    begin
    if origin.y < 0 then
      result := Point(0, rect.top)
    else if origin.y = 0 then
      result := Point(0,0) // pathalogical case
    else // origin.y > 0
      result := Point(0, rect.bottom);
    end
  else if origin.y = 0 then
    begin
    if origin.x < 0 then
      result := Point(rect.left, 0)
    else // origin.x > 0
      result := Point(rect.right, 0);
    end
  else
    begin
    slope := (origin.y * 1.0) / origin.x;
  	if (origin.x > 0) and (origin.y < 0) then
    	begin
    	if slope < rect.top * 1.0 / rect.right then
      	result := Point(round(rect.top / slope), rect.top)
    	else
      	result := Point(rect.right, round(rect.right * slope));
    	end
  	else if (origin.x > 0) and (origin.y > 0) then
    	begin
    	if slope > rect.bottom * 1.0 / rect.right then
      	result := Point(round(rect.bottom / slope), rect.bottom)
    	else
      	result := Point(rect.right, round(rect.right * slope));
    	end
  	else if (origin.x < 0) and (origin.y < 0) then
    	begin
    	if slope > rect.top * 1.0 / rect.left then
      	result := Point(round(rect.top / slope), rect.top)
    	else
      	result := Point(rect.left, round(rect.left * slope));
    	end
  	else if (origin.x < 0) and (origin.y > 0) then
    	begin
    	if slope < rect.bottom * 1.0 / rect.left then
      	result := Point(round(rect.bottom / slope), rect.bottom)
    	else
      	result := Point(rect.left, round(rect.left * slope));
    	end;
    end;
  end;

function IntersectionPointForLineAndRectangle(origin: TPoint; destRect: TRect): TPoint;
  var
  	center: TPoint;
    adjustedRect: TRect;
    adjustedOrigin: TPoint;
  begin
  center := Point((destRect.left + destRect.right) div 2, (destRect.top + destRect.bottom) div 2);
  {make center of rectangle = 0,0}
  adjustedRect := Rect(destRect.left - center.x, destRect.top - center.y, destRect.right - center.x, destRect.bottom - center.y);
	adjustedOrigin := Point(origin.x - center.x, origin.y - center.y);
	result := AdjustedIntersectionPointForLineAndRectangle(adjustedOrigin, adjustedRect);
  result.x := result.x + center.x;
  result.y := result.y + center.y;
  end;

procedure TSMapView.drawArrowToRectEdge(canvas: TCanvas; origin: TPoint; destRect: TRect);
  var
  	startPoint, endPoint, intersectPoint: TPoint;
  begin
  // add some to prevent cutting off arrow heads in certain cases for long words
  InflateRect(destRect, arrowwidth, arrowwidth);
  intersectPoint := IntersectionPointForLineAndRectangle(origin, destRect);
  endPoint := Point(intersectPoint.x + scroll.x, intersectPoint.y + scroll.y);
  startPoint := Point(origin.x + scroll.x, origin.y + scroll.y);
  canvas.pen.style := psSolid;
 	canvas.moveTo(startPoint.x, startPoint.y);
 	canvas.lineTo(endPoint.x, endPoint.y);
  {arrow head}
  self.DrawArrowhead(canvas, startPoint, endPoint);
  end;

procedure TSMapView.drawArrowFromRectEdgeToRectEdge(canvas: TCanvas; originRect: TRect; destRect: TRect);
  var
  	startPoint, endPoint, intersectPoint: TPoint;
    origin, dest: TPoint;
    //theRect: TRect;
    scrolledOriginRect: TRect;
  begin
  //IntersectRect(theRect, originRect, destRect);
  //if not IsEmptyRect(theRect) then exit;
  // add some to prevent cutting off arrow heads in certain cases for long words
  InflateRect(destRect, arrowwidth, arrowwidth);
  InflateRect(originRect, arrowwidth, arrowwidth);
  origin := Point((originRect.left + originRect.right) div 2, (originRect.top + originRect.bottom) div 2);
  dest := Point((destRect.left + destRect.right) div 2, (destRect.top + destRect.bottom) div 2);
  intersectPoint := IntersectionPointForLineAndRectangle(origin, destRect);
  endPoint := Point(intersectPoint.x + scroll.x, intersectPoint.y + scroll.y);
  startPoint := IntersectionPointForLineAndRectangle(dest, originRect);
  startPoint.x := startPoint.x + scroll.x;
  startPoint.y := startPoint.y + scroll.y;

  {clipp arrow if it would end up beind drawn incorrectly}
  scrolledOriginRect := originRect;
  OffsetRect(scrolledOriginRect, scroll.x, scroll.y);
  if PtInRect(scrolledOriginRect, endPoint) then
  	exit;

  canvas.pen.style := psSolid;
 	canvas.moveTo(startPoint.x, startPoint.y);
 	canvas.lineTo(endPoint.x, endPoint.y);
  {arrow head}
  self.DrawArrowhead(canvas, startPoint, endPoint);
  end;

procedure TSMapView.drawLineFromRectEdgeToRectEdge(canvas: TCanvas; originRect: TRect; destRect: TRect);
  var
  	startPoint, endPoint, intersectPoint: TPoint;
    origin, dest: TPoint;
  begin
  // add some to prevent cutting off arrow heads in certain cases for long words
  InflateRect(destRect, arrowwidth, arrowwidth);
  InflateRect(originRect, arrowwidth, arrowwidth);
  origin := Point((originRect.left + originRect.right) div 2, (originRect.top + originRect.bottom) div 2);
  dest := Point((destRect.left + destRect.right) div 2, (destRect.top + destRect.bottom) div 2);
  intersectPoint := IntersectionPointForLineAndRectangle(origin, destRect);
  endPoint := Point(intersectPoint.x + scroll.x, intersectPoint.y + scroll.y);
  startPoint := IntersectionPointForLineAndRectangle(dest, originRect);
  startPoint.x := startPoint.x + scroll.x;
  startPoint.y := startPoint.y + scroll.y;
  canvas.pen.style := psSolid;
 	canvas.moveTo(startPoint.x, startPoint.y);
 	canvas.lineTo(endPoint.x, endPoint.y);
  end;

procedure TSMapView.drawArrowhead(canvas: TCanvas; p1, p2: TPoint);
  var
    dx, dy, x1, y1, x2, y2: integer;
    linelen, xstep, ystep: extended;
    outerOne, outerTwo: TPoint;
    Points: array [0..2] of TPoint;
  begin

	{Code translated from C++ posted:
	Subject:      Re: calculation for drawing arrow heads
	From:         "Jesper Hansen" <jesperh@edit.se>
	Date:         1998/01/27
	Message-ID:   <01bd2b58$1d33eaa0$65656565@foo.telia.com>
	Newsgroups:   comp.os.ms-windows.programmer.graphics }

	// given line from P1 to P2
	// draws arrowhead at P2

	dx := P1.x - P2.x;
	dy := P1.y - P2.y;
	linelen := sqrt(dx * dx + dy * dy);
  if linelen = 0 then linelen := 1;
	xstep := dx / linelen;
	ystep := dy / linelen;

	// modify according to preference
	// relationship comes from tan(angle)
	// relation 10:5 is 45 degrees (2*22.5)
	//

  try
	x2 := P2.x + round(xstep * arrowlength);
 	y2 := P2.y + round(ystep * arrowlength);

	y1 := -round(xstep * arrowwidth);
	x1 :=  round(ystep * arrowwidth);
  except
  exit;
  end;

  outerOne.x := x2 + x1;
  outerOne.y := y2 + y1;
  outerTwo.x := x2 - x1;
  outerTwo.y := y2 - y1;

  points[0] := p2;
  points[1] := outerOne;
  points[2] := outerTwo;
  
  canvas.pen.style := psSolid;
  canvas.polygon(points);
  {canvas.moveTo(p2.x, p2.y);
  canvas.LineTo(outerOne.x, outerOne.y);
  canvas.moveTo(p2.x, p2.y);
  canvas.LineTo(outerTwo.x, outerTwo.y); }
	end;

procedure TSMapView.drawArrowToRect(canvas: TCanvas; origin: TPoint; destRect: TRect);
  var
  	center: TPoint;
		endPoint: TPoint;
		startPoint: TPoint;
  begin
  // add some to prevent cutting off arrow heads in certain cases for long words
  InflateRect(destRect, arrowwidth, arrowwidth);
  center := Point((destRect.left + destRect.right) div 2, (destRect.top + destRect.bottom) div 2);
  {middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y); }
  {middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y);  }
  endPoint := Point(center.x + (origin.x - center.x) div 5 + scroll.x, center.y + (origin.y - center.y) div 5 + scroll.y);
  {startPoint := Point(origin.x + (center.x - origin.x) div 5 + scroll.x, origin.y + (center.y - origin.y) div 5 + scroll.y);
  }
  startPoint := Point(origin.x + scroll.x, origin.y + scroll.y);
  canvas.pen.style := psSolid;
 	canvas.moveTo(startPoint.x, startPoint.y);
 	canvas.lineTo(endPoint.x, endPoint.y);
  {arrow head}
  self.DrawArrowhead(canvas, startPoint, endPoint);
  {self.DrawArrowhead(canvas, Point(origin.x + scroll.x, origin.y + scroll.y), Point(center.x + scroll.x, center.y + scroll.y));}
  {	canvas.moveTo(middlePoint.x + scroll.x, middlePoint.y + scroll.y);
 	canvas.lineTo(middlePoint.x + scroll.x + , middlePoint.y + scroll.y + );
 	canvas.moveTo(middlePoint.x + scroll.x, middlePoint.y + scroll.y);
 	canvas.lineTo(middlePoint.x + scroll.x + , middlePoint.y + scroll.y + ); }
  end;

function TSMapView.nearestNode(location: TPoint; const displayOptions: TSVariableDisplayOptions): TSDraggableObject;
  var
  	//nearestNode: TSDraggableObject;
    variable: TSVariable;
    rule: TSRule;
  	//distance, nearestDistance: integer;
    i: integer;
    showNode: boolean;
  begin
  result := nil;
  //nearestDistance := -1;
  //nearestNode := nil;
	for i := domain.world.variables.count - 1 downto 0 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    showNode := variable.meetsDisplayOptions(displayOptions);
    if not showNode then continue;
    //distance := (variable.position.x - location.x) * (variable.position.x - location.x) +
    //		(variable.position.y - location.y) * (variable.position.y - location.y);
    //if (nearestDistance = -1) or (distance < nearestDistance) then
    //  begin
    //  nearestDistance := distance;
    //  nearestNode := variable;
    //  end;
    if PtInRect(variable.bounds, location) then
      begin
      result := variable;
      exit;
      end;
  	end;
  //result := nearestNode;
  if not displayOptions[kRuleCommand] then exit;
	for i := domain.world.rules.count - 1 downto 0 do
    begin
    rule := TSRule(domain.world.rules[i]);
    //distance := (rule.position.x - location.x) * (rule.position.x - location.x) +
    //	(rule.position.y - location.y) * (rule.position.y - location.y);
    //if (nearestDistance = -1) or (distance < nearestDistance) then
    //  begin
    //  nearestDistance := distance;
    //  nearestNode := rule;
    //  end;
    if PtInRect(rule.bounds, location) then
      begin
      result := rule;
      exit;
      end;
  	end;
  //result := nearestNode;
  end;

procedure TSMapView.displayOn(canvas: TCanvas;
    const displayOptions: TSVariableDisplayOptions; lastChoice, previousChoice: TSDraggableObject);
	var
  	i: integer;
    variable: TSVariable;
    rule: TSRule;
    textSize: TSize;
  	oldColor: TColor;
	begin
  // calculate bounds for text boxes
  SetTextAlign(canvas.Handle, TA_LEFT or TA_TOP);
  canvas.pen.color := clBlack;
  {need to compute these first - because they are referenced}
  {could be optimized out - only do if change text...}
	for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
    // update bounds -- optimize for case where rule is selected
    if rule = RuleEditorForm.rule then
      begin
      canvas.font.style := [fsBold];
      textSize := canvas.TextExtent(rule.displayName);
      canvas.font.style := [];
      end
    else
      textSize := canvas.TextExtent(rule.displayName);
    rule.extent.x := textSize.cx;
    rule.extent.y := textSize.cy;
    end;
	for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.meetsDisplayOptions(displayOptions) then
      begin
    	textSize := canvas.TextExtent(variable.displayName);
    	variable.extent.x := textSize.cx;
    	variable.extent.y := textSize.cy;
      end;
    end;
 	oldColor := canvas.brush.color;
  // draw lines and arrows
  canvas.brush.color := clBlack;
	for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
    if displayOptions[kRuleCommand] then
      begin
      if rule.context <> domain.world.emptyEntry then
        self.drawLineFromRectEdgeToRectEdge(canvas, rule.context.bounds, rule.bounds);
      if rule.move <> domain.world.emptyEntry then
        self.drawArrowFromRectEdgeToRectEdge(canvas, rule.bounds, rule.move.bounds)
      end
    else
      begin
      if (rule.context <> domain.world.emptyEntry) and (rule.move <> domain.world.emptyEntry) then
   			self.drawArrowFromRectEdgeToRectEdge(canvas, rule.context.bounds, rule.move.bounds)
      end;
    end;
 	canvas.brush.color := oldColor;
  // draw rectangles and text
  if displayOptions[kRuleCommand] then
	  for i := 0 to domain.world.rules.count - 1 do
      begin
      rule := TSRule(domain.world.rules[i]);
      with rule do
    		self.drawCommandOrContext(canvas, displayName, bounds, position,
      		selected, rule = RuleEditorForm.rule, kDrawCommand);
    end;
	for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.meetsDisplayOptions(displayOptions) then
      begin
      with variable do
    	  self.drawCommandOrContext(canvas, displayName, bounds, position,
      	  selected, false, kDrawContext);
       end;
    end;
	end;

procedure TSMapView.drawCommandOrContext(canvas: TCanvas; text: string;
		bounds: TRect; position: TPoint; selected, focused, isCommand: boolean);
  var
    drawRect: TRect;
    textPoint: TPoint;
  begin
  with drawRect do
    begin
    left := bounds.left - 2 + scroll.x;
    top := bounds.top - 1 + scroll.y;
    right := bounds.right + 2 + scroll.x;
    bottom := bounds.bottom + 1 + scroll.y;
    end;
  with textPoint do
    begin
    x := bounds.left + scroll.x;
    y := bounds.top + scroll.y;
    end;
  setCanvasColorsForSelection(canvas, selected, focused, isCommand);
  with canvas do
    begin
    if selected then
      pen.style := psSolid
    else
      pen.style := psClear;
    with drawRect do rectangle(left, top, right, bottom);
    with textPoint do textOut(x, y, text);
    end;
  end;


end.
