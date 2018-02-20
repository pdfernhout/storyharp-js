// unit usmapview

from conversion_common import *
import usruleeditorform
import usdomain
import ucommand
import usworld
import delphi_compatability

const usmapview = usmapview || {}

// const
const kDrawCommand = true
const kDrawContext = false


// const
const arrowlength = 10
const arrowwidth = 4


//needs to have rectangle with center of 0,0 and origin adjusted to that coordinate system
function AdjustedIntersectionPointForLineAndRectangle(origin: TPoint, rect: TRect): TPoint {
    let result = new TPoint()
    let slope: double
    
    if (delphi_compatability.PtInRect(Rect, origin)) {
        result = Point(0, 0)
        return result
    }
    if ((Rect.left === 0) || (Rect.top === 0) || (Rect.right === 0) || (Rect.bottom === 0)) {
        result = Point(0, 0)
        return result
    }
    if (origin.X === 0) {
        if (origin.Y < 0) {
            //do zero cases to avoid divide by zero later
            result = Point(0, Rect.top)
        } else if (origin.Y === 0) {
            // pathalogical case
            // origin.y > 0
            result = Point(0, 0)
        } else {
            result = Point(0, Rect.bottom)
        }
    } else if (origin.Y === 0) {
        if (origin.X < 0) {
            // origin.x > 0
            result = Point(Rect.left, 0)
        } else {
            result = Point(Rect.right, 0)
        }
    } else {
        slope = (origin.Y * 1.0) / origin.X
        if ((origin.X > 0) && (origin.Y < 0)) {
            if (slope < Rect.top * 1.0 / Rect.right) {
                result = Point(intround(Rect.top / slope), Rect.top)
            } else {
                result = Point(Rect.right, intround(Rect.right * slope))
            }
        } else if ((origin.X > 0) && (origin.Y > 0)) {
            if (slope > Rect.bottom * 1.0 / Rect.right) {
                result = Point(intround(Rect.bottom / slope), Rect.bottom)
            } else {
                result = Point(Rect.right, intround(Rect.right * slope))
            }
        } else if ((origin.X < 0) && (origin.Y < 0)) {
            if (slope > Rect.top * 1.0 / Rect.left) {
                result = Point(intround(Rect.top / slope), Rect.top)
            } else {
                result = Point(Rect.left, intround(Rect.left * slope))
            }
        } else if ((origin.X < 0) && (origin.Y > 0)) {
            if (slope < Rect.bottom * 1.0 / Rect.left) {
                result = Point(intround(Rect.bottom / slope), Rect.bottom)
            } else {
                result = Point(Rect.left, intround(Rect.left * slope))
            }
        }
    }
    return result
}

function IntersectionPointForLineAndRectangle(origin: TPoint, destRect: TRect): TPoint {
    let result = new TPoint()
    let center: TPoint
    let adjustedRect: TRect
    let adjustedOrigin: TPoint
    
    center = Point((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
    //make center of rectangle = 0,0
    adjustedRect = Rect(destRect.Left - center.X, destRect.Top - center.Y, destRect.Right - center.X, destRect.Bottom - center.Y)
    adjustedOrigin = Point(origin.X - center.X, origin.Y - center.Y)
    result = AdjustedIntersectionPointForLineAndRectangle(adjustedOrigin, adjustedRect)
    result.X = result.X + center.X
    result.Y = result.Y + center.Y
    return result
}


export class TSMapView {
    scroll: TPoint = new TPoint()
    
    // TSMapView ---------------------------------------- 
    drawBogusArrow(canvas: TCanvas, a: TPoint, b: TPoint): void {
        let midPoint2: TPoint
        let oldColor: TColor
        
        //midPoint1: TPoint;
        //midPoint1 := Point(a.x + (b.x - a.x) div 3, a.y + (b.y - a.y) div 3);
        midPoint2 = Point(a.X + (b.X - a.X) / 3, a.Y + (b.Y - a.Y) / 3)
        //midPoint2 := Point(b.x - (b.x - a.x) mod 2, b.y - (b.y - a.y) div 5);
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(a.X + this.scroll.X, a.Y + this.scroll.Y)
        canvas.LineTo(b.X + this.scroll.X, b.Y + this.scroll.Y)
        oldColor = canvas.Brush.Color
        //canvas.brush.color := clGreen;
        //canvas.Ellipse(midPoint1.x-4, midPoint1.y-4,midPoint1.x+4, midPoint1.y+4);
        canvas.Brush.Color = delphi_compatability.clBlue
        canvas.Ellipse(midPoint2.X - 4 + this.scroll.X, midPoint2.Y - 4 + this.scroll.Y, midPoint2.X + 4 + this.scroll.X, midPoint2.Y + 4 + this.scroll.Y)
        canvas.Brush.Color = oldColor
    }
    
    drawArrowToRectEdge(canvas: TCanvas, origin: TPoint, destRect: TRect): void {
        let startPoint: TPoint
        let endPoint: TPoint
        let intersectPoint: TPoint
        
        // add some to prevent cutting off arrow heads in certain cases for long words
        UNRESOLVED.InflateRect(destRect, arrowwidth, arrowwidth)
        intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)
        endPoint = Point(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)
        startPoint = Point(origin.X + this.scroll.X, origin.Y + this.scroll.Y)
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
        //arrow head
        this.drawArrowhead(canvas, startPoint, endPoint)
    }
    
    drawArrowFromRectEdgeToRectEdge(canvas: TCanvas, originRect: TRect, destRect: TRect): void {
        let startPoint: TPoint
        let endPoint: TPoint
        let intersectPoint: TPoint
        let origin: TPoint
        let dest: TPoint
        let scrolledOriginRect: TRect
        
        //theRect: TRect;
        //IntersectRect(theRect, originRect, destRect);
        //if not IsEmptyRect(theRect) then exit;
        // add some to prevent cutting off arrow heads in certain cases for long words
        UNRESOLVED.InflateRect(destRect, arrowwidth, arrowwidth)
        UNRESOLVED.InflateRect(originRect, arrowwidth, arrowwidth)
        origin = Point((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2)
        dest = Point((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
        intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)
        endPoint = Point(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)
        startPoint = IntersectionPointForLineAndRectangle(dest, originRect)
        startPoint.X = startPoint.X + this.scroll.X
        startPoint.Y = startPoint.Y + this.scroll.Y
        //clipp arrow if it would end up beind drawn incorrectly
        scrolledOriginRect = originRect
        delphi_compatability.OffsetRect(scrolledOriginRect, this.scroll.X, this.scroll.Y)
        if (delphi_compatability.PtInRect(scrolledOriginRect, endPoint)) {
            return
        }
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
        //arrow head
        this.drawArrowhead(canvas, startPoint, endPoint)
    }
    
    drawLineFromRectEdgeToRectEdge(canvas: TCanvas, originRect: TRect, destRect: TRect): void {
        let startPoint: TPoint
        let endPoint: TPoint
        let intersectPoint: TPoint
        let origin: TPoint
        let dest: TPoint
        
        // add some to prevent cutting off arrow heads in certain cases for long words
        UNRESOLVED.InflateRect(destRect, arrowwidth, arrowwidth)
        UNRESOLVED.InflateRect(originRect, arrowwidth, arrowwidth)
        origin = Point((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2)
        dest = Point((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
        intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)
        endPoint = Point(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)
        startPoint = IntersectionPointForLineAndRectangle(dest, originRect)
        startPoint.X = startPoint.X + this.scroll.X
        startPoint.Y = startPoint.Y + this.scroll.Y
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
    }
    
    drawArrowhead(canvas: TCanvas, p1: TPoint, p2: TPoint): void {
        let dx: int
        let dy: int
        let x1: int
        let y1: int
        let x2: int
        let y2: int
        let linelen: double
        let xstep: double
        let ystep: double
        let outerOne: TPoint
        let outerTwo: TPoint
        let Points: TPoint[] /* 2 + 1 */
        
        //Code translated from C++ posted:
        //	Subject:      Re: calculation for drawing arrow heads
        //	From:         "Jesper Hansen" <jesperh@edit.se>
        //	Date:         1998/01/27
        //	Message-ID:   <01bd2b58$1d33eaa0$65656565@foo.telia.com>
        //	Newsgroups:   comp.os.ms-windows.programmer.graphics 
        // given line from P1 to P2
        // draws arrowhead at P2
        dx = p1.X - p2.X
        dy = p1.Y - p2.Y
        linelen = sqrt(dx * dx + dy * dy)
        if (linelen === 0) {
            linelen = 1
        }
        xstep = dx / linelen
        ystep = dy / linelen
        try {
            // modify according to preference
            // relationship comes from tan(angle)
            // relation 10:5 is 45 degrees (2*22.5)
            //
            x2 = p2.X + intround(xstep * arrowlength)
            y2 = p2.Y + intround(ystep * arrowlength)
            y1 = -intround(xstep * arrowwidth)
            x1 = intround(ystep * arrowwidth)
        } catch (Exception e) {
            return
        }
        outerOne.X = x2 + x1
        outerOne.Y = y2 + y1
        outerTwo.X = x2 - x1
        outerTwo.Y = y2 - y1
        Points[0] = p2
        Points[1] = outerOne
        Points[2] = outerTwo
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.Polygon(Points)
        //canvas.moveTo(p2.x, p2.y);
        //  canvas.LineTo(outerOne.x, outerOne.y);
        //  canvas.moveTo(p2.x, p2.y);
        //  canvas.LineTo(outerTwo.x, outerTwo.y); 
    }
    
    // procedure drawArrow(canvas: TCanvas; a, b: TPoint);
    drawArrowToRect(canvas: TCanvas, origin: TPoint, destRect: TRect): void {
        let center: TPoint
        let endPoint: TPoint
        let startPoint: TPoint
        
        // add some to prevent cutting off arrow heads in certain cases for long words
        UNRESOLVED.InflateRect(destRect, arrowwidth, arrowwidth)
        center = Point((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
        //middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y); 
        //middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y);  
        endPoint = Point(center.X + (origin.X - center.X) / 5 + this.scroll.X, center.Y + (origin.Y - center.Y) / 5 + this.scroll.Y)
        //startPoint := Point(origin.x + (center.x - origin.x) div 5 + scroll.x, origin.y + (center.y - origin.y) div 5 + scroll.y);
        //  
        startPoint = Point(origin.X + this.scroll.X, origin.Y + this.scroll.Y)
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
        //arrow head
        this.drawArrowhead(canvas, startPoint, endPoint)
        //self.DrawArrowhead(canvas, Point(origin.x + scroll.x, origin.y + scroll.y), Point(center.x + scroll.x, center.y + scroll.y));
        //	canvas.moveTo(middlePoint.x + scroll.x, middlePoint.y + scroll.y);
        // 	canvas.lineTo(middlePoint.x + scroll.x + , middlePoint.y + scroll.y + );
        // 	canvas.moveTo(middlePoint.x + scroll.x, middlePoint.y + scroll.y);
        // 	canvas.lineTo(middlePoint.x + scroll.x + , middlePoint.y + scroll.y + ); 
    }
    
    nearestNode(location: TPoint, displayOptions: TSVariableDisplayOptions): TSDraggableObject {
        let result = new TSDraggableObject()
        let variable: TSVariable
        let rule: TSRule
        let i: int
        let showNode: boolean
        
        //nearestNode: TSDraggableObject;
        //distance, nearestDistance: integer;
        result = null
        for (i = usdomain.domain.world.variables.Count - 1; i >= 0; i--) {
            //nearestDistance := -1;
            //nearestNode := nil;
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            showNode = variable.meetsDisplayOptions(displayOptions)
            if (!showNode) {
                continue
            }
            if (delphi_compatability.PtInRect(variable.bounds(), location)) {
                //distance := (variable.position.x - location.x) * (variable.position.x - location.x) +
                //		(variable.position.y - location.y) * (variable.position.y - location.y);
                //if (nearestDistance = -1) or (distance < nearestDistance) then
                //  begin
                //  nearestDistance := distance;
                //  nearestNode := variable;
                //  end;
                result = variable
                return result
            }
        }
        if (!displayOptions[usworld.kRuleCommand]) {
            //result := nearestNode;
            return result
        }
        for (i = usdomain.domain.world.rules.Count - 1; i >= 0; i--) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (delphi_compatability.PtInRect(rule.bounds(), location)) {
                //distance := (rule.position.x - location.x) * (rule.position.x - location.x) +
                //	(rule.position.y - location.y) * (rule.position.y - location.y);
                //if (nearestDistance = -1) or (distance < nearestDistance) then
                //  begin
                //  nearestDistance := distance;
                //  nearestNode := rule;
                //  end;
                result = rule
                return result
            }
        }
        //result := nearestNode;
        return result
    }
    
    displayOn(canvas: TCanvas, displayOptions: TSVariableDisplayOptions, lastChoice: TSDraggableObject, previousChoice: TSDraggableObject): void {
        let i: int
        let variable: TSVariable
        let rule: TSRule
        let textSize: TSize
        let oldColor: TColor
        
        // calculate bounds for text boxes
        UNRESOLVED.SetTextAlign(canvas.Handle, delphi_compatability.TA_LEFT || delphi_compatability.TA_TOP)
        canvas.Pen.Color = delphi_compatability.clBlack
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            //need to compute these first - because they are referenced
            //could be optimized out - only do if change text...
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (rule === usruleeditorform.RuleEditorForm.rule) {
                // update bounds -- optimize for case where rule is selected
                canvas.Font.Style = {UNRESOLVED.fsBold, }
                textSize = canvas.TextExtent(rule.displayName())
                canvas.Font.Style = {}
            } else {
                textSize = canvas.TextExtent(rule.displayName())
            }
            rule.extent.X = textSize.cx
            rule.extent.Y = textSize.cy
        }
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.meetsDisplayOptions(displayOptions)) {
                textSize = canvas.TextExtent(variable.displayName())
                variable.extent.X = textSize.cx
                variable.extent.Y = textSize.cy
            }
        }
        oldColor = canvas.Brush.Color
        // draw lines and arrows
        canvas.Brush.Color = delphi_compatability.clBlack
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (displayOptions[usworld.kRuleCommand]) {
                if (rule.context !== usdomain.domain.world.emptyEntry) {
                    this.drawLineFromRectEdgeToRectEdge(canvas, rule.context.bounds(), rule.bounds())
                }
                if (rule.move !== usdomain.domain.world.emptyEntry) {
                    this.drawArrowFromRectEdgeToRectEdge(canvas, rule.bounds(), rule.move.bounds())
                }
            } else {
                if ((rule.context !== usdomain.domain.world.emptyEntry) && (rule.move !== usdomain.domain.world.emptyEntry)) {
                    this.drawArrowFromRectEdgeToRectEdge(canvas, rule.context.bounds(), rule.move.bounds())
                }
            }
        }
        canvas.Brush.Color = oldColor
        if (displayOptions[usworld.kRuleCommand]) {
            for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
                // draw rectangles and text
                rule = usworld.TSRule(usdomain.domain.world.rules[i])
                this.drawCommandOrContext(canvas, rule.displayName(), rule.bounds(), rule.position, rule.selected, rule === usruleeditorform.RuleEditorForm.rule, kDrawCommand)
            }
        }
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.meetsDisplayOptions(displayOptions)) {
                this.drawCommandOrContext(canvas, variable.displayName(), variable.bounds(), variable.position, variable.selected, false, kDrawContext)
            }
        }
    }
    
    drawCommandOrContext(canvas: TCanvas, text: string, bounds: TRect, position: TPoint, selected: boolean, focused: boolean, isCommand: boolean): void {
        let drawRect: TRect
        let textPoint: TPoint
        
        drawRect.Left = bounds.Left - 2 + this.scroll.X
        drawRect.Top = bounds.Top - 1 + this.scroll.Y
        drawRect.Right = bounds.Right + 2 + this.scroll.X
        drawRect.Bottom = bounds.Bottom + 1 + this.scroll.Y
        textPoint.X = bounds.Left + this.scroll.X
        textPoint.Y = bounds.Top + this.scroll.Y
        usruleeditorform.setCanvasColorsForSelection(canvas, selected, focused, isCommand)
        if (selected) {
            canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        } else {
            canvas.Pen.Style = delphi_compatability.TFPPenStyle.psClear
        }
        canvas.Rectangle(drawRect.Left, drawRect.Top, drawRect.Right, drawRect.Bottom)
        canvas.TextOut(textPoint.X, textPoint.Y, text)
    }
    
}

