import { TPoint } from "./TPoint";
import { TRect } from "./TRect";
import { TSDraggableObject } from "./TSDraggableObject"
import { TSVariable } from "./TSVariable";
import { TSRule, TSRuleField } from "./TSRule"
import { TWorld } from "./TWorld"

// const
const kDrawCommand = true
const kDrawContext = false

// const
const arrowlength = 10
const arrowwidth = 4

type double = number
type int = number

function intround(value: number) {
    return Math.round(value)
}

function InflateRect(rect: TRect, extraWidth: int, extraHeight: int): TRect {
    return new TRect(rect.Left - extraWidth, rect.Top - extraHeight, rect.width + extraWidth, rect.height + extraHeight);
}

function OffsetRect(rect: TRect, x: int, y: int): TRect {
    return new TRect(rect.Left + x, rect.Top + y, rect.width, rect.height);
}

//needs to have rectangle with center of 0,0 and origin adjusted to that coordinate system
function AdjustedIntersectionPointForLineAndRectangle(origin: TPoint, rect: TRect): TPoint {
    // TODO: assigning a new TPoint here to satisfy compiler but it is not needed
    let result: TPoint = new TPoint(0, 0)
    
    if (rect.contains(origin)) {
        result = new TPoint(0, 0)
        return result
    }
    if ((rect.Left === 0) || (rect.Top === 0) || (rect.Right === 0) || (rect.Bottom === 0)) {
        result = new TPoint(0, 0)
        return result
    }
    if (origin.X === 0) {
        if (origin.Y < 0) {
            //do zero cases to avoid divide by zero later
            result = new TPoint(0, rect.Top)
        } else if (origin.Y === 0) {
            // pathalogical case
            // origin.y > 0
            result = new TPoint(0, 0)
        } else {
            result = new TPoint(0, rect.Bottom)
        }
    } else if (origin.Y === 0) {
        if (origin.X < 0) {
            // origin.x > 0
            result = new TPoint(rect.Left, 0)
        } else {
            result = new TPoint(rect.Right, 0)
        }
    } else {
        const slope: double = (origin.Y * 1.0) / origin.X
        if ((origin.X > 0) && (origin.Y < 0)) {
            if (slope < rect.Top * 1.0 / rect.Right) {
                result = new TPoint(intround(rect.Top / slope), rect.Top)
            } else {
                result = new TPoint(rect.Right, intround(rect.Right * slope))
            }
        } else if ((origin.X > 0) && (origin.Y > 0)) {
            if (slope > rect.Bottom * 1.0 / rect.Right) {
                result = new TPoint(intround(rect.Bottom / slope), rect.Bottom)
            } else {
                result = new TPoint(rect.Right, intround(rect.Right * slope))
            }
        } else if ((origin.X < 0) && (origin.Y < 0)) {
            if (slope > rect.Top * 1.0 / rect.Left) {
                result = new TPoint(intround(rect.Top / slope), rect.Top)
            } else {
                result = new TPoint(rect.Left, intround(rect.Left * slope))
            }
        } else if ((origin.X < 0) && (origin.Y > 0)) {
            if (slope < rect.Bottom * 1.0 / rect.Left) {
                result = new TPoint(intround(rect.Bottom / slope), rect.Bottom)
            } else {
                result = new TPoint(rect.Left, intround(rect.Left * slope))
            }
        }
    }
    return result
}

function IntersectionPointForLineAndRectangle(origin: TPoint, destRect: TRect): TPoint {
    const center = new TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
    //make center of rectangle = 0,0
    const adjustedRect = new TRect(destRect.Left - center.X, destRect.Top - center.Y, destRect.Right - center.X, destRect.Bottom - center.Y)
    const adjustedOrigin = new TPoint(origin.X - center.X, origin.Y - center.Y)
    const result = AdjustedIntersectionPointForLineAndRectangle(adjustedOrigin, adjustedRect)
    result.X = result.X + center.X
    result.Y = result.Y + center.Y
    return result
}


export class TSMapView {
    scroll: TPoint = new TPoint()
    
    /*
    // TSMapView ---------------------------------------- 
    drawBogusArrow(canvas: TCanvas, a: TPoint, b: TPoint): void {
        //midPoint1: TPoint;
        //midPoint1 := Point(a.x + (b.x - a.x) div 3, a.y + (b.y - a.y) div 3);
        const midPoint2 = new TPoint(a.X + (b.X - a.X) / 3, a.Y + (b.Y - a.Y) / 3)
        //midPoint2 := Point(b.x - (b.x - a.x) mod 2, b.y - (b.y - a.y) div 5);
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(a.X + this.scroll.X, a.Y + this.scroll.Y)
        canvas.LineTo(b.X + this.scroll.X, b.Y + this.scroll.Y)
        const oldColor = canvas.Brush.Color
        //canvas.brush.color := clGreen;
        //canvas.Ellipse(midPoint1.x-4, midPoint1.y-4,midPoint1.x+4, midPoint1.y+4);
        canvas.Brush.Color = delphi_compatability.clBlue
        canvas.Ellipse(midPoint2.X - 4 + this.scroll.X, midPoint2.Y - 4 + this.scroll.Y, midPoint2.X + 4 + this.scroll.X, midPoint2.Y + 4 + this.scroll.Y)
        canvas.Brush.Color = oldColor
    }
    
    drawArrowToRectEdge(canvas: TCanvas, origin: TPoint, destRect: TRect): void {
        // add some to prevent cutting off arrow heads in certain cases for long words
        InflateRect(destRect, arrowwidth, arrowwidth)
        const intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)
        const endPoint = new TPoint(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)
        const startPoint = new TPoint(origin.X + this.scroll.X, origin.Y + this.scroll.Y)
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
        //arrow head
        this.drawArrowhead(canvas, startPoint, endPoint)
    }
    */
    
    drawArrowFromRectEdgeToRectEdge(context: CanvasRenderingContext2D, originRect: TRect, destRect: TRect): void {
        //theRect: TRect;
        //IntersectRect(theRect, originRect, destRect);
        //if not IsEmptyRect(theRect) then exit;

        // TODO: InflateRect modifies the original rect, which we csan get away with as it is bounds copy, but worriesome

        // add some to prevent cutting off arrow heads in certain cases for long words
        InflateRect(destRect, arrowwidth, arrowwidth)
        InflateRect(originRect, arrowwidth, arrowwidth)

        // Draw from the middle of one rect to the mdidle of the other

        const origin = new TPoint((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2)

        const dest = new TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)

        const intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)

        const endPoint = new TPoint(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)

        const startPoint = IntersectionPointForLineAndRectangle(dest, originRect)
        startPoint.X = startPoint.X + this.scroll.X
        startPoint.Y = startPoint.Y + this.scroll.Y

        // clip arrow if it would end up being drawn incorrectly
        // TODO: scrolledOriginRect is either unneeeded or really has to make a copy
        const scrolledOriginRect = originRect
        OffsetRect(scrolledOriginRect, this.scroll.X, this.scroll.Y)
        if (scrolledOriginRect.contains(endPoint)) {
            return
        }

        // TODO: use or remove: canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        context.beginPath()
        context.moveTo(startPoint.X, startPoint.Y)
        context.lineTo(endPoint.X, endPoint.Y)
        context.stroke()

        //arrow head
        this.drawArrowhead(context, startPoint, endPoint)
    }
    
    /*
    drawLineFromRectEdgeToRectEdge(canvas: TCanvas, originRect: TRect, destRect: TRect): void {
        let startPoint: TPoint
        let endPoint: TPoint
        let intersectPoint: TPoint
        let origin: TPoint
        let dest: TPoint
        
        // add some to prevent cutting off arrow heads in certain cases for long words
        InflateRect(destRect, arrowwidth, arrowwidth)
        InflateRect(originRect, arrowwidth, arrowwidth)
        origin = new TPoint((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2)
        dest = new TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
        intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect)
        endPoint = new TPoint(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y)
        startPoint = IntersectionPointForLineAndRectangle(dest, originRect)
        startPoint.X = startPoint.X + this.scroll.X
        startPoint.Y = startPoint.Y + this.scroll.Y
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        canvas.MoveTo(startPoint.X, startPoint.Y)
        canvas.LineTo(endPoint.X, endPoint.Y)
    }
    */

    drawArrowhead(context: CanvasRenderingContext2D, p1: TPoint, p2: TPoint): void {
        //Code translated from C++ posted:
        //	Subject:      Re: calculation for drawing arrow heads
        //	From:         "Jesper Hansen" <jesperh@edit.se>
        //	Date:         1998/01/27
        //	Message-ID:   <01bd2b58$1d33eaa0$65656565@foo.telia.com>
        //	Newsgroups:   comp.os.ms-windows.programmer.graphics 
        // given line from P1 to P2
        // draws arrowhead at P2
        const dx = p1.X - p2.X
        const dy = p1.Y - p2.Y
        let linelen = Math.sqrt(dx * dx + dy * dy)
        if (linelen === 0) {
            linelen = 1
        }
        const xstep = dx / linelen
        const ystep = dy / linelen

        let x1: int
        let y1: int
        let x2: int
        let y2: int
        try {
            // modify according to preference
            // relationship comes from tan(angle)
            // relation 10:5 is 45 degrees (2*22.5)
            //
            x2 = p2.X + intround(xstep * arrowlength)
            y2 = p2.Y + intround(ystep * arrowlength)
            y1 = -intround(xstep * arrowwidth)
            x1 = intround(ystep * arrowwidth)
        } catch (e) {
            // TODO: What can cause an exception here?
            console.log("numerical exception in drawArrowhead", e) 
            return
        }

        const outerOne = new TPoint(x2 + x1, y2 + y1)
        const outerTwo = new TPoint(x2 - x1, y2 - y1)

        // TODO use or remove:
        // canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid

        context.beginPath()
        context.moveTo(p2.X, p2.Y)
        context.lineTo(outerOne.X, outerOne.Y)
        context.lineTo(outerTwo.X, outerTwo.Y)
        context.closePath()
        context.fill()

        //canvas.moveTo(p2.x, p2.y);
        //  canvas.LineTo(outerOne.x, outerOne.y);
        //  canvas.moveTo(p2.x, p2.y);
        //  canvas.LineTo(outerTwo.x, outerTwo.y); 
    }

    /*
    
    // procedure drawArrow(canvas: TCanvas; a, b: TPoint);
    drawArrowToRect(canvas: TCanvas, origin: TPoint, destRect: TRect): void {
        let center: TPoint
        let endPoint: TPoint
        let startPoint: TPoint
        
        // add some to prevent cutting off arrow heads in certain cases for long words
        InflateRect(destRect, arrowwidth, arrowwidth)
        center = new TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2)
        //middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y); 
        //middlePoint := Point((origin.x + center.x) div 2 + scroll.x, (origin.y + center.y) div 2 + scroll.y);  
        endPoint = new TPoint(center.X + (origin.X - center.X) / 5 + this.scroll.X, center.Y + (origin.Y - center.Y) / 5 + this.scroll.Y)
        //startPoint := Point(origin.x + (center.x - origin.x) div 5 + scroll.x, origin.y + (center.y - origin.y) div 5 + scroll.y);
        //  
        startPoint = new TPoint(origin.X + this.scroll.X, origin.Y + this.scroll.Y)
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
    
    nearestNode(location: TPoint, displayOptions: TSVariableDisplayOptions): TSDraggableObject | null { 
        //nearestNode: TSDraggableObject;
        //distance, nearestDistance: integer;
        const world: TWorld = domain.world
        
        let result: TSDraggableObject | null = null
        for (let i = world.variables.length - 1; i >= 0; i--) {
            //nearestDistance := -1;
            //nearestNode := nil;
            const variable: TSVariable = world.variables[i]
            const showNode = variable.meetsDisplayOptions(displayOptions)
            if (!showNode) {
                continue
            }
            if (variable.bounds().contains(location)) {
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
        if (!displayOptions[TSRuleField.kRuleCommand]) {
            //result := nearestNode;
            return result
        }
        for (let i = world.rules.length - 1; i >= 0; i--) {
            const rule: TSRule = world.rules[i]
            if (rule.bounds().contains(location)) {
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
        let textSize: TSize
        let oldColor: TColor

        const world: TWorld = domain.world
        
        // calculate bounds for text boxes
        UNRESOLVED.SetTextAlign(canvas.Handle, delphi_compatability.TA_LEFT || delphi_compatability.TA_TOP)
        canvas.Pen.Color = delphi_compatability.clBlack
        for (let i = 0; i < world.rules.length; i++) {
            // need to compute these first - because they are referenced
            // could be optimized out - only do if change text...
            const rule: TSRule = world.rules[i]
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
        for (let i = 0; i < world.variables.length; i++) {
            const variable: TSVariable = world.variables[i]
            if (variable.meetsDisplayOptions(displayOptions)) {
                textSize = canvas.TextExtent(variable.displayName())
                variable.extent.X = textSize.cx
                variable.extent.Y = textSize.cy
            }
        }
        oldColor = canvas.Brush.Color
        // draw lines and arrows
        canvas.Brush.Color = delphi_compatability.clBlack
        for (let i = 0; i < world.rules.length; i++) {
            const rule: TSRule = world.rules[i]
            if (displayOptions[TSRuleField.kRuleCommand]) {
                if (rule.context !== world.emptyEntry) {
                    this.drawLineFromRectEdgeToRectEdge(canvas, rule.context.bounds(), rule.bounds())
                }
                if (rule.move !== world.emptyEntry) {
                    this.drawArrowFromRectEdgeToRectEdge(canvas, rule.bounds(), rule.move.bounds())
                }
            } else {
                if ((rule.context !== world.emptyEntry) && (rule.move !== world.emptyEntry)) {
                    this.drawArrowFromRectEdgeToRectEdge(canvas, rule.context.bounds(), rule.move.bounds())
                }
            }
        }
        canvas.Brush.Color = oldColor
        if (displayOptions[TSRuleField.kRuleCommand]) {
            for (let i = 0; i < world.rules.length; i++) {
                // draw rectangles and text
                const rule: TSRule = world.rules[i]
                this.drawCommandOrContext(canvas, rule.displayName(), rule.bounds(), rule.position, rule.selected, rule === usruleeditorform.RuleEditorForm.rule, kDrawCommand)
            }
        }
        for (let i = 0; i < world.variables.length; i++) {
            const variable: TSVariable = world.variables[i]
            if (variable.meetsDisplayOptions(displayOptions)) {
                this.drawCommandOrContext(canvas, variable.displayName(), variable.bounds(), variable.position, variable.selected, false, kDrawContext)
            }
        }
    }
    
    */

    // TODO: psotion parameter is unused -- remove it
    drawCommandOrContext(context: CanvasRenderingContext2D, text: string, bounds: TRect, position: TPoint, selected: boolean, focused: boolean, isCommand: boolean): void {
        let drawRect: TRect = new TRect()  
        drawRect.Left = bounds.Left - 2 + this.scroll.X
        drawRect.Top = bounds.Top - 1 + this.scroll.Y
        drawRect.Right = bounds.Right + 2 + this.scroll.X
        drawRect.Bottom = bounds.Bottom + 1 + this.scroll.Y

        const textPoint = new TPoint(bounds.Left + this.scroll.X, bounds.Top + this.scroll.Y)

        /* TODO: Use or remove
        usruleeditorform.setCanvasColorsForSelection(canvas, selected, focused, isCommand)
        if (selected) {
            canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
        } else {
            canvas.Pen.Style = delphi_compatability.TFPPenStyle.psClear
        }
        */

        this.drawRect(context, drawRect)
        context.fillText(text, textPoint.X, textPoint.Y)
    }


    drawRect(context: CanvasRenderingContext2D, rect: TRect) {
        context.beginPath()
        context.moveTo(rect.Left, rect.Top)
        context.lineTo(rect.Left, rect.Bottom)
        context.lineTo(rect.Right, rect.Bottom)
        context.lineTo(rect.Right, rect.Top)
        context.closePath()
        context.stroke()
    }
}
