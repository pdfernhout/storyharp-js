import * as m from "mithril"
import { TSRuleField } from "./TSRule"
import { TSMapView } from "./TSMapView"
import { TRect } from "./TRect";
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"

export class RuleMapView {
    domain: any
    canvas: HTMLCanvasElement
    mapDrawer = new TSMapView()
    isDragging = false
    lastMouseLocation = new TPoint(0, 0)

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    /*
    function setCanvasColorsForSelection(canvas: TCanvas, selected: boolean, focused: boolean, isCommandInMap: boolean): void {
        canvas.Brush.Color = delphi_compatability.clWindow
        canvas.Font.Color = UNRESOLVED.clWindowText
        canvas.Font.Style = {}
        if (focused && selected) {
            canvas.Brush.Color = usdomain.domain.options.selectedItemColor
            canvas.Font.Color = usdomain.domain.options.selectedTextColor
            canvas.Font.Style = {UNRESOLVED.fsBold, }
        } else if (focused) {
            canvas.Font.Style = {UNRESOLVED.fsBold, }
        } else if (selected) {
            canvas.Brush.Color = usdomain.domain.options.selectedItemColor
            canvas.Font.Color = usdomain.domain.options.selectedTextColor
        }
        if (isCommandInMap) {
            canvas.Font.Color = usdomain.domain.options.commandTextColorInMap
        }
    }    

    currentGraphView(): TSMapView {
        let result = new TSMapView()
        result = usdomain.domain.mapView
        return result
    }
    
    mapChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        this.MapPaintBoxChanged()
    }
    
    MapPaintBoxChanged(): void {
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        
        if (this.ListPages.ActivePage !== this.TabSheetMap) {
            return
        }
        mapView = this.currentGraphView()
        if (mapView === null) {
            return
        }
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        // clBtnFace;
        this.MapImage.Picture.Bitmap.Canvas.Brush.Color = delphi_compatability.clWhite
        this.MapImage.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, this.MapImage.Picture.Bitmap.Width, this.MapImage.Picture.Bitmap.Height))
        mapView.scroll = Point(-this.MapScrollBarHorizontal.Position, -this.MapScrollBarVertical.Position)
        mapView.displayOn(this.MapImage.Picture.Bitmap.Canvas, displayOptions, this.lastChoice, this.previousChoice)
        this.MapImage.Invalidate()
    }
    
    ListPagesDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    MapListChange(Sender: TObject): void {
        this.lastChoice = null
        this.previousChoice = null
        this.MapPaintBoxChanged()
        this.adjustScrollBars()
    }
    
    lastChoiceText(): string {
        let result = ""
        result = ""
        if (this.lastChoice === null) {
            return result
        }
        if (this.lastChoice instanceof usworld.TSVariable) {
            result = this.lastChoice.displayName()
            return result
        }
        result = (this.lastChoice).command.phrase
        return result
    }
    
    makeChoice(choice: TSDraggableObject, multiSelect: boolean): boolean {
        let result = false
        //whether must redraw
        result = false
        if (multiSelect) {
            if (choice !== null) {
                choice.selected = !choice.selected
            }
            result = true
        } else {
            if ((choice === null) || !choice.selected) {
                result = usdomain.domain.world.deselectAllExcept(choice)
                if (choice !== null) {
                    choice.selected = true
                }
            } else {
                // do nothing except maybe drag...
            }
        }
        if (this.lastChoice === choice) {
            return result
        }
        result = true
        if ((choice !== null) && choice.selected) {
            this.previousChoice = this.lastChoice
            this.lastChoice = choice
            if (this.previousChoice instanceof usworld.TSRule) {
                this.previousChoice = choice
            }
            if (this.lastChoice instanceof usworld.TSRule) {
                this.previousChoice = choice
            }
        } else if ((choice !== null) && !choice.selected) {
            if (this.previousChoice === choice) {
                this.previousChoice = null
            }
            if (this.lastChoice === choice) {
                this.lastChoice = null
            }
        }
        return result
    }
    
    XorRect(canvas: TCanvas, rect: TRect): void {
        let oldMode: TPenMode
        
        oldMode = canvas.Pen.Mode
        canvas.Brush.Color = delphi_compatability.clNone
        canvas.Pen.Mode = delphi_compatability.TFPPenMode.pmXor
        canvas.Pen.Color = delphi_compatability.clWhite
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psDot
        //FIX unresolved WITH expression: Rect
        //canvas.Pen.width := 2;
        canvas.Rectangle(this.Left, this.Top, UNRESOLVED.right, UNRESOLVED.bottom)
        //canvas.Pen.width := 1;
        canvas.Pen.Mode = oldMode
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
    }
    
    MapImageMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let newCommand: TSMapDragCommand
        let draggedNode: TSDraggableObject
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        let multipleSelect: boolean
        let showString: string
        let textSize: TPoint
        let centerPosition: TPoint
        
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.commitChangesToRule()
        this.lastMapMouseDownPosition = Point(X + this.MapScrollBarHorizontal.Position, Y + this.MapScrollBarVertical.Position)
        mapView = this.currentGraphView()
        if (mapView === null) {
            return
        }
        this.FocusControl(this.PanelMap)
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        draggedNode = mapView.nearestNode(Point(X + this.MapScrollBarHorizontal.Position, Y + this.MapScrollBarVertical.Position), displayOptions)
        if (Button === delphi_compatability.TMouseButton.mbRight) {
            if (draggedNode !== null) {
                showString = draggedNode.displayName()
                centerPosition = Point(draggedNode.center().X - this.MapScrollBarHorizontal.Position, draggedNode.center().Y - this.MapScrollBarVertical.Position)
            } else {
                showString = "new item"
                centerPosition = Point(X, Y)
            }
            this.MapImage.Canvas.Brush.Color = delphi_compatability.clAqua
            this.MapImage.Canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
            this.MapImage.Canvas.Font.Style = {UNRESOLVED.fsBold, }
            textSize = Point(this.MapImage.Canvas.TextWidth(showString), this.MapImage.Canvas.TextHeight("W"))
            this.MapImage.Canvas.Rectangle(centerPosition.X - textSize.X / 2 - 2, centerPosition.Y - textSize.Y / 2 - 2, centerPosition.X + textSize.X / 2 + 2, centerPosition.Y + textSize.Y / 2 + 2)
            this.MapImage.Canvas.TextOut(centerPosition.X - textSize.X / 2, centerPosition.Y - textSize.Y / 2, showString)
            this.MapImage.Canvas.Font.Style = {}
        }
        if (Button !== delphi_compatability.TMouseButton.mbLeft) {
            return
        }
        multipleSelect = (delphi_compatability.TShiftStateEnum.ssShift in Shift)
        this.mapSelectionInProgress = false
        if (draggedNode === null) {
            this.makeChoice(null, multipleSelect)
            this.mapSelectionRect = Rect(X, Y, X, Y)
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = true
            return
        }
        //MapPaintBoxChanged;
        this.makeChoice(draggedNode, multipleSelect)
        if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
            this.MapPaintBoxChanged()
            this.MapImage.BeginDrag(true)
            return
        }
        if (!multipleSelect) {
            this.MapImageDblClick(Sender)
        }
        this.MapPaintBoxChanged()
        // finds selected nodes in domain
        newCommand = uscommands.TSMapDragCommand().create()
        newCommand.notifyProcedure = this.mapChangedNotification
        this.actionInProgress = usdomain.domain.worldCommandList.mouseDown(newCommand, Point(X, Y))
    }
    
    MapImageMouseMove(Sender: TObject, Shift: TShiftState, X: int, Y: int): void {
        if (this.actionInProgress) {
            usdomain.domain.worldCommandList.mouseMove(Point(X, Y))
        } else if (this.mapSelectionInProgress) {
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionRect.Right = X
            this.mapSelectionRect.Bottom = Y
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
        }
    }
    
    MapImageMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        if (this.actionInProgress) {
            usdomain.domain.worldCommandList.mouseUp(Point(X, Y))
            this.actionInProgress = false
            this.adjustScrollBars()
        } else if (this.mapSelectionInProgress) {
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = false
            if (!(delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
                usdomain.domain.world.deselectAllExcept(null)
            }
            this.mapSelectionRect = Rect(this.mapSelectionRect.Left + this.MapScrollBarHorizontal.Position, this.mapSelectionRect.Top + this.MapScrollBarVertical.Position, this.mapSelectionRect.Right + this.MapScrollBarHorizontal.Position, this.mapSelectionRect.Bottom + this.MapScrollBarVertical.Position)
            usdomain.domain.world.selectInRectangle(this.mapSelectionRect)
            this.MapPaintBoxChanged()
        }
    }
    
    MapImageDblClick(Sender: TObject): void {
        if (this.lastChoice === null) {
            //var
            //row: integer;
            //count: integer;
            //rule: TSRule;
            //ruleIndex: integer; 
            return
        }
        if (this.lastChoice instanceof usworld.TSRule) {
            this.editRule(this.lastChoice)
        }
        //
        //  else
        //    begin
        //    count := 1;
        //    ruleIndex := domain.world.rules.indexOf(self.rule);
        //    while (count <= domain.world.rules.count) do
        //      begin
        //      row := (count + ruleIndex) mod domain.world.rules.count;
        //      rule := domain.world.rules[row];
        //      // unfinished - need to check requirements & changes
        //      if (rule.context.phrase = self.lastChoiceText) or
        //      		(rule.command.phrase = self.lastChoiceText) or
        //          (rule.move.phrase = self.lastChoiceText) then
        //        begin
        //      	self.editRule(rule);
        //        exit;
        //        end;
        //      inc(count);
        //      end;
        //    end;
        //    
    }
    
    searchForAndSelectRule(aText: string, ignoreCase: boolean, goDown: boolean): void {
        let row: int
        let count: int
        let rule: TSRule
        let ruleIndex: int
        let match: boolean
        let matchText: string
        
        count = 1
        ruleIndex = usdomain.domain.world.rules.IndexOf(this.rule)
        if (ignoreCase) {
            matchText = lowercase(aText)
        } else {
            matchText = aText
        }
        while ((count <= usdomain.domain.world.rules.Count)) {
            if (goDown) {
                row = (ruleIndex + count) % usdomain.domain.world.rules.Count
            } else {
                row = ((usdomain.domain.world.rules.Count * 2) + (ruleIndex - count)) % usdomain.domain.world.rules.Count
            }
            rule = usdomain.domain.world.rules[row]
            if (ignoreCase) {
                // unfinished - need to check requirements & changes
                match = (UNRESOLVED.pos(matchText, lowercase(rule.context.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.command.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.reply)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.move.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.requirementsString)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.changesString)) > 0)
            } else {
                match = (UNRESOLVED.pos(matchText, rule.context.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.command.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.reply) > 0) || (UNRESOLVED.pos(matchText, rule.move.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.requirementsString) > 0) || (UNRESOLVED.pos(matchText, rule.changesString) > 0)
            }
            if (match) {
                usdomain.domain.world.deselectAllExcept(rule)
                this.editRule(rule)
                this.updateForRuleChange()
                rule.selected = true
                this.scrollGridSelectionsIntoView(kFromBottom)
                this.MapPaintBoxChanged()
                this.scrollMapSelectionIntoView()
                return
            }
            count += 1
        }
        ShowMessage("Search string \"" + aText + "\" not found.")
    }
    
    // accomodates growth
    // accomodates growth
    adjustScrollBars(): void {
        let top: int
        let bottom: int
        let left: int
        let right: int
        let graphView: TSMapView
        let xPosition: int
        let yPosition: int
        let mapBoundsRect: TRect
        
        graphView = this.currentGraphView()
        if (graphView !== null) {
            mapBoundsRect = usdomain.domain.world.boundsRect()
            top = mapBoundsRect.Top
            left = mapBoundsRect.Left
            bottom = mapBoundsRect.Bottom
            right = mapBoundsRect.Right
            if (bottom < this.MapImage.Height) {
                bottom = this.MapImage.Height
            }
            if (right < this.MapImage.Width) {
                right = this.MapImage.Width
            }
        } else {
            top = 0
            bottom = this.MapImage.Height
            left = 0
            right = this.MapImage.Width
        }
        left = left - kLeftRightBorderSize
        right = right + kLeftRightBorderSize
        top = top - kTopBottomBorderSize
        bottom = bottom + kTopBottomBorderSize
        right = right - this.MapImage.Width
        bottom = bottom - this.MapImage.Height
        xPosition = this.MapScrollBarHorizontal.Position
        yPosition = this.MapScrollBarVertical.Position
        if (xPosition < left) {
            //if xPosition < left then xPosition := left;
            //  if xPosition > right then xPosition := right;
            //  if yPosition < top then yPosition := top;
            //  if yPosition > bottom then yPosition := bottom;
            left = xPosition
        }
        if (xPosition > right) {
            right = xPosition
        }
        if (yPosition < top) {
            top = yPosition
        }
        if (yPosition > bottom) {
            bottom = yPosition
        }
        this.MapScrollBarHorizontal.SetParams(xPosition, left, right)
        this.MapScrollBarHorizontal.LargeChange = this.MapImage.Width
        //LocalIntMax(1, LocalIntMin((right - left) div 10, MapPaintBox.width));
        this.MapScrollBarVertical.SetParams(yPosition, top, bottom)
        this.MapScrollBarVertical.LargeChange = this.MapImage.Height
        //LocalIntMax(1, LocalIntMin((bottom - top) div 10, MapPaintBox.height));
    }
    
    goodPosition(): TPoint {
        let result = new TPoint()
        if (this.lastChoice !== null) {
            if (this.previousChoice !== null) {
                //var
                //  	mapBoundsRect: TRect;
                //    selection: TSDraggableObject; 
                result = Point((this.previousChoice.position.X + this.lastChoice.position.X) / 2, (this.previousChoice.position.Y + this.lastChoice.position.Y) / 2 + 30)
            } else {
                result = Point(this.lastChoice.position.X, this.lastChoice.position.Y + 30)
            }
        } else {
            // mapBoundsRect := domain.world.boundsRect;
            //    result.x := (mapBoundsRect.left - mapBoundsRect.right) div 2;
            //    result.y := mapBoundsRect.bottom + 30;  
            result = Point(this.MapScrollBarHorizontal.Position + this.MapImage.Width / 2, this.MapScrollBarVertical.Position + this.MapImage.Height / 2)
        }
        result.X = result.X + UNRESOLVED.random(200) - 100
        result.Y = result.Y + UNRESOLVED.random(200) - 100
        //if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject;
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x;
        //      result.y := selection.position.y + 30;
        //      end;
        //    end;
        //  result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
        //  //result.x := result.x + random(200) - 100;
        //  //result.y := result.y + random(200) - 100;
        //result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
        //  if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject;
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x;
        //      result.y := selection.position.y;
        //      end;
        //    end;
        //  result.x := result.x + random(200) - 100;
        //  result.y := result.y + random(200) - 100;   
        return result
    }
    */

    view() {
        const world: TWorld = this.domain.world

        const drawWorld = () => {
            const canvas = this.canvas
            // Canvas is cleared by resizing in update
            this.canvas.width = this.canvas.scrollWidth
            this.canvas.height = this.canvas.scrollHeight

            const context = canvas.getContext("2d")
            if (!context) return
            const displayOptions = []
            displayOptions[TSRuleField.kRuleContext] = true
            displayOptions[TSRuleField.kRuleCommand] = true
            context.setLineDash([]);
            context.lineDashOffset = 0
            this.mapDrawer.displayOn(context, displayOptions, null, null, world, null)
            context.setLineDash([4, 16]);
            context.lineDashOffset = 2;
            this.mapDrawer.drawRect(context, world.boundsRect(), true)
        }

        return m(".RuleMapView.h-100.w-100.overflow-hidden",
            m("canvas.ba.h-100.w-100.overflow-hidden", {
                oncreate: (vnode: m.VnodeDOM) => {       
                    this.canvas = <HTMLCanvasElement>vnode.dom
                    drawWorld()
                },
                onupdate: (vnode: m.VnodeDOM) => {
                    drawWorld()
                },
                onmousedown: (event: MouseEvent) => {
                    this.isDragging = true
                    ;(<any>event).redraw = false
                    this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    return false
                },
                onmousemove: (event: MouseEvent) => {
                    if (this.isDragging) {
                        this.mapDrawer.scroll.X += (event.offsetX - this.lastMouseLocation.X)
                        this.mapDrawer.scroll.Y += (event.offsetY - this.lastMouseLocation.Y)
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else {
                        (<any>event).redraw = false
                    }
                },
                onmouseup: (event: MouseEvent) => {
                    this.isDragging = false
                    ;(<any>event).redraw = false
                },
            }),
        )
    }
}
