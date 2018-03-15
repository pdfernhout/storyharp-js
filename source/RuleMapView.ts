import * as m from "mithril"
import { TSRule, TSRuleField } from "./TSRule"
import { TSMapView, defaultScale } from "./TSMapView"
import { TRect } from "./TRect"
import { TPoint } from "./TPoint"
import { TWorld } from "./TWorld"
import { TSDraggableObject } from "./TSDraggableObject"
import { TSCommandList } from "./TSCommandList"
import { TSMapDragCommand } from "./TSMapDragCommand"
import { KfCommand, KfCommandChangeType } from "./KfCommand"
import { TSVariableDisplayOptions, TSVariable } from "./TSVariable"
import { TSDomain } from "./TSDomain"

export class RuleMapView {
    domain: TSDomain
    world: TWorld
    worldCommandList: TSCommandList
    
    canvas: HTMLCanvasElement
    mapDrawer: TSMapView

    isDragging = false
    lastMouseLocation = new TPoint(0, 0)

    get previousChoice(): TSDraggableObject | null {
        return this.domain.ruleEditorForm.previousChoice
    }

    set previousChoice(value: TSDraggableObject | null) {
        this.domain.ruleEditorForm.previousChoice = value
    }

    get lastChoice(): TSDraggableObject | null {
        return this.domain.ruleEditorForm.lastChoice
    }

    set lastChoice(value: TSDraggableObject | null) {
        this.domain.ruleEditorForm.lastChoice = value
    }

    actionInProgress: boolean = false
    mapSelectionInProgress: boolean = false

    mapSelectionRect: TRect = new TRect()
    lastMapMouseDownPosition: TPoint = new TPoint()

    constructor(vnode: m.Vnode) {
        const domain: TSDomain = (<any>vnode.attrs).domain
        this.domain = domain
        this.world = domain.world
        this.worldCommandList = domain.worldCommandList
        this.mapDrawer = new TSMapView(domain.mapViewState)
    }

    /* TODO: use or remove
    
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

    */

    mapChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        this.MapPaintBoxChanged()
    }

    /* TODO: use or remove
    
    MapPaintBoxChanged(): void {
        let displayOptions: TSVariableDisplayOptions
        let i: int
        
        if (this.ListPages.ActivePage !== this.TabSheetMap) {
            return
        }
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        // clBtnFace
        this.MapImage.Picture.Bitmap.Canvas.Brush.Color = delphi_compatability.clWhite
        this.MapImage.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, this.MapImage.Picture.Bitmap.Width, this.MapImage.Picture.Bitmap.Height))
        this.mapDrawer.scroll = new TPoint(-this.mapDrawer.scroll.X, -this.mapDrawer.scroll.Y)
        this.mapDrawer.displayOn(this.MapImage.Picture.Bitmap.Canvas, displayOptions, this.lastChoice, this.previousChoice)
    }
    
    ListPagesDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    MapListChange(Sender: TObject): void {
        this.lastChoice = null
        this.previousChoice = null
        this.MapPaintBoxChanged()
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
    
    */

    makeChoice(choice: TSDraggableObject | null, multiSelect: boolean): boolean {
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
                result = this.world.deselectAllExcept(choice)
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
            if (this.previousChoice instanceof TSRule) {
                this.previousChoice = choice
            }
            if (this.lastChoice instanceof TSRule) {
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

    /*
    
    XorRect(canvas: TCanvas, rect: TRect): void {
        let oldMode: TPenMode
        
        oldMode = canvas.Pen.Mode
        canvas.Brush.Color = delphi_compatability.clNone
        canvas.Pen.Mode = delphi_compatability.TFPPenMode.pmXor
        canvas.Pen.Color = delphi_compatability.clWhite
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psDot
        //FIX unresolved WITH expression: Rect
        //canvas.Pen.width := 2
        canvas.Rectangle(this.Left, this.Top, UNRESOLVED.right, UNRESOLVED.bottom)
        //canvas.Pen.width := 1
        canvas.Pen.Mode = oldMode
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
    }

    */
    
    MapImageMouseDown(event: MouseEvent): void {
        this.lastMapMouseDownPosition = new TPoint(event.offsetX - this.mapDrawer.scroll.X, event.offsetY - this.mapDrawer.scroll.Y)

        // TODO: use or remove: this.FocusControl(this.PanelMap)

        const displayOptions: TSVariableDisplayOptions = []
        for (let i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[TSRuleField.kRuleContext] = true
        displayOptions[TSRuleField.kRuleMove] = true
        // TODO replace below with: displayOptions[TSRuleField.kRuleCommand] = this.MenuMapsShowCommands.checked
        displayOptions[TSRuleField.kRuleCommand] = true

        const draggedNode: TSDraggableObject | null = this.mapDrawer.nearestNode(new TPoint(
            event.offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X,
            event.offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y
        ), displayOptions, this.world)

        /* TODO: use or remove -- for making a new item
        if (Button === delphi_compatability.TMouseButton.mbRight) {
            let showString: string
            let centerPosition: TPoint
            if (draggedNode !== null) {
                showString = draggedNode.displayName()
                centerPosition = new TPoint(draggedNode.center().X - this.mapDrawer.scroll.X, draggedNode.center().Y - this.mapDrawer.scroll.Y)
            } else {
                showString = "new item"
                centerPosition = new TPoint(event.offsetX, event.offsetY)
            }
            this.MapImage.Canvas.Brush.Color = delphi_compatability.clAqua
            this.MapImage.Canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
            this.MapImage.Canvas.Font.Style = {UNRESOLVED.fsBold, }
            const textSize = new TPoint(this.MapImage.Canvas.TextWidth(showString), this.MapImage.Canvas.TextHeight("W"))
            this.MapImage.Canvas.Rectangle(
                centerPosition.X - textSize.X / 2 - 2,
                centerPosition.Y - textSize.Y / 2 - 2,
                centerPosition.X + textSize.X / 2 + 2,
                centerPosition.Y + textSize.Y / 2 + 2
            )
            this.MapImage.Canvas.TextOut(centerPosition.X - textSize.X / 2, centerPosition.Y - textSize.Y / 2, showString)
            this.MapImage.Canvas.Font.Style = {}
        }
        if (Button !== delphi_compatability.TMouseButton.mbLeft) {
            return
        }
        */

        const multipleSelect: boolean = event.ctrlKey
        this.mapSelectionInProgress = false
        if (draggedNode === null) {
            this.makeChoice(null, multipleSelect)
            this.mapSelectionRect = new TRect(event.offsetX, event.offsetY, event.offsetX, event.offsetY)
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = true
            return
        }
        //MapPaintBoxChanged
        this.makeChoice(draggedNode, multipleSelect)
        
        // TODO: Remove this or maybe make shift into drag scrolling?
        if (event.shiftKey) {
            this.MapPaintBoxChanged()
            // TODO use or remove: this.MapImage.BeginDrag(true)
            return
        }
        
        // TODO: MapImageDblClick seems badly named at this point
        if (!multipleSelect) {
            this.MapImageDblClick()
        }

        this.MapPaintBoxChanged()
        // finds selected nodes in domain
        const newCommand = new TSMapDragCommand(this.domain, 1 / this.mapDrawer.scale)
        // TODO: This notification may be unneeded -- check after converted design working
        newCommand.notifyProcedure = this.mapChangedNotification.bind(this)
        this.actionInProgress = this.worldCommandList.mouseDown(newCommand, new TPoint(event.offsetX, event.offsetY))
    }
    
    MapImageMouseMove(event: MouseEvent): void {
        if (this.actionInProgress) {
            this.worldCommandList.mouseMove(new TPoint(event.offsetX, event.offsetY))
        } else if (this.mapSelectionInProgress) {
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionRect.Right = event.offsetX
            this.mapSelectionRect.Bottom = event.offsetY
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
        } else {
            (<any>event).redraw = false
        }
    }
    
    MapImageMouseUp(event: MouseEvent): void {
        if (this.actionInProgress) {
            this.worldCommandList.mouseUp(new TPoint(event.offsetX, event.offsetY))
            this.actionInProgress = false
        } else if (this.mapSelectionInProgress) {
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = false
            if (!(event.ctrlKey)) {
                this.world.deselectAllExcept(null)
            }
            this.mapSelectionRect = new TRect(
                this.mapSelectionRect.Left / this.mapDrawer.scale - this.mapDrawer.scroll.X,
                this.mapSelectionRect.Top / this.mapDrawer.scale - this.mapDrawer.scroll.Y,
                this.mapSelectionRect.Right / this.mapDrawer.scale - this.mapDrawer.scroll.X,
                this.mapSelectionRect.Bottom / this.mapDrawer.scale - this.mapDrawer.scroll.Y
            )
            this.world.selectInRectangle(this.mapSelectionRect)
            this.MapPaintBoxChanged()
        } else {
            (<any>event).redraw = false
        }
    }

    MapPaintBoxChanged(): void {
        // TODO: placeholder for now; may be removed
    }

    // TODO: MapImageDblClick seems badly named at this point
    MapImageDblClick(): void {
        if (this.lastChoice === null) {
            //var
            //row: integer
            //count: integer
            //rule: TSRule
            //ruleIndex: integer; 
            return
        }
        if (this.lastChoice instanceof TSRule) {
            this.domain.editRule(this.lastChoice)
        }
        //
        //  else
        //    begin
        //    count := 1
        //    ruleIndex := domain.world.rules.indexOf(self.rule)
        //    while (count <= domain.world.rules.count) do
        //      begin
        //      row := (count + ruleIndex) mod domain.world.rules.count
        //      rule := domain.world.rules[row]
        //      // unfinished - need to check requirements & changes
        //      if (rule.context.phrase = self.lastChoiceText) or
        //      		(rule.command.phrase = self.lastChoiceText) or
        //          (rule.move.phrase = self.lastChoiceText) then
        //        begin
        //      	self.editRule(rule)
        //        exit
        //        end
        //      inc(count)
        //      end
        //    end
        //    
    }

    scrollMapSelectionIntoView(): void {
        const world = this.domain.world
        const mapDrawer = this.mapDrawer
        const canvas = this.canvas

        let visibleRect: TRect = new TRect()
    
        let upperLeftObject: TSDraggableObject | null = null
        visibleRect.Left = -mapDrawer.scroll.X
        visibleRect.Top = -mapDrawer.scroll.Y
        visibleRect.Right = visibleRect.Left + canvas.width / mapDrawer.scale
        visibleRect.Bottom = visibleRect.Top + canvas.height / mapDrawer.scale
        for (let i = 0; i < world.rules.length; i++) {
            const rule: TSRule = world.rules[i]
            if (rule.selected) {
                if (rule.bounds().intersects(visibleRect)) {
                    return
                }
                if (upperLeftObject === null) {
                    upperLeftObject = rule
                } else if (upperLeftObject.bounds().Top > rule.bounds().Top) {
                    upperLeftObject = rule
                } else if (upperLeftObject.bounds().Left > rule.bounds().Left) {
                    upperLeftObject = rule
                }
            }
        }
        let firstContextVariable: TSVariable | null = null
        for (let i = 0; i < world.variables.length; i++) {
            const variable: TSVariable = world.variables[i]
            if ((firstContextVariable === null) && (variable.hasUseagesForField(TSRuleField.kRuleContext))) {
                firstContextVariable = variable
            }
            if (variable.selected) {
                if (variable.bounds().intersects(visibleRect)) {
                    return
                }
                if (upperLeftObject === null) {
                    upperLeftObject = variable
                } else if (upperLeftObject.bounds().Top > variable.bounds().Top) {
                    upperLeftObject = variable
                } else if (upperLeftObject.bounds().Left > variable.bounds().Left) {
                    upperLeftObject = variable
                }
            }
        }
        if (upperLeftObject === null) {
            upperLeftObject = firstContextVariable
        }
        if (upperLeftObject === null) {
            return
        }
        mapDrawer.scroll.X = canvas.width / mapDrawer.scale / 2 - upperLeftObject.center().X
        mapDrawer.scroll.Y =  canvas.height / mapDrawer.scale / 2 - upperLeftObject.center().Y
    }

    view() {
        const world: TWorld = this.world

        const drawWorld = () => {
            const canvas = this.canvas

            // Canvas is cleared as side-effect by resizing in update
            this.canvas.width = this.canvas.scrollWidth
            this.canvas.height = this.canvas.scrollHeight

            // Keep viewportSize up-to-date as it is needed by goodPosition algorithm
            this.domain.mapViewState.viewportSize.X = this.canvas.scrollWidth
            this.domain.mapViewState.viewportSize.Y = this.canvas.height

            if (this.domain.pendingMapScroll) {
                this.scrollMapSelectionIntoView()
                this.domain.pendingMapScroll = false
            } 

            const context = canvas.getContext("2d")
            if (!context) return
            const displayOptions = []
            displayOptions[TSRuleField.kRuleContext] = true
            displayOptions[TSRuleField.kRuleCommand] = true
            context.setLineDash([])
            context.lineDashOffset = 0
            this.mapDrawer.displayOn(context, displayOptions, null, null, world, this.domain.editedRule, this.domain.showCommandPrefixInMap)
            context.strokeStyle = "rgb(0, 0, 0)"
            context.setLineDash([4, 16])
            context.lineDashOffset = 2
            this.mapDrawer.drawRect(context, world.boundsRect(), true)
            if (this.mapSelectionInProgress) {
                // TODO: Was XOR rectangle
                this.mapDrawer.drawRect(context, this.mapSelectionRect.scale(1 / this.mapDrawer.scale))
            }
            if (this.canvas === document.activeElement) {
                context.scale(defaultScale / this.mapDrawer.scale, defaultScale / this.mapDrawer.scale)
                context.textBaseline = "top"
                context.fillStyle = "gray"
                context.fillText("map controls: wasd arrows +- c r", 2, 2)
            }
        }

        return m(".RuleMapView.h-100.w-100.overflow-hidden",
            m("canvas.ba.h-100.w-100.overflow-hidden", {
                // set tabindex to make canvas focusable
                tabindex: 0,

                /* Experiment that does not work to show when canvas has focus via border change:
                style: {
                    "border-color": (this.canvas && this.canvas === document.activeElement)
                        ? "black"
                        : "white",
                    // "box-sizing": "border-box",
                },
                */

                oncreate: (vnode: m.VnodeDOM) => {       
                    this.canvas = <HTMLCanvasElement>vnode.dom
                    drawWorld()
                },

                onupdate: (vnode: m.VnodeDOM) => {
                    drawWorld()
                },

                onblur: () => {
                    drawWorld()
                },

                onmousedown: (event: MouseEvent) => {
                    this.canvas.focus()
                    /* TODO: Fix scrolling by dragging -- or remove
                    this.isDragging = true
                    ;(<any>event).redraw = false
                    this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    */
                    this.MapImageMouseDown(event)
                    return false
                },

                onmousemove: (event: MouseEvent) => {
                    /* TODO: Fix scrolling by dragging -- or remove
                    if (this.isDragging) {
                        this.mapDrawer.scroll.X += (event.offsetX - this.lastMouseLocation.X)
                        this.mapDrawer.scroll.Y += (event.offsetY - this.lastMouseLocation.Y)
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else {
                        (<any>event).redraw = false
                    }
                    */
                   this.MapImageMouseMove(event)
                },

                onmouseup: (event: MouseEvent) => {
                    /* TODO: Fix scrolling by dragging -- or remove
                    this.isDragging = false
                    ;(<any>event).redraw = false
                    */
                   this.MapImageMouseUp(event)
                },

                onmouseout: (event: MouseEvent) => {
                    this.MapImageMouseUp(event)
                },

                onkeydown: (event: KeyboardEvent) => {
                    // Support scrollinf map with arrow keys or WASD
                    const scrollDelta = 50
                    switch(event.keyCode) {
                    case 37: // left arrow
                    case 65: // a
                        this.mapDrawer.scroll.X += scrollDelta
                        break
                    case 38: // up arrow
                    case 87: // w
                        this.mapDrawer.scroll.Y += scrollDelta
                        break
                    case 39: // right arrow
                    case 68: // d
                        this.mapDrawer.scroll.X -= scrollDelta
                        break
                    case 40: // down arrow
                    case 83: // s
                        this.mapDrawer.scroll.Y -= scrollDelta
                        break
                    case 67: // c 
                        // center map
                        const boundsCenter = this.world.boundsRect().center()
                        this.mapDrawer.scroll.X = Math.round(this.canvas.width / 2 / this.mapDrawer.scale) - boundsCenter.X
                        this.mapDrawer.scroll.Y = Math.round(this.canvas.height / 2 / this.mapDrawer.scale) - boundsCenter.Y
                        break
                    case 187: // plus +
                        this.mapDrawer.scale = this.mapDrawer.scale * 1.2
                        break
                    case 189: // minus -
                        this.mapDrawer.scale = this.mapDrawer.scale / 1.2
                        break
                    case 82: // r to reset
                        this.mapDrawer.scroll.X = 0
                        this.mapDrawer.scroll.Y = 0
                        this.mapDrawer.scale = defaultScale
                        break
                    default:
                        (<any>event).redraw = false
                        break
                    }
                },

                onmousewheel: (event: MouseWheelEvent) => {
                    this.mapDrawer.scroll.X -= (event.deltaX / this.mapDrawer.scale)
                    this.mapDrawer.scroll.Y -= (event.deltaY / this.mapDrawer.scale)
                },
            }),
        )
    }
}
