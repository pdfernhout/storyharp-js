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
import { TSNewRulesCommand } from "./TSNewRulesCommand";

type MapMode = "select" | "drag" | "zoom" | "gesture"

// TODO: Maybe move this into domain
let numNewContextsMadeByPopupMenuThisSession = 0
let numNewCommandsMadeByPopupMenuThisSession = 0

export class RuleMapView {
    domain: TSDomain
    world: TWorld
    worldCommandList: TSCommandList
    
    canvas: HTMLCanvasElement
    mapDrawer: TSMapView

    isDragging = false
    isZooming = false
    lastMouseLocation = new TPoint(0, 0)

    mapMode: MapMode = "select"

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

    PopupNewContextClick(): void {
        while (this.domain.world.findVariable("new context " + (numNewContextsMadeByPopupMenuThisSession + 1)) !== null) {
            numNewContextsMadeByPopupMenuThisSession += 1
        }

        const newContextName = prompt("New context name?", "new context " + numNewContextsMadeByPopupMenuThisSession)
        if (!newContextName) return

        const newRulesCommand: TSNewRulesCommand = new TSNewRulesCommand(this.domain)
        const newRule: TSRule = this.domain.world.newRule()
        newRulesCommand.addRule(newRule)

        newRule.setContext(newContextName)
        newRule.setCommand("look")
        newRule.setReply("There is nothing of interest here.")
        newRule.position = new TPoint(this.lastMapMouseDownPosition.X + 30, this.lastMapMouseDownPosition.Y + 30)
        newRule.context.position = this.lastMapMouseDownPosition
        this.domain.world.deselectAllExcept(newRule)
        newRule.selected = true
        this.domain.worldCommandList.doCommand(newRulesCommand)
        this.domain.editRule(newRule)
        /* TODO: select context edit
        this.ActiveControl = this.ContextEdit
        this.ContextEdit.SelStart = 0
        this.ContextEdit.SelLength = len(this.ContextEdit.Text)
        */
    }

    PopupNewCommandClick(): void {
        while (this.domain.world.findVariable("new command " + (numNewCommandsMadeByPopupMenuThisSession + 1)) !== null) {
            numNewCommandsMadeByPopupMenuThisSession += 1
        }

        const newCommandName = prompt("New command phrase?", "new command " + numNewCommandsMadeByPopupMenuThisSession)
        if (!newCommandName) return

        let newRuleCount = 0
        const newRulesCommand = new TSNewRulesCommand(this.domain)
        for (let i = 0; i < this.domain.world.variables.length; i++) {
            const variable: TSVariable = this.domain.world.variables[i]
            if (variable.selected) {
                const newRule = this.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(variable.phrase)
                newRule.setCommand(newCommandName)
                newRule.setReply("Nothing happens.")
                newRule.position = new TPoint(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount)
                newRuleCount += 1
            }
        }
        for (let i = 0; i < this.domain.world.rules.length; i++) {
            const rule: TSRule = this.domain.world.rules[i]
            if (rule.selected) {
                const newRule = this.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(rule.context.phrase)
                newRule.setCommand(newCommandName)
                newRule.setReply("Nothing happens.")
                newRule.position = new TPoint(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount)
                newRuleCount += 1
            }
        }
        if (!newRuleCount) {
            alert("To make a new command,\nselect at least one context or command\nand then click where you want to place the new command.")
            return
        }
        this.domain.world.deselectAllExcept(null)
        this.domain.worldCommandList.doCommand(newRulesCommand)
        for (let newRule of newRulesCommand.rules) {
            newRule.selected = true
        }
        this.domain.editRule(newRulesCommand.rules[newRulesCommand.rules.length - 1])

        /* TODO: select command edit
        this.ActiveControl = this.CommandEdit
        this.CommandEdit.SelStart = 0
        this.CommandEdit.SelLength = len(this.CommandEdit.Text)
        */
    }

    /* 
    PopupNewLinkClick(): void {
        let draggableNode: TSDraggableObject
        let contextToMoveTo: TSVariable
        let newRulesCommand: TSNewRulesCommand
        let variable: TSVariable
        let rule: TSRule
        let newRule: TSRule
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        let atLeastOneRuleChanged: boolean
        
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
        draggableNode = mapView.nearestNode(this.lastMapMouseDownPosition, displayOptions)
        if ((draggableNode === null) || !(draggableNode instanceof usworld.TSVariable)) {
            MessageDialog("To build a link," + chr(13) + "select at least one context or command" + chr(13) + "and right-click on a context.", mtInformation, {mbOK, }, 0)
            this.MapPaintBoxChanged()
            return
        }
        contextToMoveTo = draggableNode
        newRule = null
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.selected) {
                newRulesCommand = uscommands.TSNewRulesCommand().create()
                newRule = usdomain.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(variable.phrase)
                newRule.setCommand("move to " + contextToMoveTo.phrase)
                newRule.setReply("You move to " + contextToMoveTo.phrase + ".")
                newRule.setMove(contextToMoveTo.phrase)
                newRule.position.X = (variable.position.X + contextToMoveTo.position.X) / 2
                newRule.position.Y = (variable.position.Y + contextToMoveTo.position.Y) / 2
                usdomain.domain.worldCommandList.doCommand(newRulesCommand)
            }
        }
        atLeastOneRuleChanged = false
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (rule.selected) {
                if (contextToMoveTo.phrase !== rule.move.phrase) {
                    usdomain.domain.worldCommandList.ruleFieldChange(rule, usworld.kRuleMove, contextToMoveTo.phrase)
                }
                atLeastOneRuleChanged = true
            }
        }
        if (newRule !== null) {
            usdomain.domain.world.deselectAllExcept(newRule)
            newRule.selected = true
            this.editRule(newRule)
            this.ActiveControl = this.CommandEdit
            this.CommandEdit.SelStart = 0
            this.CommandEdit.SelLength = len(this.CommandEdit.Text)
        }
        if ((newRule !== null) || (atLeastOneRuleChanged)) {
            this.MapPaintBoxChanged()
        } else {
            MessageDialog("To build a link," + chr(13) + "select at least one context or command" + chr(13) + "and right-click on a context.", mtInformation, {mbOK, }, 0)
            this.MapPaintBoxChanged()
        }
    }
    */
    
    MapImageMouseDown(offsetX: number, offsetY: number, ctrlKey: boolean, shiftKey: boolean): void {
        this.lastMapMouseDownPosition = new TPoint(offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X, offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y)

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
            offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X,
            offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y
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
                centerPosition = new TPoint(offsetX, offsetY)
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

        const multipleSelect: boolean = ctrlKey
        this.mapSelectionInProgress = false
        if (draggedNode === null) {
            this.makeChoice(null, multipleSelect)
            this.mapSelectionRect = new TRect(offsetX, offsetY, offsetX, offsetY)
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = true
            return
        }
        //MapPaintBoxChanged
        this.makeChoice(draggedNode, multipleSelect)
        
        // TODO: Remove this or maybe make shift into drag scrolling?
        if (shiftKey) {
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
        this.actionInProgress = this.worldCommandList.mouseDown(newCommand, new TPoint(offsetX, offsetY))
    }
    
    MapImageMouseMove(offsetX: number, offsetY: number): boolean {
        if (this.actionInProgress) {
            this.worldCommandList.mouseMove(new TPoint(offsetX, offsetY))
        } else if (this.mapSelectionInProgress) {
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionRect.Right = offsetX
            this.mapSelectionRect.Bottom = offsetY
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
        } else {
            return false
        }
        return true
    }
    
    MapImageMouseUp(offsetX: number, offsetY: number, ctrlKey: boolean): boolean {
        if (this.actionInProgress) {
            this.worldCommandList.mouseUp(new TPoint(offsetX, offsetY))
            this.actionInProgress = false
        } else if (this.mapSelectionInProgress) {
            // TODO use or remove: this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = false
            if (!(ctrlKey)) {
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
            return false
        }
        return true
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

        // Function inspired by: https://stackoverflow.com/questions/1517924/javascript-mapping-touch-events-to-mouse-events
        // Otherwise would need to calculate offsetX and offsetY
        function dispatchMouseEventForTouchEvent(event: TouchEvent) {
            const touches = event.changedTouches
            const first = touches[0]
            const map: { [eventname: string]: string } = {
                touchstart: "mousedown",
                touchmove: "mousemove",
                touchend: "mouseup"
            }
            const type: string = map[event.type];
            if (!type) return

            const simulatedEvent = new MouseEvent(type, first)
            first.target.dispatchEvent(simulatedEvent);
            event.preventDefault();
        }

        const indicator = (choice: MapMode): string => {
            if (this.mapMode === choice) return ">" + choice + "<"
            return choice 
        }

        const centerMap = () => {
            const boundsCenter = this.world.boundsRect().center()
            this.mapDrawer.scroll.X = Math.round(this.canvas.width / 2 / this.mapDrawer.scale) - boundsCenter.X
            this.mapDrawer.scroll.Y = Math.round(this.canvas.height / 2 / this.mapDrawer.scale) - boundsCenter.Y
        }

        const resetMap = () => {
            this.mapDrawer.scroll.X = 0
            this.mapDrawer.scroll.Y = 0
            this.mapDrawer.scale = defaultScale
        }

        return m(".RuleMapView.h-100.w-100.overflow-hidden.flex.flex-column",
            m("div.h2.flex-none", 
                m("button.ml1.h-75.br-pill.w4.mt1", { onclick: () => this.mapMode = "select" }, indicator("select")),
                m("button.ml1.h-75.br-pill.w4.mt1", { onclick: () => this.mapMode = "drag" }, indicator("drag")),
                m("button.ml1.h-75.br-pill.w4.mt1", { onclick: () => this.mapMode = "zoom" }, indicator("zoom")),
                m("button.ml1.h-75.br-pill.w4.mt1", { onclick: () => this.mapMode = "gesture", title: "allows mobile gestures" }, indicator("gesture")),
                m("button.ml3.h-75.mt1", { onclick: () => centerMap() }, "center"),
                m("button.ml1.h-75.mt1", { onclick: () => resetMap() }, "reset"),
                m("button.ml3.h-75.mt1", { onclick: () => this.PopupNewContextClick() }, "+context"),
                m("button.ml1.h-75.mt1", { onclick: () => this.PopupNewCommandClick() }, "+command"),
            ),
            m("canvas.ba.flex-auto", {
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
                    if (this.mapMode === "drag") {
                        this.isDragging = true
                        ;(<any>event).redraw = false
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else if (this.mapMode === "zoom") {
                        this.isZooming = true
                        ;(<any>event).redraw = false
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else {
                        this.MapImageMouseDown(event.offsetX, event.offsetY, event.ctrlKey, event.shiftKey)
                    }
                    return false
                },

                onmousemove: (event: MouseEvent) => {
                    if (this.isDragging) {
                        this.mapDrawer.scroll.X += (event.offsetX - this.lastMouseLocation.X)
                        this.mapDrawer.scroll.Y += (event.offsetY - this.lastMouseLocation.Y)
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else if (this.isZooming) {
                        const approximateDelta = event.offsetX - this.lastMouseLocation.X + event.offsetY - this.lastMouseLocation.Y
                        this.mapDrawer.scale = Math.max(0.1, this.mapDrawer.scale + approximateDelta / 100)
                        this.lastMouseLocation = new TPoint(event.offsetX, event.offsetY)
                    } else {
                        if (!this.MapImageMouseMove(event.offsetX, event.offsetY)) (<any>event).redraw = false
                    }
                },

                onmouseup: (event: MouseEvent) => {
                    if (this.isDragging) {
                        this.isDragging = false
                        ;(<any>event).redraw = false
                    } else if (this.isZooming) {
                        this.isZooming = false
                        ;(<any>event).redraw = false
                    } else {
                        if (!this.MapImageMouseUp(event.offsetX, event.offsetY, event.ctrlKey)) (<any>event).redraw = false
                    }
                },

                onmouseout: (event: MouseEvent) => {
                    this.isDragging = false
                    this.isZooming = false
                    if (!this.MapImageMouseUp(event.offsetX, event.offsetY, event.ctrlKey)) (<any>event).redraw = false
                },

                ontouchstart: (event: TouchEvent) => {
                    if (this.mapMode === "gesture") return
                    dispatchMouseEventForTouchEvent(event)
                },

                ontouchmove: (event: TouchEvent) => {
                    if (this.mapMode === "gesture") return
                    dispatchMouseEventForTouchEvent(event)
                },

                ontouchend: (event: TouchEvent) => {
                    if (this.mapMode === "gesture") return
                    dispatchMouseEventForTouchEvent(event)
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
                        centerMap()
                        break
                    case 187: // plus +
                        this.mapDrawer.scale = this.mapDrawer.scale * 1.2
                        break
                    case 189: // minus -
                        this.mapDrawer.scale = this.mapDrawer.scale / 1.2
                        break
                    case 82: // r to reset
                        resetMap()
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
