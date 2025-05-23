import * as m from "mithril"
import { TSRule, TSRuleField } from "./TSRule"
import { TSMapView } from "./TSMapView"
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
import { toast } from "./ToastView"
import { modalPrompt } from "./ModalInputView"

type MapMode = "select" | "drag" | "zoom" | "gesture"

// TODO: Maybe move this into domain
let numNewContextsMadeByPopupMenuThisSession = 0
let numNewCommandsMadeByPopupMenuThisSession = 0

export class RuleMapView {
    domain: TSDomain
    world: TWorld
    worldCommandList: TSCommandList
    
    canvas!: HTMLCanvasElement
    mapDrawer: TSMapView

    isDragging = false
    isZooming = false
    lastMouseLocation = new TPoint(0, 0)

    mapMode: MapMode = "drag"

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

    mapChangedNotification(_command: KfCommand, _state: KfCommandChangeType): void {
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

        modalPrompt("New context name?", "new context " + (numNewContextsMadeByPopupMenuThisSession + 1)).then(newContextName => {
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
            this.MapPaintBoxChanged()
        })
    }

    PopupNewCommandClick(): void {
        while (this.domain.world.findVariable("new command " + (numNewCommandsMadeByPopupMenuThisSession + 1)) !== null) {
            numNewCommandsMadeByPopupMenuThisSession += 1
        }

        modalPrompt("New command phrase?", "new command " + (numNewCommandsMadeByPopupMenuThisSession + 1)).then(newCommandName => {
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
                toast("To make a new command,\nselect at least one context or command\nand then click where you want to place the new command.")
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
            this.MapPaintBoxChanged()
        })
    }

    PopupNewLinkClick(): void {
        // let variable: TSVariable
        // let rule: TSRule
        // let mapView: TSMapView

        /* TODO -- decide if can support old linking system with right click and multiple selections
        const displayOptions: TSVariableDisplayOptions = []
        for (let i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[TSRuleField.kRuleContext] = true
        displayOptions[TSRuleField.kRuleMove] = true
        // displayOptions[TSRuleField.kRuleCommand] = true // TODO:this.MenuMapsShowCommands.checked

        // const draggableNode: TSDraggableObject | null = this.mapDrawer.nearestNode(this.lastMapMouseDownPosition, displayOptions, this.domain.world)
        */
       
        const fromNode = this.previousChoice
        if ((fromNode === null) || !(fromNode instanceof TSVariable)) {
            toast("To build a link,\nselect two contexts.")
            return
        }

        const toNode = this.lastChoice
        if ((toNode === null) || !(toNode instanceof TSVariable) || (fromNode === toNode)) {
            toast("To build a link,\nselect two contexts.")
            return
        }

        modalPrompt("command to move?", "go to " + toNode.phrase).then(commandPhrase => {
            if (!commandPhrase) return

            // Remove this if implement other approach
            const newRulesCommand: TSNewRulesCommand = new TSNewRulesCommand(this.domain)
            const newRule = this.domain.world.newRule()
            newRulesCommand.addRule(newRule)
            newRule.setContext(fromNode.phrase)
            newRule.setCommand(commandPhrase)
            newRule.setReply("You " + commandPhrase + ".")
            newRule.setMove(toNode.phrase)
            newRule.position.X = (fromNode.position.X + toNode.position.X) / 2
            newRule.position.Y = (fromNode.position.Y + toNode.position.Y) / 2

            /* TODO
            const draggableNode = this.previousChoice
            if ((draggableNode === null) || !(draggableNode instanceof TSVariable)) {
                toast("To build a link,\nselect at least one context or command\nand then select others to connect to it.")
                return
            }
            const contextToMoveTo: TSVariable = draggableNode
            const newRulesCommand: TSNewRulesCommand = new TSNewRulesCommand(this.domain)
            for (let i = 0; i < this.domain.world.variables.length; i++) {
                const variable: TSVariable = this.domain.world.variables[i]
                if (variable.selected) {
                    const newRule = this.domain.world.newRule()
                    newRulesCommand.addRule(newRule)
                    newRule.setContext(variable.phrase)
                    newRule.setCommand("go to " + contextToMoveTo.phrase)
                    newRule.setReply("You go to " + contextToMoveTo.phrase + ".")
                    newRule.setMove(contextToMoveTo.phrase)
                    newRule.position.X = (variable.position.X + contextToMoveTo.position.X) / 2
                    newRule.position.Y = (variable.position.Y + contextToMoveTo.position.Y) / 2
                }
            }
            */

            let atLeastOneRuleChanged = false
            /* TODO -- if coudl get right click to work
            for (let i = 0; i < this.domain.world.rules.length; i++) {
                const rule: TSRule = this.domain.world.rules[i]
                if (rule.selected) {
                    if (contextToMoveTo.phrase !== rule.move.phrase) {
                        this.domain.worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleMove, contextToMoveTo.phrase)
                    }
                    atLeastOneRuleChanged = true
                }
            }
            */

            if (newRulesCommand.rules.length) {
                this.domain.worldCommandList.doCommand(newRulesCommand)
                this.domain.world.deselectAllExcept(null)
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
            if (!newRulesCommand.rules.length && !atLeastOneRuleChanged) {
                // toast("To build a link,\nselect at least one context or command\nand then select others to connect to it.")
                toast("To build a link, select two contexts.")
            } else {
                this.MapPaintBoxChanged()
            }
        })
    }
    
    MapImageMouseDown(offsetX: number, offsetY: number, ctrlKey: boolean, shiftKey: boolean): void {
        this.lastMapMouseDownPosition = new TPoint(offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X, offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y)

        // TODO: use or remove: this.FocusControl(this.PanelMap)

        const displayOptions: TSVariableDisplayOptions = []
        for (let i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[TSRuleField.kRuleContext] = true
        displayOptions[TSRuleField.kRuleMove] = true
        displayOptions[TSRuleField.kRuleCommand] = this.domain.showCommandsInMap

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
            // Use scrollWidth & scrollHeight to prevent canvas from stretching
            const parentDiv: HTMLDivElement | null = <HTMLDivElement>this.canvas.parentElement
            if (parentDiv) {
                this.canvas.width = parentDiv.clientWidth
                this.canvas.height = parentDiv.clientHeight
            }

            // Keep viewportSize up-to-date as it is needed by goodPosition algorithm
            this.domain.mapViewState.viewportSize.X = this.canvas.width
            this.domain.mapViewState.viewportSize.Y = this.canvas.height

            if (this.domain.pendingMapScroll) {
                this.scrollMapSelectionIntoView()
                this.domain.pendingMapScroll = false
            } 

            const context = canvas.getContext("2d")
            if (!context) return
            const displayOptions = []
            displayOptions[TSRuleField.kRuleContext] = true
            displayOptions[TSRuleField.kRuleMove] = true
            displayOptions[TSRuleField.kRuleCommand] = this.domain.showCommandsInMap
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
            // if (this.canvas === document.activeElement) {
            //     context.scale(defaultScale / this.mapDrawer.scale, defaultScale / this.mapDrawer.scale)
            //     context.textBaseline = "top"
            //     context.fillStyle = "gray"
            //     context.fillText("map controls: wasd arrows +- c r", 2, 2)
            // }
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
            this.mapDrawer.reset()
        }

        return m(".RuleMapView.h-100.w-100.flex.flex-column",
            m("div.flex-none.mb1", 
                m("button.ml1.br-pill.mt1", { onclick: () => this.mapMode = "select" }, indicator("select")),
                m("button.ml1.br-pill.mt1", { onclick: () => this.mapMode = "drag" }, indicator("drag")),
                m("button.ml1.br-pill.mt1", { onclick: () => this.mapMode = "zoom" }, indicator("zoom")),
                // m("button.ml1.br-pill.mt1", { onclick: () => this.mapMode = "gesture", title: "allows mobile gestures" }, indicator("gesture")),
                m("div.dib.ml2",
                    m("button.mt1.f-smaller", { title: "Center the map", onclick: () => centerMap() }, "center"),
                    m("button.ml1.mt1.f-smaller", { title: "Change scale and scrolling to default value", onclick: () => resetMap() }, "reset"),
                ),
            ),
            m("div.flex-auto.ba",
                m("canvas.h-100.w-100", {
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

                    onupdate: (_vnode: m.VnodeDOM) => {
                        drawWorld()
                    },

                    onblur: () => {
                        drawWorld()
                    },

                    onmousedown: (event: MouseEvent) => {
                        // Ignore right clicks
                        if (event.button !== 0) return
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

                    onmousewheel: (event: WheelEvent) => {
                        this.mapDrawer.scroll.X -= (event.deltaX / this.mapDrawer.scale)
                        this.mapDrawer.scroll.Y -= (event.deltaY / this.mapDrawer.scale)
                    },
                }),
            ),
            m("div.flex-none.mb1", 
                m("button.mt1.f-smaller", { title: "Make a new context", onclick: () => this.PopupNewContextClick() }, "+context"),
                m("button.ml1.mt1.f-smaller", { title: "Make a new command", onclick: () => this.PopupNewCommandClick() }, "+command"),
                m("button.ml1.mt1.f-smaller", { title: "Make a new link", onclick: () => this.PopupNewLinkClick() }, "+link"),
                m("label.dib.ml2.f-smaller", 
                    m("input[type=checkbox].mr1", {
                        name: "checkbox-commands",
                        checked: this.domain.showCommandsInMap || undefined,
                        onchange: (event: { target: HTMLInputElement }) => { 
                            this.domain.showCommandsInMap = event.target.checked
                        }
                    }),
                    "commands"
                ),
            )
        )
    }
}
