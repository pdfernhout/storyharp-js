define(["require", "exports", "mithril", "./TSRule", "./TSMapView", "./TRect", "./TPoint", "./TSMapDragCommand", "./TSVariable", "./TSNewRulesCommand", "./ToastView", "./ModalInputView"], function (require, exports, m, TSRule_1, TSMapView_1, TRect_1, TPoint_1, TSMapDragCommand_1, TSVariable_1, TSNewRulesCommand_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let numNewContextsMadeByPopupMenuThisSession = 0;
    let numNewCommandsMadeByPopupMenuThisSession = 0;
    class RuleMapView {
        constructor(vnode) {
            this.isDragging = false;
            this.isZooming = false;
            this.lastMouseLocation = new TPoint_1.TPoint(0, 0);
            this.mapMode = "select";
            this.actionInProgress = false;
            this.mapSelectionInProgress = false;
            this.mapSelectionRect = new TRect_1.TRect();
            this.lastMapMouseDownPosition = new TPoint_1.TPoint();
            const domain = vnode.attrs.domain;
            this.domain = domain;
            this.world = domain.world;
            this.worldCommandList = domain.worldCommandList;
            this.mapDrawer = new TSMapView_1.TSMapView(domain.mapViewState);
        }
        get previousChoice() {
            return this.domain.ruleEditorForm.previousChoice;
        }
        set previousChoice(value) {
            this.domain.ruleEditorForm.previousChoice = value;
        }
        get lastChoice() {
            return this.domain.ruleEditorForm.lastChoice;
        }
        set lastChoice(value) {
            this.domain.ruleEditorForm.lastChoice = value;
        }
        mapChangedNotification(command, state) {
            this.MapPaintBoxChanged();
        }
        makeChoice(choice, multiSelect) {
            let result = false;
            result = false;
            if (multiSelect) {
                if (choice !== null) {
                    choice.selected = !choice.selected;
                }
                result = true;
            }
            else {
                if ((choice === null) || !choice.selected) {
                    result = this.world.deselectAllExcept(choice);
                    if (choice !== null) {
                        choice.selected = true;
                    }
                }
                else {
                }
            }
            if (this.lastChoice === choice) {
                return result;
            }
            result = true;
            if ((choice !== null) && choice.selected) {
                this.previousChoice = this.lastChoice;
                this.lastChoice = choice;
                if (this.previousChoice instanceof TSRule_1.TSRule) {
                    this.previousChoice = choice;
                }
                if (this.lastChoice instanceof TSRule_1.TSRule) {
                    this.previousChoice = choice;
                }
            }
            else if ((choice !== null) && !choice.selected) {
                if (this.previousChoice === choice) {
                    this.previousChoice = null;
                }
                if (this.lastChoice === choice) {
                    this.lastChoice = null;
                }
            }
            return result;
        }
        PopupNewContextClick() {
            while (this.domain.world.findVariable("new context " + (numNewContextsMadeByPopupMenuThisSession + 1)) !== null) {
                numNewContextsMadeByPopupMenuThisSession += 1;
            }
            ModalInputView_1.modalPrompt("New context name?", "new context " + (numNewContextsMadeByPopupMenuThisSession + 1)).then(newContextName => {
                if (!newContextName)
                    return;
                const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
                const newRule = this.domain.world.newRule();
                newRulesCommand.addRule(newRule);
                newRule.setContext(newContextName);
                newRule.setCommand("look");
                newRule.setReply("There is nothing of interest here.");
                newRule.position = new TPoint_1.TPoint(this.lastMapMouseDownPosition.X + 30, this.lastMapMouseDownPosition.Y + 30);
                newRule.context.position = this.lastMapMouseDownPosition;
                this.domain.world.deselectAllExcept(newRule);
                newRule.selected = true;
                this.domain.worldCommandList.doCommand(newRulesCommand);
                this.domain.editRule(newRule);
                this.MapPaintBoxChanged();
            });
        }
        PopupNewCommandClick() {
            while (this.domain.world.findVariable("new command " + (numNewCommandsMadeByPopupMenuThisSession + 1)) !== null) {
                numNewCommandsMadeByPopupMenuThisSession += 1;
            }
            ModalInputView_1.modalPrompt("New command phrase?", "new command " + (numNewCommandsMadeByPopupMenuThisSession + 1)).then(newCommandName => {
                if (!newCommandName)
                    return;
                let newRuleCount = 0;
                const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
                for (let i = 0; i < this.domain.world.variables.length; i++) {
                    const variable = this.domain.world.variables[i];
                    if (variable.selected) {
                        const newRule = this.domain.world.newRule();
                        newRulesCommand.addRule(newRule);
                        newRule.setContext(variable.phrase);
                        newRule.setCommand(newCommandName);
                        newRule.setReply("Nothing happens.");
                        newRule.position = new TPoint_1.TPoint(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount);
                        newRuleCount += 1;
                    }
                }
                for (let i = 0; i < this.domain.world.rules.length; i++) {
                    const rule = this.domain.world.rules[i];
                    if (rule.selected) {
                        const newRule = this.domain.world.newRule();
                        newRulesCommand.addRule(newRule);
                        newRule.setContext(rule.context.phrase);
                        newRule.setCommand(newCommandName);
                        newRule.setReply("Nothing happens.");
                        newRule.position = new TPoint_1.TPoint(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount);
                        newRuleCount += 1;
                    }
                }
                if (!newRuleCount) {
                    ToastView_1.toast("To make a new command,\nselect at least one context or command\nand then click where you want to place the new command.");
                    return;
                }
                this.domain.world.deselectAllExcept(null);
                this.domain.worldCommandList.doCommand(newRulesCommand);
                for (let newRule of newRulesCommand.rules) {
                    newRule.selected = true;
                }
                this.domain.editRule(newRulesCommand.rules[newRulesCommand.rules.length - 1]);
                this.MapPaintBoxChanged();
            });
        }
        PopupNewLinkClick() {
            let variable;
            let rule;
            let mapView;
            const fromNode = this.previousChoice;
            if ((fromNode === null) || !(fromNode instanceof TSVariable_1.TSVariable)) {
                ToastView_1.toast("To build a link,\nselect two contexts.");
                return;
            }
            const toNode = this.lastChoice;
            if ((toNode === null) || !(toNode instanceof TSVariable_1.TSVariable) || (fromNode === toNode)) {
                ToastView_1.toast("To build a link,\nselect two contexts.");
                return;
            }
            ModalInputView_1.modalPrompt("command to move?", "go to " + toNode.phrase).then(commandPhrase => {
                if (!commandPhrase)
                    return;
                const newRulesCommand = new TSNewRulesCommand_1.TSNewRulesCommand(this.domain);
                const newRule = this.domain.world.newRule();
                newRulesCommand.addRule(newRule);
                newRule.setContext(fromNode.phrase);
                newRule.setCommand(commandPhrase);
                newRule.setReply("You " + commandPhrase + ".");
                newRule.setMove(toNode.phrase);
                newRule.position.X = (fromNode.position.X + toNode.position.X) / 2;
                newRule.position.Y = (fromNode.position.Y + toNode.position.Y) / 2;
                let atLeastOneRuleChanged = false;
                if (newRulesCommand.rules.length) {
                    this.domain.worldCommandList.doCommand(newRulesCommand);
                    this.domain.world.deselectAllExcept(null);
                    for (let newRule of newRulesCommand.rules) {
                        newRule.selected = true;
                    }
                    this.domain.editRule(newRulesCommand.rules[newRulesCommand.rules.length - 1]);
                }
                if (!newRulesCommand.rules.length && !atLeastOneRuleChanged) {
                    ToastView_1.toast("To build a link, select two contexts.");
                }
                else {
                    this.MapPaintBoxChanged();
                }
            });
        }
        MapImageMouseDown(offsetX, offsetY, ctrlKey, shiftKey) {
            this.lastMapMouseDownPosition = new TPoint_1.TPoint(offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X, offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y);
            const displayOptions = [];
            for (let i = 0; i <= 5; i++) {
                displayOptions[i] = false;
            }
            displayOptions[TSRule_1.TSRuleField.kRuleContext] = true;
            displayOptions[TSRule_1.TSRuleField.kRuleMove] = true;
            displayOptions[TSRule_1.TSRuleField.kRuleCommand] = this.domain.showCommandsInMap;
            const draggedNode = this.mapDrawer.nearestNode(new TPoint_1.TPoint(offsetX / this.mapDrawer.scale - this.mapDrawer.scroll.X, offsetY / this.mapDrawer.scale - this.mapDrawer.scroll.Y), displayOptions, this.world);
            const multipleSelect = ctrlKey;
            this.mapSelectionInProgress = false;
            if (draggedNode === null) {
                this.makeChoice(null, multipleSelect);
                this.mapSelectionRect = new TRect_1.TRect(offsetX, offsetY, offsetX, offsetY);
                this.mapSelectionInProgress = true;
                return;
            }
            this.makeChoice(draggedNode, multipleSelect);
            if (shiftKey) {
                this.MapPaintBoxChanged();
                return;
            }
            if (!multipleSelect) {
                this.MapImageDblClick();
            }
            this.MapPaintBoxChanged();
            const newCommand = new TSMapDragCommand_1.TSMapDragCommand(this.domain, 1 / this.mapDrawer.scale);
            newCommand.notifyProcedure = this.mapChangedNotification.bind(this);
            this.actionInProgress = this.worldCommandList.mouseDown(newCommand, new TPoint_1.TPoint(offsetX, offsetY));
        }
        MapImageMouseMove(offsetX, offsetY) {
            if (this.actionInProgress) {
                this.worldCommandList.mouseMove(new TPoint_1.TPoint(offsetX, offsetY));
            }
            else if (this.mapSelectionInProgress) {
                this.mapSelectionRect.Right = offsetX;
                this.mapSelectionRect.Bottom = offsetY;
            }
            else {
                return false;
            }
            return true;
        }
        MapImageMouseUp(offsetX, offsetY, ctrlKey) {
            if (this.actionInProgress) {
                this.worldCommandList.mouseUp(new TPoint_1.TPoint(offsetX, offsetY));
                this.actionInProgress = false;
            }
            else if (this.mapSelectionInProgress) {
                this.mapSelectionInProgress = false;
                if (!(ctrlKey)) {
                    this.world.deselectAllExcept(null);
                }
                this.mapSelectionRect = new TRect_1.TRect(this.mapSelectionRect.Left / this.mapDrawer.scale - this.mapDrawer.scroll.X, this.mapSelectionRect.Top / this.mapDrawer.scale - this.mapDrawer.scroll.Y, this.mapSelectionRect.Right / this.mapDrawer.scale - this.mapDrawer.scroll.X, this.mapSelectionRect.Bottom / this.mapDrawer.scale - this.mapDrawer.scroll.Y);
                this.world.selectInRectangle(this.mapSelectionRect);
                this.MapPaintBoxChanged();
            }
            else {
                return false;
            }
            return true;
        }
        MapPaintBoxChanged() {
        }
        MapImageDblClick() {
            if (this.lastChoice === null) {
                return;
            }
            if (this.lastChoice instanceof TSRule_1.TSRule) {
                this.domain.editRule(this.lastChoice);
            }
        }
        scrollMapSelectionIntoView() {
            const world = this.domain.world;
            const mapDrawer = this.mapDrawer;
            const canvas = this.canvas;
            let visibleRect = new TRect_1.TRect();
            let upperLeftObject = null;
            visibleRect.Left = -mapDrawer.scroll.X;
            visibleRect.Top = -mapDrawer.scroll.Y;
            visibleRect.Right = visibleRect.Left + canvas.width / mapDrawer.scale;
            visibleRect.Bottom = visibleRect.Top + canvas.height / mapDrawer.scale;
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                if (rule.selected) {
                    if (rule.bounds().intersects(visibleRect)) {
                        return;
                    }
                    if (upperLeftObject === null) {
                        upperLeftObject = rule;
                    }
                    else if (upperLeftObject.bounds().Top > rule.bounds().Top) {
                        upperLeftObject = rule;
                    }
                    else if (upperLeftObject.bounds().Left > rule.bounds().Left) {
                        upperLeftObject = rule;
                    }
                }
            }
            let firstContextVariable = null;
            for (let i = 0; i < world.variables.length; i++) {
                const variable = world.variables[i];
                if ((firstContextVariable === null) && (variable.hasUseagesForField(TSRule_1.TSRuleField.kRuleContext))) {
                    firstContextVariable = variable;
                }
                if (variable.selected) {
                    if (variable.bounds().intersects(visibleRect)) {
                        return;
                    }
                    if (upperLeftObject === null) {
                        upperLeftObject = variable;
                    }
                    else if (upperLeftObject.bounds().Top > variable.bounds().Top) {
                        upperLeftObject = variable;
                    }
                    else if (upperLeftObject.bounds().Left > variable.bounds().Left) {
                        upperLeftObject = variable;
                    }
                }
            }
            if (upperLeftObject === null) {
                upperLeftObject = firstContextVariable;
            }
            if (upperLeftObject === null) {
                return;
            }
            mapDrawer.scroll.X = canvas.width / mapDrawer.scale / 2 - upperLeftObject.center().X;
            mapDrawer.scroll.Y = canvas.height / mapDrawer.scale / 2 - upperLeftObject.center().Y;
        }
        view() {
            const world = this.world;
            const drawWorld = () => {
                const canvas = this.canvas;
                const parentDiv = this.canvas.parentElement;
                if (parentDiv) {
                    this.canvas.width = parentDiv.clientWidth;
                    this.canvas.height = parentDiv.clientHeight;
                }
                this.domain.mapViewState.viewportSize.X = this.canvas.width;
                this.domain.mapViewState.viewportSize.Y = this.canvas.height;
                if (this.domain.pendingMapScroll) {
                    this.scrollMapSelectionIntoView();
                    this.domain.pendingMapScroll = false;
                }
                const context = canvas.getContext("2d");
                if (!context)
                    return;
                const displayOptions = [];
                displayOptions[TSRule_1.TSRuleField.kRuleContext] = true;
                displayOptions[TSRule_1.TSRuleField.kRuleMove] = true;
                displayOptions[TSRule_1.TSRuleField.kRuleCommand] = this.domain.showCommandsInMap;
                context.setLineDash([]);
                context.lineDashOffset = 0;
                this.mapDrawer.displayOn(context, displayOptions, null, null, world, this.domain.editedRule, this.domain.showCommandPrefixInMap);
                context.strokeStyle = "rgb(0, 0, 0)";
                context.setLineDash([4, 16]);
                context.lineDashOffset = 2;
                this.mapDrawer.drawRect(context, world.boundsRect(), true);
                if (this.mapSelectionInProgress) {
                    this.mapDrawer.drawRect(context, this.mapSelectionRect.scale(1 / this.mapDrawer.scale));
                }
                if (this.canvas === document.activeElement) {
                    context.scale(TSMapView_1.defaultScale / this.mapDrawer.scale, TSMapView_1.defaultScale / this.mapDrawer.scale);
                    context.textBaseline = "top";
                    context.fillStyle = "gray";
                    context.fillText("map controls: wasd arrows +- c r", 2, 2);
                }
            };
            function dispatchMouseEventForTouchEvent(event) {
                const touches = event.changedTouches;
                const first = touches[0];
                const map = {
                    touchstart: "mousedown",
                    touchmove: "mousemove",
                    touchend: "mouseup"
                };
                const type = map[event.type];
                if (!type)
                    return;
                const simulatedEvent = new MouseEvent(type, first);
                first.target.dispatchEvent(simulatedEvent);
                event.preventDefault();
            }
            const indicator = (choice) => {
                if (this.mapMode === choice)
                    return ">" + choice + "<";
                return choice;
            };
            const centerMap = () => {
                const boundsCenter = this.world.boundsRect().center();
                this.mapDrawer.scroll.X = Math.round(this.canvas.width / 2 / this.mapDrawer.scale) - boundsCenter.X;
                this.mapDrawer.scroll.Y = Math.round(this.canvas.height / 2 / this.mapDrawer.scale) - boundsCenter.Y;
            };
            const resetMap = () => {
                this.mapDrawer.reset();
            };
            return m(".RuleMapView.h-100.w-100.overflow-hidden.flex.flex-column", m("div.flex-none.mb1", m("button.ml1.h-75.br-pill.w-6rem.mt1", { onclick: () => this.mapMode = "select" }, indicator("select")), m("button.ml1.h-75.br-pill.w-6rem.mt1", { onclick: () => this.mapMode = "drag" }, indicator("drag")), m("button.ml1.h-75.br-pill.w-6rem.mt1", { onclick: () => this.mapMode = "zoom" }, indicator("zoom")), m("button.ml1.h-75.br-pill.w-6rem.mt1", { onclick: () => this.mapMode = "gesture", title: "allows mobile gestures" }, indicator("gesture")), m("div.dib.ml3", m("button.h-75.mt1", { title: "Center the map", onclick: () => centerMap() }, "center"), m("button.ml1.h-75.mt1", { title: "Change scale and scrolling to default value", onclick: () => resetMap() }, "reset")), m("div.dib.ml3", m("button.h-75.mt1", { title: "Make a new context", onclick: () => this.PopupNewContextClick() }, "+context"), m("button.ml1.h-75.mt1", { title: "Make a new command", onclick: () => this.PopupNewCommandClick() }, "+command"), m("button.ml1.h-75.mt1", { title: "Make a new link", onclick: () => this.PopupNewLinkClick() }, "+link")), m("label.dib.ml2", m("input[type=checkbox]", {
                checked: this.domain.showCommandsInMap || undefined,
                onchange: (event) => {
                    this.domain.showCommandsInMap = event.target.checked;
                }
            }), "commands")), m("div.flex-auto.ba.h-100.w-100", m("canvas", {
                tabindex: 0,
                oncreate: (vnode) => {
                    this.canvas = vnode.dom;
                    drawWorld();
                },
                onupdate: (vnode) => {
                    drawWorld();
                },
                onblur: () => {
                    drawWorld();
                },
                onmousedown: (event) => {
                    if (event.button !== 0)
                        return;
                    this.canvas.focus();
                    if (this.mapMode === "drag") {
                        this.isDragging = true;
                        event.redraw = false;
                        this.lastMouseLocation = new TPoint_1.TPoint(event.offsetX, event.offsetY);
                    }
                    else if (this.mapMode === "zoom") {
                        this.isZooming = true;
                        event.redraw = false;
                        this.lastMouseLocation = new TPoint_1.TPoint(event.offsetX, event.offsetY);
                    }
                    else {
                        this.MapImageMouseDown(event.offsetX, event.offsetY, event.ctrlKey, event.shiftKey);
                    }
                    return false;
                },
                onmousemove: (event) => {
                    if (this.isDragging) {
                        this.mapDrawer.scroll.X += (event.offsetX - this.lastMouseLocation.X);
                        this.mapDrawer.scroll.Y += (event.offsetY - this.lastMouseLocation.Y);
                        this.lastMouseLocation = new TPoint_1.TPoint(event.offsetX, event.offsetY);
                    }
                    else if (this.isZooming) {
                        const approximateDelta = event.offsetX - this.lastMouseLocation.X + event.offsetY - this.lastMouseLocation.Y;
                        this.mapDrawer.scale = Math.max(0.1, this.mapDrawer.scale + approximateDelta / 100);
                        this.lastMouseLocation = new TPoint_1.TPoint(event.offsetX, event.offsetY);
                    }
                    else {
                        if (!this.MapImageMouseMove(event.offsetX, event.offsetY))
                            event.redraw = false;
                    }
                },
                onmouseup: (event) => {
                    if (this.isDragging) {
                        this.isDragging = false;
                        event.redraw = false;
                    }
                    else if (this.isZooming) {
                        this.isZooming = false;
                        event.redraw = false;
                    }
                    else {
                        if (!this.MapImageMouseUp(event.offsetX, event.offsetY, event.ctrlKey))
                            event.redraw = false;
                    }
                },
                onmouseout: (event) => {
                    this.isDragging = false;
                    this.isZooming = false;
                    if (!this.MapImageMouseUp(event.offsetX, event.offsetY, event.ctrlKey))
                        event.redraw = false;
                },
                ontouchstart: (event) => {
                    if (this.mapMode === "gesture")
                        return;
                    dispatchMouseEventForTouchEvent(event);
                },
                ontouchmove: (event) => {
                    if (this.mapMode === "gesture")
                        return;
                    dispatchMouseEventForTouchEvent(event);
                },
                ontouchend: (event) => {
                    if (this.mapMode === "gesture")
                        return;
                    dispatchMouseEventForTouchEvent(event);
                },
                onkeydown: (event) => {
                    const scrollDelta = 50;
                    switch (event.keyCode) {
                        case 37:
                        case 65:
                            this.mapDrawer.scroll.X += scrollDelta;
                            break;
                        case 38:
                        case 87:
                            this.mapDrawer.scroll.Y += scrollDelta;
                            break;
                        case 39:
                        case 68:
                            this.mapDrawer.scroll.X -= scrollDelta;
                            break;
                        case 40:
                        case 83:
                            this.mapDrawer.scroll.Y -= scrollDelta;
                            break;
                        case 67:
                            centerMap();
                            break;
                        case 187:
                            this.mapDrawer.scale = this.mapDrawer.scale * 1.2;
                            break;
                        case 189:
                            this.mapDrawer.scale = this.mapDrawer.scale / 1.2;
                            break;
                        case 82:
                            resetMap();
                            break;
                        default:
                            event.redraw = false;
                            break;
                    }
                },
                onmousewheel: (event) => {
                    this.mapDrawer.scroll.X -= (event.deltaX / this.mapDrawer.scale);
                    this.mapDrawer.scroll.Y -= (event.deltaY / this.mapDrawer.scale);
                },
            })));
        }
    }
    exports.RuleMapView = RuleMapView;
});
