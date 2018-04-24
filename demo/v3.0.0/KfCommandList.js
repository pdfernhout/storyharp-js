define(["require", "exports", "./KfCommand", "./common", "./TPoint"], function (require, exports, KfCommand_1, common_1, TPoint_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const kMinMouseMoveDistance = 2;
    class KfCommandList {
        constructor() {
            this.mouseCommand = null;
            this.commandList = [];
            this.lastDoneCommandIndex = -1;
            this.undoLimit = 100;
        }
        clear() {
            this.commandList = [];
            this.lastDoneCommandIndex = -1;
        }
        command(index) {
            return this.commandList[index];
        }
        setNewUndoLimit(newLimit) {
            this.undoLimit = newLimit;
            this.freeCommandsAboveLimit(this.undoLimit);
        }
        freeCommandsAboveLimit(theLimit) {
            while ((this.commandList.length > theLimit) && (this.commandList.length > 0)) {
                this.commandList.pop();
                this.lastDoneCommandIndex--;
                if (this.lastDoneCommandIndex < -1)
                    this.lastDoneCommandIndex = -1;
            }
        }
        doCommand(newCommand) {
            this.clearRedoableCommands();
            this.freeCommandsAboveLimit(this.undoLimit - 1);
            newCommand.doCommand();
            this.lastDoneCommandIndex++;
            this.commandList.push(newCommand);
            if (this.notifyProcedure) {
                newCommand.notifyProcedure = this.notifyProcedure;
            }
            newCommand.doNotify();
        }
        mouseDown(newCommand, point) {
            let nextMouseCommand;
            let result = false;
            if (this.mouseCommand !== null) {
                this.mouseUp(point);
            }
            this.mouseCommand = null;
            if (newCommand !== null) {
                this.anchorPoint = point;
                this.previousPoint = point;
                nextMouseCommand = newCommand;
                this.mouseCommand = nextMouseCommand.trackMouse(KfCommand_1.TrackPhase.trackPress, this.anchorPoint, this.previousPoint, point, false, this.rightButtonDown);
                result = (this.mouseCommand !== null);
            }
            return result;
        }
        mouseMove(point) {
            let mouseDidMove;
            let nextMouseCommand;
            let pointMovedTo = new TPoint_1.TPoint(0, 0);
            nextMouseCommand = this.mouseCommand;
            this.mouseCommand = null;
            if (nextMouseCommand !== null) {
                mouseDidMove =
                    (Math.abs(this.previousPoint.X - point.X) > kMinMouseMoveDistance) ||
                        (Math.abs(this.previousPoint.Y - point.Y) > kMinMouseMoveDistance);
                if (mouseDidMove) {
                    pointMovedTo = point;
                }
                else {
                    pointMovedTo = this.previousPoint;
                }
                this.mouseCommand = nextMouseCommand.trackMouse(KfCommand_1.TrackPhase.trackMove, this.anchorPoint, this.previousPoint, pointMovedTo, mouseDidMove, this.rightButtonDown);
            }
            this.previousPoint = pointMovedTo;
        }
        mouseUp(point) {
            let mouseDidMove;
            let nextMouseCommand;
            let pointMovedTo;
            nextMouseCommand = this.mouseCommand;
            this.mouseCommand = null;
            if (nextMouseCommand !== null) {
                mouseDidMove =
                    (Math.abs(point.X - this.anchorPoint.X) > kMinMouseMoveDistance) ||
                        (Math.abs(point.Y - this.anchorPoint.Y) > kMinMouseMoveDistance);
                if (mouseDidMove) {
                    pointMovedTo = point;
                }
                else {
                    pointMovedTo = this.anchorPoint;
                }
                nextMouseCommand = nextMouseCommand.trackMouse(KfCommand_1.TrackPhase.trackRelease, this.anchorPoint, this.previousPoint, point, mouseDidMove, this.rightButtonDown);
                if (nextMouseCommand !== null) {
                    this.doCommand(nextMouseCommand);
                }
            }
        }
        isUndoEnabled() {
            return this.lastDoneCommandIndex >= 0;
        }
        isRedoEnabled() {
            return this.lastDoneCommandIndex < (this.commandList.length - 1);
        }
        undoDescription() {
            if (this.lastDoneCommandIndex >= 0) {
                return this.command(this.lastDoneCommandIndex).description();
            }
            else {
                return "";
            }
        }
        redoDescription() {
            if (this.lastDoneCommandIndex < (this.commandList.length - 1)) {
                return this.command(this.lastDoneCommandIndex + 1).description();
            }
            else {
                return "";
            }
        }
        undoLast() {
            let aCommand;
            if (this.lastDoneCommandIndex >= 0) {
                aCommand = this.command(this.lastDoneCommandIndex);
                aCommand.undoCommand();
                this.lastDoneCommandIndex--;
                aCommand.doNotify();
            }
        }
        redoLast() {
            let aCommand;
            if (this.lastDoneCommandIndex < (this.commandList.length - 1)) {
                aCommand = this.command(this.lastDoneCommandIndex + 1);
                aCommand.redoCommand();
                this.lastDoneCommandIndex++;
                aCommand.doNotify();
            }
        }
        removeCommand(aCommand) {
            if (aCommand.done) {
                throw new Error("KfCommandList.removeCommand: command not undone");
            }
            common_1.arrayRemove(this.commandList, aCommand);
        }
        lastCommand() {
            if (this.lastDoneCommandIndex >= 0) {
                return this.command(this.lastDoneCommandIndex);
            }
            return null;
        }
        clearRedoableCommands() {
            if (this.isRedoEnabled)
                for (let i = this.commandList.length - 1; i > this.lastDoneCommandIndex; i--) {
                    this.commandList.pop();
                }
        }
        lastCommandChanged() {
            this.clearRedoableCommands();
        }
    }
    exports.KfCommandList = KfCommandList;
});
