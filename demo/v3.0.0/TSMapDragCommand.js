define(["require", "exports", "./KfCommand", "./TPoint"], function (require, exports, KfCommand_1, TPoint_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSMapDragCommand extends KfCommand_1.KfCommand {
        constructor(domain, scale) {
            super();
            this.dragRecords = [];
            this.domain = domain;
            this.scale = scale;
            this.domain.world.addDragRecordsToList(this.dragRecords);
        }
        doCommand() {
            for (let i = 0; i < this.dragRecords.length; i++) {
                this.dragRecords[i].doDrag();
            }
            super.doCommand();
        }
        undoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = 0; i < this.dragRecords.length; i++) {
                this.dragRecords[i].draggedNode.selected = true;
                this.dragRecords[i].undoDrag();
            }
            super.undoCommand();
        }
        redoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = 0; i < this.dragRecords.length; i++) {
                this.dragRecords[i].draggedNode.selected = true;
                this.dragRecords[i].doDrag();
            }
            super.doCommand();
        }
        description() {
            let result = "";
            if (this.dragRecords.length > 1) {
                result = "Drag nodes";
            }
            else if (this.dragRecords.length === 1) {
                result = "Drag " + this.dragRecords[0].draggedNode.displayName();
            }
            else {
                result = "Drag";
            }
            return result;
        }
        trackMouse(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove, rightButtonDown) {
            let result;
            result = this;
            switch (aTrackPhase) {
                case KfCommand_1.TrackPhase.trackPress:
                    if (this.dragRecords.length === 0) {
                        result = null;
                    }
                    break;
                case KfCommand_1.TrackPhase.trackMove:
                    if (mouseDidMove) {
                        const delta = new TPoint_1.TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y);
                        for (let i = 0; i < this.dragRecords.length; i++) {
                            this.dragRecords[i].offset(delta.scale(this.scale));
                        }
                        if (this.notifyProcedure) {
                            this.notifyProcedure(this, KfCommand_1.KfCommandChangeType.commandDone);
                        }
                    }
                    break;
                case KfCommand_1.TrackPhase.trackRelease:
                    if (!mouseDidMove) {
                        if ((this.dragRecords[0].draggedNode.position.X !== this.dragRecords[0].originalLocation.X) || (this.dragRecords[0].draggedNode.position.Y !== this.dragRecords[0].originalLocation.Y)) {
                            for (let i = 0; i < this.dragRecords.length; i++) {
                                this.dragRecords[i].undoDrag();
                            }
                            if (this.notifyProcedure) {
                                this.notifyProcedure(this, KfCommand_1.KfCommandChangeType.commandDone);
                            }
                        }
                        result = null;
                    }
                    else {
                        const delta = new TPoint_1.TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y);
                        if ((delta.X !== 0) || (delta.Y !== 0)) {
                            for (let i = 0; i < this.dragRecords.length; i++) {
                                this.dragRecords[i].offset(delta.scale(this.scale));
                            }
                            if (this.notifyProcedure) {
                                this.notifyProcedure(this, KfCommand_1.KfCommandChangeType.commandDone);
                            }
                        }
                    }
                    break;
            }
            return result;
        }
    }
    exports.TSMapDragCommand = TSMapDragCommand;
});
