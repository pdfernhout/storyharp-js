define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var TrackPhase;
    (function (TrackPhase) {
        TrackPhase[TrackPhase["trackPress"] = 0] = "trackPress";
        TrackPhase[TrackPhase["trackMove"] = 1] = "trackMove";
        TrackPhase[TrackPhase["trackRelease"] = 2] = "trackRelease";
    })(TrackPhase = exports.TrackPhase || (exports.TrackPhase = {}));
    var KfCommandChangeType;
    (function (KfCommandChangeType) {
        KfCommandChangeType[KfCommandChangeType["commandDone"] = 0] = "commandDone";
        KfCommandChangeType[KfCommandChangeType["commandUndone"] = 1] = "commandUndone";
    })(KfCommandChangeType = exports.KfCommandChangeType || (exports.KfCommandChangeType = {}));
    class KfCommand {
        constructor() {
            this.canUndo = true;
            this.done = false;
            this.commandChangesFile = true;
        }
        doCommand() {
            this.done = true;
        }
        undoCommand() {
            this.done = false;
        }
        redoCommand() {
            this.doCommand();
        }
        description() {
            return "*command description*";
        }
        trackMouse(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove, rightButtonDown) {
            return this;
        }
        doNotify() {
            if (this.notifyProcedure) {
                if (this.done) {
                    this.notifyProcedure(this, KfCommandChangeType.commandDone);
                }
                else {
                    this.notifyProcedure(this, KfCommandChangeType.commandUndone);
                }
            }
        }
    }
    exports.KfCommand = KfCommand;
});
