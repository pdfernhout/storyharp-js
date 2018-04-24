define(["require", "exports", "./TPoint", "./TRect", "./TSRule"], function (require, exports, TPoint_1, TRect_1, TSRule_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const kDrawCommand = true;
    const kDrawContext = false;
    const arrowLength = 10;
    const arrowWidth = 4;
    exports.defaultScale = 1.5;
    const baseFontString = "10px sans-serif";
    const commandFontModifier = "";
    const contextFontModifier = "";
    function TextExtent(context, text) {
        return {
            cx: Math.ceil(context.measureText(text).width),
            cy: Math.ceil(context.measureText("M!").width),
        };
    }
    function intround(value) {
        return Math.round(value);
    }
    function InflateRect(rect, extraWidth, extraHeight) {
        return new TRect_1.TRect(rect.Left - extraWidth, rect.Top - extraHeight, rect.Right + extraWidth, rect.Bottom + extraHeight);
    }
    function OffsetRect(rect, x, y) {
        return new TRect_1.TRect(rect.Left + x, rect.Top + y, rect.Right + x, rect.Bottom + y);
    }
    function AdjustedIntersectionPointForLineAndRectangle(origin, rect) {
        if (rect.contains(origin)) {
            return new TPoint_1.TPoint(0, 0);
        }
        if ((rect.Left === 0) || (rect.Top === 0) || (rect.Right === 0) || (rect.Bottom === 0)) {
            return new TPoint_1.TPoint(0, 0);
        }
        if (origin.X === 0) {
            if (origin.Y < 0) {
                return new TPoint_1.TPoint(0, rect.Top);
            }
            else if (origin.Y === 0) {
                return new TPoint_1.TPoint(0, 0);
            }
            else {
                return new TPoint_1.TPoint(0, rect.Bottom);
            }
        }
        else if (origin.Y === 0) {
            if (origin.X < 0) {
                return new TPoint_1.TPoint(rect.Left, 0);
            }
            else {
                return new TPoint_1.TPoint(rect.Right, 0);
            }
        }
        else {
            const slope = (origin.Y * 1.0) / origin.X;
            if ((origin.X > 0) && (origin.Y < 0)) {
                if (slope < rect.Top * 1.0 / rect.Right) {
                    return new TPoint_1.TPoint(intround(rect.Top / slope), rect.Top);
                }
                else {
                    return new TPoint_1.TPoint(rect.Right, intround(rect.Right * slope));
                }
            }
            else if ((origin.X > 0) && (origin.Y > 0)) {
                if (slope > rect.Bottom * 1.0 / rect.Right) {
                    return new TPoint_1.TPoint(intround(rect.Bottom / slope), rect.Bottom);
                }
                else {
                    return new TPoint_1.TPoint(rect.Right, intround(rect.Right * slope));
                }
            }
            else if ((origin.X < 0) && (origin.Y < 0)) {
                if (slope > rect.Top * 1.0 / rect.Left) {
                    return new TPoint_1.TPoint(intround(rect.Top / slope), rect.Top);
                }
                else {
                    return new TPoint_1.TPoint(rect.Left, intround(rect.Left * slope));
                }
            }
            else if ((origin.X < 0) && (origin.Y > 0)) {
                if (slope < rect.Bottom * 1.0 / rect.Left) {
                    return new TPoint_1.TPoint(intround(rect.Bottom / slope), rect.Bottom);
                }
                else {
                    return new TPoint_1.TPoint(rect.Left, intround(rect.Left * slope));
                }
            }
            else {
                return new TPoint_1.TPoint(0, 0);
            }
        }
    }
    function IntersectionPointForLineAndRectangle(origin, destRect) {
        const center = new TPoint_1.TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2);
        const adjustedRect = new TRect_1.TRect(destRect.Left - center.X, destRect.Top - center.Y, destRect.Right - center.X, destRect.Bottom - center.Y);
        const adjustedOrigin = new TPoint_1.TPoint(origin.X - center.X, origin.Y - center.Y);
        const result = AdjustedIntersectionPointForLineAndRectangle(adjustedOrigin, adjustedRect);
        result.X = result.X + center.X;
        result.Y = result.Y + center.Y;
        return result;
    }
    function newMapViewState() {
        return {
            scroll: new TPoint_1.TPoint(0, 0),
            scale: exports.defaultScale,
            viewportSize: new TPoint_1.TPoint(0, 0),
        };
    }
    exports.newMapViewState = newMapViewState;
    class TSMapView {
        constructor(state) {
            this.state = state;
        }
        get scroll() {
            return this.state.scroll;
        }
        get scale() {
            return this.state.scale;
        }
        set scale(value) {
            this.state.scale = value;
        }
        reset() {
            this.scroll.X = 0;
            this.scroll.Y = 0;
            this.scale = exports.defaultScale;
        }
        drawArrowFromRectEdgeToRectEdge(context, originRect, destRect) {
            originRect = InflateRect(originRect, arrowWidth, arrowWidth);
            destRect = InflateRect(destRect, arrowWidth, arrowWidth);
            const origin = new TPoint_1.TPoint((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2);
            const dest = new TPoint_1.TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2);
            const intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect);
            const endPoint = new TPoint_1.TPoint(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y);
            const startPoint = IntersectionPointForLineAndRectangle(dest, originRect);
            startPoint.X = startPoint.X + this.scroll.X;
            startPoint.Y = startPoint.Y + this.scroll.Y;
            const scrolledOriginRect = OffsetRect(originRect, this.scroll.X, this.scroll.Y);
            if (scrolledOriginRect.contains(endPoint)) {
                return;
            }
            context.beginPath();
            context.moveTo(startPoint.X, startPoint.Y);
            context.lineTo(endPoint.X, endPoint.Y);
            context.stroke();
            this.drawArrowhead(context, startPoint, endPoint);
        }
        drawLineFromRectEdgeToRectEdge(context, originRect, destRect) {
            originRect = InflateRect(originRect, arrowWidth, arrowWidth);
            destRect = InflateRect(destRect, arrowWidth, arrowWidth);
            const origin = new TPoint_1.TPoint((originRect.Left + originRect.Right) / 2, (originRect.Top + originRect.Bottom) / 2);
            const dest = new TPoint_1.TPoint((destRect.Left + destRect.Right) / 2, (destRect.Top + destRect.Bottom) / 2);
            const intersectPoint = IntersectionPointForLineAndRectangle(origin, destRect);
            const endPoint = new TPoint_1.TPoint(intersectPoint.X + this.scroll.X, intersectPoint.Y + this.scroll.Y);
            const startPoint = IntersectionPointForLineAndRectangle(dest, originRect);
            startPoint.X = startPoint.X + this.scroll.X;
            startPoint.Y = startPoint.Y + this.scroll.Y;
            context.beginPath();
            context.moveTo(startPoint.X, startPoint.Y);
            context.lineTo(endPoint.X, endPoint.Y);
            context.stroke();
        }
        drawArrowhead(context, p1, p2) {
            const dx = p1.X - p2.X;
            const dy = p1.Y - p2.Y;
            let linelen = Math.sqrt(dx * dx + dy * dy);
            if (linelen === 0) {
                linelen = 1;
            }
            const xstep = dx / linelen;
            const ystep = dy / linelen;
            let x1;
            let y1;
            let x2;
            let y2;
            try {
                x2 = p2.X + intround(xstep * arrowLength);
                y2 = p2.Y + intround(ystep * arrowLength);
                y1 = -intround(xstep * arrowWidth);
                x1 = intround(ystep * arrowWidth);
            }
            catch (e) {
                console.log("numerical exception in drawArrowhead", e);
                return;
            }
            const outerOne = new TPoint_1.TPoint(x2 + x1, y2 + y1);
            const outerTwo = new TPoint_1.TPoint(x2 - x1, y2 - y1);
            context.beginPath();
            context.moveTo(p2.X, p2.Y);
            context.lineTo(outerOne.X, outerOne.Y);
            context.lineTo(outerTwo.X, outerTwo.Y);
            context.closePath();
            context.fill();
        }
        nearestNode(location, displayOptions, world) {
            let result = null;
            for (let i = world.variables.length - 1; i >= 0; i--) {
                const variable = world.variables[i];
                const showNode = variable.meetsDisplayOptions(displayOptions);
                if (!showNode) {
                    continue;
                }
                if (variable.bounds().contains(location)) {
                    result = variable;
                    return result;
                }
            }
            if (!displayOptions[TSRule_1.TSRuleField.kRuleCommand]) {
                return result;
            }
            for (let i = world.rules.length - 1; i >= 0; i--) {
                const rule = world.rules[i];
                if (rule.bounds().contains(location)) {
                    result = rule;
                    return result;
                }
            }
            return result;
        }
        displayOn(context, displayOptions, lastChoice, previousChoice, world, editedRule, showCommandPrefix) {
            context.textAlign = "start";
            context.textBaseline = "top";
            context.scale(this.scale, this.scale);
            context.font = contextFontModifier + baseFontString;
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                let textSize;
                if (rule === editedRule) {
                    textSize = TextExtent(context, rule.displayNamePrefixed(showCommandPrefix));
                }
                else {
                    textSize = TextExtent(context, rule.displayNamePrefixed(showCommandPrefix));
                }
                rule.extent.X = textSize.cx;
                rule.extent.Y = textSize.cy;
            }
            context.font = commandFontModifier + baseFontString;
            for (let i = 0; i < world.variables.length; i++) {
                const variable = world.variables[i];
                if (variable.meetsDisplayOptions(displayOptions)) {
                    const textSize = TextExtent(context, variable.displayName());
                    variable.extent.X = textSize.cx;
                    variable.extent.Y = textSize.cy;
                }
            }
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                if (displayOptions[TSRule_1.TSRuleField.kRuleCommand]) {
                    if (rule.context !== world.emptyEntry) {
                        this.drawLineFromRectEdgeToRectEdge(context, rule.context.bounds(), rule.bounds());
                    }
                    if (rule.move !== world.emptyEntry) {
                        this.drawArrowFromRectEdgeToRectEdge(context, rule.bounds(), rule.move.bounds());
                    }
                }
                else {
                    if ((rule.context !== world.emptyEntry) && (rule.move !== world.emptyEntry)) {
                        this.drawArrowFromRectEdgeToRectEdge(context, rule.context.bounds(), rule.move.bounds());
                    }
                }
            }
            if (displayOptions[TSRule_1.TSRuleField.kRuleCommand]) {
                for (let i = 0; i < world.rules.length; i++) {
                    const rule = world.rules[i];
                    this.drawCommandOrContext(context, rule.displayNamePrefixed(showCommandPrefix), rule.bounds(), rule.selected, rule === editedRule, kDrawCommand);
                }
            }
            for (let i = 0; i < world.variables.length; i++) {
                const variable = world.variables[i];
                if (variable.meetsDisplayOptions(displayOptions)) {
                    this.drawCommandOrContext(context, variable.displayName(), variable.bounds(), variable.selected, false, kDrawContext);
                }
            }
        }
        drawCommandOrContext(context, text, bounds, selected, focused, isCommand) {
            let drawRect = new TRect_1.TRect();
            drawRect.Left = bounds.Left - 2 + this.scroll.X;
            drawRect.Top = bounds.Top - 1 + this.scroll.Y;
            drawRect.Right = bounds.Right + 2 + this.scroll.X;
            drawRect.Bottom = bounds.Bottom + 1 + this.scroll.Y;
            const textPoint = new TPoint_1.TPoint(bounds.Left + this.scroll.X, bounds.Top + this.scroll.Y);
            if (isCommand) {
                context.font = commandFontModifier + baseFontString;
            }
            else {
                context.font = contextFontModifier + baseFontString;
            }
            if (selected) {
                context.strokeStyle = "rgb(0, 0, 0)";
                context.setLineDash([]);
            }
            else {
                if (isCommand) {
                    context.strokeStyle = "rgb(255, 255, 255)";
                    context.setLineDash([]);
                }
                else {
                    context.strokeStyle = "rgb(0, 0, 0)";
                    context.setLineDash([1, 1]);
                }
            }
            if (focused) {
                context.fillStyle = "#96ccff";
            }
            else {
                context.fillStyle = "rgb(255, 255, 255)";
            }
            this.drawRect(context, drawRect, false, true);
            context.fillStyle = "rgb(0, 0, 0)";
            context.fillText(text, textPoint.X, textPoint.Y);
            context.setLineDash([]);
        }
        drawRect(context, rect, scrolled = false, fill = false) {
            if (scrolled)
                rect = OffsetRect(rect, this.scroll.X, this.scroll.Y);
            context.beginPath();
            context.moveTo(rect.Left, rect.Top);
            context.lineTo(rect.Left, rect.Bottom);
            context.lineTo(rect.Right, rect.Bottom);
            context.lineTo(rect.Right, rect.Top);
            context.closePath();
            if (fill) {
                context.fill();
            }
            context.stroke();
        }
    }
    exports.TSMapView = TSMapView;
});
