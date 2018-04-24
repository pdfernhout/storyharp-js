var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) if (e.indexOf(p[i]) < 0)
            t[p[i]] = s[p[i]];
    return t;
};
define(["require", "exports", "mithril", "./TSVariable", "./TSDesiredStateVariableWrapper", "./VariablesView", "./TQuickFillComboBox"], function (require, exports, m, TSVariable_1, TSDesiredStateVariableWrapper_1, VariablesView_1, TQuickFillComboBox_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSLogicListBox {
        view(vnode) {
            const _a = vnode.attrs, { selections, items, world, onchange: onchangeCallback, onselect: onselectCallback } = _a, attrs = __rest(_a, ["selections", "items", "world", "onchange", "onselect"]);
            return m("div.dib.LogicListBox.ba.bg-white", attrs, m("ul.bg-white.pl1.pr1", {
                style: {
                    "list-style-type": "none",
                    "-webkit-margin-before": "0em",
                    "-webkit-margin-after": "0em",
                    "-webkit-padding-start": "0px",
                },
                onclick: (event) => {
                    this.comboBox.inputElement.focus();
                },
            }, selections.map((wrapper, i) => {
                return m("li.ba.fl.ml1.mb1.br1", {
                    key: wrapper.uuid,
                    style: {
                        "margin-top": "0.15rem",
                        "word-wrap": "break-word",
                        "background-color": wrapper.desiredState ? "lightgreen" : "linen",
                    },
                }, m("span.pl1.b", {
                    tabindex: 0,
                    key: 1,
                    onmousedown: (event) => {
                        wrapper.invertDesiredState();
                        if (onchangeCallback)
                            onchangeCallback(selections);
                    },
                    onkeydown: (event) => {
                        if ((event.keyCode === 13) || (event.keyCode === 32)) {
                            wrapper.invertDesiredState();
                            if (onchangeCallback)
                                onchangeCallback(selections);
                        }
                        else {
                            event.redraw = false;
                        }
                    }
                }, wrapper.desiredState ? VariablesView_1.Glyph.present : VariablesView_1.Glyph.absent), m("span", {
                    key: 2,
                    onmousedown: () => {
                        selections.splice(i, 1);
                        if (onselectCallback)
                            onselectCallback(wrapper.variable);
                    },
                }, wrapper.variable.phrase), m("span.ml2", {
                    tabindex: 0,
                    key: 3,
                    onmousedown: () => {
                        selections.splice(i, 1);
                        if (onchangeCallback)
                            onchangeCallback(selections);
                    },
                    onkeydown: (event) => {
                        if (event.keyCode === 13) {
                            selections.splice(i, 1);
                            if (onchangeCallback)
                                onchangeCallback(selections);
                        }
                        else {
                            event.redraw = false;
                        }
                    }
                }, "x"));
            }), m("li.ml1.mt1.mb1", { key: -1 }, m(TQuickFillComboBox_1.TQuickFillComboBox, {
                style: {
                    "margin-left": "0.25rem",
                    "border": "0",
                    "white-space": "nowrap",
                },
                value: "",
                items: items,
                ignoreLeadingCharacter: "~+",
                clearOnEscape: true,
                oncreate: (vnode) => {
                    this.comboBox = (vnode.state);
                },
                onchange: (event) => {
                    if (event.target.value) {
                        let desiredState = TSVariable_1.TSVariableState.kPresent;
                        let variableName = event.target.value.trim();
                        if (variableName.startsWith("~")) {
                            variableName = variableName.substring(1).trim();
                            desiredState = TSVariable_1.TSVariableState.kAbsent;
                        }
                        else if (variableName.startsWith("+")) {
                            variableName = variableName.substring(1).trim();
                        }
                        selections.push(new TSDesiredStateVariableWrapper_1.TSDesiredStateVariableWrapper(world.findOrCreateVariable(variableName, false), desiredState));
                        event.target.value = "";
                        if (onchangeCallback)
                            onchangeCallback(selections);
                        this.comboBox.clear();
                    }
                }
            }))));
        }
    }
    exports.TSLogicListBox = TSLogicListBox;
});
