define(["require", "exports", "mithril"], function (require, exports, m) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let modalCallback = null;
    function setModalCallback(callback) {
        if (callback && modalCallback) {
            alert("Only supports one modal at a time");
            throw new Error("Only supports one modal at a time");
        }
        modalCallback = callback;
        m.redraw();
    }
    exports.setModalCallback = setModalCallback;
    function modalAlert(promptText) {
        return standardModal(promptText, "alert", "OK");
    }
    exports.modalAlert = modalAlert;
    function modalConfirm(promptText) {
        return standardModal(promptText, "confirm", "OK");
    }
    exports.modalConfirm = modalConfirm;
    function modalPrompt(promptText, defaultText = "") {
        return standardModal(promptText, "prompt", defaultText);
    }
    exports.modalPrompt = modalPrompt;
    function standardModal(promptText, modalType, defaultText = "") {
        let value = defaultText;
        return new Promise((resolve, reject) => {
            setModalCallback(() => {
                return m("div.mt5.ml-auto.mr-auto.bg-near-white.pa3", { style: "width: 24rem" }, m("div.ma2", promptText), modalType === "prompt" && m("div.ma2", m("input.w-100", {
                    value: value,
                    oninput: (event) => { value = event.target.value; },
                    oncreate: (vnode) => {
                        const input = (vnode.dom);
                        input.focus();
                        input.selectionStart = 0;
                        input.selectionEnd = value.length;
                    },
                    onkeydown: (event) => {
                        if (event.keyCode === 13) {
                            setModalCallback(null);
                            resolve(value);
                            return false;
                        }
                        else if (event.keyCode === 27) {
                            setModalCallback(null);
                            resolve(null);
                            return false;
                        }
                        return true;
                    },
                })), m("div.ma2.flex.justify-end", modalType !== "alert" && m("button", {
                    onclick: () => {
                        setModalCallback(null);
                        resolve(null);
                    }
                }, "Cancel"), m("button.ml2", {
                    onclick: () => {
                        setModalCallback(null);
                        resolve(value);
                    }
                }, "OK")));
            });
        });
    }
    class ModalInputView {
        view() {
            if (modalCallback) {
                return m("div.ModalInputView.overlay", modalCallback());
            }
            else {
                return [];
            }
        }
    }
    exports.ModalInputView = ModalInputView;
});
