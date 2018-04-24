define(["require", "exports", "mithril"], function (require, exports, m) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let toastCount = 0;
    const toasts = [];
    function removeToast(record) {
        const index = toasts.indexOf(record);
        if (index >= 0)
            toasts.splice(index, 1);
    }
    function toast(text, timeout_ms = 5000) {
        const record = { id: ++toastCount, text, timeout_ms };
        toasts.push(record);
        setTimeout(() => {
            removeToast(record);
            m.redraw();
        }, record.timeout_ms);
    }
    exports.toast = toast;
    class ToastView {
        view() {
            return m("div.Toast.fixed.ml5.mt3.z-2", toasts.map(record => m("div.bg-light-blue.pa2.ba", {
                key: record.id,
                style: {
                    "min-width": "25rem",
                    "min-height": "3rem",
                    "max-width": "25rem",
                    "text-align": "center",
                },
                onclick: () => removeToast(record),
            }, record.text.split("\n").map(line => m("div", line)))));
        }
    }
    exports.ToastView = ToastView;
});
