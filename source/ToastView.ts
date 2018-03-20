import * as m from "mithril"

/* Test in your main form (use only one ToastView per application):
    m(ToastView),
    m("div", 
        m("button", {onclick: () => toast("hello there " + new Date()) }, "Toast short"),
        m("button", {onclick: () => toast("hello there and then some more it goes long and long " + new Date()) }, "Toast long"),
        m("button", {onclick: () => toast("hello there\nand then some more it goes another line\nand another " + new Date()) }, "Toast lines"),
    ),
*/

interface ToastRecord {
    id: number
    text: string
    timeout_ms: number
}

let toastCount = 0
const toasts: ToastRecord[] = []

function removeToast(record: ToastRecord) {
    const index = toasts.indexOf(record)
    if (index >= 0) toasts.splice(index, 1)
}

// If you call toast from inside a timeout or other handler without a mithril redraw, you need to call m.redraw()

export function toast(text: string, timeout_ms=5000) {
    const record = {id: ++toastCount, text, timeout_ms}
    toasts.push(record)
    setTimeout(() => {
        removeToast(record)
        m.redraw()
    }, record.timeout_ms)
}

export class ToastView {
    view() {
        return m("div.Toast.fixed.ml5.mt3.z-2",
            toasts.map(record => m("div.bg-light-blue.pa2.ba", {
                key: record.id,
                style: {
                    "min-width": "25rem",
                    "min-height": "3rem",
                    "max-width": "25rem",
                    "text-align": "center",
                },
                onclick: () => removeToast(record),
            }, record.text.split("\n").map(line => m("div", line)))
        ))
    }
}
