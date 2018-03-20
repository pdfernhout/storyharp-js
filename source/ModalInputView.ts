import * as m from "mithril"

// Only supports one modal at a time

/* Test in your main form (use only one ModalInputView per application):
    m(ModalInputView),
    m("div",
        m("button", { onclick: () => modalAlert("test alert!").then(alert) }, "Test modal alert"),
        m("button", { onclick: () => modalConfirm("test confirm?").then(alert) }, "Test modal confirm"),
        m("button", { onclick: () => modalPrompt("test prompt", "default").then(alert) }, "Test modal prompt"),
    ),
*/

type ModalCallback = (() => m.Vnode) | null

let modalCallback: ModalCallback = null

export function setModalCallback(callback: ModalCallback) {
    if (callback && modalCallback) {
        alert("Only supports one modal at a time")
        throw new Error("Only supports one modal at a time")
    }
    modalCallback = callback
    m.redraw()
}

type ModalType = "alert" | "confirm" | "prompt"

export function modalAlert(promptText: string): Promise<string | null> {
    // Promise result is "OK"
    return standardModal(promptText, "alert", "OK")
}

export function modalConfirm(promptText: string): Promise<string | null> {
    // Promise result is null or "OK"
    return standardModal(promptText, "confirm", "OK")
}

export function modalPrompt(promptText: string, defaultText: string = ""): Promise<string | null> {
    // Promise result is null or entered text
    return standardModal(promptText, "prompt", defaultText)
}

function standardModal(promptText: string, modalType: ModalType, defaultText: string = ""): Promise<string | null> {
    let value = defaultText
    return new Promise((resolve, reject) => {
        setModalCallback(() => {
            return m("div.mt5.ml-auto.mr-auto.bg-near-white.pa3",
                { style: "width: 24rem" },
                m("div.ma2", promptText),
                modalType === "prompt" && m("div.ma2",
                    m("input.w-100", {
                        value: value,
                        oninput: (event: { target: HTMLInputElement }) => { value = event.target.value },
                        oncreate: (vnode: any) => {
                            const input = (<HTMLInputElement>(vnode.dom))
                            input.focus()
                            input.selectionStart = 0
                            input.selectionEnd = value.length
                        },
                        // TODO: Handle escape or enter even if no input
                        onkeydown: (event: KeyboardEvent) => {
                            if (event.keyCode === 13) {
                                // enter
                                setModalCallback(null)
                                resolve(value)
                                return false
                            } else if (event.keyCode === 27) {
                                // escape
                                setModalCallback(null)
                                resolve(null)
                                return false
                            }
                            return true
                        },
                    }),
                ),
                m("div.ma2.flex.justify-end", 
                    modalType !== "alert" && m("button", {
                        onclick: () => {
                            setModalCallback(null)
                            resolve(null)
                        }
                    }, "Cancel"),
                    m("button.ml2", {
                        onclick: () => {
                            setModalCallback(null)
                            resolve(value)
                        }
                    }, "OK"),
                ),
            )
        })
    })
}

export class ModalInputView {
    view() {
        if (modalCallback) {
            return m("div.ModalInputView.overlay",
                modalCallback()
            )
        } else {
            return []
        }
    }
}
