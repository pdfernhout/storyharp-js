import * as m from "mithril"
import { int, expander } from "./common"

// Uncomment console.log version for debugging events
const log = (...args: any[]) => {}
// const log = console.log

// Key idea here is that the "combobox" has focus when any of the input, button, or menu items have focus.
// So, an onchange is only sent when focus is lost from all of those
export class TQuickFillComboBox {
    lastSuppliedText = ""
    textValue: string = ""
    isMenuOpen = false
    menuOpenedByButton = false
    clientWidth = 0
    inputElement: HTMLInputElement
    buttonElement: HTMLButtonElement
    ulElement: HTMLUListElement

    focus() {
        this.inputElement.focus()
    }

    clear() {
        this.lastSuppliedText = ""
        this.textValue = ""
        this.inputElement.value = ""
    }

    // TODO: Figure out how to improve vnode typing specific to component
    view(vnode: any) {
        log("===================== view ==========================")
        const suppliedText: string = vnode.attrs.value || ""
        log("supplied text", suppliedText)
        // let suppliedTextChange = false
        if (this.lastSuppliedText !== suppliedText) {
            log("suppliedTextChange", this.lastSuppliedText, suppliedText)
            // suppliedTextChange = true
            this.lastSuppliedText = suppliedText
            this.textValue = suppliedText
        }
        log("textValue", this.textValue)
        log("isMenuOpen", this.isMenuOpen)
        const items: string[] = vnode.attrs.items || []
        const onchangeCallback: (event: { target: HTMLInputElement }) => {} = vnode.attrs.onchange
        const ignoreLeadingCharacter: string = vnode.attrs.ignoreLeadingCharacter || ""
        let leadingCharacter = ""
        const extraStyling: string = vnode.attrs.extraStyling || ""
        const clearOnEscape = vnode.attrs.clearOnEscape || false
        // TODO: use or remove
        // const mustBeInList: boolean = vnode.attrs.mustBeInList || false
        // const entryRequired: boolean = vnode.attrs.required || false

        const doOnchangeCallback = () => {
            log("doOnchangeCallback", this.textValue)
            if (onchangeCallback) {
                onchangeCallback({target: this.inputElement})
            }
        }

        const openMenu = (menuOpenedByButton = false) => {
            this.isMenuOpen = true
            this.menuOpenedByButton = menuOpenedByButton
        }

        const closeMenu = (newText: string | null = null, focusOnInput: boolean = true) => {
            log("closeMenu", newText, focusOnInput)
            // This may be called a second time from the focusout of menu items if used a keypress to close
            if (!this.isMenuOpen) return
            this.isMenuOpen = false
            if (newText !== null) {
                this.textValue = newText
                // TODO: Next line may not be needed?
                this.inputElement.value = newText
                this.inputElement.selectionStart = newText.length
                this.inputElement.selectionEnd = newText.length
            }
            if (focusOnInput) {
                this.inputElement.focus()
            } else {
                // Losing focus for entire combobox complex, so let creator know if input text has changed
                if (this.textValue !== this.lastSuppliedText) doOnchangeCallback()
            }
        }

        const isEnoughRoomAtBottom = (threshold: number) => {
            const roomAtBottom = window.innerHeight - this.inputElement.getBoundingClientRect().bottom // this.inputElement.offsetTop + this.inputElement.offsetHeight
            if (roomAtBottom < threshold) return false
            return true
        }

        const calculateLeadingCharacter = () => {
            const trimmedText = this.textValue.trim()
            if (ignoreLeadingCharacter) {
                leadingCharacter = ""
                for (let i = 0; i < ignoreLeadingCharacter.length; i++) {
                    const c = ignoreLeadingCharacter[i]
                    if (trimmedText.startsWith(c)) {
                        leadingCharacter = c
                        break
                    }
                }
            }
        }

        const getItemsForMatch = (items: string[], text: string, leadingCharacter: string): string[] => {
            if (this.menuOpenedByButton) return items
            text = text.trim()
            if (leadingCharacter) {
                text = text.substring(1).trim()
            }
            return items.filter(each => each.includes(text))
        }

        calculateLeadingCharacter()

        const matchingItems = this.isMenuOpen ? getItemsForMatch(items, this.textValue, leadingCharacter) : []

        return m("div.dib.relative",
            {
                style: vnode.attrs.style || "",
                oncreate: (vnode: any) => this.clientWidth = (<HTMLInputElement>(vnode.dom)).clientWidth,
                onupdate: (vnode: any) => this.clientWidth = (<HTMLInputElement>(vnode.dom)).clientWidth,

            },
            m("input" + extraStyling, {
                value: this.textValue,
                style: {
                    width: "calc(100% - 3rem)",
                },
                oncreate: (vnode: any) => {
                    this.inputElement = <HTMLInputElement>(vnode.dom)
                },
                onchange: (event: { target: HTMLInputElement }) => {
                    log("input onchange", event.target.value, this.isMenuOpen)
                    if (this.textValue !== event.target.value) {
                        this.textValue = event.target.value
                    } else {
                        (<any>event).redraw = false
                    }
                },
                onblur: (event: FocusEvent) => {
                    log("input onblur")
                    if (!this.isMenuOpen && event.relatedTarget !== this.buttonElement) {
                        log("input onblur processed", this.inputElement.value, this.isMenuOpen)
                        this.textValue = this.inputElement.value
                        if (this.lastSuppliedText !== this.textValue) doOnchangeCallback() 
                    } else {
                        (<any>event).redraw = false
                    }
                },
                onkeydown: (event: {keyCode: number, target: HTMLInputElement, redraw: boolean}) => {
                    log("onkeydown", event)
                    if ((event.keyCode === 40) || (event.keyCode === 38)) {
                        // down arrow
                        this.textValue = event.target.value
                        openMenu()
                        return false
                    } else if (event.keyCode === 13) {
                        // enter
                        this.textValue = event.target.value
                        doOnchangeCallback()
                        return false
                    } else if (clearOnEscape && event.keyCode === 27) {
                        this.clear()
                        return false
                    } else {
                        event.redraw = false
                    }
                    return true
                },
            }),
            m("button", {
                oncreate: (vnode: any) => {
                    this.buttonElement = <HTMLButtonElement>(vnode.dom)
                },
                onclick: () => {
                    log("button onclick", this.isMenuOpen)
                    if (this.isMenuOpen) {
                        closeMenu()
                    } else {
                        openMenu(true)
                    }
                },
                onblur: (event: FocusEvent) => {
                    log("onblur button", event, this.isMenuOpen)
                    if (!this.isMenuOpen && event.relatedTarget !== this.inputElement) {
                        doOnchangeCallback()
                    } else {
                        (<any>event).redraw = false
                    }
                },
            }, expander(this.isMenuOpen)),
            this.isMenuOpen
                ? m("ul.absolute.bg-light-gray.pa2.overflow-auto",
                    {
                        style: {
                            // Override Chrome user agent settings
                            "-webkit-margin-before": "0em",
                            "-webkit-margin-after": "0em",
                            "list-style-type": "none",
                            "margin": "0",
                            "padding": "0.25rem",
                            width: this.clientWidth + "px",
                            "max-height": "200px",
                            "box-shadow": "0px 8px 16px 0px rgba(0,0,0,0.2)",
                            "z-index": 1,
                            bottom: isEnoughRoomAtBottom(200) ? null : 2 + "rem",
                        },
                        oncreate: (vnode: any) => {
                            this.ulElement = vnode.dom
                            // TODO: focus on the first matching child instead
                            const firstChild = <HTMLElement>(<HTMLElement>vnode.dom).firstChild
                            if (firstChild) {
                                firstChild.focus()
                                // firstChild.scrollIntoView(true)
                                setTimeout(() => {
                                    (<HTMLElement>vnode.dom).scrollTop = 0
                                }, 50)
                            }
                        },
                        onmouseleave: () => {
                            log("onmouseleave")
                            closeMenu()
                        },
                    },
                    matchingItems.length ? [] : m("li.focus-bg-light-blue", {
                        tabindex: 0,
                        onclick: () => {
                            log("onclick")
                            closeMenu()
                        },
                        onkeydown: (event: KeyboardEvent) => {
                            log("onkeydown", event)
                            if (event.keyCode === 13) {
                                // enter
                                closeMenu()
                                return false
                            } else if (event.keyCode === 27) {
                                // escape
                                closeMenu()
                                return false
                           } else {
                                if (event.key.length === 1) this.textValue += event.key
                                closeMenu()
                                if (event.key.length === 1) return false
                            }
                            return true
                        },
                        onblur: (event: FocusEvent) => {
                            log("onblur", event, this.isMenuOpen)
                            if (this.isMenuOpen && event.relatedTarget !== this.buttonElement) {
                                closeMenu(null, event.relatedTarget === this.inputElement)
                            } else {
                                (<any>event).redraw = false
                            }
                        },
                    }, "No matches..."),
                    matchingItems.map((item, index) => m("li.focus-bg-light-blue", {
                        tabindex: 0,
                        onclick: () => {
                            log("on click in item", item)
                            closeMenu(leadingCharacter + item)
                        },
                        onblur: (event: FocusEvent) => {
                            const relatedTarget: any = event.relatedTarget
                            log("onblur in item", item, this.isMenuOpen)
                            log("relatedTarget", relatedTarget)
                            if (this.isMenuOpen && (!relatedTarget || relatedTarget.parentElement !== this.ulElement) && relatedTarget !== this.buttonElement) {
                                log("calling closemenu from item", item)
                                closeMenu(null, event.relatedTarget === this.inputElement)
                            } else {
                                (<any>event).redraw = false
                            }
                        },
                        onkeydown: (event: KeyboardEvent) => {
                            log("onkeydown in item", item, event)
                            if (event.keyCode === 38) {
                                // up arrow
                                const node = <HTMLElement>event.target
                                if (node.previousSibling) {
                                    (<HTMLElement>node.previousSibling).focus()
                                } else {
                                    const lastChild: HTMLElement | null = <HTMLElement>(<HTMLElement>node.parentElement).lastChild
                                    if (lastChild) {
                                        lastChild.focus()
                                        // lastChild.scrollIntoView(false)
                                    }
                                }
                                return false
                            } else if (event.keyCode === 40) {
                                // down arrow
                                const node = <HTMLElement>event.target
                                if (node.nextSibling) {
                                    (<HTMLElement>node.nextSibling).focus()
                                } else {
                                   const firstChild: HTMLElement | null = <HTMLElement>(<HTMLElement>node.parentElement).firstChild
                                   if (firstChild) {
                                        firstChild.focus()
                                        // firstChild.scrollIntoView(true)
                                   }
                                }
                                return false
                            } else if (event.keyCode === 13) {
                                // enter
                                closeMenu(leadingCharacter + item)
                                return false
                            } else if (event.keyCode === 27) {
                                // escape
                                closeMenu()
                                return false
                            } else {
                                // some other key
                                if (event.key.length === 1) this.textValue += event.key
                                closeMenu()
                                if (event.key.length === 1) return false
                            }
                            return true
                        }
                    }, item))
                )
                : []
        )
    }
}
