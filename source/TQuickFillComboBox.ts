import * as m from "mithril"
import { int, expander } from "./common"

export class TQuickFillComboBox {
    items: string[]
    text: string
    menuOpen = false
    menuOpenedByButton = false
    clientWidth = 0
    inputElement: HTMLInputElement
    onchangeCallback: (event: { target: HTMLInputElement }) => {}
    ignoreLeadingCharacter: string
    leadingCharacter: string = ""
    clearAfterAccept: boolean
    // TODO: Use or remove these
    // mustBeInList: boolean = false
    // entryRequired: boolean = false

    // TODO: Figure out how to improve vnode typing specific to component
    constructor(vnode: any) {
        // TODO: Maybe want to move these assignemnts into view method and even remove those instance variables?
        this.text = vnode.attrs.value || ""
        this.ignoreLeadingCharacter = vnode.attrs.ignoreLeadingCharacter || ""
        this.clearAfterAccept = vnode.attrs.clearAfterAccept || false
        // this.mustBeInList = vnode.attrs.mustBeInList || false
        // this.entryRequired = vnode.attrs.required || false
    }

    focus() {
        this.inputElement.focus()
    }

    doOnchangeCallback() {
        if (this.onchangeCallback) {
            this.onchangeCallback(<any>{target: {value: this.text}})
        }
        if (this.clearAfterAccept) {
            this.text = ""
        }
    }

    view(vnode: any) {
        this.items = vnode.attrs.items
        this.onchangeCallback = vnode.attrs.onchange || ((event: any) => {})
        
        const extraStyling = vnode.attrs.extraStyling || ""

        const focusOnInput = () => {
            this.inputElement.focus()
            this.inputElement.selectionStart = this.text.length
            this.inputElement.selectionEnd = this.text.length
        }

        const isEnoughRoomAtBottom = (threshold: number) => {
            const roomAtBottom = window.innerHeight - this.inputElement.getBoundingClientRect().bottom // this.inputElement.offsetTop + this.inputElement.offsetHeight
            if (roomAtBottom < threshold) return false
            return true
        }

        return m("div.dib.relative",
            {
                style: vnode.attrs.style || "",
                oncreate: (vnode: any) => this.clientWidth = (<HTMLInputElement>(vnode.dom)).clientWidth,
                onupdate: (vnode: any) => this.clientWidth = (<HTMLInputElement>(vnode.dom)).clientWidth,

            },
            m("input" + extraStyling, {
                value: this.text,
                style: {
                    width: "calc(100% - 3rem)",
                },
                // oninput: (event: { target: HTMLInputElement }) => this.Text = event.target.value,
                onchange: (event: { target: HTMLInputElement }) => {
                    this.text = event.target.value
                    this.calculateLeadingCharacter()
                },
                oncreate: (vnode: any) => {
                    this.inputElement = <HTMLInputElement>(vnode.dom)
                },
                onupdate: (vnode: any) => {
                    this.inputElement = <HTMLInputElement>(vnode.dom)
                },
                onkeydown: (event: KeyboardEvent) => {
                    if (event.keyCode === 40) {
                        // down arrow
                        this.text = (<HTMLInputElement>event.target).value
                        this.calculateLeadingCharacter()
                        this.menuOpen = true
                        this.menuOpenedByButton = false
                    } else if (event.keyCode === 13) {
                        // enter
                        this.menuOpen = false
                        this.text = (<HTMLInputElement>event.target).value
                        this.doOnchangeCallback()
                    } else {
                        (<any>event).redraw = false
                    }
                    return true
                },
                onfocus:() => { this.menuOpen = false },
            }),
            m("button", {
                onclick: () => {
                    this.calculateLeadingCharacter()
                    this.menuOpen = !this.menuOpen
                    if (!this.menuOpen) focusOnInput()
                    this.menuOpenedByButton = true
                }
            }, expander(this.menuOpen)),
            this.menuOpen
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
                        tabindex: (this.items.length ? null : 0),
                        oncreate: (vnode: any) => {
                            // TODO: focus on the first matching child instead
                            const firstChild = <HTMLElement>(<HTMLElement>vnode.dom).firstChild
                            if (firstChild) {
                                firstChild.focus()
                                // firstChild.scrollIntoView(true)
                                setTimeout(() => {
                                    (<HTMLElement>vnode.dom).scrollTop = 0
                                }, 50)
                            } else {
                                (<HTMLElement>vnode.dom).focus()
                            }
                        },
                        onmouseleave: () => { this.menuOpen = false },
                        onfocusout: () => { if (!this.items.length) this.menuOpen = false },
                        onkeydown: (event: KeyboardEvent) => {
                            if (!this.items.length) {
                                this.menuOpen = false
                                if (event.key.length === 1) this.text += event.key
                                focusOnInput()
                            }
                        }
                    },
                   this.getItemsForMatch().map((item, index) => m("li.focus-bg-light-blue", {
                        tabindex: index,
                        onclick: () => {
                            this.menuOpen = false
                            this.text = this.leadingCharacter + item
                            focusOnInput()
                            this.doOnchangeCallback()
                        },
                        /*
                        onmouseover: (event: Event) {
                            const node = <HTMLElement>event.target
                            node.focus()
                        },
                        */
                        onkeydown: (event: KeyboardEvent) => {
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
                                this.menuOpen = false
                                this.text = this.leadingCharacter + item
                                focusOnInput()
                                this.doOnchangeCallback()
                            } else if (event.keyCode === 27) {
                                // escape
                                this.menuOpen = false
                                focusOnInput()
                           } else {
                                this.menuOpen = false
                                if (event.key.length === 1) this.text += event.key
                                focusOnInput()
                                return false
                            }
                            return true
                        }
                    }, item))
                )
                : []
        )
    }

    calculateLeadingCharacter() {
        const text = this.text.trim()
        if (this.ignoreLeadingCharacter) {
            this.leadingCharacter = ""
            for (let i = 0; i < this.ignoreLeadingCharacter.length; i++) {
                const c = this.ignoreLeadingCharacter[i]
                if (text.startsWith(c)) {
                    this.leadingCharacter = c
                    break
                }
            }
        }
    }

    getItemsForMatch(): string[] {
        if (this.menuOpenedByButton) return this.items
        let text = this.text.trim()
        if (this.leadingCharacter) {
            text = text.substring(1).trim()
        }
        return this.items.filter(each => each.includes(text))
    }
}
