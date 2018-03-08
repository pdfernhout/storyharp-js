import * as m from "mithril"
import { int, expander } from "./common"

export class TQuickFillComboBox {
    lastMatch: string = ""
    FMustBeInList: boolean = false
    FEntryRequired: boolean = false
    Items: string[] = []
    Text: string = ""
    menuOpen = false
    menuOpenedByButton = false
    size = 20
    inputElement: HTMLInputElement
    onchangeCallback: (event: { target: HTMLInputElement }) => {}

    constructor(vnode: m.Vnode) {
        this.Text = (<any>vnode.attrs).value
        this.onchangeCallback = (<any>vnode.attrs).onchange
        this.Items = (<any>vnode.attrs).choices
        this.FMustBeInList = (<any>vnode.attrs).mustBeInList || false
        this.FEntryRequired = (<any>vnode.attrs).required || false
    }

    view(vnode: m.Vnode) {
        const extraStyling = (<any>vnode.attrs).extraStyling || ""

        return m("div.ml1.dib.relative",
            m("input" + extraStyling, {
                value: this.Text,
                // oninput: (event: { target: HTMLInputElement }) => this.Text = event.target.value,
                onchange: (event: { target: HTMLInputElement }) => {
                    this.Text = event.target.value
                    if (this.onchangeCallback) this.onchangeCallback(event)
                },
                oncreate: (vnode: any) => {
                    this.inputElement = <HTMLInputElement>(vnode.dom)
                    this.size = this.inputElement.size
                },
                onupdate: (vnode: any) => {
                    this.inputElement = <HTMLInputElement>(vnode.dom)
                    this.size = this.inputElement.size
                },
                onkeydown: (event: KeyboardEvent) => {
                    console.log("onkeydown in input", event)
                    if (event.keyCode === 13 || event.keyCode === 40) {
                        // enter or down arrow
                        this.menuOpen = true
                        this.menuOpenedByButton = false
                        this.Text = (<HTMLInputElement>event.target).value
                    } else {
                        (<any>event).redraw = false
                    }
                    return true
                },
                onfocus:() => { this.menuOpen = false },
            }),
            m("button", {
                onclick: () => {
                    this.menuOpen = !this.menuOpen
                    this.menuOpenedByButton = true
                }
            }, expander(this.menuOpen)),
            this.menuOpen
                ? m("ul.absolute.bg-light-gray.overflow-auto",
                    {
                        style: {
                            // Override Chrome user agent settings
                            "-webkit-margin-before": "0em",
                            "-webkit-margin-after": "0em",
                            "list-style-type": "none",
                            "margin": "0",
                            "padding": "0.25rem",
                            "width": "" + this.size + "rem",
                            "max-height": "10rem",
                            "box-shadow": "0px 8px 16px 0px rgba(0,0,0,0.2)",
                            "z-index": 1,
                        },
                        oncreate: (vnode: any) => {
                            const firstChild = <HTMLElement>(<HTMLElement>vnode.dom).firstChild
                            console.log("oncreate", firstChild)
                            if (firstChild) {
                                firstChild.focus()
                            }
                        },
                        onmouseleave: () => { this.menuOpen = false },
                    },
                   this.getItemsForMatch().map((item, index) => m("li.hover-bg-light-blue.focus-bg-light-blue", {
                        tabindex: index,
                        onclick: () => {
                            this.menuOpen = false
                            this.Text = item
                            if (this.onchangeCallback) this.onchangeCallback(<any>{target: {value: item}})
                        },
                        onkeydown: (event: KeyboardEvent) => {
                            if (event.keyCode === 38) {
                                // up arrow
                                const node = <HTMLElement>event.target
                                if (node.previousSibling) {
                                    (<HTMLElement>node.previousSibling).focus()
                                } else {
                                    const lastChild: HTMLElement | null = <HTMLElement>(<HTMLElement>node.parentElement).lastChild
                                    if (lastChild) lastChild.focus()
                                }
                                return false
                            } else if (event.keyCode === 40) {
                                // down arrow
                                const node = <HTMLElement>event.target
                                if (node.nextSibling) {
                                    (<HTMLElement>node.nextSibling).focus()
                                } else {
                                   const firstChild: HTMLElement | null = <HTMLElement>(<HTMLElement>node.parentElement).firstChild
                                   if (firstChild) firstChild.focus()
                                }
                                return false
                            } else if (event.keyCode === 13) {
                                // enter
                                this.menuOpen = false
                                this.Text = item
                                if (this.onchangeCallback) this.onchangeCallback(<any>{target: {value: item}})    
                            } else if (event.keyCode === 27) {
                                // escape
                                this.menuOpen = false  
                           } else {
                                this.menuOpen = false
                                if (event.key.length === 1) this.Text += event.key
                                this.inputElement.focus()
                                this.inputElement.selectionStart = this.Text.length
                                this.inputElement.selectionEnd = this.Text.length
                                return false
                            }
                            return true
                        }
                    }, item))
                )
                : []
        )
    }

    getItemsForMatch(): string[] {
        if (this.menuOpenedByButton) return this.Items
        return this.Items.filter(each => each.includes(this.Text))
    }
}
