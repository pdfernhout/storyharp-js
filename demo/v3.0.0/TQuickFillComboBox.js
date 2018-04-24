define(["require", "exports", "mithril", "./common"], function (require, exports, m, common_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const log = (...args) => { };
    class TQuickFillComboBox {
        constructor() {
            this.lastSuppliedText = "";
            this.textValue = "";
            this.isMenuOpen = false;
            this.menuOpenedByButton = false;
            this.clientWidth = 0;
        }
        focus() {
            this.inputElement.focus();
        }
        clear() {
            this.lastSuppliedText = "";
            this.textValue = "";
            this.inputElement.value = "";
        }
        view(vnode) {
            log("===================== view ==========================");
            const suppliedText = vnode.attrs.value || "";
            log("supplied text", suppliedText);
            if (this.lastSuppliedText !== suppliedText) {
                log("suppliedTextChange", this.lastSuppliedText, suppliedText);
                this.lastSuppliedText = suppliedText;
                this.textValue = suppliedText;
            }
            log("textValue", this.textValue);
            log("isMenuOpen", this.isMenuOpen);
            const items = vnode.attrs.items || [];
            const onchangeCallback = vnode.attrs.onchange;
            const ignoreLeadingCharacter = vnode.attrs.ignoreLeadingCharacter || "";
            let leadingCharacter = "";
            const extraStyling = vnode.attrs.extraStyling || "";
            const clearOnEscape = vnode.attrs.clearOnEscape || false;
            const doOnchangeCallback = () => {
                log("doOnchangeCallback", this.textValue);
                if (onchangeCallback) {
                    onchangeCallback({ target: this.inputElement });
                }
            };
            const openMenu = (menuOpenedByButton = false) => {
                this.isMenuOpen = true;
                this.menuOpenedByButton = menuOpenedByButton;
            };
            const closeMenu = (newText = null, focusOnInput = true) => {
                log("closeMenu", newText, focusOnInput);
                if (!this.isMenuOpen)
                    return;
                this.isMenuOpen = false;
                if (newText !== null) {
                    this.textValue = newText;
                    this.inputElement.value = newText;
                    this.inputElement.selectionStart = newText.length;
                    this.inputElement.selectionEnd = newText.length;
                }
                if (focusOnInput) {
                    this.inputElement.focus();
                }
                else {
                    if (this.textValue !== this.lastSuppliedText)
                        doOnchangeCallback();
                }
            };
            const isEnoughRoomAtBottom = (threshold) => {
                const roomAtBottom = window.innerHeight - this.inputElement.getBoundingClientRect().bottom;
                if (roomAtBottom < threshold)
                    return false;
                return true;
            };
            const calculateLeadingCharacter = () => {
                const trimmedText = this.textValue.trim();
                if (ignoreLeadingCharacter) {
                    leadingCharacter = "";
                    for (let i = 0; i < ignoreLeadingCharacter.length; i++) {
                        const c = ignoreLeadingCharacter[i];
                        if (trimmedText.startsWith(c)) {
                            leadingCharacter = c;
                            break;
                        }
                    }
                }
            };
            const getItemsForMatch = (items, text, leadingCharacter) => {
                if (this.menuOpenedByButton)
                    return items;
                text = text.trim();
                if (leadingCharacter) {
                    text = text.substring(1).trim();
                }
                return items.filter(each => each.includes(text));
            };
            calculateLeadingCharacter();
            const matchingItems = this.isMenuOpen ? getItemsForMatch(items, this.textValue, leadingCharacter) : [];
            return m("div.dib.relative", {
                style: vnode.attrs.style || "",
                oncreate: (vnode) => this.clientWidth = (vnode.dom).clientWidth,
                onupdate: (vnode) => this.clientWidth = (vnode.dom).clientWidth,
            }, m("input" + extraStyling, {
                value: this.textValue,
                style: {
                    width: "calc(100% - 4rem)",
                },
                oncreate: (vnode) => {
                    this.inputElement = (vnode.dom);
                },
                oninput: (event) => {
                    log("input oninput", event.target.value, this.isMenuOpen);
                    if (this.textValue !== event.target.value) {
                        this.textValue = event.target.value;
                    }
                    else {
                        event.redraw = false;
                    }
                },
                onblur: (event) => {
                    log("input onblur");
                    if (!this.isMenuOpen && event.relatedTarget !== this.buttonElement) {
                        log("input onblur processed", this.inputElement.value, this.isMenuOpen);
                        this.textValue = this.inputElement.value;
                        if (this.lastSuppliedText !== this.textValue)
                            doOnchangeCallback();
                    }
                    else {
                        event.redraw = false;
                    }
                },
                onkeydown: (event) => {
                    log("onkeydown", event);
                    if ((event.keyCode === 40) || (event.keyCode === 38)) {
                        this.textValue = event.target.value;
                        openMenu();
                        return false;
                    }
                    else if (event.keyCode === 13) {
                        this.textValue = event.target.value;
                        doOnchangeCallback();
                        return false;
                    }
                    else if (clearOnEscape && event.keyCode === 27) {
                        this.clear();
                        return false;
                    }
                    else {
                        event.redraw = false;
                    }
                    return true;
                },
            }), m("button", {
                oncreate: (vnode) => {
                    this.buttonElement = (vnode.dom);
                },
                onclick: (event) => {
                    log("button onclick", this.isMenuOpen);
                    if (this.isMenuOpen) {
                        closeMenu();
                    }
                    else {
                        openMenu(true);
                    }
                    event.stopPropagation();
                },
                onblur: (event) => {
                    log("onblur button", event, this.isMenuOpen);
                    if (!this.isMenuOpen && event.relatedTarget !== this.inputElement) {
                        doOnchangeCallback();
                    }
                    else {
                        event.redraw = false;
                    }
                },
            }, common_1.expander(this.isMenuOpen)), this.isMenuOpen
                ? m("ul.absolute.bg-light-gray.pa2.overflow-auto", {
                    style: {
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
                    oncreate: (vnode) => {
                        this.ulElement = vnode.dom;
                        const firstChild = vnode.dom.firstChild;
                        if (firstChild) {
                            firstChild.focus();
                            setTimeout(() => {
                                vnode.dom.scrollTop = 0;
                            }, 50);
                        }
                    },
                    onmouseleave: () => {
                        log("onmouseleave");
                        closeMenu();
                    },
                }, matchingItems.length ? [] : m("li.focus-bg-light-blue", {
                    tabindex: 0,
                    onclick: (event) => {
                        log("onclick");
                        closeMenu();
                        event.stopPropagation();
                    },
                    onkeydown: (event) => {
                        log("onkeydown", event);
                        if (event.keyCode === 13) {
                            closeMenu();
                            return false;
                        }
                        else if (event.keyCode === 27) {
                            closeMenu();
                            return false;
                        }
                        else {
                            if (event.key.length === 1)
                                this.textValue += event.key;
                            closeMenu();
                            if (event.key.length === 1)
                                return false;
                        }
                        return true;
                    },
                    onblur: (event) => {
                        log("onblur", event, this.isMenuOpen);
                        if (this.isMenuOpen && event.relatedTarget !== this.buttonElement) {
                            closeMenu(null, event.relatedTarget === this.inputElement);
                        }
                        else {
                            event.redraw = false;
                        }
                    },
                }, "No matches..."), matchingItems.map((item, index) => m("li.focus-bg-light-blue", {
                    tabindex: 0,
                    onclick: (event) => {
                        log("on click in item", item);
                        closeMenu(leadingCharacter + item);
                        event.stopPropagation();
                    },
                    onblur: (event) => {
                        const relatedTarget = event.relatedTarget;
                        log("onblur in item", item, this.isMenuOpen);
                        log("relatedTarget", relatedTarget);
                        if (this.isMenuOpen && (!relatedTarget || relatedTarget.parentElement !== this.ulElement) && relatedTarget !== this.buttonElement) {
                            log("calling closemenu from item", item);
                            closeMenu(null, event.relatedTarget === this.inputElement);
                        }
                        else {
                            event.redraw = false;
                        }
                    },
                    onkeydown: (event) => {
                        log("onkeydown in item", item, event);
                        if (event.keyCode === 38) {
                            const node = event.target;
                            if (node.previousSibling) {
                                node.previousSibling.focus();
                            }
                            else {
                                const lastChild = node.parentElement.lastChild;
                                if (lastChild) {
                                    lastChild.focus();
                                }
                            }
                            return false;
                        }
                        else if (event.keyCode === 40) {
                            const node = event.target;
                            if (node.nextSibling) {
                                node.nextSibling.focus();
                            }
                            else {
                                const firstChild = node.parentElement.firstChild;
                                if (firstChild) {
                                    firstChild.focus();
                                }
                            }
                            return false;
                        }
                        else if (event.keyCode === 13) {
                            closeMenu(leadingCharacter + item);
                            return false;
                        }
                        else if (event.keyCode === 27) {
                            closeMenu();
                            return false;
                        }
                        else {
                            if (event.key.length === 1)
                                this.textValue += event.key;
                            closeMenu();
                            if (event.key.length === 1)
                                return false;
                        }
                        return true;
                    }
                }, item)))
                : []);
        }
    }
    exports.TQuickFillComboBox = TQuickFillComboBox;
});
