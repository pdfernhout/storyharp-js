import * as m from "mithril"
import { TSDomain } from "./TSDomain"

export class ConfigForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    view() {
        const domain = this.domain
        
        return m("div.ConfigForm.h-100.w-100.overflow-auto",
            m("div", "Font Size"),
            m("select.dib.ml3.mt1.mr1",
                {
                    name: "select-font-size",
                    onchange: (event: { target: HTMLSelectElement }) => {
                        const fontSize = event.target.value
                        domain.baseFontClass = fontSize
                        localStorage.setItem("storyharp-base-font", fontSize)
                    }
                },

                m("option", { value: "f1", selected: domain.baseFontClass === "f1" }, "3.0"),
                m("option", { value: "f2", selected: domain.baseFontClass === "f2" }, "2.25"),
                m("option", { value: "f3", selected: domain.baseFontClass === "f3" }, "1.5"),
                m("option", { value: "f4", selected: domain.baseFontClass === "f4" }, "1.25"),
                m("option", { value: "f5", selected: domain.baseFontClass === "f5" }, "1.0"),
                m("option", { value: "f6", selected: domain.baseFontClass === "f6" }, "0.875"),
                m("option", { value: "f7", selected: domain.baseFontClass === "f7" }, "0.75"),
            ),
            "(times the base font size)",
            m("div.mt3", "Player Configuration"),
            m("div.dib.ml3",
                m("label.dib.mt1",
                    m("input[type=checkbox].mr1", {
                        name: "checkbox-sound",
                        checked: domain.speechSystem.optionSound || undefined,
                        onchange: (event: { target: HTMLInputElement }) => { 
                            domain.speechSystem.optionSound = event.target.checked
                            if (!domain.speechSystem.optionSound) {
                                domain.speechSystem.haltSpeechAndSoundAndMusic()
                            }
                            localStorage.setItem("storyharp-option-sound", domain.speechSystem.optionSound ? "true" : "false")
                        }
                    }),
                    "sound",
                ),
                m("label.dib.ml3.mt1",
                    m("input[type=checkbox].mr1", {
                        name: "checkbox-speech",
                        checked: domain.speechSystem.optionSpeech || undefined,
                        onchange: (event: { target: HTMLInputElement }) => { 
                            domain.speechSystem.optionSpeech = event.target.checked
                            if (!domain.speechSystem.optionSpeech) {
                                domain.speechSystem.haltSpeechAndSoundAndMusic()
                            }
                            localStorage.setItem("storyharp-option-speech", domain.speechSystem.optionSpeech ? "true" : "false")
                        }
                    }),
                    "speech",
                ),
                m("label.dib.ml3.mt1",
                    m("input[type=checkbox].mr1", {
                        name: "checkbox-pictures",
                        checked: domain.speechSystem.optionPicture || undefined,
                        onchange: (event: { target: HTMLInputElement }) => { 
                            domain.speechSystem.optionPicture = event.target.checked
                            localStorage.setItem("storyharp-option-picture", domain.speechSystem.optionPicture ? "true" : "false")
                        }
                    }),
                    "pictures",
                ),
            ),
        )
    }
}
