import * as m from "mithril"
import { toast } from "./ToastView"
import { modalConfirm } from "./ModalInputView"

let dbFailed = false
let db: IDBDatabase

const log: string[] = []

function openLogDBIfNeeded(): Promise<void> {
    return new Promise((resolve, reject) => {
        if (!dbFailed && db) {
            resolve()
            return
        }

        const openDBRequest: IDBOpenDBRequest = indexedDB.open("StoryHarp", 1)

        openDBRequest.onupgradeneeded = function() {
            const db = openDBRequest.result
            db.createObjectStore("log", { keyPath: "id", autoIncrement: true })
        }

        openDBRequest.onerror = function() {
            console.log("db error for log", openDBRequest.error)
            dbFailed = true
            reject()
        }

        openDBRequest.onsuccess = function() {
            db = openDBRequest.result;
            readAllItems()
                .then(() => resolve())
                .catch((error) => reject(error))
        }
    })
}

function readAllItems(): Promise<void> {
    return new Promise((resolve, reject) => {
        const cursorRequest = db
            .transaction(["log"], "readonly")
            .objectStore("log")
            .openCursor()
                                
        cursorRequest.onsuccess = (event) => {
            var cursor = (<any>(event.target)).result
            if (cursor) {
                log.push(cursor.value.text)
                cursor.continue()
            } else {
                resolve()
            }
        }
        
        cursorRequest.onerror = function(error) {
            reject(error)      
        }
    })
}

function write(text: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const request = db
            .transaction(["log"], "readwrite")
            .objectStore("log")
            .add({ text: text })
                                
        request.onsuccess = function() {
            resolve()
        };
        
        request.onerror = function(error) {
            dbFailed = true
            reject(error)      
        }
    })
}

// Assuming these log writing promises will resolve sequenctially -- but it is not a big deal if they don't
export function addToLog(text: string): Promise<void> {
    if (!dbFailed && !indexedDB) {
        toast("the log can't be stored on this platform")
        dbFailed = true
    }
    if (dbFailed) return Promise.resolve()
    return openLogDBIfNeeded()
        .then(() => { write(text) })
        .then(() => {
            log.push(text)
            m.redraw()
        } )
        .catch((error) => {
            console.log("failed to write to log", error)
            dbFailed = true
            m.redraw()
        })
}

function clearLog() {
    return new Promise((resolve, reject) => {
        const request = db
            .transaction(["log"], "readwrite")
            .objectStore("log")
            .clear()
                                
        request.onsuccess = function() {
            resolve(undefined)
        };
        
        request.onerror = function(error) {
            dbFailed = true
            reject(error)      
        }
    })
}

function confirmClearLog() {
    if (dbFailed || !db) return toast("The log is not opened")
    modalConfirm("This will remove everything stored in the log. Proceed?").then(value => {
        if (!value) return
        clearLog().then(() => {
            log.splice(0, log.length)
            toast("Log cleared")
            m.redraw()
        }).catch((error) => {
            console.log("Error clearing log", error)
            toast("Something went wrong clearing the log")
            m.redraw()
        })
    })
}

export class LoggingView {
    view() {
        return m("div.LoggingView.h-100.w-100.overflow-hidden.flex.flex-column",
            m("div.mb2.flex-none",
                m("button", {
                    onclick: confirmClearLog,
                    disabled: dbFailed || !db,
                }, "Clear log"),
            ),
            dbFailed ? m("div.red.flex-none", "Something went wrong with the log") : [],
            m("div.h-100.w-100.overflow-auto.flex-auto",
                log.map(text => m("pre.pre-wrap", text))
            )
        )
    }
}
