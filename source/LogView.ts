import * as m from "mithril"

// Open (or create) the database
var open = indexedDB.open("MyDatabase", 1);

let dbFailed = false
let db: IDBDatabase
let logStore: IDBObjectStore

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
            const store = db.createObjectStore("log", { keyPath: "id", autoIncrement: true })
        }

        openDBRequest.onerror = function(event) {
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
        alert("the log can't be stored on this platform")
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
            resolve()
        };
        
        request.onerror = function(error) {
            dbFailed = true
            reject(error)      
        }
    })
}

function confirmClearLog() {
    if (dbFailed || !db) return alert("The log is not opened")
    if (!confirm("This will remove eveything stored in the log. Proceed?")) return
    clearLog().then(() => {
        log.splice(0, log.length)
        m.redraw()
        setTimeout(() => alert("Log cleared"), 50)
    }).catch((error) => {
        console.log("Error clearing log", error)
        m.redraw()
        setTimeout(() => alert("Something went wrong clearing the log"), 50)
    })
}

export class LogView {
    view() {
        return m("div.LogView.h-100.overflow-hidden",
            m("div.mb2",
                m("button", {
                    onclick: confirmClearLog,
                    disabled: dbFailed || !db,
                }, "Clear log"),
            ),
            dbFailed ? m("div.red", "Something went wrong with the log") : [],
            m("div.overflow-auto",
                {
                    style: {
                        height: "calc(100% - 2rem)"
                    }
                },
                log.map(text => m("pre.pre-wrap", text))
            )
        )
    }
}
