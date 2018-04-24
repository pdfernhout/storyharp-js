define(["require", "exports", "mithril", "./ToastView", "./ModalInputView"], function (require, exports, m, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var open = indexedDB.open("MyDatabase", 1);
    let dbFailed = false;
    let db;
    let logStore;
    const log = [];
    function openLogDBIfNeeded() {
        return new Promise((resolve, reject) => {
            if (!dbFailed && db) {
                resolve();
                return;
            }
            const openDBRequest = indexedDB.open("StoryHarp", 1);
            openDBRequest.onupgradeneeded = function () {
                const db = openDBRequest.result;
                const store = db.createObjectStore("log", { keyPath: "id", autoIncrement: true });
            };
            openDBRequest.onerror = function (event) {
                console.log("db error for log", openDBRequest.error);
                dbFailed = true;
                reject();
            };
            openDBRequest.onsuccess = function () {
                db = openDBRequest.result;
                readAllItems()
                    .then(() => resolve())
                    .catch((error) => reject(error));
            };
        });
    }
    function readAllItems() {
        return new Promise((resolve, reject) => {
            const cursorRequest = db
                .transaction(["log"], "readonly")
                .objectStore("log")
                .openCursor();
            cursorRequest.onsuccess = (event) => {
                var cursor = (event.target).result;
                if (cursor) {
                    log.push(cursor.value.text);
                    cursor.continue();
                }
                else {
                    resolve();
                }
            };
            cursorRequest.onerror = function (error) {
                reject(error);
            };
        });
    }
    function write(text) {
        return new Promise((resolve, reject) => {
            const request = db
                .transaction(["log"], "readwrite")
                .objectStore("log")
                .add({ text: text });
            request.onsuccess = function () {
                resolve();
            };
            request.onerror = function (error) {
                dbFailed = true;
                reject(error);
            };
        });
    }
    function addToLog(text) {
        if (!dbFailed && !indexedDB) {
            ToastView_1.toast("the log can't be stored on this platform");
            dbFailed = true;
        }
        if (dbFailed)
            return Promise.resolve();
        return openLogDBIfNeeded()
            .then(() => { write(text); })
            .then(() => {
            log.push(text);
            m.redraw();
        })
            .catch((error) => {
            console.log("failed to write to log", error);
            dbFailed = true;
            m.redraw();
        });
    }
    exports.addToLog = addToLog;
    function clearLog() {
        return new Promise((resolve, reject) => {
            const request = db
                .transaction(["log"], "readwrite")
                .objectStore("log")
                .clear();
            request.onsuccess = function () {
                resolve();
            };
            request.onerror = function (error) {
                dbFailed = true;
                reject(error);
            };
        });
    }
    function confirmClearLog() {
        if (dbFailed || !db)
            return ToastView_1.toast("The log is not opened");
        ModalInputView_1.modalConfirm("This will remove everything stored in the log. Proceed?").then(value => {
            if (!value)
                return;
            clearLog().then(() => {
                log.splice(0, log.length);
                ToastView_1.toast("Log cleared");
                m.redraw();
            }).catch((error) => {
                console.log("Error clearing log", error);
                ToastView_1.toast("Something went wrong clearing the log");
                m.redraw();
            });
        });
    }
    class LogView {
        view() {
            return m("div.LogView.h-100.w-100.overflow-hidden.flex.flex-column", m("div.mb2.flex-none", m("button", {
                onclick: confirmClearLog,
                disabled: dbFailed || !db,
            }, "Clear log")), dbFailed ? m("div.red.flex-none", "Something went wrong with the log") : [], m("div.h-100.w-100.overflow-auto.flex-auto", log.map(text => m("pre.pre-wrap", text))));
        }
    }
    exports.LogView = LogView;
});
