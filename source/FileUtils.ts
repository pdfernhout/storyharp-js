import { modalPrompt } from "./ModalInputView"

// Conversion function from: http://stackoverflow.com/questions/9267899/arraybuffer-to-base64-encoded-string
function _arrayBufferToBase64(buffer: Buffer) {
    let binary = ""
    const bytes = new Uint8Array(buffer)
    const len = bytes.byteLength
    for (let i = 0; i < len; i++) {
        binary += String.fromCharCode(bytes[i])
    }
    return window.btoa(binary)
}

export class FileUtils {
    // This fileControl input node is not created via main Mithril app because stand-alone apps might want to use this too
    static fileControl: HTMLInputElement

    static callback: (a: any, b: any) => null

    static loadFromFile(convertToBase64: boolean, callback: any) {
        if (typeof convertToBase64 === "function") {
            callback = convertToBase64
            convertToBase64 = false
        }
        // Mithril can clobber the fileControl if it mounts something on the body
        // if (true && !FileUtils.fileControl) {
            const fileControl = document.createElement("input")
            FileUtils.fileControl = <any>fileControl
            fileControl.type = "file"
            fileControl.style.display = "none"
            document.body.appendChild(FileUtils.fileControl)

            fileControl.addEventListener("change", function (event: any) {
                if (event.target.files.length < 1) return
                const file = event.target.files[0]
                const reader = new FileReader()
                reader.onload = function() {
                    let contents
                    if (convertToBase64) {
                        var base64Text = _arrayBufferToBase64(reader.result)
                        contents = base64Text
                    } else {
                        contents = reader.result
                    }
                
                    if (FileUtils.callback) FileUtils.callback(file.name, contents)
                    document.body.removeChild(FileUtils.fileControl)
                }
                
                reader.onerror = function(event: any) {
                    console.error("File could not be read! Code " + event.target.error.code)
                    if (FileUtils.callback) FileUtils.callback(null, null)
                    document.body.removeChild(FileUtils.fileControl)
                }
                            
                if (convertToBase64) {
                    reader.readAsArrayBuffer(file)
                } else {
                    reader.readAsText(file)
                }
            }, false)
        // }

        FileUtils.callback = callback
        FileUtils.fileControl.click()
    }

    static saveToFile(provisionalFileName: string, fileContents: string, hiddenExtension: string, callback: any) {
        modalPrompt("Please enter a file name for saving", provisionalFileName).then(fileName => {
            if (!fileName) return

            let addedExtension = false
            if (hiddenExtension && !fileName.endsWith(hiddenExtension)) {
                fileName = fileName + hiddenExtension
                addedExtension = true
            }
            
            const downloadLink = document.createElement("a")
            downloadLink.setAttribute("href", "data:text/plain;charset=utf-8," + encodeURIComponent(fileContents))
            downloadLink.setAttribute("download", fileName)
            downloadLink.style.display = "none"
            document.body.appendChild(downloadLink)
            downloadLink.click()
            document.body.removeChild(downloadLink)
            if (addedExtension) {
                // remove the extension we added
                fileName = fileName.substring(0, fileName.length - hiddenExtension.length)
            }
            if (callback) callback(fileName)
        })
    }
}
