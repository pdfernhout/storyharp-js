// unit usstream

from conversion_common import *
import delphi_compatability

const usstream = usstream || {}

// Returns type: boolean
function skipToProfileSection(fileStream: TextFile, section: String) {
    let result = false
    let inputLine: String
    let stream: KfStringStream
    
    stream = KfStringStream.create
    //set separator to ']' so can read sections
    stream.separator = "]"
    result = false
    while (!UNRESOLVED.eof(fileStream)) {
        UNRESOLVED.readln(fileStream, inputLine)
        stream.onString(inputLine)
        stream.skipSpaces()
        if (stream.nextCharacter() === "[") {
            if (stream.nextToken() === section) {
                result = true
                break
            }
        }
    }
    stream.free
    return result
}


export class KfStringStream {
    source: String = ""
    remainder: String = ""
    separator: String = ""
    
    createFromString(aString: String): void {
        this.create
        this.onStringSeparator(aString, " ")
    }
    
    onString(aString: String): void {
        this.source = aString
        this.remainder = aString
    }
    
    onStringSeparator(aString: String, aSeparator: String): void {
        this.source = aString
        this.remainder = aString
        this.separator = aSeparator
    }
    
    spaceSeparator(): void {
        this.separator = " "
    }
    
    skipSpaces(): void {
        while ((UNRESOLVED.copy(this.remainder, 1, 1) === " ")) {
            this.remainder = UNRESOLVED.copy(this.remainder, 2, 255)
        }
    }
    
    nextToken(): String {
        let result = ""
        let location: long
        
        this.skipSpaces()
        location = UNRESOLVED.pos(this.separator, this.remainder)
        if (location !== 0) {
            result = UNRESOLVED.copy(this.remainder, 1, location - 1)
            this.remainder = UNRESOLVED.copy(this.remainder, location + len(this.separator), 255)
        } else {
            result = this.remainder
            this.remainder = ""
        }
        return result
    }
    
    nextInteger(): long {
        let result = 0
        let token: String
        
        token = this.nextToken()
        result = StrToIntDef(token, 0)
        return result
    }
    
    nextSingle(): float {
        let result = 0.0
        let token: String
        
        result = 0.0
        token = this.nextToken()
        try {
            result = StrToFloat(token)
        } catch (EConvertError e) {
            result = 0.0
        }
        return result
    }
    
    nextCharacter(): char {
        let result = ' '
        result = this.remainder[1]
        this.remainder = UNRESOLVED.copy(this.remainder, 2, 255)
        return result
    }
    
    //return true if next few characters match string
    match(aString: String): boolean {
        let result = false
        result = (UNRESOLVED.pos(aString, this.remainder) === 1)
        return result
    }
    
    skipWhiteSpace(): void {
        while ((this.remainder[1] === " ") || (this.remainder[1] === chr(9))) {
            //tab
            this.remainder = UNRESOLVED.copy(this.remainder, 2, 255)
        }
    }
    
    empty(): boolean {
        let result = false
        result = (this.remainder === "")
        return result
    }
    
}

