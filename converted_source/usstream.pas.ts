// unit usstream

from conversion_common import *
import delphi_compatability

const usstream = usstream || {}

function skipToProfileSection(fileStream: TextFile, section: string): boolean {
    let result = false
    let inputLine: string
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
    source: string = ""
    remainder: string = ""
    separator: string = ""
    
    createFromString(aString: string): void {
        this.create
        this.onStringSeparator(aString, " ")
    }
    
    onString(aString: string): void {
        this.source = aString
        this.remainder = aString
    }
    
    onStringSeparator(aString: string, aSeparator: string): void {
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
    
    nextToken(): string {
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
        let token: string
        
        token = this.nextToken()
        result = StrToIntDef(token, 0)
        return result
    }
    
    nextSingle(): float {
        let result = 0.0
        let token: string
        
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
    match(aString: string): boolean {
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

