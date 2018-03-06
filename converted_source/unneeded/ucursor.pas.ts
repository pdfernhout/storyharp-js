// unit ucursor

from conversion_common import *
import delphi_compatability

// const
const crMagMinus = 1
const crMagPlus = 2
const crScroll = 3
const crRotate = 4
const crDragPlant = 5
const crAddTriangle = 6
const crDeleteTriangle = 7
const crFlipTriangle = 8
const crDragPoint = 9


// var
let waitState: int


//$R CURSOR32
function cursor_initializeWait(): void {
    waitState = 0
}

function cursor_startWait(): void {
    waitState += 1
    delphi_compatability.Screen.Cursor = delphi_compatability.crHourGlass
}

function cursor_startWaitIfNotWaiting(): void {
    if (waitState === 0) {
        cursor_startWait()
    }
}

function cursor_stopWait(): void {
    if (waitState > 0) {
        waitState -= 1
        if (waitState === 0) {
            delphi_compatability.Screen.Cursor = delphi_compatability.crDefault
        }
    }
}

//Note:	You don't need to call the WinAPI function DestroyCursor when you are finished using the custom
//cursor; Delphi does this automatically. 
function cursor_loadCustomCursors(): void {
    delphi_compatability.Screen.Cursors[crMagMinus] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "MAGMINUS")
    delphi_compatability.Screen.Cursors[crMagPlus] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "MAGPLUS")
    delphi_compatability.Screen.Cursors[crScroll] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "SCROLL")
    delphi_compatability.Screen.Cursors[crRotate] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "ROTATE")
    delphi_compatability.Screen.Cursors[crDragPlant] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "DRAGPLANT")
    delphi_compatability.Screen.Cursors[crAddTriangle] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "ADDTRIANGLEPOINT")
    delphi_compatability.Screen.Cursors[crDeleteTriangle] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "DELETETRIANGLE")
    delphi_compatability.Screen.Cursors[crFlipTriangle] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "FLIPTRIANGLE")
    delphi_compatability.Screen.Cursors[crDragPoint] = UNRESOLVED.LoadCursor(UNRESOLVED.HInstance, "DRAGPOINT")
}


