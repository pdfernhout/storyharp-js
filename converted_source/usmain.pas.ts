// unit usmain

from conversion_common import *
import delphi_compatability

const usmain = usmain || {}

// var
let Form1: TForm1



export class TForm1 {
    TForm1.prototype = new TForm()
    TForm1.prototype.constructor = TForm1
    
}

//$R *.DFM
