
// Exported from: ../../storyharp-js/converted_source/uschangelog.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewChangeLogForm() {
    return m("ChangeLogForm.TChangeLogForm",
        {
        // ActiveControl: "CopySelectedTextButton",
        // Caption: "Log file",
        // ClientHeight: "240",
        // ClientWidth: "320",
        // Font.Height: "-11",
        // Font.Name: "Arial",
        // KeyPreview: "True",
        // OnCreate: "FormCreate",
        // OnKeyUp: "FormKeyUp",
        // PixelsPerInch: "100",
        // Position: "poScreenCenter",
        // TextHeight: "14",
        // HorzScrollBar.Page: "319",
        // VertScrollBar.Page: "239",
        // Left: "387",
        // Height: "240",
        // Top: "632",
        // Width: "320",
        },
        m("div.bottomPanel.TPanel",
            {
                // Align: "alBottom",
                // BevelInner: "bvRaised",
                // BevelOuter: "bvLowered",
                // ClientHeight: "30",
                // ClientWidth: "320",
                // TabOrder: "0",
                // OnResize: "bottomPanelResize",
                // Height: "30",
                // Top: "210",
                // Width: "320",
            },
            m("button.CopySelectedTextButton.TButton",
                {
                    onclick: CopySelectedTextButtonClick,
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "0",
                    // Left: "6",
                    // Height: "21",
                    // Top: "5",
                    // Width: "87",
                },
                caption("&Copy selection"),
            ),
            m("button.UpdateButton.TButton",
                {
                    onclick: UpdateButtonClick,
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "1",
                    // Left: "96",
                    // Height: "21",
                    // Top: "5",
                    // Width: "55",
                },
                caption("&Reload"),
            ),
            m("button.changeLogFile.TButton",
                {
                    onclick: changeLogFileClick,
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "2",
                    // Left: "200",
                    // Height: "21",
                    // Top: "5",
                    // Width: "56",
                },
                caption("Ch&ange..."),
            ),
            m("button.clearLogFile.TButton",
                {
                    onclick: clearLogFileClick,
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "3",
                    // Left: "154",
                    // Height: "21",
                    // Top: "5",
                    // Width: "43",
                },
                caption("C&lear"),
            ),
            m("button.helpButton.TButton",
                {
                    onclick: helpButtonClick,
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "4",
                    // Left: "259",
                    // Height: "21",
                    // Top: "5",
                    // Width: "43",
                },
                caption("&Help"),
            ),
        ),
        m("textarea.LogContentsRichEdit.TSynEdit",
            {
                // Font.Height: "-16",
                // Font.Name: "courier",
                // Height: "200",
                // Name: "LogContentsRichEdit",
                // ParentColor: "False",
                // TabOrder: "1",
                // Width: "312",
                // BookMarkOptions.OnChange: "nil",
                // Gutter.OnChange: "nil",
                // Gutter.CodeFoldingWidth: "14",
                // Keystrokes: [{"Command":"3","ShortCut":"38"},{"Command":"103","ShortCut":"8230"},{"Command":"211","ShortCut":"16422"},{"Command":"4","ShortCut":"40"},{"Command":"104","ShortCut":"8232"},{"Command":"212","ShortCut":"16424"},{"Command":"1","ShortCut":"37"},{"Command":"101","ShortCut":"8229"},{"Command":"5","ShortCut":"16421"},{"Command":"105","ShortCut":"24613"},{"Command":"2","ShortCut":"39"},{"Command":"102","ShortCut":"8231"},{"Command":"6","ShortCut":"16423"},{"Command":"106","ShortCut":"24615"},{"Command":"10","ShortCut":"34"},{"Command":"110","ShortCut":"8226"},{"Command":"14","ShortCut":"16418"},{"Command":"114","ShortCut":"24610"},{"Command":"9","ShortCut":"33"},{"Command":"109","ShortCut":"8225"},{"Command":"13","ShortCut":"16417"},{"Command":"113","ShortCut":"24609"},{"Command":"7","ShortCut":"36"},{"Command":"107","ShortCut":"8228"},{"Command":"15","ShortCut":"16420"},{"Command":"115","ShortCut":"24612"},{"Command":"8","ShortCut":"35"},{"Command":"108","ShortCut":"8227"},{"Command":"16","ShortCut":"16419"},{"Command":"116","ShortCut":"24611"},{"Command":"223","ShortCut":"45"},{"Command":"201","ShortCut":"16429"},{"Command":"604","ShortCut":"8237"},{"Command":"502","ShortCut":"46"},{"Command":"603","ShortCut":"8238"},{"Command":"501","ShortCut":"8"},{"Command":"501","ShortCut":"8200"},{"Command":"504","ShortCut":"16392"},{"Command":"601","ShortCut":"32776"},{"Command":"602","ShortCut":"40968"},{"Command":"509","ShortCut":"13"},{"Command":"199","ShortCut":"16449"},{"Command":"201","ShortCut":"16451"},{"Command":"610","ShortCut":"24649"},{"Command":"509","ShortCut":"16461"},{"Command":"510","ShortCut":"16462"},{"Command":"503","ShortCut":"16468"},{"Command":"611","ShortCut":"24661"},{"Command":"604","ShortCut":"16470"},{"Command":"603","ShortCut":"16472"},{"Command":"507","ShortCut":"16473"},{"Command":"506","ShortCut":"24665"},{"Command":"601","ShortCut":"16474"},{"Command":"602","ShortCut":"24666"},{"Command":"301","ShortCut":"16432"},{"Command":"302","ShortCut":"16433"},{"Command":"303","ShortCut":"16434"},{"Command":"304","ShortCut":"16435"},{"Command":"305","ShortCut":"16436"},{"Command":"306","ShortCut":"16437"},{"Command":"307","ShortCut":"16438"},{"Command":"308","ShortCut":"16439"},{"Command":"309","ShortCut":"16440"},{"Command":"310","ShortCut":"16441"},{"Command":"351","ShortCut":"24624"},{"Command":"352","ShortCut":"24625"},{"Command":"353","ShortCut":"24626"},{"Command":"354","ShortCut":"24627"},{"Command":"355","ShortCut":"24628"},{"Command":"356","ShortCut":"24629"},{"Command":"357","ShortCut":"24630"},{"Command":"358","ShortCut":"24631"},{"Command":"359","ShortCut":"24632"},{"Command":"360","ShortCut":"24633"},{"Command":"231","ShortCut":"24654"},{"Command":"232","ShortCut":"24643"},{"Command":"233","ShortCut":"24652"},{"Command":"612","ShortCut":"9"},{"Command":"613","ShortCut":"8201"},{"Command":"250","ShortCut":"24642"}],
                // Lines.Strings: ["LogContentsRichEdit"],
                // SelectedColor.OnChange: "nil",
                // Cursor: "crIBeam",
                // Left: "8",
                // Top: "8",
            },
        ),
        m("TOpenDialog.OpenDialog.TOpenDialog",
            {
                // Options: "[ofOverwritePrompt, ofHideReadOnly]",
                // Title: "Open existing file",
                // FilterIndex: "0",
                // left: "372",
                // top: "188",
            },
        ),
    )
}

function CopySelectedTextButtonClick() { console.log("CopySelectedTextButtonClick") }

function UpdateButtonClick() { console.log("UpdateButtonClick") }

function changeLogFileClick() { console.log("changeLogFileClick") }

function clearLogFileClick() { console.log("clearLogFileClick") }

function helpButtonClick() { console.log("helpButtonClick") }
