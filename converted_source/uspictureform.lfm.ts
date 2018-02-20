
// Exported from: ../../storyharp-js/converted_source/uspictureform.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewPictureForm() {
    return m("PictureForm.TPictureForm",
        {
        // Top: "304",
        // Left: "1837",
        // Height: "282",
        // Width: "249",
        // Caption: "StoryHarp Pictures",
        // ClientHeight: "282",
        // ClientWidth: "249",
        // Font.Height: "-11",
        // Font.Name: "Arial",
        // OnCreate: "FormCreate",
        // OnDestroy: "FormDestroy",
        // OnResize: "FormResize",
        // PixelsPerInch: "100",
        // Position: "poScreenCenter",
        // TextHeight: "14",
        // HorzScrollBar.Page: "248",
        // VertScrollBar.Page: "281",
        },
        m("TScrollBox.PictureScrollBox.TScrollBox",
            {
                // Top: "0",
                // Left: "0",
                // Height: "217",
                // Width: "237",
                // Ctl3D: "True",
                // ParentCtl3D: "False",
                // TabOrder: "1",
                // HorzScrollBar.Page: "232",
                // VertScrollBar.Page: "212",
            },
            m("img.PictureImage.TImage",
                {
                    // Top: "0",
                    // Left: "0",
                    // Height: "209",
                    // Width: "229",
                    // AutoSize: "True",
                    // Center: "True",
                    // Transparent: "True",
                },
            ),
        ),
        m("div.controlsPanel.TPanel",
            {
                // Top: "216",
                // Left: "0",
                // Height: "29",
                // Width: "237",
                // ClientHeight: "29",
                // ClientWidth: "237",
                // TabOrder: "0",
            },
            m("Group.Group.g00000002",
                m("button.FirstPictureButton.TSpeedButton",
                    {
                        onclick: FirstPictureButtonClick,
                        // Top: "2",
                        // Left: "2",
                        // Height: "25",
                        // Width: "25",
                        // Enabled: "False",
                        // Glyph.Data: "9C0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22313620313620332031222C0A222E2063204E6F6E65222C0A222C20632023303030303030222C0A222D20632023303046464646222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2C2E2E2E2E2C2E2E222C0A222E2E2E2E2E2E2E2C2C2E2E2E2C2C2E2E222C0A222E2E2E2E2E2E2C2D2C2E2E2C2D2C2E2E222C0A222E2E2E2E2E2C2D2D2C2E2C2D2D2C2E2E222C0A222E2E2E2E2C2D2D2D2C2C2D2D2D2C2E2E222C0A222E2E2E2C2D2D2D2D2C2D2D2D2D2C2E2E222C0A222E2E2C2D2D2D2D2D2C2D2D2D2D2C2E2E222C0A222E2E2E2C2D2D2D2D2C2D2D2D2D2C2E2E222C0A222E2E2E2E2C2D2D2D2C2C2D2D2D2C2E2E222C0A222E2E2E2E2E2C2D2D2C2E2C2D2D2C2E2E222C0A222E2E2E2E2E2E2C2D2C2E2E2C2D2C2E2E222C0A222E2E2E2E2E2E2E2C2C2E2E2E2C2C2E2E222C0A222E2E2E2E2E2E2E2E2C2E2E2E2E2C2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A",
                        // NumGlyphs: "0",
                    },
                ),
                m("button.PreviousPictureButton.TSpeedButton",
                    {
                        onclick: PreviousPictureButtonClick,
                        // Top: "2",
                        // Left: "30",
                        // Height: "25",
                        // Width: "25",
                        // Enabled: "False",
                        // Glyph.Data: "9C0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22313620313620332031222C0A222E2063204E6F6E65222C0A222C20632023303030303030222C0A222D20632023303046464646222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2C2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2C2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2C2D2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2C2D2D2D2D2C2E2E2E2E222C0A222E2E2E2E2E2C2D2D2D2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2C2D2D2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2C2D2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2C2D2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2C2D2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2C2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A",
                        // NumGlyphs: "0",
                    },
                ),
                m("button.NextPictureButton.TSpeedButton",
                    {
                        onclick: NextPictureButtonClick,
                        // Top: "2",
                        // Left: "58",
                        // Height: "25",
                        // Width: "25",
                        // Enabled: "False",
                        // Glyph.Data: "9C0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22313620313620332031222C0A222E2063204E6F6E65222C0A222C20632023303030303030222C0A222D20632023303046464646222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2C2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2C2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2D2C2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2D2D2C2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2D2D2D2C2E2E2E2E2E222C0A222E2E2E2E2C2D2D2D2D2C2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2D2C2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2D2C2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2D2C2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2C2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A",
                        // NumGlyphs: "0",
                    },
                ),
                m("button.LastPictureButton.TSpeedButton",
                    {
                        onclick: LastPictureButtonClick,
                        // Top: "2",
                        // Left: "86",
                        // Height: "25",
                        // Width: "25",
                        // Enabled: "False",
                        // Glyph.Data: "9C0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22313620313620332031222C0A222E2063204E6F6E65222C0A222C20632023303030303030222C0A222D20632023303046464646222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2C2E2E2E2E2C2E2E2E2E2E2E2E2E222C0A222E2E2C2C2E2E2E2C2C2E2E2E2E2E2E2E222C0A222E2E2C2D2C2E2E2C2D2C2E2E2E2E2E2E222C0A222E2E2C2D2D2C2E2C2D2D2C2E2E2E2E2E222C0A222E2E2C2D2D2D2C2C2D2D2D2C2E2E2E2E222C0A222E2E2C2D2D2D2D2C2D2D2D2D2C2E2E2E222C0A222E2E2C2D2D2D2D2C2D2D2D2D2D2C2E2E222C0A222E2E2C2D2D2D2D2C2D2D2D2D2C2E2E2E222C0A222E2E2C2D2D2D2C2C2D2D2D2C2E2E2E2E222C0A222E2E2C2D2D2C2E2C2D2D2C2E2E2E2E2E222C0A222E2E2C2D2C2E2E2C2D2C2E2E2E2E2E2E222C0A222E2E2C2C2E2E2E2C2C2E2E2E2E2E2E2E222C0A222E2E2C2E2E2E2E2C2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A",
                        // NumGlyphs: "0",
                    },
                ),
            ),
            m("div.numbersLabel.TLabel",
                {
                    // Top: "8",
                    // Left: "116",
                    // Height: "17",
                    // Width: "43",
                    // Color: "clNone",
                    // ParentColor: "False",
                },
                "3 of 4",
            ),
        ),
        m("textarea.CaptionRichEdit.TSynEdit",
            {
                // Top: "248",
                // Left: "-32",
                // Height: "150",
                // Width: "200",
                // Font.Height: "-16",
                // Font.Name: "courier",
                // Name: "CaptionRichEdit",
                // ParentColor: "False",
                // TabOrder: "2",
                // BookMarkOptions.OnChange: "nil",
                // Gutter.OnChange: "nil",
                // Gutter.CodeFoldingWidth: "14",
                // Keystrokes: [{"Command":"3","ShortCut":"38"},{"Command":"103","ShortCut":"8230"},{"Command":"211","ShortCut":"16422"},{"Command":"4","ShortCut":"40"},{"Command":"104","ShortCut":"8232"},{"Command":"212","ShortCut":"16424"},{"Command":"1","ShortCut":"37"},{"Command":"101","ShortCut":"8229"},{"Command":"5","ShortCut":"16421"},{"Command":"105","ShortCut":"24613"},{"Command":"2","ShortCut":"39"},{"Command":"102","ShortCut":"8231"},{"Command":"6","ShortCut":"16423"},{"Command":"106","ShortCut":"24615"},{"Command":"10","ShortCut":"34"},{"Command":"110","ShortCut":"8226"},{"Command":"14","ShortCut":"16418"},{"Command":"114","ShortCut":"24610"},{"Command":"9","ShortCut":"33"},{"Command":"109","ShortCut":"8225"},{"Command":"13","ShortCut":"16417"},{"Command":"113","ShortCut":"24609"},{"Command":"7","ShortCut":"36"},{"Command":"107","ShortCut":"8228"},{"Command":"15","ShortCut":"16420"},{"Command":"115","ShortCut":"24612"},{"Command":"8","ShortCut":"35"},{"Command":"108","ShortCut":"8227"},{"Command":"16","ShortCut":"16419"},{"Command":"116","ShortCut":"24611"},{"Command":"223","ShortCut":"45"},{"Command":"201","ShortCut":"16429"},{"Command":"604","ShortCut":"8237"},{"Command":"502","ShortCut":"46"},{"Command":"603","ShortCut":"8238"},{"Command":"501","ShortCut":"8"},{"Command":"501","ShortCut":"8200"},{"Command":"504","ShortCut":"16392"},{"Command":"601","ShortCut":"32776"},{"Command":"602","ShortCut":"40968"},{"Command":"509","ShortCut":"13"},{"Command":"199","ShortCut":"16449"},{"Command":"201","ShortCut":"16451"},{"Command":"610","ShortCut":"24649"},{"Command":"509","ShortCut":"16461"},{"Command":"510","ShortCut":"16462"},{"Command":"503","ShortCut":"16468"},{"Command":"611","ShortCut":"24661"},{"Command":"604","ShortCut":"16470"},{"Command":"603","ShortCut":"16472"},{"Command":"507","ShortCut":"16473"},{"Command":"506","ShortCut":"24665"},{"Command":"601","ShortCut":"16474"},{"Command":"602","ShortCut":"24666"},{"Command":"301","ShortCut":"16432"},{"Command":"302","ShortCut":"16433"},{"Command":"303","ShortCut":"16434"},{"Command":"304","ShortCut":"16435"},{"Command":"305","ShortCut":"16436"},{"Command":"306","ShortCut":"16437"},{"Command":"307","ShortCut":"16438"},{"Command":"308","ShortCut":"16439"},{"Command":"309","ShortCut":"16440"},{"Command":"310","ShortCut":"16441"},{"Command":"351","ShortCut":"24624"},{"Command":"352","ShortCut":"24625"},{"Command":"353","ShortCut":"24626"},{"Command":"354","ShortCut":"24627"},{"Command":"355","ShortCut":"24628"},{"Command":"356","ShortCut":"24629"},{"Command":"357","ShortCut":"24630"},{"Command":"358","ShortCut":"24631"},{"Command":"359","ShortCut":"24632"},{"Command":"360","ShortCut":"24633"},{"Command":"231","ShortCut":"24654"},{"Command":"232","ShortCut":"24643"},{"Command":"233","ShortCut":"24652"},{"Command":"612","ShortCut":"9"},{"Command":"613","ShortCut":"8201"},{"Command":"250","ShortCut":"24642"}],
                // Lines.Strings: ["CaptionRichEdit"],
                // SelectedColor.OnChange: "nil",
                // Cursor: "crIBeam",
            },
        ),
    )
}

function FirstPictureButtonClick() { console.log("FirstPictureButtonClick") }

function LastPictureButtonClick() { console.log("LastPictureButtonClick") }

function NextPictureButtonClick() { console.log("NextPictureButtonClick") }

function PreviousPictureButtonClick() { console.log("PreviousPictureButtonClick") }
