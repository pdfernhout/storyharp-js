
// Exported from: ../../storyharp-js/converted_source/usmediadirform.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewExtraMediaDirectoryForm() {
    return m("ExtraMediaDirectoryForm.TExtraMediaDirectoryForm",
        {
        // Top: "140",
        // Left: "441",
        // Height: "0",
        // Width: "0",
        // AutoScroll: "False",
        // Caption: "Extra Sound and Music Directory",
        // ClientHeight: "241",
        // ClientWidth: "439",
        // Font.Charset: "DEFAULT_CHARSET",
        // Font.Color: "clWindowText",
        // Font.Height: "-11",
        // Font.Name: "Arial",
        // Font.Style: "[]",
        // Position: "poScreenCenter",
        // OnActivate: "FormActivate",
        // PixelsPerInch: "96",
        // TextHeight: "14",
        },
        m("button.Close.TButton",
            {
                onclick: CloseClick,
                // Top: "4",
                // Left: "376",
                // Height: "21",
                // Width: "60",
                // Default: "True",
                // TabOrder: "0",
            },
            "OK",
        ),
        m("div.Label1.TLabel",
            {
                // Top: "8",
                // Left: "8",
                // Height: "14",
                // Width: "264",
            },
            "To find a sound or music file, I will look in these places.",
        ),
        m("button.cancel.TButton",
            {
                // Top: "27",
                // Left: "376",
                // Height: "21",
                // Width: "60",
                // Cancel: "True",
                // ModalResult: "2",
                // TabOrder: "1",
            },
            "Cancel",
        ),
        m("div.Label2.TLabel",
            {
                // Top: "30",
                // Left: "27",
                // Height: "14",
                // Width: "244",
            },
            "First, in the directory the open world is in, which is",
        ),
        m("input.openWorldFileDirectory.TEdit",
            {
                // Top: "47",
                // Left: "47",
                // Height: "22",
                // Width: "320",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "3",
            },
        ),
        m("button.helpButton.TSpeedButton",
            {
                onclick: helpButtonClick,
                // Top: "63",
                // Left: "376",
                // Height: "21",
                // Width: "60",
            },
            caption("&Help"),
        ),
        m("div.Label3.TLabel",
            {
                // Top: "75",
                // Left: "27",
                // Height: "14",
                // Width: "318",
            },
            "Second, in the Extra Sound and Music Directory you specify here:",
        ),
        m("input.extraMediaDirectoryEdit.TEdit",
            {
                // Top: "92",
                // Left: "47",
                // Height: "22",
                // Width: "320",
                // TabOrder: "2",
            },
        ),
        m("div.Label4.TLabel",
            {
                // Top: "121",
                // Left: "27",
                // Height: "14",
                // Width: "256",
            },
            "Third, in the directory where the program is, which is",
        ),
        m("input.exeDirectory.TEdit",
            {
                // Top: "141",
                // Left: "47",
                // Height: "22",
                // Width: "320",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "4",
            },
        ),
        m("div.Label5.TLabel",
            {
                // Top: "172",
                // Left: "27",
                // Height: "14",
                // Width: "260",
            },
            "And finally in your Windows media directory, which is",
        ),
        m("input.windowsMediaDirectory.TEdit",
            {
                // Top: "190",
                // Left: "47",
                // Height: "22",
                // Width: "320",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "5",
            },
        ),
        m("div.Label6.TLabel",
            {
                // Top: "220",
                // Left: "8",
                // Height: "14",
                // Width: "343",
            },
            "For each directory shown here, I will also look in all directories under it.",
        ),
    )
}

function CloseClick() { console.log("CloseClick") }

function helpButtonClick() { console.log("helpButtonClick") }
