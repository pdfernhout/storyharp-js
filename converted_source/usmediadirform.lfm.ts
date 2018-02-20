
// Exported from: ../../storyharp-js/converted_source/usmediadirform.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewExtraMediaDirectoryForm() {
    return m("ExtraMediaDirectoryForm.TExtraMediaDirectoryForm",
        {
        // Left: "441",
        // Top: "140",
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
                // Left: "376",
                // Top: "4",
                // Width: "60",
                // Height: "21",
                // Default: "True",
                // TabOrder: "0",
            },
            "OK",
        ),
        m("div.Label1.TLabel",
            {
                // Left: "8",
                // Top: "8",
                // Width: "264",
                // Height: "14",
            },
            "To find a sound or music file, I will look in these places.",
        ),
        m("button.cancel.TButton",
            {
                // Left: "376",
                // Top: "27",
                // Width: "60",
                // Height: "21",
                // Cancel: "True",
                // ModalResult: "2",
                // TabOrder: "1",
            },
            "Cancel",
        ),
        m("div.Label2.TLabel",
            {
                // Left: "27",
                // Top: "30",
                // Width: "244",
                // Height: "14",
            },
            "First, in the directory the open world is in, which is",
        ),
        m("input.openWorldFileDirectory.TEdit",
            {
                // Left: "47",
                // Top: "47",
                // Width: "320",
                // Height: "22",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "3",
            },
        ),
        m("button.helpButton.TSpeedButton",
            {
                onclick: helpButtonClick,
                // Left: "376",
                // Top: "63",
                // Width: "60",
                // Height: "21",
            },
            caption("&Help"),
        ),
        m("div.Label3.TLabel",
            {
                // Left: "27",
                // Top: "75",
                // Width: "318",
                // Height: "14",
            },
            "Second, in the Extra Sound and Music Directory you specify here:",
        ),
        m("input.extraMediaDirectoryEdit.TEdit",
            {
                // Left: "47",
                // Top: "92",
                // Width: "320",
                // Height: "22",
                // TabOrder: "2",
            },
        ),
        m("div.Label4.TLabel",
            {
                // Left: "27",
                // Top: "121",
                // Width: "256",
                // Height: "14",
            },
            "Third, in the directory where the program is, which is",
        ),
        m("input.exeDirectory.TEdit",
            {
                // Left: "47",
                // Top: "141",
                // Width: "320",
                // Height: "22",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "4",
            },
        ),
        m("div.Label5.TLabel",
            {
                // Left: "27",
                // Top: "172",
                // Width: "260",
                // Height: "14",
            },
            "And finally in your Windows media directory, which is",
        ),
        m("input.windowsMediaDirectory.TEdit",
            {
                // Left: "47",
                // Top: "190",
                // Width: "320",
                // Height: "22",
                // Color: "clBtnFace",
                // ReadOnly: "True",
                // TabOrder: "5",
            },
        ),
        m("div.Label6.TLabel",
            {
                // Left: "8",
                // Top: "220",
                // Width: "343",
                // Height: "14",
            },
            "For each directory shown here, I will also look in all directories under it.",
        ),
    )
}

function CloseClick() { console.log("CloseClick") }

function helpButtonClick() { console.log("helpButtonClick") }
