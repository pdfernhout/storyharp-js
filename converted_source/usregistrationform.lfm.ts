
// Exported from: ../../storyharp-js/converted_source/usregistrationform.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewRegistrationForm() {
    return m("RegistrationForm.TRegistrationForm",
        {
        // Top: "209",
        // Left: "492",
        // Height: "439",
        // Width: "561",
        // ActiveControl: "UserNameEdit",
        // BorderStyle: "bsDialog",
        // Caption: "Registration",
        // ClientHeight: "439",
        // ClientWidth: "561",
        // Font.Height: "-13",
        // Font.Name: "MS Sans Serif",
        // OnActivate: "FormActivate",
        // PixelsPerInch: "100",
        // Position: "poScreenCenter",
        // TextHeight: "16",
        // HorzScrollBar.Page: "560",
        // VertScrollBar.Page: "438",
        },
        m("textarea.DisplayMemo.TMemo",
            {
                // Top: "12",
                // Left: "16",
                // Height: "205",
                // Width: "485",
                // Lines.Strings: ["This fully functional evaluation copy may be used for evaluation purposes only.","If you use this software for purposes other than evaluation, ","you are legally required to register it. ","As a special exception, if you use a copy of this program ","only to play stories created by others, (such as using the /P command line ","option), you do not need to register that copy.","Under no circumstances are you licensed to distribute output created ","by an unregistered copy of this program.","This is a summary only. See the accompanying license.txt file for exact details."],
                // ReadOnly: "True",
                // TabOrder: "4",
            },
        ),
        m("button.CancelButton.TButton",
            {
                onclick: CancelButtonClick,
                // Top: "228",
                // Left: "412",
                // Height: "25",
                // Width: "103",
                // BorderSpacing.InnerBorder: "2",
                // TabOrder: "3",
            },
            "Cancel",
        ),
        m("Group.Group.g00000232",
            m("div.DateInstalledLabel.TLabel",
                {
                    // Top: "232",
                    // Left: "20",
                    // Height: "17",
                    // Width: "134",
                    // Color: "clNone",
                    // ParentColor: "False",
                    // Visible: "False",
                },
                "DateInstalledLabel",
            ),
            m("div.DaysSinceInstallationLabel.TLabel",
                {
                    // Top: "232",
                    // Left: "220",
                    // Height: "17",
                    // Width: "193",
                    // Color: "clNone",
                    // ParentColor: "False",
                    // Visible: "False",
                },
                "DaysSinceInstallationLabel",
            ),
        ),
        m("input.UserNameEdit.TEdit",
            {
                // Top: "264",
                // Left: "124",
                // Height: "24",
                // Width: "217",
                // OnKeyPress: "UserNameEditKeyPress",
                // TabOrder: "0",
            },
        ),
        m("div.Label1.TLabel",
            {
                // Top: "268",
                // Left: "48",
                // Height: "17",
                // Width: "81",
                // Color: "clNone",
                // ParentColor: "False",
            },
            "User name",
        ),
        m("button.EnableEditorButton.TButton",
            {
                onclick: EnableEditorButtonClick,
                // Top: "276",
                // Left: "408",
                // Height: "25",
                // Width: "107",
                // BorderSpacing.InnerBorder: "2",
                // TabOrder: "5",
            },
            "Enable editor",
        ),
        m("input.RegistrationCodeEdit.TEdit",
            {
                // Top: "300",
                // Left: "124",
                // Height: "24",
                // Width: "217",
                // OnKeyPress: "RegistrationCodeEditKeyPress",
                // TabOrder: "1",
            },
        ),
        m("div.Label2.TLabel",
            {
                // Top: "304",
                // Left: "12",
                // Height: "17",
                // Width: "127",
                // Color: "clNone",
                // ParentColor: "False",
            },
            "Registration code",
        ),
        m("button.OKButton.TButton",
            {
                onclick: OKButtonClick,
                // Top: "308",
                // Left: "408",
                // Height: "25",
                // Width: "109",
                // BorderSpacing.InnerBorder: "2",
                // TabOrder: "2",
            },
            "Register",
        ),
        m("Group.Group.g00000336",
            m("button.AbortButton.TButton",
                {
                    // Top: "336",
                    // Left: "324",
                    // Height: "25",
                    // Width: "75",
                    // BorderSpacing.InnerBorder: "2",
                    // Enabled: "False",
                    // TabOrder: "7",
                },
                "Abort",
            ),
            m("button.OnlineRegistrationButton.TButton",
                {
                    onclick: OnlineRegistrationButtonClick,
                    // Top: "336",
                    // Left: "408",
                    // Height: "25",
                    // Width: "109",
                    // BorderSpacing.InnerBorder: "2",
                    // TabOrder: "6",
                },
                "Pay now online",
            ),
        ),
    )
}

function CancelButtonClick() { console.log("CancelButtonClick") }

function EnableEditorButtonClick() { console.log("EnableEditorButtonClick") }

function OKButtonClick() { console.log("OKButtonClick") }

function OnlineRegistrationButtonClick() { console.log("OnlineRegistrationButtonClick") }
