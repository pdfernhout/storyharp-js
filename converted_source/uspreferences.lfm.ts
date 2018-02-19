
// Exported from: ../../storyharp-js/converted_source/uspreferences.lfm

import { m } from "mithril"
import { caption } from "common"

export function viewPreferencesForm() {
    return m("PreferencesForm.TPreferencesForm",
        {
        // Left: "174",
        // Top: "399",
        // BorderStyle: "bsDialog",
        // Caption: "Editor Preferences",
        // ClientHeight: "323",
        // ClientWidth: "363",
        // Font.Charset: "DEFAULT_CHARSET",
        // Font.Color: "clWindowText",
        // Font.Height: "-11",
        // Font.Name: "Arial",
        // Font.Style: "[]",
        // Position: "poScreenCenter",
        // OnActivate: "FormActivate",
        // PixelsPerInch: "96",
        // TextHeight: "13",
        },
        m("button.helpButton.TSpeedButton",
            {
                onclick: helpButtonClick,
                // Left: "299",
                // Top: "63",
                // Width: "60",
                // Height: "21",
            },
            caption("&Help"),
        ),
        m("button.Close.TButton",
            {
                // Left: "299",
                // Top: "4",
                // Width: "60",
                // Height: "21",
                // Default: "True",
                // ModalResult: "1",
                // TabOrder: "0",
            },
            "OK",
        ),
        m("button.cancel.TButton",
            {
                // Left: "299",
                // Top: "27",
                // Width: "60",
                // Height: "21",
                // Cancel: "True",
                // ModalResult: "2",
                // TabOrder: "1",
            },
            "Cancel",
        ),
        m("div.Panel1.TPanel",
            {
                // Left: "4",
                // Top: "4",
                // Width: "289",
                // Height: "313",
                // BevelInner: "bvRaised",
                // BevelOuter: "bvLowered",
                // TabOrder: "2",
            },
            m("div.Label1.TLabel",
                {
                    // Left: "12",
                    // Top: "52",
                    // Width: "42",
                    // Height: "13",
                    // FocusControl: "changeMapFont",
                },
                caption("&Map font"),
            ),
            m("div.Label3.TLabel",
                {
                    // Left: "12",
                    // Top: "185",
                    // Width: "132",
                    // Height: "13",
                    // FocusControl: "changeTextColor",
                },
                caption("Te&xt color for selected items"),
            ),
            m("div.Label2.TLabel",
                {
                    // Left: "12",
                    // Top: "141",
                    // Width: "169",
                    // Height: "13",
                    // FocusControl: "changeBackgroundColor",
                },
                caption("Background &color for selected items"),
            ),
            m("div.Label4.TLabel",
                {
                    // Left: "12",
                    // Top: "8",
                    // Width: "48",
                    // Height: "13",
                    // FocusControl: "changeTableFont",
                },
                caption("&Table font"),
            ),
            m("div.Label5.TLabel",
                {
                    // Left: "12",
                    // Top: "96",
                    // Width: "59",
                    // Height: "13",
                    // FocusControl: "changeBrowserFont",
                },
                caption("&Browser font"),
            ),
            m("div.Label6.TLabel",
                {
                    // Left: "12",
                    // Top: "229",
                    // Width: "150",
                    // Height: "13",
                    // FocusControl: "ChangeMapCommandsColor",
                },
                caption("Text color for c&ommands in map"),
            ),
            m("input.tableFontNameEdit.TEdit",
                {
                    // Left: "32",
                    // Top: "24",
                    // Width: "169",
                    // Height: "21",
                    // Color: "clBtnFace",
                    // ReadOnly: "True",
                    // TabOrder: "0",
                },
            ),
            m("button.changeTableFont.TButton",
                {
                    onclick: changeTableFontClick,
                    // Left: "208",
                    // Top: "24",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "1",
                },
                "Change...",
            ),
            m("div.backgroundColorPanel.TPanel",
                {
                    // Left: "32",
                    // Top: "157",
                    // Width: "53",
                    // Height: "21",
                    // BevelInner: "bvLowered",
                    // BevelOuter: "bvLowered",
                    // Color: "clYellow",
                    // TabOrder: "2",
                },
            ),
            m("div.textColorPanel.TPanel",
                {
                    // Left: "32",
                    // Top: "201",
                    // Width: "53",
                    // Height: "21",
                    // BevelInner: "bvLowered",
                    // BevelOuter: "bvLowered",
                    // Color: "clBlack",
                    // TabOrder: "3",
                },
            ),
            m("label.checkbox-inline.showMapCommands.TCheckBox", m("input[type=checkbox]",
                {
                    onclick: showMapCommandsClick,
                    // Left: "12",
                    // Top: "272",
                    // Width: "201",
                    // Height: "17",
                    // TabOrder: "4",
                },),
                caption("Show map &commands with prefix \">\""),
            ),
            m("button.changeBackgroundColor.TButton",
                {
                    onclick: changeBackgroundColorClick,
                    // Left: "92",
                    // Top: "158",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "5",
                },
                "Change...",
            ),
            m("button.changeTextColor.TButton",
                {
                    onclick: changeTextColorClick,
                    // Left: "92",
                    // Top: "202",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "6",
                },
                "Change...",
            ),
            m("input.mapFontNameEdit.TEdit",
                {
                    // Left: "32",
                    // Top: "68",
                    // Width: "169",
                    // Height: "21",
                    // Color: "clBtnFace",
                    // ReadOnly: "True",
                    // TabOrder: "7",
                },
            ),
            m("button.changeMapFont.TButton",
                {
                    onclick: changeMapFontClick,
                    // Left: "208",
                    // Top: "68",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "8",
                },
                "Change...",
            ),
            m("input.browserFontNameEdit.TEdit",
                {
                    // Left: "32",
                    // Top: "112",
                    // Width: "169",
                    // Height: "21",
                    // Color: "clBtnFace",
                    // ReadOnly: "True",
                    // TabOrder: "9",
                },
            ),
            m("button.changeBrowserFont.TButton",
                {
                    onclick: changeBrowserFontClick,
                    // Left: "208",
                    // Top: "112",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "10",
                },
                "Change...",
            ),
            m("div.mapCommandsColorPanel.TPanel",
                {
                    // Left: "32",
                    // Top: "245",
                    // Width: "53",
                    // Height: "21",
                    // BevelInner: "bvLowered",
                    // BevelOuter: "bvLowered",
                    // Color: "clBlue",
                    // TabOrder: "11",
                },
            ),
            m("button.ChangeMapCommandsColor.TButton",
                {
                    onclick: ChangeMapCommandsColorClick,
                    // Left: "92",
                    // Top: "246",
                    // Width: "69",
                    // Height: "21",
                    // TabOrder: "12",
                },
                "Change...",
            ),
            m("label.checkbox-inline.symbolButtons.TCheckBox", m("input[type=checkbox]",
                {
                    onclick: symbolButtonsClick,
                    // Left: "12",
                    // Top: "292",
                    // Width: "233",
                    // Height: "17",
                    // TabOrder: "13",
                },),
                caption("Use &symbols instead of pictures on buttons"),
            ),
        ),
        m("TColorDialog.ColorDialog.TColorDialog",
            {
                // Left: "308",
                // Top: "104",
            },
        ),
        m("TFontDialog.FontDialog.TFontDialog",
            {
                // Font.Charset: "DEFAULT_CHARSET",
                // Font.Color: "clWindowText",
                // Font.Height: "-11",
                // Font.Name: "Arial",
                // Font.Style: "[]",
                // MinFontSize: "0",
                // MaxFontSize: "0",
                // Options: "[fdForceFontExist]",
                // Left: "308",
                // Top: "136",
            },
        ),
    )
}

function ChangeMapCommandsColorClick() { console.log("ChangeMapCommandsColorClick") }

function changeBackgroundColorClick() { console.log("changeBackgroundColorClick") }

function changeBrowserFontClick() { console.log("changeBrowserFontClick") }

function changeMapFontClick() { console.log("changeMapFontClick") }

function changeTableFontClick() { console.log("changeTableFontClick") }

function changeTextColorClick() { console.log("changeTextColorClick") }

function helpButtonClick() { console.log("helpButtonClick") }

function showMapCommandsClick() { console.log("showMapCommandsClick") }

function symbolButtonsClick() { console.log("symbolButtonsClick") }
