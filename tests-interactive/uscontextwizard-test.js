
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm

import "../node_modules/mithril/mithril.js"
/* global m */

function caption(text) {
    return text.replace("&", "")
}

function hexToBase64(hexString) {
    hexString = hexString.substring("07544269746D6170FA040000".length)
    // hexString = hexString.substring("7544269746D6170FA0400002F2A2058504D202A2F0A".length)
    return btoa(hexString.match(/\w{2}/g).map(function(a) {
        return String.fromCharCode(parseInt(a, 16))
    }).join(""))
}

let wizardPage = 1

m.mount(document.body, { view: viewContextWizardForm })

export function viewContextWizardForm() {
    return m("ContextWizardForm.TContextWizardForm",
        {
        // ActiveControl: "goBack",
        // BorderStyle: "bsDialog",
        // Caption: "New Contexts Wizard",
        // ClientHeight: "240",
        // ClientWidth: "320",
        // Font.Height: "-11",
        // Font.Name: "Arial",
        // OnActivate: "FormActivate",
        // OnCloseQuery: "FormCloseQuery",
        // PixelsPerInch: "100",
        // Position: "poScreenCenter",
        // TextHeight: "14",
        // HorzScrollBar.Page: "319",
        // VertScrollBar.Page: "239",
        // Left: "291",
        // Height: "240",
        // Top: "193",
        // Width: "320",
        },
        m("button.goBack",
            {
                onclick: goBackClick,
                // BorderSpacing.InnerBorder: "2",
                // TabOrder: "0",
                // Left: "229",
                // Height: "21",
                // Top: "344",
                // Width: "70",
            },
            caption("<< &Back")
        ),
        m("button.goNext.ml2",
            {
                onclick: goNextClick,
                // BorderSpacing.InnerBorder: "2",
            },
            wizardPage === 4 ? "Finish" : caption("&Next >>")
        ),
        m("button.cancel.ml2",
            {
                onclick: cancelClick,
                // BorderSpacing.InnerBorder: "2",
            },
            caption("&Cancel")
        ),
        m("button.helpButton.ml2",
            {
                onclick: helpButtonClick,
                // BorderSpacing.InnerBorder: "2",
            },
            caption("&Help")
        ),
        m("TNotebook.notebook.TNotebook",
            {
                // Align: "alTop",
                // OnPageChanged: "notebookPageChanged",
                // PageIndex: "0",
                // Height: "341",
                // Width: "320",
            },
            wizardPage !== 1 ? [] : m("TPage.page1.TPage",
                {
                    // ClientWidth: "316",
                    // ClientHeight: "307",
                    // Left: "2",
                    // Height: "307",
                    // Top: "32",
                    // Width: "316",
                },
                m("h1", "Start"),
                m("div.Label1.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Height: "-13",
                        // Font.Name: "Arial",
                        // Font.Style: "[fsBold]",
                        // ParentColor: "False",
                        // Left: "60",
                        // Height: "12",
                        // Top: "24",
                        // Width: "246",
                    },
                    "Welcome to the New Contexts Wizard!"
                ),
                m("div.Label2.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // ParentShowHint: "False",
                        // Left: "24",
                        // Height: "17",
                        // Top: "62",
                        // Width: "626",
                    },
                    "This wizard will help you quickly create a set of new rules based on contexts you enter."
                ),
                m("div.Label3.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Height: "-11",
                        // Font.Name: "Arial",
                        // Font.Style: "[fsBold]",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "21",
                        // Top: "300",
                        // Width: "145",
                    },
                    "Click the Next button to begin."
                ),
                m("div.Label7.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "77",
                        // Top: "167",
                        // Width: "359",
                    },
                    "You can enter a descriptive reply for each new context you enter here. The descriptive replies will be accessed with a common command such as \"look\"."
                ),
                m("div.Label9.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "69",
                        // Height: "17",
                        // Top: "205",
                        // Width: "440",
                    },
                    "A reply is what the computer says after you say a command."
                ),
                m("img.Image1.TImage",
                    {
                        // AutoSize: "True",
                        src: "data:image/xpm;base64," + hexToBase64(
                            "07544269746D6170FA0400002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22333220333220352031222C0A222E20632023433643364336222C0A222C20632023464646463030222C0A222D20632023303046464646222C0A222A2063204E6F6E65222C0A226120632023303030303834222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2C2C2E2E2E2E2E2E2C2C2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2C2C2C2E2E2E2E2E2C2C2E2E2E2E2E2C2C2C2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2C2C2E2E2E2E2C2C2C2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2C2C2E2E2E2E2C2C2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2C2C2C2E2E2E2C2C2E2E2E2C2C2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2C2C2C2E2E2C2C2E2E2C2C2C2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2C2C2C2E2E2E2E2C2C2C2C2E2E222C0A222E2E2E2C2C2C2C2C2C2C2C2E2E2E2E2E2E2E2E2C2C2E2E2E2C2C2C2C2C2C2E2E222C0A222E2E2E2C2C2C2C2C2C2C2C2E2E2D2D2E2E2E2E2E2E2C2C2C2C2C2C2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2D2D2D2A2E2E2E2E2E2C2C2C2C2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2D2D2D2A2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2D2D2D2A2A2A2A2A2E2E2E2E2C2C2C2C2C2C2C2C2E2E222C0A222E2E2E2E2E2E2E2E2E2D2D2D2A2A2A2A2A2A2A2E2E2E2C2C2C2C2C2C2C2C2E2E222C0A222E2E2E2E2E2E2E2E2D2D2D2A2A2A2A2A2A61612E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2D2D2D2A2A2A2A2A2A6161612E2E2C2C2C2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2D2D2D2A2A2A2A2A2A6161612E2E2E2C2C2C2C2E2E2E2E2E2E2E222C0A222E2E2E2E2E2D2D2D2A2A2A2A2A2A6161612E2E2E2C2C2E2C2C2C2C2E2E2E2E2E222C0A222E2E2E2E2D2D2D2A2A2A2A2A2A6161612E2E2E2E2C2C2E2E2C2C2C2C2E2E2E2E222C0A222E2E2E2D2D2D2A2A2A2A2A2A6161612E2E2E2E2E2E2C2C2E2E2E2C2C2C2C2E2E222C0A222E2E2D2D2D2A2A2A2A2A2A6161612E2E2E2C2C2E2E2C2C2E2E2E2E2C2C2C2E2E222C0A222E2D2D2D2A2A2A2A2A2A6161612E2E2E2E2C2C2E2E2E2C2C2E2E2E2E2E2E2E2E222C0A222D2D2D2A2A2A2A2A2A6161612E2E2E2E2E2C2C2E2E2E2C2C2E2E2E2E2E2E2E2E222C0A222D2D2A2A2A2A2A2A6161612E2E2E2E2E2E2C2C2E2E2E2E2C2C2E2E2E2E2E2E2E222C0A222D2A2A2A2A2A2A6161612E2E2E2E2E2E2E2C2C2E2E2E2E2C2C2E2E2E2E2E2E2E222C0A222A2A2A2A2A2A6161612E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222A2A2A2A2A6161612E2E2E2E2E2E2E2E2E2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222A2A2A2A6161612E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222A2A2A6161612E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A"
                        )
                        // Transparent: "True",
                        // Left: "24",
                        // Height: "32",
                        // Top: "12",
                        // Width: "32",
                    }
                ),
                m("div.Label10.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "69",
                        // Height: "17",
                        // Top: "87",
                        // Width: "330",
                    },
                    "A command is what you say to the computer."
                ),
                m("div.Label26.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "24",
                        // Height: "17",
                        // Top: "317",
                        // Width: "600",
                    },
                    "You can click Cancel at any time to close the wizard without making any new rules."
                ),
                m("div.Label8.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "69",
                        // Height: "57",
                        // Top: "117",
                        // Width: "289",
                    },
                    "A context is the single most important requirement for the user of a command, usually a physical location."
                ),
                m("img.commandStartPageImage.TImage",
                    {
                        // AutoSize: "True",
                        // Transparent: "True",
                        // Left: "48",
                        // Height: "16",
                        // Top: "86",
                        // Width: "16",
                    }
                ),
                m("img.contextStartPageImage.TImage",
                    {
                        // AutoSize: "True",
                        // Transparent: "True",
                        // Left: "48",
                        // Height: "16",
                        // Top: "117",
                        // Width: "16",
                    }
                ),
                m("img.replyStartPageImage.TImage",
                    {
                        // AutoSize: "True",
                        // Transparent: "True",
                        // Left: "48",
                        // Height: "16",
                        // Top: "204",
                        // Width: "16",
                    }
                )
            ),
            wizardPage !== 2 ? [] : m("TPage.page2.TPage",
                {
                    // ClientWidth: "316",
                    // ClientHeight: "307",
                    // Left: "2",
                    // Height: "307",
                    // Top: "32",
                    // Width: "316",
                },
                m("h1", "Enter Contexts"),
                m("div.Label5.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "57",
                        // Top: "8",
                        // Width: "440",
                    },
                    "Enter or paste the contexts you want to create in the area below, separating each context from its descriptive reply by a pipe bar. For example, \"house | You are in a house\"."
                ),
                m("div.Label6.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "24",
                        // Height: "17",
                        // Top: "323",
                        // Width: "378",
                    },
                    " When you are finished entering contexts, click Next."
                ),
                m("div.Label21.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "57",
                        // Top: "36",
                        // Width: "312",
                    },
                    "Descriptions are optional. It's okay to wrap entries on more than one line. Use carriage returns to separate entries."
                ),
                m("div.Label22.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Color: "clBlue",
                        // Font.Height: "-11",
                        // Font.Name: "Arial",
                        // ParentColor: "False",
                        // Left: "8",
                        // Height: "12",
                        // Top: "69",
                        // Width: "134",
                    },
                    "Context | Descriptive Reply"
                ),
                m("img.Image3.TImage",
                    {
                        // AutoSize: "True",
                        // Picture.Data: "07544269746D61701B0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A223820313620332031222C0A222E2063204E6F6E65222C0A222C20632023433643364336222C0A222D20632023303038343030222C0A222E2C2C2C2C2C2C2C222C0A222E2E2C2C2C2C2C2C222C0A222E2D2E2C2C2C2C2C222C0A222E2D2D2E2C2C2C2C222C0A222E2D2D2D2E2C2C2C222C0A222E2D2D2D2D2E2C2C222C0A222E2D2D2D2D2D2E2C222C0A222E2D2D2D2D2D2D2E222C0A222E2D2D2D2D2D2D2E222C0A222E2D2D2D2D2D2E2C222C0A222E2D2D2D2D2E2C2C222C0A222E2D2D2D2E2C2C2C222C0A222E2D2D2E2C2C2C2C222C0A222E2D2E2C2C2C2C2C222C0A222E2E2C2C2C2C2C2C222C0A222E2C2C2C2C2C2C2C227D0A",
                        // Transparent: "True",
                        // Left: "12",
                        // Height: "16",
                        // Top: "8",
                        // Width: "8",
                    }
                ),
                m("textarea.NewContextsMemo.TMemo",
                    {
                        // ScrollBars: "ssVertical",
                        // TabOrder: "0",
                        // Left: "6",
                        // Height: "233",
                        // Top: "84",
                        // Width: "479",
                    }
                )
            ),
            wizardPage !== 3 ? [] : m("TPage.page3.TPage",
                {
                    // ClientWidth: "316",
                    // ClientHeight: "307",
                    // Left: "2",
                    // Height: "307",
                    // Top: "32",
                    // Width: "316",
                },
                m("h1", "Generate Descriptions"),
                m("div.DescribeLabel.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Height: "-11",
                        // Font.Name: "Arial",
                        // ParentColor: "False",
                        // Left: "24",
                        // Height: "12",
                        // Top: "13",
                        // Width: "359",
                    },
                    " What command should the user to say to access these descriptive replies?"
                ),
                m("div.DescribeLabelExtra.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "54",
                        // Height: "17",
                        // Top: "70",
                        // Width: "207",
                    },
                    "Some generic examples are:"
                ),
                m("div.Label15.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // Left: "80",
                        // Height: "17",
                        // Top: "93",
                        // Width: "362",
                    },
                    "\"look\", \"listen\", \"smell\", \"feel\", \"taste\", and \"sense\"."
                ),
                m("div.Label4.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "54",
                        // Height: "77",
                        // Top: "170",
                        // Width: "303",
                    },
                    "If you have not entered a description for a context, the wizard will add a default description of 'There is nothing of interest here.' "
                ),
                m("img.DescribeImage.TImage",
                    {
                        // AutoSize: "True",
                        // Picture.Data: "07544269746D61701B0100002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A223820313620332031222C0A222E2063204E6F6E65222C0A222C20632023433643364336222C0A222D20632023303038343030222C0A222E2C2C2C2C2C2C2C222C0A222E2E2C2C2C2C2C2C222C0A222E2D2E2C2C2C2C2C222C0A222E2D2D2E2C2C2C2C222C0A222E2D2D2D2E2C2C2C222C0A222E2D2D2D2D2E2C2C222C0A222E2D2D2D2D2D2E2C222C0A222E2D2D2D2D2D2D2E222C0A222E2D2D2D2D2D2D2E222C0A222E2D2D2D2D2D2E2C222C0A222E2D2D2D2D2E2C2C222C0A222E2D2D2D2E2C2C2C222C0A222E2D2D2E2C2C2C2C222C0A222E2D2E2C2C2C2C2C222C0A222E2E2C2C2C2C2C2C222C0A222E2C2C2C2C2C2C2C227D0A",
                        // Transparent: "True",
                        // Left: "12",
                        // Height: "16",
                        // Top: "12",
                        // Width: "8",
                    }
                ),
                m("img.commandImage.TImage",
                    {
                        // AutoSize: "True",
                        // Transparent: "True",
                        // Left: "54",
                        // Height: "16",
                        // Top: "40",
                        // Width: "16",
                    }
                ),
                m("div.Label16.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "52",
                        // Height: "77",
                        // Top: "117",
                        // Width: "309",
                    },
                    "You should stick with \"look\" unless you are doing something special. You can change individual commands later (in the editor) to deal with specific situations."
                ),
                m("input.DescribeEdit.TEdit",
                    {
                        // TabOrder: "0",
                        // Text: "look",
                        // Left: "74",
                        // Height: "22",
                        // Top: "38",
                        // Width: "291",
                    }
                )
            ),
            wizardPage !== 4 ? [] : m("TPage.page4.TPage",
                {
                    // ClientWidth: "316",
                    // ClientHeight: "307",
                    // Left: "2",
                    // Height: "307",
                    // Top: "32",
                    // Width: "316",
                },
                m("h1", "Finish"),
                m("div.Label13.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "57",
                        // Top: "58",
                        // Width: "375",
                    },
                    "You have completed the information the wizard needs to generate a new set of rules based on your the contexts and descriptions you have entered."
                ),
                m("div.Label14.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Height: "-11",
                        // Font.Name: "Arial",
                        // Font.Style: "[fsBold]",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "21",
                        // Top: "249",
                        // Width: "301",
                    },
                    "Click Finish to create the new rules and close the wizard."
                ),
                m("div.Label18.TLabel",
                    {
                        // Color: "clNone",
                        // Font.Height: "-13",
                        // Font.Name: "Arial",
                        // Font.Style: "[fsBold]",
                        // ParentColor: "False",
                        // Left: "64",
                        // Height: "12",
                        // Top: "24",
                        // Width: "113",
                    },
                    "Congratulations!"
                ),
                m("div.Label19.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "37",
                        // Top: "304",
                        // Width: "358",
                    },
                    "Click Back to review your choices. Click Cancel to close the wizard without making any new rules."
                ),
                m("img.Image2.TImage",
                    {
                        // AutoSize: "True",
                        // Picture.Data: "07544269746D6170EB0400002F2A2058504D202A2F0A7374617469632063686172202A677261706869635B5D203D207B0A22333220333220342031222C0A222E2063204E6F6E65222C0A222C20632023303030304646222C0A222D20632023464646463030222C0A222A20632023303030303030222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2A2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E222C0A222E2E2E2E2E2E2E2E2E2E2E2A2A2C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E222C0A222E2E2E2E2E2E2E2E2E2A2A2E2E2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E222C0A222E2E2E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2A2A2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E222C0A222E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E227D0A",
                        // Transparent: "True",
                        // Left: "24",
                        // Height: "32",
                        // Top: "15",
                        // Width: "32",
                    }
                ),
                m("div.Label11.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "57",
                        // Top: "271",
                        // Width: "334",
                    },
                    "After you finish the wizard, you can choose Undo from the Edit menu to remove your new rules."
                ),
                m("div.Label12.TLabel",
                    {
                        // Color: "clNone",
                        // ParentColor: "False",
                        // WordWrap: "True",
                        // Left: "24",
                        // Height: "37",
                        // Top: "92",
                        // Width: "365",
                    },
                    "The text you entered here will also be saved in the log file (even if you cancel using the wizard)."
                )
            )
        )
    )
}

function cancelClick() { console.log("cancelClick") }

function goBackClick() { wizardPage = Math.max(1, wizardPage - 1) }

function goNextClick() { 
    if (wizardPage === 4) {
        alert("Finished!!!")
    }
    wizardPage = Math.min(4, wizardPage + 1)
}

function helpButtonClick() { console.log("helpButtonClick") }
