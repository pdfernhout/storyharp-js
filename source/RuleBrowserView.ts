import * as m from "mithril"

export class RuleBrowserView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    firstListBoxImageClick() {
        console.log("firstListBoxImageClick")
    }

    view() {
        return m("TTabSheet.TabSheetBrowse.TTabSheet",
            {
            },
            "Browser",
            m("div.PanelLists.TPanel",
                {
                },
                "PanelLists",
                m("div.PanelFirstList.TPanel",
                    {
                    },
                    m("Group.Group.g00000002",
                        m("img.firstListBoxImage.TImage",
                            {
                                onclick: () => this.firstListBoxImageClick(),
                            },
                        ),
                        m("div.firstListBoxLabel.TLabel",
                            {
                            },
                            "Contexts",
                        ),
                    ),
                    m("TListBox.FirstListBox.TListBox",
                        {
                        },
                    ),
                ),
                m("div.SplitterLists.TSplitter",
                    {
                    },
                ),
                m("div.PanelSecondList.TPanel",
                    {
                    },
                    m("Group.Group.g00000002",
                        m("img.SecondListBoxImage.TImage",
                            {
                            },
                        ),
                        m("div.SecondListBoxLabel.TLabel",
                            {
                            },
                            "Commands",
                        ),
                    ),
                    m("TListBox.SecondListBox.TListBox",
                        {
                        },
                    ),
                ),
            ),
        )
    }
}
