
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

const fakeDomain = {
    test: "hello"
}

const MyComponent = { view: () => viewConsoleForm(fakeDomain) }

m.mount(document.body, MyComponent)
