const css = `
.w-3 { width: 3%; }
.w-15 { width: 15%; }
.w-17 { width: 17%; }
.w-10rem { width: 10rem }
.us-none { "user-select: none" }
`

export function addExtraStylesBeyondTachyons() {
    const style = document.createElement('style')
    style.type = 'text/css'
    style.appendChild(document.createTextNode(css))
    document.head.appendChild(style)
}
