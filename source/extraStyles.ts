const css = `
.w-3 { width: 3%; }
.w-15 { width: 15%; }
.w-17 { width: 17%; }
.w-6rem { width: 6rem }
.w-10rem { width: 10rem }
.us-none { user-select: none }
.pre-wrap { white-space: pre-wrap }
.f-smaller { font-size: smaller }
.f-larger { font-size: larger }
input[type="checkbox"] { width: 1rem; height: 1rem; }
.overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0,0,0,0.3);
    z-index: 10000;
  }
`

export function addExtraStylesBeyondTachyons() {
    const style = document.createElement('style')
    style.type = 'text/css'
    style.appendChild(document.createTextNode(css))
    document.head.appendChild(style)
}
