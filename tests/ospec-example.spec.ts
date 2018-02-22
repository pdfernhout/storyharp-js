import * as o from "ospec"

o("addition", function() {
    o(1 + 1).equals(2)
})

o("subtraction", function() {
    o(1 - 1).notEquals(2)
})
