*KEYWORD
*PARAMETER
Rgbl          200.0
*SECTION_SOLID_TITLE
$# title
top level section using global
$#   secid    elform       aet
        10  &gbl
*INCLUDE
include_with_local.k
*SECTION_SOLID_TITLE
$# title
top level section should not see local param
$#   secid    elform       aet
        40  &loc
$
*END
