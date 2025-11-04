*KEYWORD
*PARAMETER
Rpm_l2         100.0
*PARAMETER_EXPRESSION
Rpmex_l2    &pm_l2*2
*SECTION_SOLID_TITLE
$# title
main pm l2
$#   secid    elform       aet
        30  &pm_main
$
*SECTION_SOLID_TITLE
$# title
main pmex  l2
$#   secid    elform       aet
        31&pmex_main
$
*SECTION_SOLID_TITLE
$# title
l1 pm  l2
$#   secid    elform       aet
        32  &pm_l1
$
*SECTION_SOLID_TITLE
$# title
l1 pmex  l2
$#   secid    elform       aet
        33&pmex_l1
$
*SECTION_SOLID_TITLE
$# title
l2 pm  l2
$#   secid    elform       aet
        34  &pm_l2
$
*SECTION_SOLID_TITLE
$# title
l2 pmex  l2
$#   secid    elform       aet
        35&pmex_l2
$
*END