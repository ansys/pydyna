*KEYWORD
*PARAMETER
Rpm_l1         100.0
*PARAMETER_EXPRESSION
Rpmex_l1    &pm_l1*2
*SECTION_SOLID_TITLE
$# title
main pm l1
$#   secid    elform       aet
        20  &pm_main
$
*SECTION_SOLID_TITLE
$# title
main pmex  l1
$#   secid    elform       aet
        21&pmex_main
$
*SECTION_SOLID_TITLE
$# title
l1 pm  l1
$#   secid    elform       aet
        22  &pm_l1
$
*SECTION_SOLID_TITLE
$# title
l1 pmex  l1
$#   secid    elform       aet
        23&pmex_l1
$
*SECTION_SOLID_TITLE
$# title
l2 pm  l1
$#   secid    elform       aet
        24  &pm_l2
$
*SECTION_SOLID_TITLE
$# title
l2 pmex  l1
$#   secid    elform       aet
        25&pmex_l2
$
*INCLUDE
02_minimal_parameter_test_level2.k
$
*END