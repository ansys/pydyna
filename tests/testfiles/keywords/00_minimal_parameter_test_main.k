*KEYWORD
*PARAMETER
Rpm_main      100.0
*PARAMETER_EXPRESSION
Rpmex_main &pm_main*2
*SECTION_SOLID_TITLE
$# title
main pm main
$#   secid    elform       aet
        10  &pm_main
$
*SECTION_SOLID_TITLE
$# title
main pmex main
$#   secid    elform       aet
        11&pmex_main
$
*SECTION_SOLID_TITLE
$# title
l1 pm main
$#   secid    elform       aet
        12  &pm_l1
$
*SECTION_SOLID_TITLE
$# title
l1 pmex main
$#   secid    elform       aet
        13&pmex_l1
$
*SECTION_SOLID_TITLE
$# title
l2 pm main
$#   secid    elform       aet
        14  &pm_l2
$
*SECTION_SOLID_TITLE
$# title
l2 pmex main
$#   secid    elform       aet
        15&pmex_l2
$
*INCLUDE
01_minimal_parameter_test_level1.k
$
*END
