*KEYWORD 
$===============================================================================
$ Geometry
$===============================================================================
*INCLUDE
bolts_partsec.key
$===============================================================================
$ Control and output cards
$===============================================================================
*INCLUDE
control_cards_nonlin.key
*INCLUDE
database_cards_static.key
*DEFINE_CURVE_TITLE
Time stepping curve
       700
                  0.                0.05
                  1.                 0.2
                  2.                 0.2
                  3.                 0.2
                  4.                 0.2
                  5.                 0.2
*CONTROL_MPP_IO_NODUMP
$===============================================================================
$ Contacts
$===============================================================================
*CONTACT_AUTOMATIC_SINGLE_SURFACE_MORTAR_ID
1010      Bolts all contact
       111                   2
      0.15                        
        5.                


         5
$===============================================================================
$ Loading and boundary conditions
$===============================================================================
*BOUNDARY_SPC_SET
       112                   1         1         1
$===============================================================================
$ Additional output
$===============================================================================
*DATABASE_HISTORY_NODE_SET
       109
*CONTACT_FORCE_TRANSDUCER_PENALTY_ID
  19000004Bolts to plate
         4       101         2         3


$===============================================================================
$ Cases
$===============================================================================
*CASE_BEGIN_1
*INCLUDE
bolts_nodel.key
*INCLUDE
run_pretens.key
*CASE_END_1
*CASE_BEGIN_2
*INCLUDE
case1.dynain.lsda
*INCLUDE
run_xload.key
*CASE_END_2
*CASE_BEGIN_3
*INCLUDE
case2.dynain.lsda
*INCLUDE
run_yload.key
*CASE_END_3
*END