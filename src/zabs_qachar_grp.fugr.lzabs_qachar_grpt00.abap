*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.04.2020 at 14:07:51
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_VQACHAR_GRP................................*
TABLES: ZABS_VQACHAR_GRP, *ZABS_VQACHAR_GRP. "view work areas
CONTROLS: TCTRL_ZABS_VQACHAR_GRP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABS_VQACHAR_GRP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABS_VQACHAR_GRP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABS_VQACHAR_GRP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABS_VQACHAR_GRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VQACHAR_GRP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABS_VQACHAR_GRP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABS_VQACHAR_GRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VQACHAR_GRP_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_QACHAR_GRP                .
