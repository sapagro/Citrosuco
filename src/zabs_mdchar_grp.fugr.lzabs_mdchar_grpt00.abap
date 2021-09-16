*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.05.2020 at 10:23:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_VMDCHAR_GRP................................*
TABLES: ZABS_VMDCHAR_GRP, *ZABS_VMDCHAR_GRP. "view work areas
CONTROLS: TCTRL_ZABS_VMDCHAR_GRP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABS_VMDCHAR_GRP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABS_VMDCHAR_GRP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABS_VMDCHAR_GRP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABS_VMDCHAR_GRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VMDCHAR_GRP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABS_VMDCHAR_GRP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABS_VMDCHAR_GRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VMDCHAR_GRP_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_MDCHAR_GRP                .
