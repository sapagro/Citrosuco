*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.12.2020 at 09:22:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_BGP_ACELOG................................*
TABLES: ZABSV_BGP_ACELOG, *ZABSV_BGP_ACELOG. "view work areas
CONTROLS: TCTRL_ZABSV_BGP_ACELOG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_BGP_ACELOG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_BGP_ACELOG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_BGP_ACELOG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_BGP_ACELOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_BGP_ACELOG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_BGP_ACELOG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_BGP_ACELOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_BGP_ACELOG_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_BGP_ACCELOG               .
