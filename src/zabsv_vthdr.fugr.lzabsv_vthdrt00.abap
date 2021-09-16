*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.06.2019 at 07:45:11
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_VTHDR.....................................*
TABLES: ZABSV_VTHDR, *ZABSV_VTHDR. "view work areas
CONTROLS: TCTRL_ZABSV_VTHDR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_VTHDR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_VTHDR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_VTHDR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTHDR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTHDR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_VTHDR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTHDR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTHDR_TOTAL.

*.........table declarations:.................................*
TABLES: ZABST_VTHDR                    .
