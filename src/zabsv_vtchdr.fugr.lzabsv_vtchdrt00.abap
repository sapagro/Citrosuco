*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.06.2019 at 07:47:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_VTCHDR....................................*
TABLES: ZABSV_VTCHDR, *ZABSV_VTCHDR. "view work areas
CONTROLS: TCTRL_ZABSV_VTCHDR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_VTCHDR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_VTCHDR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_VTCHDR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTCHDR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTCHDR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_VTCHDR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTCHDR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTCHDR_TOTAL.

*.........table declarations:.................................*
TABLES: ZABST_VTCHDR                   .
