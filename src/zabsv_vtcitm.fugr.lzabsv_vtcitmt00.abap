*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.06.2019 at 08:45:09
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_VTCITM....................................*
TABLES: ZABSV_VTCITM, *ZABSV_VTCITM. "view work areas
CONTROLS: TCTRL_ZABSV_VTCITM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_VTCITM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_VTCITM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_VTCITM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTCITM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTCITM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_VTCITM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTCITM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTCITM_TOTAL.

*.........table declarations:.................................*
TABLES: ZABST_VTCITM                   .
