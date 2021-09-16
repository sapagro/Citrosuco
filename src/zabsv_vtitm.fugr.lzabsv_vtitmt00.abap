*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.06.2019 at 08:44:15
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_VTITM.....................................*
TABLES: ZABSV_VTITM, *ZABSV_VTITM. "view work areas
CONTROLS: TCTRL_ZABSV_VTITM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_VTITM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_VTITM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_VTITM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTITM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTITM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_VTITM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_VTITM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_VTITM_TOTAL.

*.........table declarations:.................................*
TABLES: ZABST_VTITM                    .
