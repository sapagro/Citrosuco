*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.10.2019 at 19:30:03
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVFMACWORK_SHIFT................................*
TABLES: ZVFMACWORK_SHIFT, *ZVFMACWORK_SHIFT. "view work areas
CONTROLS: TCTRL_ZVFMACWORK_SHIFT
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZVFMACWORK_SHIFT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVFMACWORK_SHIFT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVFMACWORK_SHIFT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVFMACWORK_SHIFT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFMACWORK_SHIFT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVFMACWORK_SHIFT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVFMACWORK_SHIFT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFMACWORK_SHIFT_TOTAL.

*.........table declarations:.................................*
TABLES: ZFMACWORK_SHIFT                .
