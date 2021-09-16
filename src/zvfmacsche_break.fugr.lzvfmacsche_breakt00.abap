*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.10.2019 at 19:30:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVFMACSCHE_BREAK................................*
TABLES: ZVFMACSCHE_BREAK, *ZVFMACSCHE_BREAK. "view work areas
CONTROLS: TCTRL_ZVFMACSCHE_BREAK
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_ZVFMACSCHE_BREAK. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVFMACSCHE_BREAK.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVFMACSCHE_BREAK_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVFMACSCHE_BREAK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFMACSCHE_BREAK_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVFMACSCHE_BREAK_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVFMACSCHE_BREAK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFMACSCHE_BREAK_TOTAL.

*.........table declarations:.................................*
TABLES: ZFMACSCHED_BREAK               .
