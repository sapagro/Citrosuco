*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.07.2019 at 01:20:19
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_NRPLANNING................................*
TABLES: ZABSV_NRPLANNING, *ZABSV_NRPLANNING. "view work areas
CONTROLS: TCTRL_ZABSV_NRPLANNING
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_NRPLANNING. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_NRPLANNING.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_NRPLANNING_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_NRPLANNING.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_NRPLANNING_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_NRPLANNING_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_NRPLANNING.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_NRPLANNING_TOTAL.

*.........table declarations:.................................*
TABLES: MAKT                           .
TABLES: MARA                           .
TABLES: T001W                          .
TABLES: ZABST_NRPLANNING               .
