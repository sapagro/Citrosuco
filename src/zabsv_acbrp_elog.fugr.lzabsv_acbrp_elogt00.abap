*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.09.2020 at 09:57:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_ACBRP_ELOG................................*
TABLES: ZABSV_ACBRP_ELOG, *ZABSV_ACBRP_ELOG. "view work areas
CONTROLS: TCTRL_ZABSV_ACBRP_ELOG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_ACBRP_ELOG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_ACBRP_ELOG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_ACBRP_ELOG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_ACBRP_ELOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_ACBRP_ELOG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_ACBRP_ELOG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_ACBRP_ELOG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_ACBRP_ELOG_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_ACBGRP_ELOG               .
