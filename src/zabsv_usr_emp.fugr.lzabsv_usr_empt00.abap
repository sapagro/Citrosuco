*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.05.2020 at 07:26:07
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_USR_EMP...................................*
TABLES: ZABSV_USR_EMP, *ZABSV_USR_EMP. "view work areas
CONTROLS: TCTRL_ZABSV_USR_EMP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_USR_EMP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_USR_EMP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_USR_EMP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_USR_EMP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_USR_EMP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_USR_EMP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_USR_EMP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_USR_EMP_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_USR_EMP                   .
