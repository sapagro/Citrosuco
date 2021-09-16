*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 25.05.2020 at 01:49:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_EMP_ROLE..................................*
TABLES: ZABSV_EMP_ROLE, *ZABSV_EMP_ROLE. "view work areas
CONTROLS: TCTRL_ZABSV_EMP_ROLE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_EMP_ROLE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_EMP_ROLE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_EMP_ROLE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_EMP_ROLE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_EMP_ROLE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_EMP_ROLE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_EMP_ROLE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_EMP_ROLE_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_EMP_ROLE                  .
