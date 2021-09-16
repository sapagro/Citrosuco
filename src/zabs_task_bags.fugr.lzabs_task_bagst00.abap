*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.05.2020 at 13:12:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_V_TASK_BAGS................................*
TABLES: ZABS_V_TASK_BAGS, *ZABS_V_TASK_BAGS. "view work areas
CONTROLS: TCTRL_ZABS_V_TASK_BAGS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABS_V_TASK_BAGS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABS_V_TASK_BAGS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABS_V_TASK_BAGS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABS_V_TASK_BAGS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_V_TASK_BAGS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABS_V_TASK_BAGS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABS_V_TASK_BAGS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_V_TASK_BAGS_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_TASK_BAGS                 .
