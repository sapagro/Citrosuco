*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.04.2020 at 14:28:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_TASK_APP..................................*
DATA:  BEGIN OF STATUS_ZABST_TASK_APP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_TASK_APP                .
CONTROLS: TCTRL_ZABST_TASK_APP
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZABST_TASK_APP                .
TABLES: ZABST_TASK_APP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
