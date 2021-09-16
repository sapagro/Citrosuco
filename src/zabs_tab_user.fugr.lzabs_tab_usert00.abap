*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.12.2019 at 02:19:49
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_TAB_USER...................................*
DATA:  BEGIN OF STATUS_ZABS_TAB_USER                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_TAB_USER                 .
CONTROLS: TCTRL_ZABS_TAB_USER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_TAB_USER                 .
TABLES: ZABS_TAB_USER                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
