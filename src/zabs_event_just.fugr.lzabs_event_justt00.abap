*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.10.2019 at 14:43:34
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_EVENT_JUST.................................*
DATA:  BEGIN OF STATUS_ZABS_EVENT_JUST               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_EVENT_JUST               .
CONTROLS: TCTRL_ZABS_EVENT_JUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_EVENT_JUST               .
TABLES: ZABS_EVENT_JUST                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
