*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.04.2021 at 11:00:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_CSKS.......................................*
DATA:  BEGIN OF STATUS_ZABS_CSKS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_CSKS                     .
CONTROLS: TCTRL_ZABS_CSKS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_CSKS                     .
TABLES: ZABS_CSKS                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
