*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.11.2020 at 07:20:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_IRR_USR....................................*
DATA:  BEGIN OF STATUS_ZABS_IRR_USR                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_IRR_USR                  .
CONTROLS: TCTRL_ZABS_IRR_USR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_IRR_USR                  .
TABLES: ZABS_IRR_USR                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
