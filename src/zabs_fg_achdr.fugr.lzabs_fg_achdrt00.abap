*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.09.2020 at 08:55:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_HDR_ACCOM..................................*
DATA:  BEGIN OF STATUS_ZABS_HDR_ACCOM                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_HDR_ACCOM                .
CONTROLS: TCTRL_ZABS_HDR_ACCOM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_HDR_ACCOM                .
TABLES: ZABS_HDR_ACCOM                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
