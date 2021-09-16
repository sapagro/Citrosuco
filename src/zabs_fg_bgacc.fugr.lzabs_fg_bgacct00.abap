*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.09.2020 at 08:42:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_BGP_ACCOM..................................*
DATA:  BEGIN OF STATUS_ZABS_BGP_ACCOM                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_BGP_ACCOM                .
CONTROLS: TCTRL_ZABS_BGP_ACCOM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_BGP_ACCOM                .
TABLES: ZABS_BGP_ACCOM                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
