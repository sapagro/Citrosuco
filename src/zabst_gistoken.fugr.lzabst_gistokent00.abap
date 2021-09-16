*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.05.2020 at 05:08:45
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_GISTOKEN..................................*
DATA:  BEGIN OF STATUS_ZABST_GISTOKEN                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_GISTOKEN                .
CONTROLS: TCTRL_ZABST_GISTOKEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_GISTOKEN                .
TABLES: ZABST_GISTOKEN                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
