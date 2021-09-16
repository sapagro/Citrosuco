*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.11.2019 at 11:05:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFMACTYP.......................................*
DATA:  BEGIN OF STATUS_ZTFMACTYP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFMACTYP                     .
CONTROLS: TCTRL_ZTFMACTYP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFMACTYP                     .
TABLES: ZTFMACTYP                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
