*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.03.2020 at 08:15:45
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFMRCTYP.......................................*
DATA:  BEGIN OF STATUS_ZTFMRCTYP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFMRCTYP                     .
CONTROLS: TCTRL_ZTFMRCTYP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFMRCTYP                     .
TABLES: ZTFMRCTYP                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
