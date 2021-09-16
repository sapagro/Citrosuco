*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.02.2020 at 14:00:01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_SF_ESTIMAT................................*
DATA:  BEGIN OF STATUS_ZABST_SF_ESTIMAT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_SF_ESTIMAT              .
CONTROLS: TCTRL_ZABST_SF_ESTIMAT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_SF_ESTIMAT              .
TABLES: ZABST_SF_ESTIMAT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
