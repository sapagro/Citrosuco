*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.09.2019 at 12:07:35
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFMFPSAFRAS.....................................*
DATA:  BEGIN OF STATUS_ZFMFPSAFRAS                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFMFPSAFRAS                   .
CONTROLS: TCTRL_ZFMFPSAFRAS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFMFPSAFRAS                   .
TABLES: ZFMFPSAFRAS                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
