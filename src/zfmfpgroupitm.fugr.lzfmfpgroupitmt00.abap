*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.09.2019 at 08:18:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFMFPGROUPITM...................................*
DATA:  BEGIN OF STATUS_ZFMFPGROUPITM                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFMFPGROUPITM                 .
CONTROLS: TCTRL_ZFMFPGROUPITM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFMFPGROUPITM                 .
TABLES: ZFMFPGROUPITM                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
