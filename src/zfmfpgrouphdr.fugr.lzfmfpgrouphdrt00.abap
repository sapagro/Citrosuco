*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.09.2019 at 11:05:32
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFMFPGROUPHDR...................................*
DATA:  BEGIN OF STATUS_ZFMFPGROUPHDR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFMFPGROUPHDR                 .
CONTROLS: TCTRL_ZFMFPGROUPHDR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFMFPGROUPHDR                 .
TABLES: ZFMFPGROUPHDR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
