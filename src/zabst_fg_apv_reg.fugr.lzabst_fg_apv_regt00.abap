*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.05.2021 at 10:00:24
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_APV_REGIAO................................*
DATA:  BEGIN OF STATUS_ZABST_APV_REGIAO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_APV_REGIAO              .
CONTROLS: TCTRL_ZABST_APV_REGIAO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_APV_REGIAO              .
TABLES: ZABST_APV_REGIAO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
