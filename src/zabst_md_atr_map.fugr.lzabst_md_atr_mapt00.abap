*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.09.2019 at 06:52:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_MD_ATR_MAP................................*
DATA:  BEGIN OF STATUS_ZABST_MD_ATR_MAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_MD_ATR_MAP              .
CONTROLS: TCTRL_ZABST_MD_ATR_MAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_MD_ATR_MAP              .
TABLES: ZABST_MD_ATR_MAP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
