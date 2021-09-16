*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.05.2020 at 05:51:18
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_TSK_GRP...................................*
DATA:  BEGIN OF STATUS_ZABST_TSK_GRP                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_TSK_GRP                 .
CONTROLS: TCTRL_ZABST_TSK_GRP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_TSK_GRP                 .
TABLES: ZABST_TSK_GRP                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
