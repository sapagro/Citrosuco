*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02.09.2020 at 08:49:44
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_REC_OBRIG.................................*
DATA:  BEGIN OF STATUS_ZABST_REC_OBRIG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_REC_OBRIG               .
CONTROLS: TCTRL_ZABST_REC_OBRIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_REC_OBRIG               .
TABLES: ZABST_REC_OBRIG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
