*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 05.08.2020 at 10:07:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_ORCAMENTO.................................*
DATA:  BEGIN OF STATUS_ZABST_ORCAMENTO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_ORCAMENTO               .
CONTROLS: TCTRL_ZABST_ORCAMENTO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_ORCAMENTO               .
TABLES: ZABST_ORCAMENTO                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
