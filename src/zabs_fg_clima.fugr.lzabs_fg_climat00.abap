*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.05.2021 at 09:30:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_CLIMA_HIST................................*
DATA:  BEGIN OF STATUS_ZABST_CLIMA_HIST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_CLIMA_HIST              .
CONTROLS: TCTRL_ZABST_CLIMA_HIST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_CLIMA_HIST              .
TABLES: ZABST_CLIMA_HIST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
