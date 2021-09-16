*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.05.2020 at 06:57:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_FLEET.....................................*
DATA:  BEGIN OF STATUS_ZABST_FLEET                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_FLEET                   .
CONTROLS: TCTRL_ZABST_FLEET
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_FLEET                   .
TABLES: ZABST_FLEET                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
