*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.05.2020 at 14:24:58
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_QUAL_CHAR..................................*
DATA:  BEGIN OF STATUS_ZABS_QUAL_CHAR                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_QUAL_CHAR                .
CONTROLS: TCTRL_ZABS_QUAL_CHAR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_QUAL_CHAR                .
TABLES: ZABS_QUAL_CHAR                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
