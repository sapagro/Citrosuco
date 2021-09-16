*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.09.2020 at 14:34:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_USR_CENTRO................................*
DATA:  BEGIN OF STATUS_ZABST_USR_CENTRO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_USR_CENTRO              .
CONTROLS: TCTRL_ZABST_USR_CENTRO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABST_USR_CENTRO              .
TABLES: ZABST_USR_CENTRO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
