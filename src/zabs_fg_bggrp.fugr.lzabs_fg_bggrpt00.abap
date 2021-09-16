*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.06.2020 at 10:26:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_AC_BGGRP...................................*
DATA:  BEGIN OF STATUS_ZABS_AC_BGGRP                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_AC_BGGRP                 .
CONTROLS: TCTRL_ZABS_AC_BGGRP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_AC_BGGRP                 .
TABLES: ZABS_AC_BGGRP                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
