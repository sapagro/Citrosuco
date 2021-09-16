*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 25.09.2020 at 01:44:41
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_AC_BGGRP..................................*
TABLES: ZABSV_AC_BGGRP, *ZABSV_AC_BGGRP. "view work areas
CONTROLS: TCTRL_ZABSV_AC_BGGRP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_AC_BGGRP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_AC_BGGRP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_AC_BGGRP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_AC_BGGRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_AC_BGGRP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_AC_BGGRP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_AC_BGGRP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_AC_BGGRP_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_AC_BGGRP                  .
