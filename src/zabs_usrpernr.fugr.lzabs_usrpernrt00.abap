*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.05.2020 at 07:28:26
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_USRPERNR...................................*
DATA:  BEGIN OF STATUS_ZABS_USRPERNR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_USRPERNR                 .
CONTROLS: TCTRL_ZABS_USRPERNR
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZABS_V_USRPERNR.................................*
TABLES: ZABS_V_USRPERNR, *ZABS_V_USRPERNR. "view work areas
CONTROLS: TCTRL_ZABS_V_USRPERNR
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZABS_V_USRPERNR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABS_V_USRPERNR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABS_V_USRPERNR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABS_V_USRPERNR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_V_USRPERNR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABS_V_USRPERNR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABS_V_USRPERNR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_V_USRPERNR_TOTAL.

*.........table declarations:.................................*
TABLES: *ZABS_USRPERNR                 .
TABLES: ZABS_USRPERNR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
