*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.11.2020 at 05:15:34
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABST_TRAN_TYP..................................*
DATA:  BEGIN OF STATUS_ZABST_TRAN_TYP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABST_TRAN_TYP                .
CONTROLS: TCTRL_ZABST_TRAN_TYP
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZABSV_TRAN_TYP..................................*
TABLES: ZABSV_TRAN_TYP, *ZABSV_TRAN_TYP. "view work areas
CONTROLS: TCTRL_ZABSV_TRAN_TYP
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZABSV_TRAN_TYP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_TRAN_TYP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_TRAN_TYP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_TRAN_TYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_TRAN_TYP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_TRAN_TYP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_TRAN_TYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_TRAN_TYP_TOTAL.

*.........table declarations:.................................*
TABLES: *ZABST_TRAN_TYP                .
TABLES: /AGRI/GLFLOT                   .
TABLES: LFA1                           .
TABLES: ZABST_TRAN_TYP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
