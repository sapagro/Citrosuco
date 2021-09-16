*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.12.2020 at 09:15:05
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_BGP_ACCOM.................................*
TABLES: ZABSV_BGP_ACCOM, *ZABSV_BGP_ACCOM. "view work areas
CONTROLS: TCTRL_ZABSV_BGP_ACCOM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_BGP_ACCOM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_BGP_ACCOM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_BGP_ACCOM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_BGP_ACCOM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_BGP_ACCOM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_BGP_ACCOM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_BGP_ACCOM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_BGP_ACCOM_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_BGP_ACCOM                 .
