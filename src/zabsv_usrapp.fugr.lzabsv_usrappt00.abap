*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.07.2020 at 07:54:37
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_USRAPP....................................*
TABLES: ZABSV_USRAPP, *ZABSV_USRAPP. "view work areas
CONTROLS: TCTRL_ZABSV_USRAPP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABSV_USRAPP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABSV_USRAPP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABSV_USRAPP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABSV_USRAPP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_USRAPP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABSV_USRAPP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABSV_USRAPP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABSV_USRAPP_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_USRAPP                    .
