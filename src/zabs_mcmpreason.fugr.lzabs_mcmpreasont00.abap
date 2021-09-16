*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.04.2020 at 12:50:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_VMCMPREASON................................*
TABLES: ZABS_VMCMPREASON, *ZABS_VMCMPREASON. "view work areas
CONTROLS: TCTRL_ZABS_VMCMPREASON
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZABS_VMCMPREASON. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZABS_VMCMPREASON.
* Table for entries selected to show on screen
DATA: BEGIN OF ZABS_VMCMPREASON_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZABS_VMCMPREASON.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VMCMPREASON_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZABS_VMCMPREASON_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZABS_VMCMPREASON.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZABS_VMCMPREASON_TOTAL.

*.........table declarations:.................................*
TABLES: ZABS_MCMPREASON                .
