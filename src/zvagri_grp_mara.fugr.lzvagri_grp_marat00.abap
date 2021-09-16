*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 30.08.2019 at 11:20:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVAGRI_GRP_MARA.................................*
TABLES: ZVAGRI_GRP_MARA, *ZVAGRI_GRP_MARA. "view work areas
CONTROLS: TCTRL_ZVAGRI_GRP_MARA
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVAGRI_GRP_MARA. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVAGRI_GRP_MARA.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVAGRI_GRP_MARA_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVAGRI_GRP_MARA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVAGRI_GRP_MARA_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVAGRI_GRP_MARA_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVAGRI_GRP_MARA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVAGRI_GRP_MARA_TOTAL.

*.........table declarations:.................................*
TABLES: T134                           .
TABLES: T134T                          .
TABLES: TWEW                           .
TABLES: TWEWT                          .
TABLES: ZAGRI_GRP_MARA                 .
