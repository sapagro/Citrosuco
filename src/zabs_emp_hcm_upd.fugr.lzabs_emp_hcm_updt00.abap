*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.06.2021 at 14:15:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_EMP_HCM_UPD................................*
DATA:  BEGIN OF STATUS_ZABS_EMP_HCM_UPD              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_EMP_HCM_UPD              .
CONTROLS: TCTRL_ZABS_EMP_HCM_UPD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_EMP_HCM_UPD              .
TABLES: ZABS_EMP_HCM_UPD               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
