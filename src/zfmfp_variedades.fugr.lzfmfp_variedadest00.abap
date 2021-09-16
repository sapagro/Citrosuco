*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.10.2019 at 10:24:26
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFMFP_VARIEDADES................................*
DATA:  BEGIN OF STATUS_ZFMFP_VARIEDADES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFMFP_VARIEDADES              .
CONTROLS: TCTRL_ZFMFP_VARIEDADES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFMFP_VARIEDADES              .
TABLES: ZFMFP_VARIEDADES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
