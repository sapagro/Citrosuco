*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFMFP_VARIEDADES
*   generation date: 08.10.2019 at 10:24:26
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFMFP_VARIEDADES   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
