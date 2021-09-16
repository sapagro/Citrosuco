*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABS_FG_FMRCTYP
*   generation date: 31.03.2020 at 08:15:45
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABS_FG_FMRCTYP    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
