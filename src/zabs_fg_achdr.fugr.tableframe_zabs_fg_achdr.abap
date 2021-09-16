*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABS_FG_ACHDR
*   generation date: 18.09.2020 at 08:55:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABS_FG_ACHDR      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
