*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABS_FG_USR_CENT
*   generation date: 03.09.2020 at 14:34:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABS_FG_USR_CENT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
