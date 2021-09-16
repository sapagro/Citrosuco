*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABST_TRAN_TYP
*   generation date: 06.04.2020 at 05:32:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABST_TRAN_TYP     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
