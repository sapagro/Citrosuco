*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABST_SF_ESTIMAT
*   generation date: 23.02.2020 at 14:00:01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABST_SF_ESTIMAT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
