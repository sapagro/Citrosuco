*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZABST_MD_ATR_MAP
*   generation date: 18.09.2019 at 06:52:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZABST_MD_ATR_MAP   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
