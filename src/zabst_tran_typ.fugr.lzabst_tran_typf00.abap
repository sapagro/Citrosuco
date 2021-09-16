*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 09.11.2020 at 05:15:34
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_TRAN_TYP..................................*
FORM GET_DATA_ZABSV_TRAN_TYP.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZABST_TRAN_TYP WHERE
(VIM_WHERETAB) .
    CLEAR ZABSV_TRAN_TYP .
ZABSV_TRAN_TYP-MANDT =
ZABST_TRAN_TYP-MANDT .
ZABSV_TRAN_TYP-FARM =
ZABST_TRAN_TYP-FARM .
ZABSV_TRAN_TYP-INT_TR_PROV =
ZABST_TRAN_TYP-INT_TR_PROV .
ZABSV_TRAN_TYP-IN_LICPLATE =
ZABST_TRAN_TYP-IN_LICPLATE .
ZABSV_TRAN_TYP-TRANS_BORD =
ZABST_TRAN_TYP-TRANS_BORD .
ZABSV_TRAN_TYP-LOADER =
ZABST_TRAN_TYP-LOADER .
    SELECT SINGLE * FROM /AGRI/GLFLOT WHERE
TPLNR_FL = ZABST_TRAN_TYP-FARM .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-PLTXT =
/AGRI/GLFLOT-PLTXT .
    ENDIF.
    SELECT SINGLE * FROM LFA1 WHERE
LIFNR = ZABST_TRAN_TYP-INT_TR_PROV .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-NAME1 =
LFA1-NAME1 .
    ENDIF.
<VIM_TOTAL_STRUC> = ZABSV_TRAN_TYP.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZABSV_TRAN_TYP .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZABSV_TRAN_TYP.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZABSV_TRAN_TYP-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZABST_TRAN_TYP WHERE
  FARM = ZABSV_TRAN_TYP-FARM AND
  INT_TR_PROV = ZABSV_TRAN_TYP-INT_TR_PROV AND
  IN_LICPLATE = ZABSV_TRAN_TYP-IN_LICPLATE .
    IF SY-SUBRC = 0.
    DELETE ZABST_TRAN_TYP .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZABST_TRAN_TYP WHERE
  FARM = ZABSV_TRAN_TYP-FARM AND
  INT_TR_PROV = ZABSV_TRAN_TYP-INT_TR_PROV AND
  IN_LICPLATE = ZABSV_TRAN_TYP-IN_LICPLATE .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZABST_TRAN_TYP.
    ENDIF.
ZABST_TRAN_TYP-MANDT =
ZABSV_TRAN_TYP-MANDT .
ZABST_TRAN_TYP-FARM =
ZABSV_TRAN_TYP-FARM .
ZABST_TRAN_TYP-INT_TR_PROV =
ZABSV_TRAN_TYP-INT_TR_PROV .
ZABST_TRAN_TYP-IN_LICPLATE =
ZABSV_TRAN_TYP-IN_LICPLATE .
ZABST_TRAN_TYP-TRANS_BORD =
ZABSV_TRAN_TYP-TRANS_BORD .
ZABST_TRAN_TYP-LOADER =
ZABSV_TRAN_TYP-LOADER .
    IF SY-SUBRC = 0.
    UPDATE ZABST_TRAN_TYP ##WARN_OK.
    ELSE.
    INSERT ZABST_TRAN_TYP .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZABSV_TRAN_TYP-UPD_FLAG,
STATUS_ZABSV_TRAN_TYP-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZABSV_TRAN_TYP.
  SELECT SINGLE * FROM ZABST_TRAN_TYP WHERE
FARM = ZABSV_TRAN_TYP-FARM AND
INT_TR_PROV = ZABSV_TRAN_TYP-INT_TR_PROV AND
IN_LICPLATE = ZABSV_TRAN_TYP-IN_LICPLATE .
ZABSV_TRAN_TYP-MANDT =
ZABST_TRAN_TYP-MANDT .
ZABSV_TRAN_TYP-FARM =
ZABST_TRAN_TYP-FARM .
ZABSV_TRAN_TYP-INT_TR_PROV =
ZABST_TRAN_TYP-INT_TR_PROV .
ZABSV_TRAN_TYP-IN_LICPLATE =
ZABST_TRAN_TYP-IN_LICPLATE .
ZABSV_TRAN_TYP-TRANS_BORD =
ZABST_TRAN_TYP-TRANS_BORD .
ZABSV_TRAN_TYP-LOADER =
ZABST_TRAN_TYP-LOADER .
    SELECT SINGLE * FROM /AGRI/GLFLOT WHERE
TPLNR_FL = ZABST_TRAN_TYP-FARM .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-PLTXT =
/AGRI/GLFLOT-PLTXT .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZABSV_TRAN_TYP-PLTXT .
    ENDIF.
    SELECT SINGLE * FROM LFA1 WHERE
LIFNR = ZABST_TRAN_TYP-INT_TR_PROV .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-NAME1 =
LFA1-NAME1 .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZABSV_TRAN_TYP-NAME1 .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZABSV_TRAN_TYP USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZABSV_TRAN_TYP-FARM TO
ZABST_TRAN_TYP-FARM .
MOVE ZABSV_TRAN_TYP-INT_TR_PROV TO
ZABST_TRAN_TYP-INT_TR_PROV .
MOVE ZABSV_TRAN_TYP-IN_LICPLATE TO
ZABST_TRAN_TYP-IN_LICPLATE .
MOVE ZABSV_TRAN_TYP-MANDT TO
ZABST_TRAN_TYP-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZABST_TRAN_TYP'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZABST_TRAN_TYP TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZABST_TRAN_TYP'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZABSV_TRAN_TYP USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZABST_TRAN_TYP-MANDT =
ZABSV_TRAN_TYP-MANDT .
ZABST_TRAN_TYP-FARM =
ZABSV_TRAN_TYP-FARM .
ZABST_TRAN_TYP-INT_TR_PROV =
ZABSV_TRAN_TYP-INT_TR_PROV .
ZABST_TRAN_TYP-IN_LICPLATE =
ZABSV_TRAN_TYP-IN_LICPLATE .
ZABST_TRAN_TYP-TRANS_BORD =
ZABSV_TRAN_TYP-TRANS_BORD .
ZABST_TRAN_TYP-LOADER =
ZABSV_TRAN_TYP-LOADER .
    SELECT SINGLE * FROM /AGRI/GLFLOT WHERE
TPLNR_FL = ZABST_TRAN_TYP-FARM .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-PLTXT =
/AGRI/GLFLOT-PLTXT .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZABSV_TRAN_TYP-PLTXT .
    ENDIF.
    SELECT SINGLE * FROM LFA1 WHERE
LIFNR = ZABST_TRAN_TYP-INT_TR_PROV .
    IF SY-SUBRC EQ 0.
ZABSV_TRAN_TYP-NAME1 =
LFA1-NAME1 .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZABSV_TRAN_TYP-NAME1 .
    ENDIF.
ENDFORM.

* base table related FORM-routines.............
INCLUDE LSVIMFTX .