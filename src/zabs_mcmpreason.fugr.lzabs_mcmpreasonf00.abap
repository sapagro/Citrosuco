*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 29.04.2020 at 12:50:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_VMCMPREASON................................*
FORM GET_DATA_ZABS_VMCMPREASON.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZABS_MCMPREASON WHERE
(VIM_WHERETAB) .
    CLEAR ZABS_VMCMPREASON .
ZABS_VMCMPREASON-MANDT =
ZABS_MCMPREASON-MANDT .
ZABS_VMCMPREASON-REASON =
ZABS_MCMPREASON-REASON .
ZABS_VMCMPREASON-DESCRP =
ZABS_MCMPREASON-DESCRP .
<VIM_TOTAL_STRUC> = ZABS_VMCMPREASON.
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
FORM DB_UPD_ZABS_VMCMPREASON .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZABS_VMCMPREASON.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZABS_VMCMPREASON-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_MCMPREASON WHERE
  REASON = ZABS_VMCMPREASON-REASON .
    IF SY-SUBRC = 0.
    DELETE ZABS_MCMPREASON .
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_MCMPREASON WHERE
  REASON = ZABS_VMCMPREASON-REASON .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZABS_MCMPREASON.
    ENDIF.
ZABS_MCMPREASON-MANDT =
ZABS_VMCMPREASON-MANDT .
ZABS_MCMPREASON-REASON =
ZABS_VMCMPREASON-REASON .
ZABS_MCMPREASON-DESCRP =
ZABS_VMCMPREASON-DESCRP .
    IF SY-SUBRC = 0.
    UPDATE ZABS_MCMPREASON ##WARN_OK.
    ELSE.
    INSERT ZABS_MCMPREASON .
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
CLEAR: STATUS_ZABS_VMCMPREASON-UPD_FLAG,
STATUS_ZABS_VMCMPREASON-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZABS_VMCMPREASON.
  SELECT SINGLE * FROM ZABS_MCMPREASON WHERE
REASON = ZABS_VMCMPREASON-REASON .
ZABS_VMCMPREASON-MANDT =
ZABS_MCMPREASON-MANDT .
ZABS_VMCMPREASON-REASON =
ZABS_MCMPREASON-REASON .
ZABS_VMCMPREASON-DESCRP =
ZABS_MCMPREASON-DESCRP .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZABS_VMCMPREASON USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZABS_VMCMPREASON-REASON TO
ZABS_MCMPREASON-REASON .
MOVE ZABS_VMCMPREASON-MANDT TO
ZABS_MCMPREASON-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZABS_MCMPREASON'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZABS_MCMPREASON TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZABS_MCMPREASON'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*