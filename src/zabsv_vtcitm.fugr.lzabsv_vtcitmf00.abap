*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 26.06.2019 at 08:45:09
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_VTCITM....................................*
FORM GET_DATA_ZABSV_VTCITM.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZABST_VTCITM WHERE
(VIM_WHERETAB) .
    CLEAR ZABSV_VTCITM .
ZABSV_VTCITM-MANDT =
ZABST_VTCITM-MANDT .
ZABSV_VTCITM-OBJID =
ZABST_VTCITM-OBJID .
ZABSV_VTCITM-K1VAL =
ZABST_VTCITM-K1VAL .
ZABSV_VTCITM-K2VAL =
ZABST_VTCITM-K2VAL .
ZABSV_VTCITM-K3VAL =
ZABST_VTCITM-K3VAL .
ZABSV_VTCITM-K4VAL =
ZABST_VTCITM-K4VAL .
ZABSV_VTCITM-K5VAL =
ZABST_VTCITM-K5VAL .
ZABSV_VTCITM-SEQNO =
ZABST_VTCITM-SEQNO .
ZABSV_VTCITM-CNVAL1 =
ZABST_VTCITM-CNVAL1 .
ZABSV_VTCITM-CNVAL2 =
ZABST_VTCITM-CNVAL2 .
ZABSV_VTCITM-CNVAL3 =
ZABST_VTCITM-CNVAL3 .
ZABSV_VTCITM-SIGN =
ZABST_VTCITM-SIGN .
ZABSV_VTCITM-OPTIN =
ZABST_VTCITM-OPTIN .
ZABSV_VTCITM-LOW =
ZABST_VTCITM-LOW .
ZABSV_VTCITM-HIGH =
ZABST_VTCITM-HIGH .
<VIM_TOTAL_STRUC> = ZABSV_VTCITM.
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
FORM DB_UPD_ZABSV_VTCITM .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZABSV_VTCITM.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZABSV_VTCITM-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZABST_VTCITM WHERE
  OBJID = ZABSV_VTCITM-OBJID AND
  K1VAL = ZABSV_VTCITM-K1VAL AND
  K2VAL = ZABSV_VTCITM-K2VAL AND
  K3VAL = ZABSV_VTCITM-K3VAL AND
  K4VAL = ZABSV_VTCITM-K4VAL AND
  K5VAL = ZABSV_VTCITM-K5VAL AND
  SEQNO = ZABSV_VTCITM-SEQNO .
    IF SY-SUBRC = 0.
    DELETE ZABST_VTCITM .
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
  SELECT SINGLE FOR UPDATE * FROM ZABST_VTCITM WHERE
  OBJID = ZABSV_VTCITM-OBJID AND
  K1VAL = ZABSV_VTCITM-K1VAL AND
  K2VAL = ZABSV_VTCITM-K2VAL AND
  K3VAL = ZABSV_VTCITM-K3VAL AND
  K4VAL = ZABSV_VTCITM-K4VAL AND
  K5VAL = ZABSV_VTCITM-K5VAL AND
  SEQNO = ZABSV_VTCITM-SEQNO .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZABST_VTCITM.
    ENDIF.
ZABST_VTCITM-MANDT =
ZABSV_VTCITM-MANDT .
ZABST_VTCITM-OBJID =
ZABSV_VTCITM-OBJID .
ZABST_VTCITM-K1VAL =
ZABSV_VTCITM-K1VAL .
ZABST_VTCITM-K2VAL =
ZABSV_VTCITM-K2VAL .
ZABST_VTCITM-K3VAL =
ZABSV_VTCITM-K3VAL .
ZABST_VTCITM-K4VAL =
ZABSV_VTCITM-K4VAL .
ZABST_VTCITM-K5VAL =
ZABSV_VTCITM-K5VAL .
ZABST_VTCITM-SEQNO =
ZABSV_VTCITM-SEQNO .
ZABST_VTCITM-CNVAL1 =
ZABSV_VTCITM-CNVAL1 .
ZABST_VTCITM-CNVAL2 =
ZABSV_VTCITM-CNVAL2 .
ZABST_VTCITM-CNVAL3 =
ZABSV_VTCITM-CNVAL3 .
ZABST_VTCITM-SIGN =
ZABSV_VTCITM-SIGN .
ZABST_VTCITM-OPTIN =
ZABSV_VTCITM-OPTIN .
ZABST_VTCITM-LOW =
ZABSV_VTCITM-LOW .
ZABST_VTCITM-HIGH =
ZABSV_VTCITM-HIGH .
    IF SY-SUBRC = 0.
    UPDATE ZABST_VTCITM ##WARN_OK.
    ELSE.
    INSERT ZABST_VTCITM .
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
CLEAR: STATUS_ZABSV_VTCITM-UPD_FLAG,
STATUS_ZABSV_VTCITM-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZABSV_VTCITM.
  SELECT SINGLE * FROM ZABST_VTCITM WHERE
OBJID = ZABSV_VTCITM-OBJID AND
K1VAL = ZABSV_VTCITM-K1VAL AND
K2VAL = ZABSV_VTCITM-K2VAL AND
K3VAL = ZABSV_VTCITM-K3VAL AND
K4VAL = ZABSV_VTCITM-K4VAL AND
K5VAL = ZABSV_VTCITM-K5VAL AND
SEQNO = ZABSV_VTCITM-SEQNO .
ZABSV_VTCITM-MANDT =
ZABST_VTCITM-MANDT .
ZABSV_VTCITM-OBJID =
ZABST_VTCITM-OBJID .
ZABSV_VTCITM-K1VAL =
ZABST_VTCITM-K1VAL .
ZABSV_VTCITM-K2VAL =
ZABST_VTCITM-K2VAL .
ZABSV_VTCITM-K3VAL =
ZABST_VTCITM-K3VAL .
ZABSV_VTCITM-K4VAL =
ZABST_VTCITM-K4VAL .
ZABSV_VTCITM-K5VAL =
ZABST_VTCITM-K5VAL .
ZABSV_VTCITM-SEQNO =
ZABST_VTCITM-SEQNO .
ZABSV_VTCITM-CNVAL1 =
ZABST_VTCITM-CNVAL1 .
ZABSV_VTCITM-CNVAL2 =
ZABST_VTCITM-CNVAL2 .
ZABSV_VTCITM-CNVAL3 =
ZABST_VTCITM-CNVAL3 .
ZABSV_VTCITM-SIGN =
ZABST_VTCITM-SIGN .
ZABSV_VTCITM-OPTIN =
ZABST_VTCITM-OPTIN .
ZABSV_VTCITM-LOW =
ZABST_VTCITM-LOW .
ZABSV_VTCITM-HIGH =
ZABST_VTCITM-HIGH .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZABSV_VTCITM USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZABSV_VTCITM-OBJID TO
ZABST_VTCITM-OBJID .
MOVE ZABSV_VTCITM-K1VAL TO
ZABST_VTCITM-K1VAL .
MOVE ZABSV_VTCITM-K2VAL TO
ZABST_VTCITM-K2VAL .
MOVE ZABSV_VTCITM-K3VAL TO
ZABST_VTCITM-K3VAL .
MOVE ZABSV_VTCITM-K4VAL TO
ZABST_VTCITM-K4VAL .
MOVE ZABSV_VTCITM-K5VAL TO
ZABST_VTCITM-K5VAL .
MOVE ZABSV_VTCITM-SEQNO TO
ZABST_VTCITM-SEQNO .
MOVE ZABSV_VTCITM-MANDT TO
ZABST_VTCITM-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZABST_VTCITM'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZABST_VTCITM TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZABST_VTCITM'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*