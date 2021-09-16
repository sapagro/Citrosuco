*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 29.10.2019 at 11:01:39
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVFMACWORK_SHIFT................................*
FORM GET_DATA_ZVFMACWORK_SHIFT.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZFMACWORK_SHIFT WHERE
(VIM_WHERETAB) .
    CLEAR ZVFMACWORK_SHIFT .
ZVFMACWORK_SHIFT-MANDT =
ZFMACWORK_SHIFT-MANDT .
ZVFMACWORK_SHIFT-WERKS =
ZFMACWORK_SHIFT-WERKS .
ZVFMACWORK_SHIFT-ARBPL =
ZFMACWORK_SHIFT-ARBPL .
ZVFMACWORK_SHIFT-FABKL =
ZFMACWORK_SHIFT-FABKL .
ZVFMACWORK_SHIFT-ACTRN =
ZFMACWORK_SHIFT-ACTRN .
ZVFMACWORK_SHIFT-ACPPG =
ZFMACWORK_SHIFT-ACPPG .
ZVFMACWORK_SHIFT-PERIOD =
ZFMACWORK_SHIFT-PERIOD .
ZVFMACWORK_SHIFT-ACDTR =
ZFMACWORK_SHIFT-ACDTR .
ZVFMACWORK_SHIFT-ACHIT =
ZFMACWORK_SHIFT-ACHIT .
ZVFMACWORK_SHIFT-ACHFT =
ZFMACWORK_SHIFT-ACHFT .
ZVFMACWORK_SHIFT-ACTUT =
ZFMACWORK_SHIFT-ACTUT .
ZVFMACWORK_SHIFT-ACOCP =
ZFMACWORK_SHIFT-ACOCP .
ZVFMACWORK_SHIFT-ACNCI =
ZFMACWORK_SHIFT-ACNCI .
ZVFMACWORK_SHIFT-ACCPD =
ZFMACWORK_SHIFT-ACCPD .
ZVFMACWORK_SHIFT-ACMEI =
ZFMACWORK_SHIFT-ACMEI .
<VIM_TOTAL_STRUC> = ZVFMACWORK_SHIFT.
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
FORM DB_UPD_ZVFMACWORK_SHIFT .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVFMACWORK_SHIFT.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVFMACWORK_SHIFT-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZFMACWORK_SHIFT WHERE
  WERKS = ZVFMACWORK_SHIFT-WERKS AND
  ARBPL = ZVFMACWORK_SHIFT-ARBPL AND
  FABKL = ZVFMACWORK_SHIFT-FABKL AND
  ACTRN = ZVFMACWORK_SHIFT-ACTRN AND
  ACPPG = ZVFMACWORK_SHIFT-ACPPG AND
  PERIOD = ZVFMACWORK_SHIFT-PERIOD .
    IF SY-SUBRC = 0.
    DELETE ZFMACWORK_SHIFT .
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
  SELECT SINGLE FOR UPDATE * FROM ZFMACWORK_SHIFT WHERE
  WERKS = ZVFMACWORK_SHIFT-WERKS AND
  ARBPL = ZVFMACWORK_SHIFT-ARBPL AND
  FABKL = ZVFMACWORK_SHIFT-FABKL AND
  ACTRN = ZVFMACWORK_SHIFT-ACTRN AND
  ACPPG = ZVFMACWORK_SHIFT-ACPPG AND
  PERIOD = ZVFMACWORK_SHIFT-PERIOD .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZFMACWORK_SHIFT.
    ENDIF.
ZFMACWORK_SHIFT-MANDT =
ZVFMACWORK_SHIFT-MANDT .
ZFMACWORK_SHIFT-WERKS =
ZVFMACWORK_SHIFT-WERKS .
ZFMACWORK_SHIFT-ARBPL =
ZVFMACWORK_SHIFT-ARBPL .
ZFMACWORK_SHIFT-FABKL =
ZVFMACWORK_SHIFT-FABKL .
ZFMACWORK_SHIFT-ACTRN =
ZVFMACWORK_SHIFT-ACTRN .
ZFMACWORK_SHIFT-ACPPG =
ZVFMACWORK_SHIFT-ACPPG .
ZFMACWORK_SHIFT-PERIOD =
ZVFMACWORK_SHIFT-PERIOD .
ZFMACWORK_SHIFT-ACDTR =
ZVFMACWORK_SHIFT-ACDTR .
ZFMACWORK_SHIFT-ACHIT =
ZVFMACWORK_SHIFT-ACHIT .
ZFMACWORK_SHIFT-ACHFT =
ZVFMACWORK_SHIFT-ACHFT .
ZFMACWORK_SHIFT-ACTUT =
ZVFMACWORK_SHIFT-ACTUT .
ZFMACWORK_SHIFT-ACOCP =
ZVFMACWORK_SHIFT-ACOCP .
ZFMACWORK_SHIFT-ACNCI =
ZVFMACWORK_SHIFT-ACNCI .
ZFMACWORK_SHIFT-ACCPD =
ZVFMACWORK_SHIFT-ACCPD .
ZFMACWORK_SHIFT-ACMEI =
ZVFMACWORK_SHIFT-ACMEI .
    IF SY-SUBRC = 0.
    UPDATE ZFMACWORK_SHIFT ##WARN_OK.
    ELSE.
    INSERT ZFMACWORK_SHIFT .
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
CLEAR: STATUS_ZVFMACWORK_SHIFT-UPD_FLAG,
STATUS_ZVFMACWORK_SHIFT-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZVFMACWORK_SHIFT.
  SELECT SINGLE * FROM ZFMACWORK_SHIFT WHERE
WERKS = ZVFMACWORK_SHIFT-WERKS AND
ARBPL = ZVFMACWORK_SHIFT-ARBPL AND
FABKL = ZVFMACWORK_SHIFT-FABKL AND
ACTRN = ZVFMACWORK_SHIFT-ACTRN AND
ACPPG = ZVFMACWORK_SHIFT-ACPPG AND
PERIOD = ZVFMACWORK_SHIFT-PERIOD .
ZVFMACWORK_SHIFT-MANDT =
ZFMACWORK_SHIFT-MANDT .
ZVFMACWORK_SHIFT-WERKS =
ZFMACWORK_SHIFT-WERKS .
ZVFMACWORK_SHIFT-ARBPL =
ZFMACWORK_SHIFT-ARBPL .
ZVFMACWORK_SHIFT-FABKL =
ZFMACWORK_SHIFT-FABKL .
ZVFMACWORK_SHIFT-ACTRN =
ZFMACWORK_SHIFT-ACTRN .
ZVFMACWORK_SHIFT-ACPPG =
ZFMACWORK_SHIFT-ACPPG .
ZVFMACWORK_SHIFT-PERIOD =
ZFMACWORK_SHIFT-PERIOD .
ZVFMACWORK_SHIFT-ACDTR =
ZFMACWORK_SHIFT-ACDTR .
ZVFMACWORK_SHIFT-ACHIT =
ZFMACWORK_SHIFT-ACHIT .
ZVFMACWORK_SHIFT-ACHFT =
ZFMACWORK_SHIFT-ACHFT .
ZVFMACWORK_SHIFT-ACTUT =
ZFMACWORK_SHIFT-ACTUT .
ZVFMACWORK_SHIFT-ACOCP =
ZFMACWORK_SHIFT-ACOCP .
ZVFMACWORK_SHIFT-ACNCI =
ZFMACWORK_SHIFT-ACNCI .
ZVFMACWORK_SHIFT-ACCPD =
ZFMACWORK_SHIFT-ACCPD .
ZVFMACWORK_SHIFT-ACMEI =
ZFMACWORK_SHIFT-ACMEI .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVFMACWORK_SHIFT USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVFMACWORK_SHIFT-WERKS TO
ZFMACWORK_SHIFT-WERKS .
MOVE ZVFMACWORK_SHIFT-ARBPL TO
ZFMACWORK_SHIFT-ARBPL .
MOVE ZVFMACWORK_SHIFT-FABKL TO
ZFMACWORK_SHIFT-FABKL .
MOVE ZVFMACWORK_SHIFT-ACTRN TO
ZFMACWORK_SHIFT-ACTRN .
MOVE ZVFMACWORK_SHIFT-ACPPG TO
ZFMACWORK_SHIFT-ACPPG .
MOVE ZVFMACWORK_SHIFT-PERIOD TO
ZFMACWORK_SHIFT-PERIOD .
MOVE ZVFMACWORK_SHIFT-MANDT TO
ZFMACWORK_SHIFT-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZFMACWORK_SHIFT'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZFMACWORK_SHIFT TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZFMACWORK_SHIFT'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*