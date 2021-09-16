*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 27.07.2020 at 07:54:37
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_USRAPP....................................*
FORM GET_DATA_ZABSV_USRAPP.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZABS_USRAPP WHERE
(VIM_WHERETAB) .
    CLEAR ZABSV_USRAPP .
ZABSV_USRAPP-MANDT =
ZABS_USRAPP-MANDT .
ZABSV_USRAPP-UROLE =
ZABS_USRAPP-UROLE .
ZABSV_USRAPP-COUNTER =
ZABS_USRAPP-COUNTER .
ZABSV_USRAPP-APP =
ZABS_USRAPP-APP .
ZABSV_USRAPP-ERNAM =
ZABS_USRAPP-ERNAM .
ZABSV_USRAPP-ERDAT =
ZABS_USRAPP-ERDAT .
ZABSV_USRAPP-ERZET =
ZABS_USRAPP-ERZET .
ZABSV_USRAPP-AENAM =
ZABS_USRAPP-AENAM .
ZABSV_USRAPP-AEDAT =
ZABS_USRAPP-AEDAT .
ZABSV_USRAPP-AEZET =
ZABS_USRAPP-AEZET .
<VIM_TOTAL_STRUC> = ZABSV_USRAPP.
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
FORM DB_UPD_ZABSV_USRAPP .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZABSV_USRAPP.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZABSV_USRAPP-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_USRAPP WHERE
  UROLE = ZABSV_USRAPP-UROLE AND
  COUNTER = ZABSV_USRAPP-COUNTER .
    IF SY-SUBRC = 0.
    DELETE ZABS_USRAPP .
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_USRAPP WHERE
  UROLE = ZABSV_USRAPP-UROLE AND
  COUNTER = ZABSV_USRAPP-COUNTER .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZABS_USRAPP.
    ENDIF.
ZABS_USRAPP-MANDT =
ZABSV_USRAPP-MANDT .
ZABS_USRAPP-UROLE =
ZABSV_USRAPP-UROLE .
ZABS_USRAPP-COUNTER =
ZABSV_USRAPP-COUNTER .
ZABS_USRAPP-APP =
ZABSV_USRAPP-APP .
ZABS_USRAPP-ERNAM =
ZABSV_USRAPP-ERNAM .
ZABS_USRAPP-ERDAT =
ZABSV_USRAPP-ERDAT .
ZABS_USRAPP-ERZET =
ZABSV_USRAPP-ERZET .
ZABS_USRAPP-AENAM =
ZABSV_USRAPP-AENAM .
ZABS_USRAPP-AEDAT =
ZABSV_USRAPP-AEDAT .
ZABS_USRAPP-AEZET =
ZABSV_USRAPP-AEZET .
    IF SY-SUBRC = 0.
    UPDATE ZABS_USRAPP ##WARN_OK.
    ELSE.
    INSERT ZABS_USRAPP .
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
CLEAR: STATUS_ZABSV_USRAPP-UPD_FLAG,
STATUS_ZABSV_USRAPP-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZABSV_USRAPP.
  SELECT SINGLE * FROM ZABS_USRAPP WHERE
UROLE = ZABSV_USRAPP-UROLE AND
COUNTER = ZABSV_USRAPP-COUNTER .
ZABSV_USRAPP-MANDT =
ZABS_USRAPP-MANDT .
ZABSV_USRAPP-UROLE =
ZABS_USRAPP-UROLE .
ZABSV_USRAPP-COUNTER =
ZABS_USRAPP-COUNTER .
ZABSV_USRAPP-APP =
ZABS_USRAPP-APP .
ZABSV_USRAPP-ERNAM =
ZABS_USRAPP-ERNAM .
ZABSV_USRAPP-ERDAT =
ZABS_USRAPP-ERDAT .
ZABSV_USRAPP-ERZET =
ZABS_USRAPP-ERZET .
ZABSV_USRAPP-AENAM =
ZABS_USRAPP-AENAM .
ZABSV_USRAPP-AEDAT =
ZABS_USRAPP-AEDAT .
ZABSV_USRAPP-AEZET =
ZABS_USRAPP-AEZET .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZABSV_USRAPP USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZABSV_USRAPP-UROLE TO
ZABS_USRAPP-UROLE .
MOVE ZABSV_USRAPP-COUNTER TO
ZABS_USRAPP-COUNTER .
MOVE ZABSV_USRAPP-MANDT TO
ZABS_USRAPP-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZABS_USRAPP'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZABS_USRAPP TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZABS_USRAPP'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
