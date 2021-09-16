*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 01.12.2020 at 09:22:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABSV_BGP_ACELOG................................*
FORM GET_DATA_ZABSV_BGP_ACELOG.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZABS_BGP_ACCELOG WHERE
(VIM_WHERETAB) .
    CLEAR ZABSV_BGP_ACELOG .
ZABSV_BGP_ACELOG-MANDT =
ZABS_BGP_ACCELOG-MANDT .
ZABSV_BGP_ACELOG-ZZMACKEY =
ZABS_BGP_ACCELOG-ZZMACKEY .
ZABSV_BGP_ACELOG-ZZKEYPOSNR =
ZABS_BGP_ACCELOG-ZZKEYPOSNR .
ZABSV_BGP_ACELOG-MSGID =
ZABS_BGP_ACCELOG-MSGID .
ZABSV_BGP_ACELOG-MSGNO =
ZABS_BGP_ACCELOG-MSGNO .
ZABSV_BGP_ACELOG-MSGTYP =
ZABS_BGP_ACCELOG-MSGTYP .
ZABSV_BGP_ACELOG-MSGTXT =
ZABS_BGP_ACCELOG-MSGTXT .
ZABSV_BGP_ACELOG-ERNAM =
ZABS_BGP_ACCELOG-ERNAM .
ZABSV_BGP_ACELOG-ERDAT =
ZABS_BGP_ACCELOG-ERDAT .
ZABSV_BGP_ACELOG-ERZET =
ZABS_BGP_ACCELOG-ERZET .
ZABSV_BGP_ACELOG-AENAM =
ZABS_BGP_ACCELOG-AENAM .
ZABSV_BGP_ACELOG-AEDAT =
ZABS_BGP_ACCELOG-AEDAT .
ZABSV_BGP_ACELOG-AEZET =
ZABS_BGP_ACCELOG-AEZET .
<VIM_TOTAL_STRUC> = ZABSV_BGP_ACELOG.
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
FORM DB_UPD_ZABSV_BGP_ACELOG .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZABSV_BGP_ACELOG.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZABSV_BGP_ACELOG-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_BGP_ACCELOG WHERE
  ZZMACKEY = ZABSV_BGP_ACELOG-ZZMACKEY AND
  ZZKEYPOSNR = ZABSV_BGP_ACELOG-ZZKEYPOSNR .
    IF SY-SUBRC = 0.
    DELETE ZABS_BGP_ACCELOG .
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
  SELECT SINGLE FOR UPDATE * FROM ZABS_BGP_ACCELOG WHERE
  ZZMACKEY = ZABSV_BGP_ACELOG-ZZMACKEY AND
  ZZKEYPOSNR = ZABSV_BGP_ACELOG-ZZKEYPOSNR .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZABS_BGP_ACCELOG.
    ENDIF.
ZABS_BGP_ACCELOG-MANDT =
ZABSV_BGP_ACELOG-MANDT .
ZABS_BGP_ACCELOG-ZZMACKEY =
ZABSV_BGP_ACELOG-ZZMACKEY .
ZABS_BGP_ACCELOG-ZZKEYPOSNR =
ZABSV_BGP_ACELOG-ZZKEYPOSNR .
ZABS_BGP_ACCELOG-MSGID =
ZABSV_BGP_ACELOG-MSGID .
ZABS_BGP_ACCELOG-MSGNO =
ZABSV_BGP_ACELOG-MSGNO .
ZABS_BGP_ACCELOG-MSGTYP =
ZABSV_BGP_ACELOG-MSGTYP .
ZABS_BGP_ACCELOG-MSGTXT =
ZABSV_BGP_ACELOG-MSGTXT .
ZABS_BGP_ACCELOG-ERNAM =
ZABSV_BGP_ACELOG-ERNAM .
ZABS_BGP_ACCELOG-ERDAT =
ZABSV_BGP_ACELOG-ERDAT .
ZABS_BGP_ACCELOG-ERZET =
ZABSV_BGP_ACELOG-ERZET .
ZABS_BGP_ACCELOG-AENAM =
ZABSV_BGP_ACELOG-AENAM .
ZABS_BGP_ACCELOG-AEDAT =
ZABSV_BGP_ACELOG-AEDAT .
ZABS_BGP_ACCELOG-AEZET =
ZABSV_BGP_ACELOG-AEZET .
    IF SY-SUBRC = 0.
    UPDATE ZABS_BGP_ACCELOG ##WARN_OK.
    ELSE.
    INSERT ZABS_BGP_ACCELOG .
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
CLEAR: STATUS_ZABSV_BGP_ACELOG-UPD_FLAG,
STATUS_ZABSV_BGP_ACELOG-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZABSV_BGP_ACELOG.
  SELECT SINGLE * FROM ZABS_BGP_ACCELOG WHERE
ZZMACKEY = ZABSV_BGP_ACELOG-ZZMACKEY AND
ZZKEYPOSNR = ZABSV_BGP_ACELOG-ZZKEYPOSNR .
ZABSV_BGP_ACELOG-MANDT =
ZABS_BGP_ACCELOG-MANDT .
ZABSV_BGP_ACELOG-ZZMACKEY =
ZABS_BGP_ACCELOG-ZZMACKEY .
ZABSV_BGP_ACELOG-ZZKEYPOSNR =
ZABS_BGP_ACCELOG-ZZKEYPOSNR .
ZABSV_BGP_ACELOG-MSGID =
ZABS_BGP_ACCELOG-MSGID .
ZABSV_BGP_ACELOG-MSGNO =
ZABS_BGP_ACCELOG-MSGNO .
ZABSV_BGP_ACELOG-MSGTYP =
ZABS_BGP_ACCELOG-MSGTYP .
ZABSV_BGP_ACELOG-MSGTXT =
ZABS_BGP_ACCELOG-MSGTXT .
ZABSV_BGP_ACELOG-ERNAM =
ZABS_BGP_ACCELOG-ERNAM .
ZABSV_BGP_ACELOG-ERDAT =
ZABS_BGP_ACCELOG-ERDAT .
ZABSV_BGP_ACELOG-ERZET =
ZABS_BGP_ACCELOG-ERZET .
ZABSV_BGP_ACELOG-AENAM =
ZABS_BGP_ACCELOG-AENAM .
ZABSV_BGP_ACELOG-AEDAT =
ZABS_BGP_ACCELOG-AEDAT .
ZABSV_BGP_ACELOG-AEZET =
ZABS_BGP_ACCELOG-AEZET .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZABSV_BGP_ACELOG USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZABSV_BGP_ACELOG-ZZMACKEY TO
ZABS_BGP_ACCELOG-ZZMACKEY .
MOVE ZABSV_BGP_ACELOG-ZZKEYPOSNR TO
ZABS_BGP_ACCELOG-ZZKEYPOSNR .
MOVE ZABSV_BGP_ACELOG-MANDT TO
ZABS_BGP_ACCELOG-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZABS_BGP_ACCELOG'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZABS_BGP_ACCELOG TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZABS_BGP_ACCELOG'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
