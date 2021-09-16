*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 30.08.2019 at 11:20:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVAGRI_GRP_MARA.................................*
FORM GET_DATA_ZVAGRI_GRP_MARA.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZAGRI_GRP_MARA WHERE
(VIM_WHERETAB) .
    CLEAR ZVAGRI_GRP_MARA .
ZVAGRI_GRP_MARA-MANDT =
ZAGRI_GRP_MARA-MANDT .
ZVAGRI_GRP_MARA-SPRAS =
ZAGRI_GRP_MARA-SPRAS .
ZVAGRI_GRP_MARA-MTART =
ZAGRI_GRP_MARA-MTART .
ZVAGRI_GRP_MARA-EXTWG =
ZAGRI_GRP_MARA-EXTWG .
    SELECT SINGLE * FROM T134 WHERE
MTART = ZAGRI_GRP_MARA-MTART .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T134T WHERE
MTART = T134-MTART AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-MTBEZ =
T134T-MTBEZ .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM TWEW WHERE
EXTWG = ZAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TWEWT WHERE
EXTWG = TWEW-EXTWG AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-EWBEZ =
TWEWT-EWBEZ .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZVAGRI_GRP_MARA.
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
FORM DB_UPD_ZVAGRI_GRP_MARA .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVAGRI_GRP_MARA.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVAGRI_GRP_MARA-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZAGRI_GRP_MARA WHERE
  SPRAS = ZVAGRI_GRP_MARA-SPRAS AND
  MTART = ZVAGRI_GRP_MARA-MTART AND
  EXTWG = ZVAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC = 0.
    DELETE ZAGRI_GRP_MARA .
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
  SELECT SINGLE FOR UPDATE * FROM ZAGRI_GRP_MARA WHERE
  SPRAS = ZVAGRI_GRP_MARA-SPRAS AND
  MTART = ZVAGRI_GRP_MARA-MTART AND
  EXTWG = ZVAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZAGRI_GRP_MARA.
    ENDIF.
ZAGRI_GRP_MARA-MANDT =
ZVAGRI_GRP_MARA-MANDT .
ZAGRI_GRP_MARA-SPRAS =
ZVAGRI_GRP_MARA-SPRAS .
ZAGRI_GRP_MARA-MTART =
ZVAGRI_GRP_MARA-MTART .
ZAGRI_GRP_MARA-EXTWG =
ZVAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC = 0.
    UPDATE ZAGRI_GRP_MARA ##WARN_OK.
    ELSE.
    INSERT ZAGRI_GRP_MARA .
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
CLEAR: STATUS_ZVAGRI_GRP_MARA-UPD_FLAG,
STATUS_ZVAGRI_GRP_MARA-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZVAGRI_GRP_MARA.
  SELECT SINGLE * FROM ZAGRI_GRP_MARA WHERE
SPRAS = ZVAGRI_GRP_MARA-SPRAS AND
MTART = ZVAGRI_GRP_MARA-MTART AND
EXTWG = ZVAGRI_GRP_MARA-EXTWG .
ZVAGRI_GRP_MARA-MANDT =
ZAGRI_GRP_MARA-MANDT .
ZVAGRI_GRP_MARA-SPRAS =
ZAGRI_GRP_MARA-SPRAS .
ZVAGRI_GRP_MARA-MTART =
ZAGRI_GRP_MARA-MTART .
ZVAGRI_GRP_MARA-EXTWG =
ZAGRI_GRP_MARA-EXTWG .
    SELECT SINGLE * FROM T134 WHERE
MTART = ZAGRI_GRP_MARA-MTART .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T134T WHERE
MTART = T134-MTART AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-MTBEZ =
T134T-MTBEZ .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZVAGRI_GRP_MARA-MTBEZ .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVAGRI_GRP_MARA-MTBEZ .
    ENDIF.
    SELECT SINGLE * FROM TWEW WHERE
EXTWG = ZAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TWEWT WHERE
EXTWG = TWEW-EXTWG AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-EWBEZ =
TWEWT-EWBEZ .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZVAGRI_GRP_MARA-EWBEZ .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVAGRI_GRP_MARA-EWBEZ .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVAGRI_GRP_MARA USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVAGRI_GRP_MARA-SPRAS TO
ZAGRI_GRP_MARA-SPRAS .
MOVE ZVAGRI_GRP_MARA-MTART TO
ZAGRI_GRP_MARA-MTART .
MOVE ZVAGRI_GRP_MARA-EXTWG TO
ZAGRI_GRP_MARA-EXTWG .
MOVE ZVAGRI_GRP_MARA-MANDT TO
ZAGRI_GRP_MARA-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZAGRI_GRP_MARA'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZAGRI_GRP_MARA TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZAGRI_GRP_MARA'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZVAGRI_GRP_MARA USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZAGRI_GRP_MARA-MANDT =
ZVAGRI_GRP_MARA-MANDT .
ZAGRI_GRP_MARA-SPRAS =
ZVAGRI_GRP_MARA-SPRAS .
ZAGRI_GRP_MARA-MTART =
ZVAGRI_GRP_MARA-MTART .
ZAGRI_GRP_MARA-EXTWG =
ZVAGRI_GRP_MARA-EXTWG .
    SELECT SINGLE * FROM T134 WHERE
MTART = ZAGRI_GRP_MARA-MTART .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T134T WHERE
MTART = T134-MTART AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-MTBEZ =
T134T-MTBEZ .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZVAGRI_GRP_MARA-MTBEZ .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVAGRI_GRP_MARA-MTBEZ .
    ENDIF.
    SELECT SINGLE * FROM TWEW WHERE
EXTWG = ZAGRI_GRP_MARA-EXTWG .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TWEWT WHERE
EXTWG = TWEW-EXTWG AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZVAGRI_GRP_MARA-EWBEZ =
TWEWT-EWBEZ .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZVAGRI_GRP_MARA-EWBEZ .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZVAGRI_GRP_MARA-EWBEZ .
    ENDIF.
ENDFORM.