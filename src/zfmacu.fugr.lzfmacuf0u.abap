*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0U .
*----------------------------------------------------------------------*
FORM update_entries_collect  TABLES lt_xdata
                                    lt_ydata
                                    lt_insert
                                    lt_update
                                    lt_delete.

  FIELD-SYMBOLS: <lv_xupdkz>, <lv_yupdkz>.

  ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE lt_xdata TO <lv_xupdkz>.
  ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE lt_ydata TO <lv_yupdkz>.

  REFRESH: lt_delete, lt_insert, lt_update.

****Collect DELETE entries
  LOOP AT lt_ydata.
    CHECK: <lv_yupdkz> EQ c_updkz_delete.
    APPEND lt_ydata TO lt_delete.
  ENDLOOP.

****Collect INSERT and UPDATE entries
  LOOP AT lt_xdata.
    CASE <lv_xupdkz> .
      WHEN c_updkz_new.
        APPEND lt_xdata TO lt_insert.
      WHEN c_updkz_update.
        APPEND lt_xdata TO lt_update.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " UPDATE_ENTRIES_COLLECT

FORM user_statuses_save  USING lv_acnum TYPE zfmacnum
                               lv_objnr
                               lv_updkz
                               lv_update_task
                     CHANGING  lt_messages TYPE /agri/t_gprolog.

  CALL FUNCTION '/AGRI/G_USER_STATUS_SAVE_NEW'
    EXPORTING
      i_objnr           = lv_objnr
      i_updkz           = lv_updkz
      i_set_update_task = lv_update_task
      i_objkey          = lv_acnum
*     iref_object       = ref_status_handler
    IMPORTING
      et_messages       = lt_messages.

ENDFORM.                    " USER_STATUSES_SAVE
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS_CURRENT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_status_current_get  USING   lv_objnr
                           CHANGING   lv_ustat.

  CHECK NOT lv_objnr IS INITIAL.

  CALL FUNCTION '/AGRI/G_STATUS_ACTIVE_GET'
    EXPORTING
      i_objnr          = lv_objnr
    IMPORTING
      e_current_status = lv_ustat
* TABLES
*     T_STATUS         =
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " USER_STATUS_CURRENT_GET
