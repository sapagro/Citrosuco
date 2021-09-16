*&---------------------------------------------------------------------*
*&      Form  FLDOC_CHECK_CHANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acdoc_check_changes  USING    lwa_acdata TYPE zsc_fmac_data
                          CHANGING lv_changed.

  IF lwa_acdata-achdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.
  IF lwa_acdata-achdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.

  CHECK lv_changed IS INITIAL.
  LOOP AT lwa_acdata-acitm TRANSPORTING NO FIELDS
                            WHERE updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDLOOP.

  CHECK lv_changed IS INITIAL.
  LOOP AT lwa_acdata-acvlc TRANSPORTING NO FIELDS
                            WHERE updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDLOOP.

ENDFORM.                    " FLDOC_CHECK_CHANGES
*&---------------------------------------------------------------------*
*&      Form  fl_save_addnl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_save  USING lv_set_update_task
                          lwa_acdoc TYPE zsc_fmac_doc
                          lref_text TYPE REF TO /agri/cl_gtext_process.


  DATA: lt_xachdr TYPE zt_fmachdr,
        lt_yachdr TYPE zt_fmachdr,
        lt_xacitm TYPE zt_fmacitm,
        lt_yacitm TYPE zt_fmacitm,
        lt_xacvlc TYPE zt_fmacvlcl,
        lt_yacvlc TYPE zt_fmacvlcl,
        lt_acdoc  TYPE zsc_fmac_doc.

  DATA:
    lwa_achdr TYPE zsc_fmachdr,
    lwa_acitm TYPE zsc_fmacitm,
    lwa_acvlc TYPE zsc_fmacvlcl.

  DEFINE ac_xy_tables_fill.
    APPEND lwa_acdoc-&1-achdr TO lt_&1achdr.

    LOOP AT lwa_acdoc-&1-acitm INTO lwa_acitm.
      APPEND lwa_acitm TO lt_&1acitm.
    ENDLOOP.

     LOOP AT lwa_acdoc-&1-acvlc INTO lwa_acvlc.
      APPEND lwa_acvlc TO lt_&1acvlc.
    ENDLOOP.

  END-OF-DEFINITION.

  ac_xy_tables_fill x.
  ac_xy_tables_fill y.

  CALL FUNCTION 'ZFMAC_UPDATE'
    EXPORTING
      i_set_update_task = lv_set_update_task
    TABLES
      t_xachdr          = lt_xachdr
      t_yachdr          = lt_yachdr
      t_xacitm          = lt_xacitm
      t_yacitm          = lt_yacitm
      t_xacvlc          = lt_xacvlc
      t_yacvlc          = lt_yacvlc.

  CALL FUNCTION 'ADDR_MEMORY_SAVE'
    EXPORTING
      execute_in_update_task = lv_set_update_task
    EXCEPTIONS
      address_number_missing = 1
      person_number_missing  = 2
      internal_error         = 3
      database_error         = 4
      reference_missing      = 5
      OTHERS                 = 6.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'COMMIT_TEXT'.

****Save Texts
  IF NOT lref_text IS INITIAL.
**** ESP6 Task #30035 - Global Text Engine Integration
*    CALL METHOD lref_text->text_save.
    CALL METHOD lref_text->text_save
      EXPORTING
        i_set_update_task = lv_set_update_task.
****
  ENDIF.

ENDFORM.                    " fl_save_addnl
*&---------------------------------------------------------------------*
*&      Form  FL_SAVE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fl_save_status  USING lv_set_update_task
                           lwa_acdoc TYPE /agri/s_glfl_doc.

ENDFORM.                    " FL_SAVE_STATUS
*&---------------------------------------------------------------------*
*&      Form  ac_label_replace
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_label_replace USING lwa_acdoc TYPE zsc_fmac_doc
                            lref_text TYPE REF TO /agri/cl_gtext_process.

  DATA: lv_acnum_old TYPE zfmacnum.

  FIELD-SYMBOLS:
    <lwa_acitm> TYPE zsc_fmacitm,
    <lwa_acvlc> TYPE zsc_fmacvlcl.

  lv_acnum_old = lwa_acdoc-x-achdr-acnum.
  lwa_acdoc-x-achdr-acnum = lwa_acdoc-acnum.

  LOOP AT lwa_acdoc-x-acitm ASSIGNING <lwa_acitm>.
    <lwa_acitm>-acnum = lwa_acdoc-x-achdr-acnum.
  ENDLOOP.

  LOOP AT lwa_acdoc-x-acvlc ASSIGNING <lwa_acvlc>.
    <lwa_acvlc>-acnum = lwa_acdoc-x-achdr-acnum.
  ENDLOOP.

  PERFORM text_object_value_switch USING lwa_acdoc-acnum
                                CHANGING lref_text.

ENDFORM.                    " ac_label_replace

FORM ac_number_generate USING lv_tplkz
                              lv_actyp
                     CHANGING lv_acnum
                              lv_failed.


  DATA: ls_fmactyp TYPE ztfmactyp.

  SELECT SINGLE * FROM ztfmactyp
                  INTO ls_fmactyp
                       WHERE actyp = lv_actyp.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = ls_fmactyp-numki
      object                  = c_number_range-ac
    IMPORTING
      number                  = lv_acnum
    EXCEPTIONS
      interval_not_found      = 01
      number_range_not_intern = 02
      object_not_found        = 03.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
    lv_failed = c_true.
    EXIT.
  ENDIF.

ENDFORM.
