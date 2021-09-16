*&---------------------------------------------------------------------*
*&      Form  FLDOC_CHECK_CHANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pldoc_check_changes  USING    lwa_pldata TYPE zsc_fmpl_data
                          CHANGING lv_changed.

  IF lwa_pldata-plhdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.
  IF lwa_pldata-plhdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.

  CHECK lv_changed IS INITIAL.
  LOOP AT lwa_pldata-plitm TRANSPORTING NO FIELDS
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
FORM pl_save  USING lv_set_update_task
                          lwa_pldoc TYPE zsc_fmpl_doc
                          lref_text TYPE REF TO /agri/cl_gtext_process.

  DATA: lt_xplhdr TYPE zt_fmplhdr,
        lt_yplhdr TYPE zt_fmplhdr,
        lt_xplitm TYPE zt_fmplitm,
        lt_yplitm TYPE zt_fmplitm,
        lt_pldoc  TYPE zsc_fmpl_doc.

  DATA:
    lwa_plhdr TYPE zsc_fmplhdr,
    lwa_plitm TYPE zsc_fmplitm.

  DEFINE pl_xy_tables_fill.
    APPEND lwa_pldoc-&1-plhdr TO lt_&1plhdr.

    LOOP AT lwa_pldoc-&1-plitm INTO lwa_plitm.
      APPEND lwa_plitm TO lt_&1plitm.
    ENDLOOP.

  END-OF-DEFINITION.

  pl_xy_tables_fill x.
  pl_xy_tables_fill y.

  CALL FUNCTION 'ZFMPL_UPDATE'
    EXPORTING
      i_set_update_task = lv_set_update_task
    TABLES
      t_xplhdr          = lt_xplhdr
      t_yplhdr          = lt_yplhdr
      t_xplitm          = lt_xplitm
      t_yplitm          = lt_yplitm.

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
FORM pl_label_replace USING lwa_pldoc TYPE zsc_fmpl_doc
                            lref_text TYPE REF TO /agri/cl_gtext_process.

  DATA: lv_plnum_old TYPE zfmplnum.

  FIELD-SYMBOLS:
                 <lwa_plitm>  TYPE zsc_fmplitm.

  lv_plnum_old = lwa_pldoc-x-plhdr-plnum.
  lwa_pldoc-x-plhdr-plnum = lwa_pldoc-plnum.

  LOOP AT lwa_pldoc-x-plitm ASSIGNING <lwa_plitm>.
    <lwa_plitm>-plnum = lwa_pldoc-x-plhdr-plnum.
  ENDLOOP.

  PERFORM text_object_value_switch USING lwa_pldoc-plnum
                                CHANGING lref_text.

ENDFORM.                    " ac_label_replace

FORM pl_number_generate USING lv_pltyp TYPE ZFMPLANN
                     CHANGING lv_plnum
                              lv_failed.

  DATA: ls_fmpltyp TYPE ztfmpltyp.

  SELECT SINGLE * FROM ztfmpltyp
           INTO ls_fmpltyp
                WHERE plann = lv_pltyp.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = ls_fmpltyp-numke
      object                  = c_number_range-pl
    IMPORTING
      number                  = lv_plnum
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
