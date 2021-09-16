*&---------------------------------------------------------------------*
*&      Form  FLDOC_CHECK_CHANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rcdoc_check_changes  USING    lwa_acdata TYPE zsc_fmrc_data
                          CHANGING lv_changed.

  IF lwa_acdata-rchdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.
  IF lwa_acdata-rchdr-updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDIF.

  CHECK lv_changed IS INITIAL.

  CHECK lv_changed IS INITIAL.
  LOOP AT lwa_acdata-rclst TRANSPORTING NO FIELDS
                            WHERE updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDLOOP.

  CHECK lv_changed IS INITIAL.
  LOOP AT lwa_acdata-rcbom TRANSPORTING NO FIELDS
                            WHERE updkz IS NOT INITIAL.
    lv_changed = abap_true.
    EXIT.
  ENDLOOP.

CHECK lv_changed IS INITIAL.
  LOOP AT lwa_acdata-rcvrs TRANSPORTING NO FIELDS
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
FORM recipe_save  USING lv_set_update_task
                          lwa_rcdoc TYPE zsc_fmrc_doc
                          lref_text TYPE REF TO /agri/cl_gtext_process.


  DATA: lt_xrchdr TYPE zt_fmrchdr,
        lt_yrchdr TYPE zt_fmrchdr,
        lt_xrclst TYPE zt_fmrclst,
        lt_yrclst TYPE zt_fmrclst,
        lt_xrcbom TYPE zt_fmrcbom,
        lt_yrcbom TYPE zt_fmrcbom,
        lt_xrcvrs TYPE zt_fmrcvrs,
        lt_yrcvrs TYPE zt_fmrcvrs,
        lt_rcdoc  TYPE zsc_fmrc_doc.

  DATA:
    lwa_rchdr TYPE zsc_fmrchdr,
    lwa_rclst TYPE zsc_fmrclst,
    lwa_rcvrs TYPE zsc_fmrcvrs,
    lwa_rcbom TYPE zsc_fmrcbom.

  DEFINE recipe_xy_tables_fill.
    APPEND lwa_rcdoc-&1-rchdr TO lt_&1rchdr.

    LOOP AT lwa_rcdoc-&1-rclst INTO lwa_rclst.
      APPEND lwa_rclst TO lt_&1rclst.
    ENDLOOP.

     LOOP AT lwa_rcdoc-&1-rcbom INTO lwa_rcbom.
      APPEND lwa_rcbom TO lt_&1rcbom.
    ENDLOOP.

     LOOP AT lwa_rcdoc-&1-rcvrs INTO lwa_rcvrs.
      APPEND lwa_rcvrs TO lt_&1rcvrs.
    ENDLOOP.

  END-OF-DEFINITION.

  recipe_xy_tables_fill x.
  recipe_xy_tables_fill y.

  CALL FUNCTION 'ZFMRC_UPDATE'
    EXPORTING
      i_set_update_task = lv_set_update_task
    TABLES
      t_xrchdr          = lt_xrchdr
      t_yrchdr          = lt_yrchdr
      t_xrclst          = lt_xrclst
      t_yrclst          = lt_yrclst
      t_xrcbom          = lt_xrcbom
      t_yrcbom          = lt_yrcbom
      t_xrcvrs          = lt_xrcvrs
      t_yrcvrs          = lt_yrcvrs.

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
                           lwa_rcdoc TYPE /agri/s_glfl_doc.

ENDFORM.                    " FL_SAVE_STATUS
*&---------------------------------------------------------------------*
*&      Form  ac_label_replace
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rc_tables_replace USING lwa_rcdoc TYPE zsc_fmrc_doc
                            lref_text TYPE REF TO /agri/cl_gtext_process.

  DATA: lv_rcnum_old TYPE zfmrcnum.

  FIELD-SYMBOLS:
    <lwa_rclst> TYPE zsc_fmrclst,
    <lwa_rcbom> TYPE zsc_fmrcbom,
    <lwa_rcvrs> TYPE zsc_fmrcvrs.

  lv_rcnum_old = lwa_rcdoc-x-rchdr-rcnum.
  lwa_rcdoc-x-rchdr-rcnum = lwa_rcdoc-rcnum.

  LOOP AT lwa_rcdoc-x-rclst ASSIGNING <lwa_rclst>.
    <lwa_rclst>-rcnum = lwa_rcdoc-x-rchdr-rcnum.
  ENDLOOP.

  LOOP AT lwa_rcdoc-x-rcbom ASSIGNING <lwa_rcbom>.
    <lwa_rcbom>-rcnum = lwa_rcdoc-x-rchdr-rcnum.
  ENDLOOP.

 LOOP AT lwa_rcdoc-x-rcvrs ASSIGNING <lwa_rcvrs>.
    <lwa_rcvrs>-rcnum = lwa_rcdoc-x-rchdr-rcnum.
  ENDLOOP.

  PERFORM text_object_value_switch USING lwa_rcdoc-rcnum
                                CHANGING lref_text.

ENDFORM.                    " ac_label_replace

FORM ac_number_generate USING lv_tplkz
                              lv_actyp
                     CHANGING lv_rcnum
                              lv_failed.


  DATA: ls_fmrctyp TYPE ztfmrctyp.

  SELECT SINGLE * FROM ztfmrctyp
                  INTO ls_fmrctyp
                       WHERE rctyp = lv_actyp.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = ls_fmrctyp-numki
      object                  = c_number_range-rc
    IMPORTING
      number                  = lv_rcnum
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
