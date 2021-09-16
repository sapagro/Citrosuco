*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0L.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form LABEL_STYLES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_FL_LABELS_LAYOUT
*&---------------------------------------------------------------------*
FORM label_styles_prepare  CHANGING lwa_fl_labels_layout
                               TYPE /agri/s_glflos_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'STRNO'.
  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lwa_style INTO TABLE lwa_fl_labels_layout-styles.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LABEL_GRID_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM label_grid_update .

* DATA: lv_modified,
*        lv_subrc,
*        lv_valid.
*
*  IF ok_code EQ c_fcode-desc_delete.
**    PERFORM fcode_desc_delete.
*    CLEAR ok_code.
*  ENDIF.
*
*  lv_modified = ref_multi_lang_desc_grid->data_modified_check( ).
*  IF lv_modified EQ c_true.
*    CALL METHOD ref_multi_lang_desc_grid->check_changed_data
*      IMPORTING
*        e_valid = lv_valid.
*    IF lv_valid IS INITIAL.
*      CLEAR ok_code.
*      EXIT.
*    ELSE.
*      gs_variables-refresh_desc_grid = c_true.
*    ENDIF.
*
*  ENDIF.

*  PERFORM desc_update CHANGING lv_subrc.

ENDFORM.
