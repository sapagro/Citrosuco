*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0U
*&---------------------------------------------------------------------*
FORM user_additional_data_update .
  DATA: lv_modified,
        lv_valid,
        lwa_achdr  TYPE /agri/s_fmachdr,
        lwa_xachdr TYPE /agri/s_fmachdr.

  CHECK NOT gs_variables-user_structure IS INITIAL AND
            gs_variables-overview_mode NE c_mode_display.

  lv_modified = ref_grid_additional_data->data_modified_check( ).

  IF lv_modified EQ c_true OR gs_variables-manual_changes EQ c_true.

    CALL METHOD ref_grid_additional_data->check_changed_data
      IMPORTING
        e_valid = lv_valid.

    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO lwa_achdr.
    PERFORM field_value_conversions USING '2'.
    MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO lwa_xachdr.
    IF lwa_xachdr NE lwa_achdr.
      IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
        gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
      ENDIF.
      gs_variables-data_changed = c_true.
    ENDIF.
    gs_variables-refresh_additional_data_grid = c_true.

  ENDIF.
ENDFORM.                    " USER_ADDITIONAL_DATA_UPDATE
