REPORT zabs_excel_upload.

INCLUDE zabs_inc_excel_upload_top.
INCLUDE zabs_inc_excel_upload_f01.

INITIALIZATION.
  PERFORM authorization_check.

  PERFORM prepare_to_import_excel.

  PERFORM screen_update.

AT SELECTION-SCREEN.

  PERFORM validate_file_path USING p_file.

START-OF-SELECTION.

  PERFORM read_excel_file.

  IF lines( t_sheets_out ) > 0.
    IF p_ag IS NOT INITIAL. "Attribute groups
      PERFORM attribute_groups_create USING t_sheets_out.
    ELSEIF p_at IS NOT INITIAL. "Attributes
      PERFORM attributes_create USING t_sheets_out.
    ELSEIF p_cm IS NOT INITIAL. "Crop master
      PERFORM crop_master_create USING t_sheets_out.
    ELSEIF p_cs IS NOT INITIAL. "Crop season
*      PERFORM crop_season_create USING t_sheets_out.
      PERFORM zabs_crop_season_create USING t_sheets_out.
    ELSEIF p_md IS NOT INITIAL. "Measurement documents
      PERFORM measurement_document_create USING t_sheets_out.
    ELSEIF p_te IS NOT INITIAL. "Terrains
      PERFORM terrains_create USING t_sheets_out.
    ELSEIF p_ie IS NOT INITIAL. "Irrigation Equipment
      PERFORM irrigation_equipment_create USING t_sheets_out.
    ELSEIF p_bud IS NOT INITIAL. "ZABS_ORCAMENTO
      PERFORM budget_update USING t_sheets_out.
    ENDIF.
  ENDIF.

  IF gs_variables-initiator IS INITIAL.
    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save.
    PERFORM message_add_table.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.
