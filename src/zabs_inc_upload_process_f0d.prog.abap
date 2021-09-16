*----------------------------------------------------------------------*
***INCLUDE /AGRI/GLPG_UPLOAD_PROCESS_F0D .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_GET
*&---------------------------------------------------------------------*
FORM data_get  USING    p_file.

  DATA: lt_sheet TYPE /agri/t_excel_sheet,
        lv_route TYPE char0256.

  lv_route = p_file.

  CALL METHOD /agri/cl_glupload_master_data=>read_excel_file
    EXPORTING
      i_route  = lv_route
    IMPORTING
      et_sheet = lt_sheet.

  IF lines( lt_sheet ) > 0.
    IF p_ag IS NOT INITIAL. "Alocação Compon. Proc./Tarefas
      PERFORM zrouting_change USING lt_sheet.
    ELSEIF p_at IS NOT INITIAL. "Atributos de Terrenos
      PERFORM zattributes_create USING lt_sheet.
    ELSEIF p_af IS NOT INITIAL. "Dados Adicionais de Terrenos
      PERFORM zadditional_fields USING lt_sheet.
    ELSEIF p_cm IS NOT INITIAL. "Receitas
      PERFORM zrecipes_create USING lt_sheet.
    ELSEIF p_cs IS NOT INITIAL. "Atualizar Índices Técnicos
      PERFORM zupdate_technical_indexes USING lt_sheet.
    ELSEIF p_te IS NOT INITIAL. "Imobilizado (Épocas Cultura)
      PERFORM zcrop_season_change USING lt_sheet.
    ELSEIF p_md IS NOT INITIAL. "Measurement documents
      PERFORM measurement_document_create USING lt_sheet.
    ELSEIF p_ie IS NOT INITIAL. "Irrigation Equipment
      PERFORM irrigation_equipment_create USING lt_sheet.
    ELSEIF p_ro IS NOT INITIAL. "Routes
      PERFORM zroutes_change USING lt_sheet.
    ELSEIF p_sol IS NOT INITIAL. "Sólidos por Hectare
      PERFORM zsolidosha_update USING lt_sheet.
    ENDIF.
  ENDIF.

ENDFORM.                    " DATA_GET
