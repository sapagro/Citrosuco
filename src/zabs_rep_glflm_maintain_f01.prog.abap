*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_GLFLM_MAINTAIN_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHANGE_SCREEN_OPTIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM change_screen_options .

  LOOP AT SCREEN.
    IF screen-group1 = 'UP2'.
      screen-invisible = 1.
      screen-input     = 0.
      screen-active    = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATTRIBUTES_CREATE
*&---------------------------------------------------------------------*
FORM zattributes_create USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->attribute_create( EXPORTING it_table    = lt_sheet
                                IMPORTING et_messages = gt_message ).

ENDFORM.                    " ATTRIBUTES_CREATE

*&---------------------------------------------------------------------*
*&      Form  ZADDITIONAL_FIELDS
*&---------------------------------------------------------------------*
FORM zadditional_fields USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->additional_fields_populate( EXPORTING it_table    = lt_sheet
                                          IMPORTING et_messages = gt_message ).

ENDFORM.                    " ZADDITIONAL_FIELDS

*&---------------------------------------------------------------------*
*& Form ZROUTING_CHANGE
*&---------------------------------------------------------------------*
FORM zrouting_change USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->routing_change( EXPORTING it_table    = lt_sheet
                              IMPORTING et_messages = gt_message ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZRECIPES_CREATE
*&---------------------------------------------------------------------*
FORM zrecipes_create USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->recipes_create( EXPORTING it_table    = lt_sheet
                              IMPORTING et_messages = gt_message ).

ENDFORM.                    " ZRECIPES_CREATE

*&---------------------------------------------------------------------*
*& Form ZUPDATE_TECHNICAL_INDEXES
*&---------------------------------------------------------------------*
FORM zupdate_technical_indexes USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->update_technical_indexes( EXPORTING it_table    = lt_sheet
                                        IMPORTING et_messages = gt_message ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZCROP_SEASON_CHANGE
*&---------------------------------------------------------------------*
FORM zcrop_season_change USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->crop_season_change( EXPORTING it_table    = lt_sheet
                                  IMPORTING et_messages = gt_message ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZROUTES_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SHEET
*&---------------------------------------------------------------------*
FORM zroutes_change USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data.

  CREATE OBJECT ref_upload.
  ref_upload->routes_change( EXPORTING it_table    = lt_sheet
                             IMPORTING et_messages = gt_message ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZSOLIDOSHA_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SHEET
*&---------------------------------------------------------------------*
FORM zsolidosha_update USING lt_sheet TYPE /agri/t_excel_sheet.

  DATA: ref_upload TYPE REF TO zcl_abs_glupload_master_data2.

  CREATE OBJECT ref_upload.
  ref_upload->solidosha_update( EXPORTING it_table    = lt_sheet
                                IMPORTING et_messages = gt_message ).

ENDFORM.
