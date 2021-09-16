*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_processing INPUT.
  PERFORM exit_processing.
ENDMODULE.                 " EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.                 " FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_check INPUT.
  PERFORM header_data_check.
ENDMODULE.                 " HEADER_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_update INPUT.
  PERFORM header_data_update.
ENDMODULE.                 " HEADER_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  DESC_DATA_FILL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc_data_fill INPUT.
  PERFORM desc_data_fill.
ENDMODULE.                 " DESC_DATA_FILL  INPUT
*&---------------------------------------------------------------------*
*&      Module  FUNLOC_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE funloc_data_update INPUT.
  PERFORM funloc_data_update.
ENDMODULE.                 " FUNLOC_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORGANIZATION_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE organization_data_update INPUT.
  PERFORM organization_data_update.
ENDMODULE.                 " ORGANIZATION_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  ATTRIBUTES_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE attributes_grid_update INPUT.
  PERFORM attributes_grid_update.
ENDMODULE.                 " ATTRIBUTES_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CLASSES_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE classes_grid_update INPUT.
  PERFORM classes_grid_update.
ENDMODULE.                 " CLASSES_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  DESC_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc_grid_update INPUT.
  PERFORM desc_grid_update.
ENDMODULE.                 " DESC_GRID_UPDATE  INPUT

*----------------------------------------------------------------------*
*  MODULE address_data_import INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE address_data_import INPUT.
  PERFORM address_data_import.
ENDMODULE.                 " address_data_import  INPUT

*----------------------------------------------------------------------*
*  MODULE cam_okcode_set INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE cam_okcode_set INPUT.
  PERFORM cam_okcode_set.
ENDMODULE.                 " cam_okcode_set  INPUT
*&---------------------------------------------------------------------*
*&      Module  PARTNERS_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE partners_data_import INPUT.
*  PERFORM paratners_data_import.
*ENDMODULE.                 " PARTNERS_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  ASSIGNMENTS_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE assignments_data_import INPUT.
  PERFORM assignments_data_import.
ENDMODULE.                 " ASSIGNMENTS_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_FL_CLASS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE update_fl_class INPUT.
*  PERFORM update_fl_class.
*ENDMODULE.                 " UPDATE_FL_CLASS  INPUT
*&---------------------------------------------------------------------*
*&      Module  TEXTS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE texts_update INPUT.
  PERFORM texts_update.
ENDMODULE.                 " TEXTS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_ADDITIONAL_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_additional_data_update INPUT.
  PERFORM user_additional_data_update.
ENDMODULE.                 " USER_ADDITIONAL_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  OWNERS_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE owners_data_update INPUT.
  PERFORM owners_data_update.
ENDMODULE.                 " OWNERS_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  OWNER_VALUES_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE owner_values_check INPUT.
  PERFORM owner_values_check USING c_true
                                   /agri/s_glflot-owner
                                   /agri/s_glflot-owrol.
ENDMODULE.                 " OWNER_VALUES_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  OWNER_VALUES_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE owner_values_get INPUT.

  DATA: lv_display.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_display = c_true.
  ENDIF.
  PERFORM owner_values_get USING c_true
                                 lv_display
                        CHANGING /agri/s_glflot-owner.

ENDMODULE.                 " OWNER_VALUES_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  SUPERIOR_FL_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE superior_fl_check INPUT.
  PERFORM superior_fl_check.
ENDMODULE.                 " SUPERIOR_FL_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  ORGANIZATION_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE organization_data_check INPUT.
  PERFORM organization_data_check.
ENDMODULE.                 " ORGANIZATION_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  FUNLOC_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE funloc_data_check INPUT.
  PERFORM funloc_data_check.
ENDMODULE.                 " FUNLOC_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_update INPUT.
  PERFORM status_update.
ENDMODULE.                 " STATUS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  RELEASE_STATUS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE release_status_update INPUT.
  PERFORM release_status_update.
ENDMODULE.                 " RELEASE_STATUS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  PLANTS_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE plants_grid_update INPUT.
  PERFORM plants_grid_update.
ENDMODULE.                 " PLANTS_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CUSTOMER_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE customer_data_import INPUT.
  PERFORM customer_data_import.
ENDMODULE.                 " CUSTOMER_DATA_IMPORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  LABEL_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE label_grid_update INPUT.
  PERFORM label_grid_update.
ENDMODULE.
