*&---------------------------------------------------------------------*
*& Include          LZFG_ACMI01
*&---------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
  PERFORM transaction_init USING c_mode_display.
ENDMODULE.                 " TRANSACTION_INIT  OUTPUT
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
*&      Module  DESC_DATA_FILL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc_data_fill INPUT.
  PERFORM desc_data_fill.
ENDMODULE.                 " DESC_DATA_FILL  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_update INPUT.
  PERFORM header_data_update.
ENDMODULE.                 " HEADER_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_update_1 INPUT.
  PERFORM header_data_update_1.
ENDMODULE.                 " HEADER_DATA_UPDATE  INPUT

*&---------------------------------------------------------------------*
*&      Module  ACCOM_NUMBER_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE accom_number_check INPUT.
*  PERFORM accom_number_check.
*ENDMODULE.                 " ACCOM_NUMBER_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_check INPUT.
  PERFORM header_data_check.
ENDMODULE.                 " HEADER_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_CHECK_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_check_1 INPUT.
  PERFORM header_data_check_1.
ENDMODULE.                 " HEADER_DATA_CHECK_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  ITEMS_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE items_grid_update INPUT.
  PERFORM items_grid_update.
ENDMODULE.                 " ITEMS_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_update INPUT.
  PERFORM status_update.
ENDMODULE.                 " STATUS_UPDATE  INPUT
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
*&      Module  DESC_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
MODULE desc_grid_update INPUT.
  PERFORM desc_grid_update.
ENDMODULE.                 " DESC_GRID_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  AUFNR_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE aufnr_data_check INPUT.
  PERFORM aufnr_data_check.
ENDMODULE.                 " AUFNR_DATA_CHECK  INPUT
