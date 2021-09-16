*&---------------------------------------------------------------------*
*&      Module  TRANSACTION_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
  PERFORM transaction_init USING c_mode_display.
ENDMODULE.                 " TRANSACTION_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.                 " STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.                 " TITLE_SET  OUTPUT
*----------------------------------------------------------------------*
*  MODULE controls_display OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.                    "controls_display OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SUBSCREEN_AREA_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE subscreen_area_set OUTPUT.
  PERFORM subscreen_area_set.
ENDMODULE.                 " SUBSCREEN_AREA_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HEADER_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE header_data_display OUTPUT.
  PERFORM header_data_display.
ENDMODULE.                 " HEADER_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_modify OUTPUT.
  PERFORM screen_modify.
ENDMODULE.                 " SCREEN_MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DROPDOWN_VALUES_FILL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dropdown_values_fill OUTPUT.
  PERFORM dropdown_values_fill.
ENDMODULE.                 " DROPDOWN_VALUES_FILL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABSTRIP_INITIALIZE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tabstrip_initialize OUTPUT.
  PERFORM tabstrip_initialize.
ENDMODULE.                 " TABSTRIP_INITIALIZE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DESCRIPTIONS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE descriptions_display OUTPUT.
  PERFORM descriptions_display.
ENDMODULE.                 " DESCRIPTIONS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ATTRIBUTES_MASS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE attributes_mass_display OUTPUT.
*  PERFORM attributes_mass_display.
ENDMODULE.                 " ATTRIBUTES_MASS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*& Module ITEMS_GRID_DATA_PREPARE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE dose_grid_data_prepare OUTPUT.
  PERFORM dose_grid_data_prepare.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module rcbom_GRID_DATA_PREPARE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE rcbom_grid_data_prepare OUTPUT.
  PERFORM rcbom_grid_data_prepare.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module VERSION_GRID_DATA_PREPARE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE version_grid_data_prepare OUTPUT.
  PERFORM version_grid_data_prepare.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DEFAULT_VALUES_PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE default_values_pbo OUTPUT.

  CASE sy-dynnr.
    WHEN c_screen-popup_version_create.
      PERFORM default_value.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SIMILAR_GRID_PREPARE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE similar_grid_prepare OUTPUT.

  PERFORM set_grid_lines.
  PERFORM prepare_catalog.
  PERFORM prepare_layout.
  PERFORM display_alv.

ENDMODULE.
