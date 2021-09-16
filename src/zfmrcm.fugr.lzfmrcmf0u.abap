*&---------------------------------------------------------------------*
*& Form UNIT_MANAGEMENT_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&      <-- LWA_DOSE_LAYOUT_UNITS
*&---------------------------------------------------------------------*
FORM unit_management_display  USING VALUE(lv_material)
                              CHANGING lv_unit TYPE meins.
  PERFORM conver_material_set USING lv_material
                           CHANGING lv_material.
  SELECT SINGLE MEINS FROM mara
                      INTO (lv_unit)
                           WHERE matnr Eq lv_material.
ENDFORM.
