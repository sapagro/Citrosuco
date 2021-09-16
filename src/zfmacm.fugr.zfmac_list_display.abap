FUNCTION ZFMAC_LIST_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHARACTERISTICS) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(IT_achdr) TYPE  /AGRI/T_GLachdr
*"  CHANGING
*"     REFERENCE(C_PROGRAM) LIKE  SY-REPID
*"     REFERENCE(C_SUBSCREEN) LIKE  SY-DYNNR
*"     REFERENCE(C_REFRESH) TYPE  CHAR1 DEFAULT 'X'
*"  EXCEPTIONS
*"      INCOMPLETE_PARAMETERS
*"      NO_DATA_EXISTS
*"--------------------------------------------------------------------

***  c_program = c_program_acm.
***  c_subscreen = c_screen-list.
***  gs_variables-characteristics_display = i_characteristics.
***
***  CHECK c_refresh IS NOT INITIAL.
***  CLEAR: c_refresh, gt_mdlist[], gt_attr_list[].
***  gs_variables-refresh_latest_values_grid   = c_true.
***  gs_variables-refresh_display_output_table = c_true.
***  gs_variables-attribute_description        = c_true.
***
***  IF it_achdr[] IS INITIAL.
***    RAISE no_data_exists.
***  ENDIF.
***
***  PERFORM list_data_prepare USING it_achdr.

ENDFUNCTION.
