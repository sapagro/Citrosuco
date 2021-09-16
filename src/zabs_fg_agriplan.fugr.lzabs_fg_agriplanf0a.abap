*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0A .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING lwa_achdr TYPE zsc_fmachdr
                            lv_activity
                            lv_message_type
                   CHANGING lv_subrc.

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM admin_data_maintain .

  DATA: lv_subobj       LIKE dd03p-fieldname,
        ls_screenfields TYPE /agri/gadminscrfields.

****Header Admin Data
  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_screenfields.
  CALL FUNCTION '/AGRI/GADMIN_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp              = c_object-bor
      i_objkey              = gs_acdoc_infocus-x-achdr-acnum
      i_subobj              = lv_subobj
      is_scrfields          = ls_screenfields
      i_suppress_activities = c_true
      i_suppress_notes      = c_false
    CHANGING
      c_program             = gs_variables-admin_program
      c_subscreen           = gs_variables-subscr_admin.

ENDFORM.                    " ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
*& Form ATTRIBUTE_INTENALNAME_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LV_INTERNAL
*&---------------------------------------------------------------------*
FORM attribute_intenalname_get  USING VALUE(lv_attributename)
                                CHANGING lv_internal TYPE atinn.

*  SELECT SINGLE atinn FROM cabn
*                 INTO ( lv_internal )
*                       WHERE atnam = lv_attributename.
  SELECT atinn UP TO 1 ROWS
    FROM cabn
    INTO @lv_internal
   WHERE atnam = @lv_attributename.
  ENDSELECT.

ENDFORM.

FORM quantities_totalize.

  DATA: lv_sumqtyplants TYPE zfmacqpl.
  DATA: lv_sumabsolarea TYPE /agri/glaarea.
  DATA: lv_sumavolumen TYPE zfmacvlctl.

  FIELD-SYMBOLS: <lwa_acitm> TYPE zsc_fmacitm.

  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm> .
    ADD <lwa_acitm>-aarea TO lv_sumabsolarea.
    ADD <lwa_acitm>-adqpl TO lv_sumqtyplants.
    ADD <lwa_acitm>-advlc TO lv_sumavolumen.
  ENDLOOP.

  MOVE lv_sumqtyplants TO zsc_fmachdr-qtplt.
  MOVE c_unit_of_measurement-unit TO zsc_fmachdr-unplt.
  MOVE lv_sumabsolarea TO zsc_fmachdr-arabs.
  MOVE c_unit_of_measurement-hectare TO zsc_fmachdr-unabs.
  MOVE lv_sumavolumen TO  zsc_fmachdr-vlctl.

  PERFORM header_data_update.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ASSIGNED_ITEMS_STYLES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_ITEMS_FCAT
*&---------------------------------------------------------------------*
FORM assigned_items_styles_prepare  CHANGING lwa_items_fcat TYPE zsc_fmacitm_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'TPLNR_FL'.
  IF NOT lwa_items_fcat-tplnr_fl IS INITIAL.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  INSERT lwa_style INTO TABLE lwa_items_fcat-styles.
  CLEAR lwa_style.

ENDFORM.
