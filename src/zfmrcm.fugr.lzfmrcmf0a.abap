*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0A .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING lwa_rchdr TYPE zsc_fmrchdr
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
  MOVE-CORRESPONDING gs_rcdoc_infocus-x-rchdr TO ls_screenfields.
  CALL FUNCTION '/AGRI/GADMIN_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp              = c_object-bor
      i_objkey              = gs_rcdoc_infocus-x-rchdr-rcnum
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
FORM attribute_intenalname_get  USING VALUE(lv_attributename).
*                                CHANGING lv_internal TYPE atinn.
*  SELECT SINGLE atinn FROM cabn
*                 INTO ( lv_internal )
*                       WHERE atnam = lv_attributename.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ASSIGNED_ITEMS_STYLES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_ITEMS_FCAT
*&---------------------------------------------------------------------*
FORM assigned_dose_styles_prepare  CHANGING lwa_dose_fcat TYPE zsc_fmrclst_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'MATNR_INS'.
  IF NOT lwa_dose_fcat-matnr_ins IS INITIAL.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  INSERT lwa_style INTO TABLE lwa_dose_fcat-styles.
  CLEAR lwa_style.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALTERNATIVE_MAX_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ALTER_HIGHT
*&---------------------------------------------------------------------*
FORM alternative_max_get  CHANGING lv_alter_hight TYPE stalt
                                   VALUE(lv_subrc).

  SELECT MAX( stlal ) FROM mast
    INTO lv_alter_hight
    BYPASSING BUFFER
   WHERE matnr EQ zsc_fmrchdr-matnr
     AND stlan = '1'
     AND werks EQ zsc_fmrchdr-werks .

  MOVE sy-subrc TO lv_subrc.

ENDFORM.

FORM alternative_f4_values_get  USING     lv_display
                              CHANGING lv_value TYPE stlal
                                       lv_change
                                       lv_subrc TYPE sy-subrc.

  DATA:
    lt_alter_tab      TYPE zt_fmrcalter_tab,
    lwa_alter_data    LIKE LINE OF lt_alter_tab,
    lwa_field_mapping TYPE dselc,
    lt_field_mapping  TYPE TABLE OF dselc.

  DATA: lv_title(80),
       lv_fieldname TYPE fieldname.

  DATA: lt_return_values TYPE TABLE OF ddshretval INITIAL SIZE 0,
        lwa_return_value TYPE ddshretval.

  FIELD-SYMBOLS: <lt_alter_tab> TYPE STANDARD TABLE.

  PERFORM alter_read CHANGING lt_alter_tab
                                 lv_subrc.
  CHECK lv_subrc EQ 0.
  CHECK lt_alter_tab[] IS NOT INITIAL.
  lv_title = TEXT-050.
  CONDENSE lv_title.
  ASSIGN lt_alter_tab[] TO <lt_alter_tab>.
  lv_fieldname = 'STLAL'.

  CHECK <lt_alter_tab> IS ASSIGNED.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_fieldname
      window_title    = lv_title
      value_org       = 'S'
      display         = lv_display
    TABLES
      value_tab       = <lt_alter_tab>[]
      return_tab      = lt_return_values
      dynpfld_mapping = lt_field_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_return_values INTO lwa_return_value
                INDEX 1.
  IF sy-subrc EQ 0.
    lv_change = c_true.
    lv_value = lwa_return_value-fieldval.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALTER_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ALTER_TAB
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM alter_read  CHANGING lt_alter_tab TYPE zt_fmrcalter_tab
                      VALUE(lv_subrc).

  SELECT * FROM mast
    INTO CORRESPONDING FIELDS OF TABLE lt_alter_tab
   WHERE matnr = zsc_fmrchdr-matnr
     AND werks = zsc_fmrchdr-werks.

  MOVE sy-subrc TO lv_subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALTERNATIVE_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alternative_change.

  SELECT * FROM zfmrclst
    INTO TABLE @DATA(lt_fmrclst)
    WHERE rcnum = @zsc_fmrchdr-rcnum
      AND werks = @zsc_fmrchdr-werks
      AND matnr = @zsc_fmrchdr-matnr
      AND stlal = @zsc_fmrchdr-stlal.

*...Vistex-11.06.2019/Begin
  gs_rcdoc_infocus-x-rclst = CORRESPONDING #( lt_fmrclst[] ).
  gs_variables-refresh_dose_grid = c_true.

  PERFORM set_grid_lines.
*...Vistex-11.06.2019/End

ENDFORM.

FORM all_versions_delete.

  DATA:
    lt_mkal_i     TYPE TABLE OF mkal,
    lt_mkal_u     TYPE TABLE OF mkal,
    lt_mkal_d     TYPE TABLE OF mkal,
    ls_mkal_d     TYPE  mkal,
    lv_subrc_vers TYPE int4,
    lt_mkal_aend  TYPE TABLE OF  mkal_aend.

  DATA: lv         TYPE i,
        lv_message TYPE string,
        lr_exc     TYPE REF TO cx_sy_open_sql_error.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  LOOP AT gs_rcdoc_infocus-y-rcvrs INTO DATA(lwa_rcvrs_d).
    MOVE-CORRESPONDING lwa_rcvrs_d TO ls_mkal_d.
    APPEND ls_mkal_d TO lt_mkal_d.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '100' WITH ls_mkal_d-verid
                                                            INTO sy-msgli.
    message_simple c_false.
  ENDLOOP.
  TRY.
      CALL FUNCTION 'CM_FV_PROD_VERS_DB_UPDATE'
        TABLES
          it_mkal_i    = lt_mkal_i
          it_mkal_u    = lt_mkal_u
          it_mkal_d    = lt_mkal_d
          it_mkal_aend = lt_mkal_aend.

    CATCH cx_sy_open_sql_error INTO lr_exc.
      lv_message = lr_exc->get_text( ).
  ENDTRY.

  REFRESH:
        lt_mkal_i,
        lt_mkal_u,
        lt_mkal_d,
        lt_mkal_aend.

  PERFORM bapi_commit.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_ALTERNATIVES_DELETE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM all_alternatives_delete .

  DATA: lt_bom_itm       TYPE TABLE OF stpo_api03,
        ls_bom_itm       TYPE stpo_api03,
        ls_bom_hdr       TYPE stko_api01,
        ls_bom_hdr_expot TYPE stko_api02,
        ls_stpo_read     TYPE stpo_api02,
        lt_stko_read     TYPE TABLE OF stko_api02,
        fl_warnin        LIKE  capiflag-flwarning,
        lt_stpo          TYPE tty_stpo,
        lt_stas          TYPE TABLE OF stas,
        lwa_stpo         TYPE stpo,
        lv_subrc         TYPE int4,
        lv_lines         TYPE systepl VALUE 1,
        lwa_stas         TYPE stpo,
        lv_datum         TYPE csap_mbom-datuv,
        lt_rclst         TYPE zt_fmrclst,
        lv_bomnumber     TYPE stko_api02-bom_no,
        lv_text          TYPE char100,
        lv_answer        TYPE c,
        ls_csap_mbom     TYPE csap_mbom.

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  PERFORM messages_initialize USING gs_variables-initiator
                                   c_log_subobject-create
                                   gs_rcdoc_infocus-x-rchdr.

  PERFORM all_alternatives_obtein CHANGING lt_rclst.

  SORT: lt_rclst ASCENDING BY stlal.
  DELETE ADJACENT DUPLICATES FROM lt_rclst COMPARING stlal.

  MOVE-CORRESPONDING zsc_fmrchdr TO ls_csap_mbom.
  MOVE zsc_fmrchdr-datuv TO lv_datum.
  date_formt_ddmmyyyy lv_datum lv_datum.
*---Enquee Exceptions
  CALL FUNCTION 'CALO_INIT_API'
    EXCEPTIONS
      log_object_not_found     = 1
      log_sub_object_not_found = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0
       AND NOT sy-msgid IS INITIAL
       AND NOT sy-msgty IS INITIAL
       AND NOT sy-msgno IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.
    message_simple space.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.

  LOOP AT lt_rclst INTO DATA(lwa_rclst) WHERE stlal IS NOT INITIAL.

    CALL FUNCTION 'CSAP_MAT_BOM_OPEN'
      EXPORTING
        material    = zsc_fmrchdr-matnr
        plant       = zsc_fmrchdr-werks
        bom_usage   = '1'
        alternative = lwa_rclst-stlal
        valid_from  = lv_datum
      IMPORTING
        o_stko      = ls_bom_hdr_expot
        fl_warning  = fl_warnin
*    TABLES
*       t_stpo      = lt_bom_itm
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0
        AND NOT sy-msgid IS INITIAL
        AND NOT sy-msgty IS INITIAL
        AND NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.
      message_simple space.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.

    CALL FUNCTION 'CSAP_MAT_BOM_DELETE'
      EXPORTING
        material           = ls_csap_mbom-matnr
        plant              = ls_csap_mbom-werks
        bom_usage          = '1'
        alternative        = lwa_rclst-stlal
        valid_from         = lv_datum
        fl_commit_and_wait = 'X'
*    IMPORTING
*       fl_warning         =
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
      CALL FUNCTION 'VRM_REFRESH_VALUES'.
    ELSE.
      IF sy-subrc <> 0
      AND NOT sy-msgid IS INITIAL
      AND NOT sy-msgty IS INITIAL
      AND NOT sy-msgno IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        message_simple space.
        PERFORM messages_display USING gs_variables-initiator.
      ENDIF.
    ENDIF.
*Close BOM
    CALL FUNCTION 'CSAP_MAT_BOM_CLOSE'
      EXPORTING
        fl_commit_and_wait = 'X'
      IMPORTING
        fl_warning         = fl_warnin
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.

  ENDLOOP.

ENDFORM.

FORM all_alternatives_obtein CHANGING lt_bdrclst TYPE zt_fmrclst.

**  IF sy-uname EQ 'T_T.KONNO'.
**    BREAK-POINT.
**  ENDIF.

  CHECK gs_rcdoc_infocus-x-rclst[] IS NOT INITIAL.
  SELECT * FROM zfmrclst
    INTO TABLE lt_bdrclst
    FOR ALL ENTRIES IN gs_rcdoc_infocus-x-rclst[]
   WHERE rcnum = gs_rcdoc_infocus-x-rclst-rcnum.

ENDFORM.

FORM assigned_vrs_styles_prepare CHANGING lwa_vrs_fcat TYPE zsc_fmrcvrs_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'VERID'.
  IF NOT lwa_vrs_fcat-verid IS INITIAL.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    lwa_style-style = /agri/cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  INSERT lwa_style INTO TABLE lwa_vrs_fcat-styles.
  CLEAR lwa_style.

ENDFORM.
