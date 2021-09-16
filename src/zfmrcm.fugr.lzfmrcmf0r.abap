*&---------------------------------------------------------------------*
*& Form RECIPE_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM recipe_create .

  DATA: lv_subrc       TYPE sy-subrc,
        lv_not_allowed,
        ls_tabstrip    TYPE /agri/s_gtabstrip.

  PERFORM document_data_initialize USING c_true.
  gs_variables-document_mode = c_mode_create.

  CLEAR: zsc_fmrchdr.
  CALL SCREEN 204 STARTING AT 5 5.

  CHECK zsc_fmrchdr-rctyp IS NOT INITIAL.

  MOVE-CORRESPONDING zsc_fmrchdr TO gs_rcdoc_infocus-x-rchdr.
  MOVE-CORRESPONDING zsc_fmrchdr TO gs_rcdoc_infocus.

  gs_rcdoc_infocus-x-rchdr-updkz = c_updkz_new.
  gs_rcdoc_infocus-updkz  = c_updkz_new.

  PERFORM document_infocus_prepare.
  CHECK gs_rcdoc_infocus IS NOT INITIAL.

  PERFORM authority_check USING gs_rcdoc_infocus-x-rchdr
                                c_authorization_activity-release
                                c_msg_type-info
                       CHANGING lv_subrc.

ENDFORM.

FORM rout_f4_values_get USING     lv_display
                              CHANGING lv_value TYPE plnnr
                                       lv_change
                                       lv_subrc TYPE sy-subrc.

  DATA:
    lt_rout_tab       TYPE zt_fmrcrout_tab,
    lwa_rout_data     LIKE LINE OF lt_rout_tab,
    lwa_field_mapping TYPE dselc,
    lt_field_mapping  TYPE TABLE OF dselc.

  DATA: lv_title(80),
       lv_fieldname TYPE fieldname.

  DATA: lt_return_values TYPE TABLE OF ddshretval INITIAL SIZE 0,
        lwa_return_value TYPE ddshretval.

  FIELD-SYMBOLS: <lt_rout_tab> TYPE STANDARD TABLE.

  PERFORM rout_read CHANGING lt_rout_tab
                                 lv_subrc.
  CHECK lv_subrc EQ 0.
  CHECK lt_rout_tab[] IS NOT INITIAL.
  lv_title = TEXT-051.
  CONDENSE lv_title.
  ASSIGN lt_rout_tab[] TO <lt_rout_tab>.
  lv_fieldname = 'PLNNR'.

  CHECK <lt_rout_tab> IS ASSIGNED.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_fieldname
      window_title    = lv_title
      value_org       = 'S'
      display         = lv_display
    TABLES
      value_tab       = <lt_rout_tab>[]
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
*& Form ROUT_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ROUT_TAB
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM rout_read  CHANGING lt_rout_tab TYPE zt_fmrcrout_tab
                         VALUE(lv_subrc).

  SELECT * FROM mapl
                INTO CORRESPONDING FIELDS OF TABLE lt_rout_tab
          WHERE matnr = zsc_fmrchdr-matnr
             AND werks = zsc_fmrchdr-werks.
  MOVE sy-subrc TO lv_subrc.

ENDFORM.
