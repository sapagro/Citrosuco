*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0B
*&---------------------------------------------------------------------*
FORM badi_document_check  USING    lwa_acdoc TYPE /agri/s_fmacs_doc
                          CHANGING lv_stop_save TYPE xfeld.

  DATA: lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM badi_reference_get USING lwa_acdoc-x-achdr-actyp.

  IF ref_badi_fmac_all IS BOUND.

    CALL BADI ref_badi_fmac_all->document_check
      EXPORTING
        flt_val     = lwa_acdoc-x-achdr-accom
        i_updkz     = lwa_acdoc-x-achdr-updkz
        is_xachdr    = lwa_acdoc-x-achdr
        it_xacitm    = lwa_acdoc-x-acitm
        it_xacdet    = lwa_acdoc-x-acdet
        is_yachdr   = lwa_acdoc-x-achdr
        it_yacitm   = lwa_acdoc-x-acitm
        it_yacdet   = lwa_acdoc-x-acdet
      CHANGING
        c_stop_save = lv_stop_save
        ct_messages = lt_messages.

    IF NOT lt_messages IS INITIAL.

      LOOP AT lt_messages INTO ls_message.
        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
           NUMBER ls_message-msgno WITH ls_message-msgv1
           ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                      INTO sy-msgli.
        message_simple space.
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.                    " BADI_DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*&      Form  BADI_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ACHDR  text
*      -->P_LV_ACTIVITY  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM badi_authority_check  USING    lwa_achdr TYPE /agri/s_fmachdr
                                    lv_activity
                           CHANGING lv_subrc.

  DATA: ls_message TYPE /agri/s_gprolog.

  PERFORM badi_reference_get USING lwa_achdr-actyp.

  IF ref_badi_fmac_all IS BOUND.

    CALL BADI ref_badi_fmac_all->authority_check
      EXPORTING
        flt_val       = lwa_achdr-actyp
        is_achdr      = lwa_achdr
        i_activity    = lv_activity
      CHANGING
        c_return_code = lv_subrc
        cs_message    = ls_message.

    IF NOT ls_message IS INITIAL.
      IF ls_message-msgid IS NOT INITIAL AND
         ls_message-msgty IS NOT INITIAL.
        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
        NUMBER ls_message-msgno WITH ls_message-msgv1
        ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
        INTO sy-msgli.
        message_simple space.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " BADI_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  BADI_REFERENCE_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ACHDR_ACCOM  text
*----------------------------------------------------------------------*
FORM badi_reference_get  USING    lv_actyp TYPE /agri/fmactyp.

  STATICS: lv_actyp_prev  TYPE /agri/fmactyp.

  IF lv_actyp_prev NE lv_actyp.
    CLEAR ref_badi_fmac_all.
    lv_actyp_prev = lv_actyp.
  ENDIF.

  CHECK ref_badi_fmac_all IS NOT BOUND.

  TRY.

      GET BADI ref_badi_fmac_all
        FILTERS
          actyp = lv_actyp.

    CATCH cx_badi_not_implemented.

  ENDTRY.

ENDFORM.                    " BADI_REFERENCE_GET
*&---------------------------------------------------------------------*
*&      Form  BADI_HEADER_DEFAULTS_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM badi_header_defaults_fill .
  PERFORM badi_reference_get USING gs_acdoc_infocus-x-achdr-actyp.

  IF ref_badi_fmac_all IS BOUND.
    CALL BADI ref_badi_fmac_all->header_defaults_fill
      EXPORTING
        flt_val  = gs_acdoc_infocus-x-achdr-accom
      CHANGING
        cs_achdr = gs_acdoc_infocus-x-achdr.
  ENDIF.

ENDFORM.                    " BADI_HEADER_DEFAULTS_FILL
