*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLMF0B
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BADI_reference_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM badi_reference_get USING lv_fltyp TYPE /agri/glfltyp.

  STATICS: lv_fltyp_prev  TYPE /agri/glfltyp.

  IF lv_fltyp_prev NE lv_fltyp.
    CLEAR ref_badi_glfl_all.
    lv_fltyp_prev = lv_fltyp.
  ENDIF.

  CHECK ref_badi_glfl_all IS NOT BOUND.

  TRY.

      GET BADI ref_badi_glfl_all
        FILTERS
          fltyp = lv_fltyp.

    CATCH cx_badi_not_implemented.

  ENDTRY.

ENDFORM.                    " BADI_reference_get
*&---------------------------------------------------------------------*
*&      Form  BADI_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_FLHDR  text
*      -->P_LV_ACTIVITY  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM badi_authority_check  USING    lwa_flhdr TYPE /agri/s_glflot
                                    lv_activity
                                    lv_display_messages
                           CHANGING lv_subrc.

  DATA: ls_message TYPE /agri/s_gprolog.

  PERFORM badi_reference_get USING lwa_flhdr-fltyp.

  IF ref_badi_glfl_all IS BOUND.

    CALL BADI ref_badi_glfl_all->authority_check
      EXPORTING
        flt_val       = lwa_flhdr-fltyp
        is_flhdr      = lwa_flhdr
        i_activity    = lv_activity
      CHANGING
        c_return_code = lv_subrc
        cs_message    = ls_message.

    CHECK lv_display_messages IS NOT INITIAL.
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

ENDFORM.                    " badi_authority_check
*&---------------------------------------------------------------------*
*&      Form  BADI_DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM badi_document_check CHANGING lv_stop_save TYPE xfeld.

  DATA: lt_messages TYPE /agri/t_gprolog,
        lt_stat_msgs TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM badi_reference_get USING  gs_fldoc_infocus-x-flhdr-fltyp.

  IF ref_badi_glfl_all IS BOUND.

    CALL BADI ref_badi_glfl_all->document_check
      EXPORTING
        flt_val     = gs_fldoc_infocus-x-flhdr-fltyp
        is_flhdr    = gs_fldoc_infocus-x-flhdr
*        is_iflot    = gs_fldoc_infocus-x-iflot
        is_adrc     = gs_fldoc_infocus-x-adrc
*        is_iloa     = gs_fldoc_infocus-x-iloa
        it_iflotx   = gs_fldoc_infocus-x-iflotx
        it_ihpa     = gs_fldoc_infocus-x-ihpa
        it_flatg    = gs_fldoc_infocus-x-flatg
        it_flatv    = gs_fldoc_infocus-x-flatv
        it_flcma    = gs_fldoc_infocus-x-flcma
        it_flown    = gs_fldoc_infocus-x-flown
        is_yflhdr   = gs_fldoc_infocus-y-flhdr
*        is_yiflot   = gs_fldoc_infocus-y-iflot
        is_yadrc    = gs_fldoc_infocus-y-adrc
*        is_yiloa    = gs_fldoc_infocus-y-iloa
        it_yiflotx  = gs_fldoc_infocus-y-iflotx
        it_yihpa    = gs_fldoc_infocus-y-ihpa
        it_yflatg   = gs_fldoc_infocus-y-flatg
        it_yflatv   = gs_fldoc_infocus-y-flatv
        it_yflcma   = gs_fldoc_infocus-y-flcma
        it_yflown   = gs_fldoc_infocus-y-flown
      CHANGING
        c_stop_save = lv_stop_save
        ct_messages = lt_messages.

    IF ref_status_handler IS INITIAL.
      CREATE OBJECT ref_status_handler.
    ENDIF.

    CALL FUNCTION '/AGRI/G_STATUS_FLOWOUTCOME_SET'
      EXPORTING
        i_objnr     = gs_fldoc_infocus-x-flhdr-objnr
        iref_object = ref_status_handler
      IMPORTING
        et_messages = lt_stat_msgs.
    CALL FUNCTION '/AGRI/G_STATUSPROF_TRIGGER_SET'
      EXPORTING
        i_objnr     = gs_fldoc_infocus-x-flhdr-objnr
        iref_object = ref_status_handler.
    PERFORM status_update.

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

ENDFORM.                    " badi_document_check
*&---------------------------------------------------------------------*
*&      Form  BADI_REFERENCE_CS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM badi_reference_cs_get  USING   lv_fltyp TYPE /agri/glfltyp.

  STATICS: lv_fltyp_prev  TYPE /agri/glfltyp.

  IF lv_fltyp_prev NE lv_fltyp.
    CLEAR ref_badi_glfl_cs.
    lv_fltyp_prev = lv_fltyp.
  ENDIF.

  CHECK ref_badi_glfl_cs IS NOT BOUND.

  TRY.

      GET BADI ref_badi_glfl_cs
        FILTERS
          fltyp = lv_fltyp.

    CATCH cx_badi_not_implemented.

  ENDTRY.

ENDFORM.                    " BADI_REFERENCE_CS_GET
