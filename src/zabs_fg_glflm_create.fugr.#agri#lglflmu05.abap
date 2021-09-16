FUNCTION /agri/glfl_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODE) LIKE  RV56A-SELKZ DEFAULT 'A'
*"     VALUE(I_DISPLAY_ONLY) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(IT_TPLNR) TYPE  /AGRI/T_GLTPLNR OPTIONAL
*"     VALUE(I_SAVE) LIKE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(IT_FILTER) TYPE  LVC_T_FILT OPTIONAL
*"     VALUE(IS_WSLINK) TYPE  /AGRI/S_GWPLINK OPTIONAL
*"  EXCEPTIONS
*"      ENTER_TERRAIN
*"      INVALID_PARAMETER_COMBINATION
*"      NO_DOCUMENTS_TO_PROCESS
*"----------------------------------------------------------------------
  DATA: lwa_flhdr          TYPE /agri/s_glflot,
        lwa_tplnr          TYPE /agri/s_gltplnr,
        lt_link_attributes TYPE /agri/t_gwplink_attr,
        lwa_link_attribute TYPE /agri/s_gwplink_attr,
        lv_set_param(20)   TYPE c,
*            lt_selections      TYPE rsds_texpr,
        ls_ws_context      TYPE /agri/s_glcr_ws_context,
        lv_subrc           TYPE sy-subrc.

  RANGES: lr_tplnr FOR /agri/glflot-tplnr_fl,
          lr_fltyp FOR /agri/glflot-fltyp.
*** S4/HANA: Search Profile related
  DATA: lt_selections TYPE /agri/t_bsel_values,
        lt_trange     TYPE rsds_trange,
        lwa_selection TYPE /agri/s_bsel_values,
        lwa_selopt    TYPE /agri/s_bsel_selopt.
***
  FIELD-SYMBOLS: <lv_value>    TYPE any.


  IF is_wslink IS INITIAL.
    IF  i_save EQ space
    AND i_mode NE c_mode_display.
      RAISE invalid_parameter_combination.
    ENDIF.
  ENDIF.

  CLEAR gs_variables-overview_mode.
  PERFORM document_data_initialize USING c_true.
  PERFORM transaction_init  USING i_mode.
  gs_variables-external = c_true.

  IF NOT is_wslink-hnvgt IS INITIAL.
    CLEAR gs_variables-navigator_is_visible.
  ENDIF.

  IF is_wslink IS NOT INITIAL.
    gs_variables-call_from_portal = c_true.
    gs_variables-transaction_code = is_wslink-tcode.
  ENDIF.

  IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_create.
    CALL FUNCTION '/AGRI/GWPWS_LINK_ATRIBUTES_GET'
      EXPORTING
        is_wslink          = is_wslink
      IMPORTING
        et_link_attributes = lt_link_attributes.

    LOOP AT lt_link_attributes INTO lwa_link_attribute.
      IF lwa_link_attribute-attrt EQ
           /agri/if_gwpws_constants=>c_attrb_context.
        ASSIGN COMPONENT lwa_link_attribute-attrb OF STRUCTURE ls_ws_context
        TO <lv_value>.
        CHECK sy-subrc EQ 0.
        <lv_value> = lwa_link_attribute-attrv.
      ELSEIF lwa_link_attribute-attrt EQ
        /agri/if_gwpws_constants=>c_attrb_spa_gpa.
        lv_set_param = lwa_link_attribute-attrb.
        SET PARAMETER ID lv_set_param FIELD lwa_link_attribute-attrv.
      ENDIF.
    ENDLOOP.

    CLEAR lwa_flhdr.
    MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_flhdr.
    MOVE-CORRESPONDING ls_ws_context TO lwa_flhdr.

    IF NOT lwa_flhdr-fltyp IS INITIAL.
      gs_flhdr_portal = lwa_flhdr.
      PERFORM portal_flhdr_update CHANGING gs_flhdr_portal
                                           lv_subrc.

    ENDIF.

    gs_variables-document_mode = c_mode_create.
    gs_variables-overview_mode = c_mode_change.
  ELSE.
****Display or change from Portal
    IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change OR
       is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display.
      CALL FUNCTION '/AGRI/GWPWS_LINK_ATRIBUTES_GET'
        EXPORTING
          is_wslink = is_wslink
        IMPORTING
*         et_selections = lt_selections.
          et_trange = lt_trange.
*** S4/HANA: Search Profile related
      IF lt_trange[] IS NOT INITIAL.
        CALL FUNCTION '/AGRI/BSEL_TRANGE_TO_SELOPT'
          EXPORTING
            it_trange      = lt_trange
          IMPORTING
            et_bsel_values = lt_selections.
      ENDIF.
***
      IF is_wslink-varid IS NOT INITIAL.
        IF lt_selections IS INITIAL.
*** S4/HANA: Search Profile related
          EXPORT varid = is_wslink-varid
                 skip_screen = 'X' TO MEMORY ID 'GLFLOT_SRCH'.
          SUBMIT /agri/glfl_process    "#EC CI_SUBMIT
*                USING SELECTION-SET  is_wslink-varid
                WITH p_excall = c_true
                AND RETURN.

        ELSE.
*** S4/HANA: Search Profile related
          EXPORT varid = is_wslink-varid
                 t_selections = lt_selections
                 skip_screen = 'X' TO MEMORY ID 'GLFLOT_SRCH'.
          SUBMIT /agri/glfl_process      "#EC CI_SUBMIT
*          USING SELECTION-SET  is_wslink-varid
*          WITH FREE SELECTIONS lt_selections
          WITH p_excall = c_true
          AND RETURN.
        ENDIF.

*      ELSE.
      ELSEIF lt_selections IS NOT INITIAL.
*** S4/HANA: Search Profile related
*        lr_fltyp-sign   = 'I'.
*        lr_fltyp-option = 'CP'.
*        lr_fltyp-low    = '*'.
*        APPEND lr_fltyp.
        lwa_selection-tabname = '/AGRI/GLFLOT'.
        lwa_selection-fieldname = 'FLTYP'.
        lwa_selopt-low = '*'.
        lwa_selopt-sign = 'I'.
        lwa_selopt-option = 'CP'.
        APPEND lwa_selopt TO lwa_selection-selopt.
        APPEND lwa_selection TO lt_selections.

        EXPORT t_selections = lt_selections
               skip_screen = 'X' TO MEMORY ID 'GLFLOT_SRCH'.
        SUBMIT /agri/glfl_process           "#EC CI_SUBMIT
*                   WITH so_fltyp IN lr_fltyp
*                   WITH FREE SELECTIONS lt_selections
                   WITH p_excall = c_true
                   AND RETURN.
      ENDIF.

      IMPORT /agri/glflot = gt_search_header[] FROM MEMORY ID 'GLFLOT_SRCH'.
      FREE MEMORY ID 'GLFLOT_SRCH'.

    ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change_id OR
       is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display_id.

*** S4/HANA: Search Profile related
*      lr_tplnr-sign   = 'I'.
*      lr_tplnr-option = 'EQ'.
*      lr_tplnr-low    = is_wslink-objid.
*      APPEND lr_tplnr.
      lwa_selection-tabname = '/AGRI/GLFLOT'.
      lwa_selection-fieldname = 'TPLNR_FL'.
      lwa_selopt-low = is_wslink-objid.
      lwa_selopt-sign = 'I'.
      lwa_selopt-option = 'EQ'.
      APPEND lwa_selopt TO lwa_selection-selopt.
      APPEND lwa_selection TO lt_selections.
      EXPORT t_selections = lt_selections
             skip_screen = 'X' TO MEMORY ID 'GLFLOT_SRCH'.
      SUBMIT /agri/glfl_process       "#EC CI_SUBMIT
*             WITH so_strno IN lr_tplnr
                  WITH p_excall = c_true
                 AND RETURN.
      IMPORT /agri/glflot = gt_search_header "gt_critm = gt_search_item
     FROM MEMORY ID 'GLFLOT_SRCH'.
    ELSE.
      gs_variables-display_only = i_display_only.

      LOOP AT it_tplnr INTO lwa_tplnr.
        CHECK lwa_tplnr-tplnr_fl IS NOT INITIAL.
        lr_tplnr-sign   = 'I'.
        lr_tplnr-option = 'EQ'.

*---Replace Unreleased Interface
*        CALL FUNCTION 'CONVERSION_EXIT_TPLNR_OUTPUT'
*          EXPORTING
*            input  = lwa_tplnr-tplnr_fl
*          IMPORTING
*            output = lr_tplnr-low.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_OUTPUT'
          EXPORTING
            i_input  = lwa_tplnr-tplnr_fl
          IMPORTING
            o_output = lr_tplnr-low.
*---
        APPEND lr_tplnr.
      ENDLOOP.
      IF NOT lr_tplnr[] IS INITIAL AND it_tplnr IS NOT INITIAL.
        SELECT * FROM /agri/glflot        "#EC CI_FAE_LINES_ENSURED
        INTO CORRESPONDING FIELDS OF TABLE gt_search_header
        FOR ALL ENTRIES IN it_tplnr
        WHERE tplnr_fl EQ it_tplnr-tplnr_fl.

        IF sy-subrc NE 0.
          message_simple space.
          RAISE no_documents_to_process.
        ENDIF.
*        SUBMIT /agri/glfl_process
*               WITH so_strno IN lr_tplnr
*                    WITH FREE SELECTIONS lt_selections
*                    WITH p_excall = c_true
*                    AND RETURN.
*      ELSE.
*        RAISE enter_terrain.
      ENDIF.

*      IMPORT /agri/glflot = gt_search_header  FROM MEMORY ID 'GLFLOT_SRCH'.
*      FREE MEMORY ID 'GLFLOT_SRCH'.
    ENDIF.

  ENDIF.

  IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change OR
     is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change_id.
    gs_variables-document_mode = c_mode_change.
    gs_variables-overview_mode = c_mode_change.

  ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display OR
         is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display_id.
    gs_variables-display_only = i_display_only.

  ENDIF.

  gs_variables-refresh_worklist = c_true.

  PERFORM fl_data_display.
  PERFORM terrain_display.

  CLEAR: gs_variables-display_only,gs_variables-external.
  REFRESH: gt_search_header.

ENDFUNCTION.
