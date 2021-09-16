FUNCTION zfmac_process.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MODE) LIKE  RV56A-SELKZ DEFAULT 'A'
*"     VALUE(I_DISPLAY_ONLY) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(I_SAVE) LIKE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(IT_acnum) TYPE  /AGRI/T_GLacnum OPTIONAL
*"     VALUE(IS_WSLINK) TYPE  /AGRI/S_GWPLINK OPTIONAL
*"  EXPORTING
*"     VALUE(E_acnum) TYPE  /AGRI/GLacnum
*"     VALUE(E_DATA_CHANGED) TYPE  CHAR1
*"  EXCEPTIONS
*"      ENTER_MESASUREMENT_DOCUMENT
*"      INVALID_PARAMETER_COMBINATION
*"      NO_DOCUMENTS_TO_PROCESS
*"--------------------------------------------------------------------
***
***  DATA: lv_subrc           TYPE sy-subrc,
***        lwa_search_header  LIKE LINE OF gt_search_header,
***        lwa_achdr          TYPE zsc_fmachdr,
***        lwa_acnum          TYPE /agri/s_glacnum,
***        lt_link_attributes TYPE /agri/t_gwplink_attr,
***        lwa_link_attribute TYPE /agri/s_gwplink_attr,
***        lv_set_param(20)   TYPE c.
****        lt_selections TYPE rsds_texpr.
***
***  RANGES: lt_acnum FOR /agri/glachdr-acnum,
***          lr_actyp FOR /agri/glachdr-actyp.
***
***  FIELD-SYMBOLS: <lv_value>         TYPE any,
***                 <ls_search_header> TYPE zsc_fmachdr_wl.
***
***  DATA:  ls_ws_context TYPE /agri/s_glcr_ws_context.
***
***
***** S4/HANA: Search Profile related
***  DATA: lt_selections TYPE /agri/t_bsel_values,
***        lt_trange     TYPE rsds_trange,
***        lwa_selection TYPE /agri/s_bsel_values,
***        lwa_selopt    TYPE /agri/s_bsel_selopt.
*****
***
***  IF is_wslink IS INITIAL.
***    IF i_save EQ space
***    AND i_mode NE c_mode_display.
***      RAISE invalid_parameter_combination.
***    ENDIF.
***  ENDIF.
***
***  CLEAR gs_variables-overview_mode.
***  PERFORM document_data_initialize USING space.
***  PERFORM transaction_init USING i_mode.
***
***  IF NOT is_wslink-hnvgt IS INITIAL.
***    CLEAR gs_variables-navigator_is_visible.
***  ENDIF.
***
***  IF NOT is_wslink IS INITIAL.
***    gs_variables-call_from_portal = c_true.
***    gs_variables-transaction_code = is_wslink-tcode.
****  ELSE.
****    gs_variables-display_only = i_display_only.
***  ENDIF.
*******Create Mode
***  IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_create.
****    PERFORM fcode_crea.
********Display or change
****  ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change OR
****     is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display.
***
***    CALL FUNCTION '/AGRI/GWPWS_LINK_ATRIBUTES_GET'
***      EXPORTING
***        is_wslink          = is_wslink
***      IMPORTING
****       et_selections      = lt_selections.
***        et_link_attributes = lt_link_attributes.
******
***    LOOP AT lt_link_attributes INTO lwa_link_attribute.
***      IF lwa_link_attribute-attrt EQ
***           /agri/if_gwpws_constants=>c_attrb_context.
***        ASSIGN COMPONENT lwa_link_attribute-attrb OF STRUCTURE  ls_ws_context
***        TO <lv_value>.
***        CHECK sy-subrc EQ 0.
***        <lv_value> = lwa_link_attribute-attrv.
***      ELSEIF lwa_link_attribute-attrt EQ
***        /agri/if_gwpws_constants=>c_attrb_spa_gpa.
***        lv_set_param = lwa_link_attribute-attrb.
***        SET PARAMETER ID lv_set_param FIELD lwa_link_attribute-attrv.
***      ENDIF.
***    ENDLOOP.
***
***    PERFORM document_data_initialize USING c_true.
***    CLEAR lwa_achdr.
***    MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO lwa_achdr.
***    MOVE-CORRESPONDING  ls_ws_context TO lwa_achdr.
***
***    IF NOT lwa_achdr-actyp IS INITIAL.
***      gs_achdr_portal = lwa_achdr.
***      PERFORM portal_achdr_update CHANGING gs_achdr_portal
***                                           lv_subrc.
***    ENDIF.
***
***    gs_variables-overview_mode = c_mode_change.
***  ELSE.
*******Display or change from Portal
***    IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change OR
***       is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display.
***      CALL FUNCTION '/AGRI/GWPWS_LINK_ATRIBUTES_GET'
***        EXPORTING
***          is_wslink = is_wslink
***        IMPORTING
****         et_selections = lt_selections.
***          et_trange = lt_trange.
***
****** S4/HANA: Search Profile related
***      IF lt_trange[] IS NOT INITIAL.
***        CALL FUNCTION '/AGRI/BSEL_TRANGE_TO_SELOPT'
***          EXPORTING
***            it_trange      = lt_trange
***          IMPORTING
***            et_bsel_values = lt_selections.
***      ENDIF.
***      IF is_wslink-varid IS NOT INITIAL.
***        IF lt_selections IS INITIAL.
****** S4/HANA: Search Profile related
***          EXPORT varid = is_wslink-varid
***                 skip_screen = 'X' TO MEMORY ID 'FMAC_SRCH'.
***          SUBMIT /agri/fmac_process        "#EC NEEDED   "#EC CI_SUBMIT
***               WITH p_excall = c_true
****             USING SELECTION-SET  is_wslink-varid
***               AND RETURN.
***        ELSE.
****** S4/HANA: Search Profile related
***          EXPORT varid = is_wslink-varid
***                 t_selections = lt_selections
***                 skip_screen = 'X' TO MEMORY ID 'FMAC_SRCH'.
***          SUBMIT /agri/fmac_process                      "#EC CI_SUBMIT
****             WITH FREE SELECTIONS lt_selections
***               WITH p_excall = c_true
****             USING SELECTION-SET  is_wslink-varid
***               AND RETURN.
***        ENDIF.
****** S4/HANA: Search Profile related
***      ELSEIF lt_selections IS NOT INITIAL.
****    ELSE.
****      lr_actyp-sign = 'I'.
****      lr_actyp-option = 'CP'.
****      lr_actyp-low = '*'.
****      APPEND lr_actyp.
***        lwa_selection-tabname = '/AGRI/GLachdr'.
***        lwa_selection-fieldname = 'actyp'.
***        lwa_selopt-low = '*'.
***        lwa_selopt-sign = 'I'.
***        lwa_selopt-option = 'CP'.
***        APPEND lwa_selopt TO lwa_selection-selopt.
***        APPEND lwa_selection TO lt_selections.
***
***        EXPORT t_selections = lt_selections
***               skip_screen = 'X' TO MEMORY ID 'FMAC_SRCH'.
***
***        SUBMIT /agri/fmac_process                        "#EC CI_SUBMIT
****                    WITH so_actyp IN lr_actyp
****                    WITH FREE SELECTIONS lt_selections
***                      WITH p_excall = c_true
***                       AND RETURN.
***      ENDIF.
***
***      IMPORT gt_glachdr = gt_search_header FROM MEMORY ID 'FMAC_SRCH'.
***      FREE MEMORY ID 'FMAC_SRCH'.
***
*******Display or change
***    ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change_id OR
***       is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display_id.
****** S4/HANA: Search Profile related
****    lt_acnum-sign = 'I'.
****    lt_acnum-option = 'EQ'.
****    lt_acnum-low = is_wslink-objid.
****    APPEND lt_acnum.
***      lwa_selection-tabname = '/AGRI/GLachdr'.
***      lwa_selection-fieldname = 'acnum'.
***      lwa_selopt-low = is_wslink-objid.
***      lwa_selopt-sign = 'I'.
***      lwa_selopt-option = 'EQ'.
***      APPEND lwa_selopt TO lwa_selection-selopt.
***      APPEND lwa_selection TO lt_selections.
***
***      EXPORT t_selections = lt_selections
***             skip_screen = 'X' TO MEMORY ID 'FMAC_SRCH'.
***      SUBMIT /agri/fmac_process                          "#EC CI_SUBMIT
****              WITH so_acnum IN lt_acnum
***                WITH p_excall = c_true
***                AND RETURN.
***      IMPORT gt_glachdr = gt_search_header FROM MEMORY ID 'FMAC_SRCH'.
***      FREE MEMORY ID 'FMAC_SRCH'.
***    ELSE.
***      gs_variables-display_only = i_display_only.
***    ENDIF.
***    IF NOT it_acnum[] IS INITIAL.
***      SELECT *
***      FROM /agri/glachdr
***      INTO CORRESPONDING FIELDS OF TABLE gt_search_header
***      FOR ALL ENTRIES IN it_acnum
***      WHERE acnum EQ it_acnum-acnum.
***
***      IF sy-subrc NE 0.
****      MESSAGE ID '/AGRI/GRCA' TYPE c_msg_type-success NUMBER '103'
****      INTO sy-msgli.
****      message_simple space.
***        RAISE no_documents_to_process.
***      ENDIF.
***    ENDIF.
***  ENDIF.
***
****  gs_variables-refresh_worklist = c_true.
****
****  DESCRIBE TABLE gt_search_header LINES sy-tfill.
****  IF sy-tfill EQ 1.
****    READ TABLE gt_search_header INTO lwa_search_header INDEX 1.
****    PERFORM document_infocus_set USING lwa_search_header-acnum
****                              CHANGING lv_subrc.
****  ENDIF.
*******Portal
***  IF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change OR
***      is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change_id.
***    gs_variables-document_mode = c_mode_change.
***    gs_variables-overview_mode = c_mode_change.
***  ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display OR
***      is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display_id.
***    gs_variables-display_only = c_mode_display.
***  ENDIF.
*******
***
***  PERFORM acdoc_data_set.
***  gs_variables-refresh_worklist = c_true.
***  CALL SCREEN 100.
***
****PERFORM measurement_display.
***
***  CLEAR: gs_variables-display_only,gs_variables-external.
***  REFRESH: gt_search_header.

ENDFUNCTION.
