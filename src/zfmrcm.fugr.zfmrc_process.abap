FUNCTION ZFMRC_PROCESS.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MODE) LIKE  RV56A-SELKZ DEFAULT 'A'
*"     VALUE(I_DISPLAY_ONLY) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(I_SAVE) LIKE  RV56A-SELKZ DEFAULT 'X'
*"     VALUE(IT_MDOCM) TYPE  /AGRI/T_GLMDOCM OPTIONAL
*"     VALUE(IS_WSLINK) TYPE  /AGRI/S_GWPLINK OPTIONAL
*"  EXPORTING
*"     VALUE(E_MDOCM) TYPE  /AGRI/GLMDOCM
*"     VALUE(E_DATA_CHANGED) TYPE  CHAR1
*"  EXCEPTIONS
*"      ENTER_MESASUREMENT_DOCUMENT
*"      INVALID_PARAMETER_COMBINATION
*"      NO_DOCUMENTS_TO_PROCESS
*"--------------------------------------------------------------------
***
***  DATA: lv_subrc           TYPE sy-subrc,
***        lwa_search_header  LIKE LINE OF gt_search_header,
***        lwa_RCHDR          TYPE zsc_fmRCHDR,
***        lwa_RCNUM          TYPE /agri/s_glRCNUM,
***        lt_link_attributes TYPE /agri/t_gwplink_attr,
***        lwa_link_attribute TYPE /agri/s_gwplink_attr,
***        lv_set_param(20)   TYPE c.
****        lt_selections TYPE rsds_texpr.
***
***  RANGES: lt_RCNUM FOR /agri/glRCHDR-RCNUM,
***          lr_RCTYP FOR /agri/glRCHDR-RCTYP.
***
***  FIELD-SYMBOLS: <lv_value>         TYPE any,
***                 <ls_search_header> TYPE zsc_fmRCHDR_wl.
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
***    CLEAR lwa_RCHDR.
***    MOVE-CORRESPONDING gs_RCDOC_infocus-x-RCHDR TO lwa_RCHDR.
***    MOVE-CORRESPONDING  ls_ws_context TO lwa_RCHDR.
***
***    IF NOT lwa_RCHDR-RCTYP IS INITIAL.
***      gs_RCHDR_portal = lwa_RCHDR.
***      PERFORM portal_RCHDR_update CHANGING gs_RCHDR_portal
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
***                 skip_screen = 'X' TO MEMORY ID 'FMRC_SRCH'.
***          SUBMIT /agri/FMRC_process        "#EC NEEDED   "#EC CI_SUBMIT
***               WITH p_excall = c_true
****             USING SELECTION-SET  is_wslink-varid
***               AND RETURN.
***        ELSE.
****** S4/HANA: Search Profile related
***          EXPORT varid = is_wslink-varid
***                 t_selections = lt_selections
***                 skip_screen = 'X' TO MEMORY ID 'FMRC_SRCH'.
***          SUBMIT /agri/FMRC_process                      "#EC CI_SUBMIT
****             WITH FREE SELECTIONS lt_selections
***               WITH p_excall = c_true
****             USING SELECTION-SET  is_wslink-varid
***               AND RETURN.
***        ENDIF.
****** S4/HANA: Search Profile related
***      ELSEIF lt_selections IS NOT INITIAL.
****    ELSE.
****      lr_RCTYP-sign = 'I'.
****      lr_RCTYP-option = 'CP'.
****      lr_RCTYP-low = '*'.
****      APPEND lr_RCTYP.
***        lwa_selection-tabname = '/AGRI/GLRCHDR'.
***        lwa_selection-fieldname = 'RCTYP'.
***        lwa_selopt-low = '*'.
***        lwa_selopt-sign = 'I'.
***        lwa_selopt-option = 'CP'.
***        APPEND lwa_selopt TO lwa_selection-selopt.
***        APPEND lwa_selection TO lt_selections.
***
***        EXPORT t_selections = lt_selections
***               skip_screen = 'X' TO MEMORY ID 'FMRC_SRCH'.
***
***        SUBMIT /agri/FMRC_process                        "#EC CI_SUBMIT
****                    WITH so_RCTYP IN lr_RCTYP
****                    WITH FREE SELECTIONS lt_selections
***                      WITH p_excall = c_true
***                       AND RETURN.
***      ENDIF.
***
***      IMPORT gt_glRCHDR = gt_search_header FROM MEMORY ID 'FMRC_SRCH'.
***      FREE MEMORY ID 'FMRC_SRCH'.
***
*******Display or change
***    ELSEIF is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_change_id OR
***       is_wslink-prtyp EQ /agri/if_gwpws_constants=>c_prtyp_display_id.
****** S4/HANA: Search Profile related
****    lt_RCNUM-sign = 'I'.
****    lt_RCNUM-option = 'EQ'.
****    lt_RCNUM-low = is_wslink-objid.
****    APPEND lt_RCNUM.
***      lwa_selection-tabname = '/AGRI/GLRCHDR'.
***      lwa_selection-fieldname = 'RCNUM'.
***      lwa_selopt-low = is_wslink-objid.
***      lwa_selopt-sign = 'I'.
***      lwa_selopt-option = 'EQ'.
***      APPEND lwa_selopt TO lwa_selection-selopt.
***      APPEND lwa_selection TO lt_selections.
***
***      EXPORT t_selections = lt_selections
***             skip_screen = 'X' TO MEMORY ID 'FMRC_SRCH'.
***      SUBMIT /agri/FMRC_process                          "#EC CI_SUBMIT
****              WITH so_RCNUM IN lt_RCNUM
***                WITH p_excall = c_true
***                AND RETURN.
***      IMPORT gt_glRCHDR = gt_search_header FROM MEMORY ID 'FMRC_SRCH'.
***      FREE MEMORY ID 'FMRC_SRCH'.
***    ELSE.
***      gs_variables-display_only = i_display_only.
***    ENDIF.
***    IF NOT it_RCNUM[] IS INITIAL.
***      SELECT *
***      FROM /agri/glRCHDR
***      INTO CORRESPONDING FIELDS OF TABLE gt_search_header
***      FOR ALL ENTRIES IN it_RCNUM
***      WHERE RCNUM EQ it_RCNUM-RCNUM.
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
****    PERFORM document_infocus_set USING lwa_search_header-RCNUM
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
***  PERFORM RCDOC_data_set.
***  gs_variables-refresh_worklist = c_true.
***  CALL SCREEN 100.
***
****PERFORM measurement_display.
***
***  CLEAR: gs_variables-display_only,gs_variables-external.
***  REFRESH: gt_search_header.

ENDFUNCTION.
