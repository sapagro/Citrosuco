*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0O .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ORGANIZATION_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM organization_data_display .

  CLEAR: /agri/s_glflot. "/agri/s_gliloa, /agri/s_gliflot,
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iloa TO /agri/s_gliloa.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iflot TO /agri/s_gliflot.
  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO /agri/s_glflot.

ENDFORM.                    " ORGANIZATION_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ORGANIZATION_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM organization_data_update .

  DATA: "ls_iloa TYPE /agri/s_gliloa,
        "ls_iflot TYPE /agri/s_gliflot,
        ls_flhdr TYPE /agri/s_glflot.

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iloa TO ls_iloa.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iflot TO ls_iflot.
  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO ls_flhdr.

*  MOVE-CORRESPONDING /agri/s_glflot TO /agri/s_gliloa.
*  MOVE-CORRESPONDING /agri/s_glflot TO /agri/s_gliflot.
*  /agri/s_gliloa-owner = ls_iloa-owner.
*  /agri/s_gliloa-stort = ls_iloa-stort.
*  /agri/s_gliloa-adrnr = ls_iloa-adrnr.
*  /agri/s_gliflot-erdat = ls_iflot-erdat.
*  /agri/s_gliflot-ernam = ls_iflot-ernam.
*  /agri/s_gliflot-aedat = ls_iflot-aedat.
*  /agri/s_gliflot-aenam = ls_iflot-aenam.
*
*  IF ls_iloa NE /agri/s_gliloa.
*
*    MOVE-CORRESPONDING /agri/s_gliloa TO gs_fldoc_infocus-x-iloa.
*    IF gs_fldoc_infocus-x-iloa-updkz NE c_updkz_new.
*      gs_fldoc_infocus-x-iloa-updkz = c_updkz_update.
*    ENDIF.
*    gs_variables-data_changed = c_true.
*
*  ENDIF.
*
*  IF ls_iflot NE /agri/s_gliflot.
*
*    MOVE-CORRESPONDING /agri/s_gliflot TO gs_fldoc_infocus-x-iflot.
*    IF gs_fldoc_infocus-x-iflot-updkz NE c_updkz_new.
*      gs_fldoc_infocus-x-iflot-updkz = c_updkz_update.
*    ENDIF.
*    gs_variables-data_changed = c_true.
*
*  ENDIF.

  IF ls_flhdr NE /agri/s_glflot.

    MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
    gs_variables-data_changed = c_true.

  ENDIF.

ENDFORM.                    " ORGANIZATION_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  OWNERS_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owners_data_prepare .

  DATA: lwa_flown TYPE /agri/s_glflown,
        lwa_flown_fcat TYPE /agri/s_glflown_fcat.

  CHECK ref_owners_grid IS INITIAL OR
        gs_variables-refresh_owners_grid IS NOT INITIAL.

  REFRESH: gt_owners.
  LOOP AT gs_fldoc_infocus-x-flown INTO lwa_flown.
    CLEAR lwa_flown_fcat.
    MOVE-CORRESPONDING lwa_flown TO lwa_flown_fcat.
    PERFORM owner_styles_prepare CHANGING lwa_flown_fcat.
    APPEND lwa_flown_fcat TO gt_owners.
  ENDLOOP.

ENDFORM.                    " OWNERS_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  OWNERS_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owners_data_update .

  DATA: lwa_flown  TYPE /agri/s_glflown,
        lwa_flown_fcat TYPE /agri/s_glflown_fcat,
        lwa_mod_row TYPE lvc_s_modi,
        lv_modified,
        lv_subrc TYPE sy-subrc,
        lv_errors,
        lv_valid.

  FIELD-SYMBOLS: <lwa_flown> TYPE /agri/s_glflown.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_fldoc_infocus-x-flhdr.

  IF ok_code EQ c_fcode-owner_insert AND
     gs_fldoc_infocus-x-flhdr-owrol IS INITIAL.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '018' INTO sy-msgli.
    message_simple space.
    PERFORM messages_display USING gs_variables-initiator.
    CLEAR ok_code.
    EXIT.
  ENDIF.

  IF ok_code EQ c_fcode-owner_delete.
    PERFORM fcode_owner_delete.
    CLEAR ok_code.
  ENDIF.

  lv_modified = ref_owners_grid->data_modified_check( ).
  IF lv_modified EQ c_true.
    CALL METHOD ref_owners_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_owners_grid = c_true.
    ENDIF.
  ENDIF.

  LOOP AT gt_owner_mod_rows INTO lwa_mod_row.

    READ TABLE gt_owners INTO lwa_flown_fcat
                                INDEX lwa_mod_row-row_id.
    CHECK lwa_flown_fcat IS NOT INITIAL.

    MOVE-CORRESPONDING lwa_flown_fcat TO lwa_flown.

    PERFORM owner_data_check USING lwa_flown
                                   lv_subrc.
    IF lv_subrc NE 0.
      lv_errors = c_true.
      CLEAR gs_variables-errors.
      CONTINUE.
    ELSE.
      DELETE gt_owner_mod_rows WHERE row_id EQ lwa_mod_row-row_id.
    ENDIF.

    READ TABLE gs_fldoc_infocus-x-flown ASSIGNING <lwa_flown>
                                         WITH KEY owner = lwa_flown-owner
                                                  tplnr_fl = lwa_flown-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_flown NE <lwa_flown>.
        MOVE lwa_flown TO <lwa_flown>.
        IF <lwa_flown>-updkz NE c_updkz_new.
          gs_variables-data_changed = c_true.
          <lwa_flown>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ELSE.
      gs_variables-refresh_owners_grid = c_true.
*      gs_variables-document_changed = c_true.
      lwa_flown_fcat-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
      lwa_flown_fcat-updkz = c_updkz_new.
      PERFORM owner_styles_prepare CHANGING lwa_flown_fcat.
      MOVE-CORRESPONDING lwa_flown_fcat TO lwa_flown.
      APPEND lwa_flown TO gs_fldoc_infocus-x-flown.
    ENDIF.

  ENDLOOP.

  IF lv_errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    PERFORM refresh_owners_grid.
  ELSE.
    IF ok_code(2) EQ 'T\'.
      PERFORM owners_data_check USING space
                             CHANGING lv_subrc.
      IF lv_subrc NE 0.
        CLEAR: ok_code, gs_variables-errors.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.                    " OWNERS_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  OWNER_STYLES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_styles_prepare
                 CHANGING lwa_flown_layout TYPE /agri/s_glflown_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  REFRESH: lwa_flown_layout-styles.
  IF lwa_flown_layout-owner IS NOT INITIAL.
    lwa_style-fieldname = 'OWNER'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_flown_layout-styles.
  ENDIF.

ENDFORM.                    " OWNER_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  OWNER_VALUES_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_values_check  USING lv_display_error
                               lv_owner TYPE /agri/glowner
                               lv_owrol TYPE /agri/glowrol.

  DATA: lv_nrart TYPE nrart,
        lwa_flown TYPE /agri/s_glflown.

  FIELD-SYMBOLS: <lwa_flown> TYPE /agri/s_glflown.

  CHECK gs_variables-document_mode NE c_mode_display.

  IF "lv_display_error EQ c_true AND
     gs_tglfllvl-owrol IS INITIAL.
    IF NOT lv_owner IS INITIAL AND
       lv_owrol IS INITIAL.
      IF lv_display_error EQ c_true.
        SET CURSOR FIELD '/AGRI/S_GLFLOT-OWNER'.
      ENDIF.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '017' INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
    ENDIF.

    IF NOT lv_owrol IS INITIAL AND
           lv_owner IS INITIAL.
      IF lv_display_error EQ c_true.
        SET CURSOR FIELD '/AGRI/S_GLFLOT-OWROL'.
      ENDIF.
      MESSAGE ID '/AGRI/GCOM' TYPE 'E' NUMBER '031' INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
    ENDIF.
  ENDIF.

  IF NOT lv_owrol IS INITIAL
 AND NOT lv_owner IS INITIAL.

    SELECT SINGLE nrart FROM tpar INTO lv_nrart
                         WHERE parvw = lv_owrol.

*{   REPLACE        S4HK903710                                      1
*\    CALL FUNCTION '/AGRI/G_PARTNER_VALID_CHECK'
    CALL FUNCTION '/AGRI/G_PARTNER_VALID_CHECK'
*}   REPLACE
      EXPORTING
        i_nrart                    = lv_nrart
        i_ktonr                    = lv_owner
        i_suppress_message         = c_true
      IMPORTING
        e_ktonr                    = lv_owner
      EXCEPTIONS
        invalid_partner            = 1
        partner_function_not_found = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      IF lv_display_error EQ c_true.
        SET CURSOR FIELD '/AGRI/S_GLFLOT-OWNER'.
      ENDIF.
      MESSAGE ID '/AGRI/GCOM' TYPE 'E' NUMBER '047' WITH lv_owner lv_owrol
                                                            INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
    ENDIF.
  ENDIF.

  PERFORM owner_account_group_check USING lv_nrart lv_owrol lv_owner.

*--update owners
  IF gs_variables-errors IS INITIAL AND
     lv_display_error EQ c_true.

    IF lv_owrol NE gs_fldoc_infocus-x-flhdr-owrol.
      REFRESH: gs_fldoc_infocus-x-flown.
      LOOP AT gs_fldoc_infocus-y-flown ASSIGNING <lwa_flown>.
        <lwa_flown>-updkz = c_updkz_delete.
      ENDLOOP.
    ENDIF.

    IF lv_owner NE gs_fldoc_infocus-x-flhdr-owner.
      DELETE gs_fldoc_infocus-x-flown
       WHERE owner = gs_fldoc_infocus-x-flhdr-owner.
      LOOP AT gs_fldoc_infocus-y-flown ASSIGNING <lwa_flown>
                        WHERE owner = gs_fldoc_infocus-x-flhdr-owner.
        <lwa_flown>-updkz = c_updkz_delete.
      ENDLOOP.
    ENDIF.

    READ TABLE gs_fldoc_infocus-x-flown TRANSPORTING NO FIELDS
                                        WITH KEY owner = lv_owner.
    IF sy-subrc NE 0.
      CLEAR lwa_flown.
      lwa_flown-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
      lwa_flown-owner = lv_owner.
      lwa_flown-owrol = lv_owrol.
      lwa_flown-updkz = c_updkz_new.
      lwa_flown-ownpc = 100.
      APPEND lwa_flown TO gs_fldoc_infocus-x-flown.
    ENDIF.
    gs_variables-refresh_owners_grid = c_true.

  ENDIF.

ENDFORM.                    " OWNER_VALUES_CHECK
*&---------------------------------------------------------------------*
*&      Form  OWNER_ACCOUNT_GROUP_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_account_group_check USING lv_nrart lv_owrol
                                     lv_owner.
  DATA: ls_kna1 TYPE kna1,
        ls_lfa1 TYPE lfa1,
        ls_tpakd TYPE tpakd,
        ls_tpakl TYPE tpakl.

  CHECK NOT lv_owrol IS INITIAL AND NOT lv_owner IS INITIAL.

  CASE lv_nrart.
    WHEN 'KU'.
****Release 60E-SO
*      SELECT SINGLE * FROM kna1 INTO ls_kna1 WHERE kunnr EQ lv_owner.
      CALL FUNCTION '/AGRI/G_KNA1_SINGLE_READ'
        EXPORTING
          i_kunnr            = lv_owner
*         I_READ_CAM         =
*         I_BYPASSING_BUFFER =
*         I_REFRESH_BUFFER   =
        IMPORTING
          es_kna1            = ls_kna1
*         ES_ADDRESS_VALUE   =
        EXCEPTIONS ##FM_SUBRC_OK
          no_records_found   = 1
          internal_error     = 2
          OTHERS             = 3.
****

      SELECT SINGLE * FROM tpakd INTO ls_tpakd WHERE parvw EQ lv_owrol
                                   AND ktokd EQ ls_kna1-ktokd.
      IF sy-subrc NE 0.
        MESSAGE ID '/AGRI/GCOM' TYPE 'E' NUMBER '048'
                         WITH ls_kna1-ktokd lv_owrol INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
      ENDIF.
    WHEN 'LI'.
****Release 60E-SO
*      SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr EQ lv_owner.
      CALL FUNCTION '/AGRI/G_LFA1_SINGLE_READ'
        EXPORTING
          i_lifnr            = lv_owner
        IMPORTING
          es_lfa1            = ls_lfa1
        EXCEPTIONS ##FM_SUBRC_OK
          no_record_found    = 1
          OTHERS             = 2.
**

      SELECT SINGLE * FROM tpakl INTO ls_tpakl WHERE parvw EQ lv_owrol
                                   AND ktokk EQ ls_lfa1-ktokk.
      IF sy-subrc NE 0.
        MESSAGE ID '/AGRI/GCOM' TYPE 'E' NUMBER '048'
                         WITH ls_lfa1-ktokk lv_owrol INTO sy-msgli.
        message_simple space.
        gs_variables-errors = c_true.
      ENDIF.
  ENDCASE.

ENDFORM.                    " owner_account_group_check
*&---------------------------------------------------------------------*
*&      Form  OWNER_VALUES_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_values_get USING lv_display_error
                            lv_display
                   CHANGING lv_owner TYPE /agri/glowner.

****F4 Help for Owner Numbers
  DATA: BEGIN OF lt_dynpfields OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields,
        lv_tabname     LIKE dd03v-tabname,
        lv_fieldname   LIKE dd03v-fieldname,
        lv_parvw_field LIKE dynpread-fieldname VALUE
                                                   '/AGRI/S_GLFLOT-OWROL',
        lv_parvw       LIKE knvp-parvw,
        lv_searchhelp  LIKE help_info-mcobj,
        lv_dynprofield LIKE help_info-dynprofld,
        lwa_tpar       LIKE tpar,
        lv_dynnr       TYPE sy-dynnr,
        lwa_return_values TYPE ddshretval,
        lt_return_values TYPE TABLE OF ddshretval.

  IF lv_display_error IS NOT INITIAL.

    CLEAR lt_dynpfields.
    REFRESH lt_dynpfields.
    lt_dynpfields-fieldname = lv_parvw_field.
    APPEND lt_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
***Release 60D - Display Profile
        dyname               = c_program-funloc
****
        dynumb               = c_screen-general
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    IF sy-subrc EQ 0.
      READ TABLE lt_dynpfields INDEX 1.
      IF sy-subrc EQ 0.
        SET LOCALE LANGUAGE sy-langu.
        TRANSLATE lt_dynpfields-fieldvalue TO UPPER CASE.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
          EXPORTING
            i_parvw  = lt_dynpfields-fieldvalue
          IMPORTING
            o_parvw = lv_parvw.
      ENDIF.
    ENDIF.

  ELSE.

    lv_parvw = gs_fldoc_infocus-x-flhdr-owrol.

  ENDIF.

  IF NOT lv_parvw IS INITIAL.
    SELECT SINGLE * FROM tpar INTO lwa_tpar WHERE parvw = lv_parvw.
    CASE lwa_tpar-nrart.
      WHEN 'KU'.
        lv_fieldname = 'KUNNR'.
        lv_searchhelp = 'DEBI'.
      WHEN 'LI'.
        lv_fieldname = 'LIFNR'.
        lv_searchhelp = 'KRED'.
      WHEN 'PE'.
        lv_fieldname = 'PERNR'.
        lv_searchhelp = 'PREM'.
      WHEN 'AP'.
        AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID 'TCD' FIELD 'VPE3'.
        IF sy-subrc NE 0.
          MESSAGE s360(f2).
          EXIT.
        ENDIF.
        lv_fieldname = 'PARNR'.
        lv_searchhelp = 'VKNK'.
    ENDCASE.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = lv_tabname
        fieldname         = lv_fieldname
        searchhelp        = lv_searchhelp
        dynpprog          = c_program-funloc
        dynpnr            = lv_dynnr
*       dynprofield       = lv_dynprofield
        display           = lv_display
      TABLES
        return_tab        = lt_return_values[]
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      READ TABLE lt_return_values INTO lwa_return_values INDEX 1.
      lv_owner = lwa_return_values-fieldval.
    ENDIF.
  ELSE.
    MESSAGE s017(/agri/glfl).
  ENDIF.

ENDFORM.                    " OWNER_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  OWNERS_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owners_data_check  USING lv_before_save
                        CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_ownpc  TYPE kbetr,
        lwa_flown TYPE /agri/s_glflown.

  IF gs_fldoc_infocus-x-flown IS NOT INITIAL.
    LOOP AT gs_fldoc_infocus-x-flown INTO lwa_flown.
      lv_ownpc = lv_ownpc + lwa_flown-ownpc.
    ENDLOOP.
    IF lv_ownpc NE 100.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '024' INTO sy-msgli.
      message_simple space.
      lv_subrc = 4.
    ENDIF.
  ENDIF.

  IF lv_before_save IS NOT INITIAL.
    IF gs_fldoc_infocus-x-flown IS NOT INITIAL AND
       gs_fldoc_infocus-x-flhdr-owner IS INITIAL.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '022' INTO sy-msgli.
      message_simple space.
      lv_subrc = 4.
    ENDIF.

    IF gs_fldoc_infocus-x-flown IS INITIAL AND
       gs_fldoc_infocus-x-flhdr-owrol IS NOT INITIAL.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '017' INTO sy-msgli.
      message_simple space.
      lv_subrc = 4.
    ENDIF.
  ENDIF.

ENDFORM.                    " OWNERS_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  OWNER_TEXT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_text_get USING lv_owrol lv_owner
                 CHANGING lv_owner_txt.

  DATA: lv_nrart LIKE tpar-nrart.

  CHECK NOT lv_owrol IS INITIAL AND NOT lv_owner IS INITIAL.

  SELECT SINGLE nrart FROM tpar INTO lv_nrart
                         WHERE parvw = lv_owrol.

  partner_name_get lv_nrart
                   lv_owner
                   space
                   space
                   lv_owner_txt.

ENDFORM.                    " OWNER_TEXT_GET
*&---------------------------------------------------------------------*
*&      Form  ORGANIZATION_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM organization_data_check .


  IF /agri/s_glflot-bukrs IS INITIAL.
    CLEAR /agri/s_glflot-kokrs.
    EXIT.
  ENDIF.

*---Replace Unreleased Interfaces
*  CALL FUNCTION 'ITOB_CHECK_COMPANYCODE'
*    EXPORTING
*      bukrs_imp         = /agri/s_glflot-bukrs
*      determine_mode    = c_true
*      gsber_imp         = /agri/s_glflot-gsber
**     USE_BUF           = 'X'
**     DIALOG_MODE       = 'X'
**     DIALOG_CURSOR     = ' '
**     DIALOG_STEPL      = 0
**     INIT_MESSAGE_DATA = 'X'
**     X_MESS_TYPE       = 'E'
*    IMPORTING
**     T001_EXP          =
*      kokrs_exp         = /agri/s_glflot-kokrs
**     TKA01_EXP         =
*    EXCEPTIONS
*      empty_key         = 1
*      application_error = 2
*      OTHERS            = 3.
   CALL FUNCTION '/AGRI/G_ITOB_CHECK_COMPANYCODE'
     EXPORTING
      i_bukrs_imp               = /agri/s_glflot-bukrs
      I_DETERMINE_MODE          = c_true"'X'
      I_GSBER_IMP               = /agri/s_glflot-gsber
*      I_USE_BUF                = 'X'
*      I_DIALOG_MODE            = 'X'
*      I_DIALOG_CURSOR          = ' '
*      I_DIALOG_STEPL           = 0
*      I_INIT_MESSAGE_DATA      = 'X'
*      I_X_MESS_TYPE            = 'E'
     IMPORTING
*      E_T001                   =
      E_KOKRS                   = /agri/s_glflot-kokrs
*      E_TKA01                  =
     EXCEPTIONS
      EMPTY_KEY                 = 1
      APPLICATION_ERROR         = 2
      OTHERS                    = 3.
*---
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

  IF gs_fldoc_infocus-x-flhdr-iwerk NE /agri/s_glflot-iwerk AND
     gs_fldoc_infocus-x-flhdr-iwerk IS NOT INITIAL.
    LOOP AT gs_fldoc_infocus-x-flcma TRANSPORTING NO FIELDS
                    WHERE tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl
                      AND iwerk = gs_fldoc_infocus-x-flhdr-iwerk
                      AND astat = 'A'
                      AND loevm IS INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      /agri/s_glflot-iwerk = gs_fldoc_infocus-x-flhdr-iwerk.
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-error
                              NUMBER '023'
                              WITH gs_fldoc_infocus-x-flhdr-iwerk
                              INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " ORGANIZATION_DATA_CHECK
