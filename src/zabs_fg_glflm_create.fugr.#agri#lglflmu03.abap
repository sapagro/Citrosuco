FUNCTION /agri/glfl_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MESSAGES_DISPLAY) TYPE  SY-DATAR DEFAULT SPACE
*"     VALUE(I_SAVE_MESSAGES) TYPE  SY-DATAR DEFAULT SPACE
*"     VALUE(I_COMMIT_WORK) TYPE  SY-DATAR DEFAULT 'X'
*"     VALUE(I_CHECK_ADDRESS) TYPE  XFELD DEFAULT 'X'
*"     VALUE(I_DIALOG) TYPE  XFELD DEFAULT SPACE
*"     VALUE(IS_FLHDR) TYPE  /AGRI/S_GLFLOT
*"     VALUE(IS_IFLOT) TYPE  /AGRI/S_GLIFLOT OPTIONAL
*"     VALUE(IS_ADRC) TYPE  BAPIADDR1 OPTIONAL
*"     VALUE(IS_ILOA) TYPE  /AGRI/S_GLILOA OPTIONAL
*"     VALUE(IT_IFLOTX) TYPE  /AGRI/T_GLIFLOTX OPTIONAL
*"     VALUE(IT_IHPA) TYPE  /AGRI/T_GLIHPA OPTIONAL
*"     VALUE(IT_FLOWN) TYPE  /AGRI/T_GLFLOWN OPTIONAL
*"     VALUE(IT_FLOS) TYPE  /AGRI/T_GLFLOS OPTIONAL
*"  EXPORTING
*"     VALUE(ES_GLFL_DOC) TYPE  /AGRI/S_GLFL_DOC
*"     VALUE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"     REFERENCE(E_LOG_NUMBER) TYPE  BALOGNR
*"  EXCEPTIONS
*"      NO_DOCUMENTS_TO_PROCESS
*"      NO_AUTHORIZATION
*"      CREATION_FAILED
*"----------------------------------------------------------------------

  DATA: lv_subrc             TYPE sy-subrc,
        lv_stop_save,
        lv_ktonr             TYPE ktonr,
        lv_ktonr_old         TYPE ktonr,
        lwa_ihpa             TYPE /agri/s_glihpa,
        lwa_tpar             TYPE tpar,
        lwa_flown            TYPE /agri/s_glflown,
        lwa_fldesc           TYPE /agri/s_gliflotx,
        lwa_flos             TYPE /agri/s_glflos,
        lwa_address_complete TYPE szadr_addr1_complete,
        lwa_errors           TYPE addr_error,
        lt_errors            TYPE TABLE OF addr_error,
        ls_variant           TYPE disvariant,
        ls_message           TYPE /agri/s_gprolog,
        ls_partner_attr      TYPE /agri/s_gparattr,
        lt_messages          TYPE /agri/t_gprolog.

  FIELD-SYMBOLS: <ls_ihpa> TYPE  /agri/s_glihpa.

  STATICS: lt_tpar TYPE TABLE OF tpar.

  DEFINE messages_all_display.
    IF i_messages_display EQ c_true.
      messages_display gs_variables-initiator c_true space
                       space ls_variant.
    ELSE.
      messages_get gs_variables-initiator et_messages[].
    ENDIF.
    IF i_save_messages EQ c_true.
      messages_save gs_variables-initiator c_true.
    ENDIF.
    CLEAR gs_variables-initiator.
    e_log_number = gs_log_variables-log_number.
    messages_init.
  END-OF-DEFINITION.

  PERFORM document_data_initialize USING c_true.

  PERFORM transaction_init USING c_mode_change.

  gs_variables-overview_mode = c_mode_create.
  gs_variables-document_mode = c_mode_create.
  gs_variables-initiator = c_log_initiator-change.

*  MOVE-CORRESPONDING is_flhdr TO lwa_flhdr.

  MOVE-CORRESPONDING: is_flhdr TO /agri/s_glflot.

****Not required any conversion
*  CALL FUNCTION '/AGRI/G_CONV_EXIT_KONPD_INPUT'
*    EXPORTING
*      i_input       = is_flhdr-pspnr
**     I_NO_MESSAGE  =
*    IMPORTING
*      e_output      = /agri/s_glflot-pspnr
**     ES_RETURN     =
*    EXCEPTIONS
*      no_conversion = 1
*      OTHERS        = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

  TRANSLATE /agri/s_glflot-tplnr_fl TO UPPER CASE.
  TRANSLATE /agri/s_glflot-tplma TO UPPER CASE.
  TRANSLATE /agri/s_glflot-strno TO UPPER CASE.

  CLEAR: gs_variables-errors.
  IF i_dialog IS NOT INITIAL.
    CALL SCREEN 201 STARTING AT 5 5.
  ENDIF.

  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-change
                                    /agri/s_glflot.

  PERFORM header_data_check.
  IF gs_variables-errors IS INITIAL.
    PERFORM funloc_data_check.
  ENDIF.

  PERFORM funloc_control_read USING /agri/s_glflot-tplkz
                                  /agri/s_glflot-tplvl.
  IF gs_variables-errors IS NOT INITIAL.
*{   INSERT         C4DK903073                                        1
  messages_all_display.
*}   INSERT
    RAISE creation_failed.
  ENDIF.

  IF ( /agri/s_glflot-owner IS NOT INITIAL OR
       /agri/s_glflot-owrol IS NOT INITIAL ).
**S4H
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = /agri/s_glflot-owner
      IMPORTING
        output = /agri/s_glflot-owner.
**
    PERFORM owner_values_check USING space
                                   /agri/s_glflot-owner
                                   /agri/s_glflot-owrol.
  ENDIF.

  IF gs_variables-errors IS NOT INITIAL.
*{   INSERT         C4DK903073                                        2
  messages_all_display.
*}   INSERT
    RAISE creation_failed.
  ENDIF.

*--Call defaults fill here
  IF gs_tglfllvl-owrol IS NOT INITIAL.
    /agri/s_glflot-owrol = gs_tglfllvl-owrol.
  ENDIF.
  IF gs_tglfllvl-fltyp IS NOT INITIAL.
    /agri/s_glflot-fltyp = gs_tglfllvl-fltyp.
  ENDIF.

  MOVE-CORRESPONDING: /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
*                      is_iflot       TO gs_fldoc_infocus-x-iflot,
*                      is_iloa        TO gs_fldoc_infocus-x-iloa,
*                      /agri/s_glflot TO gs_fldoc_infocus-x-iflot,
*                      /agri/s_glflot TO gs_fldoc_infocus-x-iloa.

****Description same as Terrain
  gs_fldoc_infocus-x-flhdr-pltxt = gs_fldoc_infocus-x-flhdr-tplnr_fl.

*--call authority check
  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                c_authorization_activity-create
                                c_msg_type-error
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                        3
  messages_all_display.
*}   INSERT
    RAISE no_authorization.
  ENDIF.

  gs_variables-data_changed = c_true.

*--Status Object
  gs_fldoc_infocus-x-flhdr-stsma = gs_tglfllvl-stsma.
  PERFORM status_object_create USING gs_fldoc_infocus-x-flhdr-stsma
                            CHANGING gs_fldoc_infocus-x-flhdr-objnr.
*  gs_fldoc_infocus-x-iflot-objnr = gs_fldoc_infocus-x-flhdr-objnr.

  gs_fldoc_infocus-x-flhdr-pargr = gs_tglfllvl-pargr.

  gs_fldoc_infocus-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
  gs_fldoc_infocus-x-flhdr-updkz = c_updkz_new.
*  gs_fldoc_infocus-x-iloa-updkz = c_updkz_new.
*  gs_fldoc_infocus-x-iflot-updkz = c_updkz_new.

  PERFORM address_init.
  PERFORM partners_init.
  PERFORM assignments_init.

  LOOP AT it_iflotx INTO lwa_fldesc.
    lwa_fldesc-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
    lwa_fldesc-updkz = c_updkz_new.
    PERFORM desc_duplicates_check USING lwa_fldesc lv_subrc.
    IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                        4
  messages_all_display.
*}   INSERT
      RAISE creation_failed.
    ENDIF.
    APPEND lwa_fldesc TO gs_fldoc_infocus-x-iflotx.
  ENDLOOP.

  LOOP AT it_flos INTO lwa_flos.
    lwa_flos-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
    IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                        5
  messages_all_display.
*}   INSERT
      RAISE creation_failed.
    ENDIF.
    APPEND lwa_flos TO gs_fldoc_infocus-x-flos.
  ENDLOOP.

  LOOP AT it_ihpa INTO lwa_ihpa.

*    CALL FUNCTION 'PM_PARTNER_MAINTAIN'
*      EXPORTING
*        objnr               = gs_fldoc_infocus-x-flhdr-objnr
*        parnr               = lwa_ihpa-parnr
*        parvw               = lwa_ihpa-parvw
*        adrnr               = lwa_ihpa-adrnr
**       ADR_HANDLE          =
**       TABIX               = 0
**       TABIX_OBJNR         = 0
**       MSGTY               = 'E'
*        cpd_pop_up          = c_false
**       CHECK_DEBITOR       = 'X'
**     IMPORTING
**       COUNTER             =
*      EXCEPTIONS
*        invalid_parnr       = 1
*        cpd_customer        = 2
*        invalid_parvw       = 3
*        parvw_unique        = 4
*        OTHERS              = 5.
*    IF sy-subrc <> 0.
*      MESSAGE e025(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
*                         INTO sy-msgli.
*      message_simple space.
*      RAISE creation_failed.
*    ENDIF.
    lwa_ihpa-ptrno = gs_fldoc_infocus-x-flhdr-ptrno.
    lwa_ihpa-updkz = c_updkz_new.

    READ TABLE lt_tpar INTO lwa_tpar WITH KEY parvw = lwa_ihpa-parvw.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM tpar INTO lwa_tpar WHERE parvw = lwa_ihpa-parvw. "#EC CI_SEL_NESTED
      APPEND lwa_tpar TO lt_tpar.
    ENDIF.

    CASE lwa_tpar-nrart.
      WHEN 'KU'.
        lv_ktonr = lwa_ihpa-kunnr.
      WHEN 'LI'.
        lv_ktonr = lwa_ihpa-lifnr.
      WHEN 'PE'.
        lv_ktonr = lwa_ihpa-pernr.
      WHEN 'AP'.
        lv_ktonr = lwa_ihpa-parnr.
***E SP2 Business Partner
      WHEN 'XP'.
        lv_ktonr = lwa_ihpa-ablad.
    ENDCASE.
***QA#9227
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ktonr
      IMPORTING
        output = lv_ktonr.
***

    partner_valid_check lwa_ihpa-parvw
                        lv_ktonr
                        space
                        lv_subrc.
    IF sy-subrc NE 0.
*****CI-IP #3213 60D-SP3
****Add Partner message
      IF NOT sy-msgid IS INITIAL AND NOT sy-msgty IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        message_simple space.
      ENDIF.
      messages_all_display.
*{   INSERT         C4DK903073                                        6
  messages_all_display.
*}   INSERT
      RAISE creation_failed.
    ENDIF.
    IF lwa_ihpa-adrda NE 'E'.
      CLEAR: lwa_ihpa-adrnr, lwa_ihpa-adrda.
    ENDIF.
****maintain partner
    PERFORM partners_maintain TABLES gs_fldoc_infocus-x-ihpa
                                     gs_fldoc_infocus-y-ihpa
                               USING lwa_ihpa-parvw
                                     lv_ktonr
                                     lv_ktonr_old
*                                     gs_fldoc_infocus-x-kona-vkorg
*                                     gs_fldoc_infocus-x-kona-vtweg
*                                     gs_fldoc_infocus-x-kona-spart
                                     lwa_ihpa-adrnr
                                     lwa_ihpa-adrda
                                     c_true
                            CHANGING lv_subrc.
    IF lv_subrc NE 0.
      messages_all_display.
      RAISE creation_failed.
    ELSE.
      IF lwa_ihpa-adrda EQ 'E' OR
         lwa_ihpa-ablad IS NOT INITIAL.
        MOVE-CORRESPONDING lwa_ihpa TO ls_partner_attr.
***E SP2 Business Partner
        READ TABLE gs_fldoc_infocus-x-ihpa ASSIGNING <ls_ihpa>
                             WITH KEY parvw = lwa_ihpa-parvw
                                      kunnr = lwa_ihpa-kunnr
                                      lifnr = lwa_ihpa-lifnr
                                      pernr = lwa_ihpa-pernr
                                      parnr = lwa_ihpa-parnr
                                      ablad = lwa_ihpa-ablad.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_partner_attr TO <ls_ihpa>.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR lv_ktonr.
  ENDLOOP.

  CALL FUNCTION '/AGRI/G_PAR_DOC_ADDR_OVERRIDE'
    CHANGING
      ct_partners = gs_fldoc_infocus-x-ihpa.

*  CALL FUNCTION 'PM_PARTNER_COMPLETE'
*    EXPORTING
**     EQUNR              = ' '
**     EXIT               = ' '
*      objnr              = gs_fldoc_infocus-x-flhdr-objnr
*      obtyp              = 'IFL'
*      pargr              = gs_fldoc_infocus-x-flhdr-pargr
**     SAVE               = ' '
**     TPLNR              = ' '
**     SUBSCREEN_EXT      = 0000
**     SUBSCREEN_PROG_EXT = '        '
*      handling           = 'E'
**     IV_TRANS_TEXT      =
*    IMPORTING
*      partner_missing    = gs_variables-errors
*    EXCEPTIONS
*      cancel             = 1
*      exit               = 2
*      save               = 3
*      OTHERS             = 4.
*  IF sy-subrc <> 0 OR
*     gs_variables-errors IS NOT INITIAL.
*    MESSAGE e025(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
*                             INTO sy-msgli.
*    message_simple space.
*    RAISE creation_failed.
*  ENDIF.

  IF is_adrc IS NOT INITIAL.
    CALL FUNCTION 'ADDR_CONVERT_FROM_BAPIADDR1'
      EXPORTING
        addr1_complete_bapi = is_adrc
      IMPORTING
        addr1_complete      = lwa_address_complete.
    gs_fldoc_infocus-x-flhdr-adrnr = lwa_address_complete-addrnumber.
    PERFORM address_handle_prepare.
    PERFORM address_maintain TABLES lt_errors
                              USING gv_address_handle
                                    i_check_address
                                    c_updkz_new
                           CHANGING lwa_address_complete.
    LOOP AT lt_errors INTO lwa_errors.
      IF lwa_errors-msg_type EQ c_msg_type-error.
        lv_subrc = 4.
      ENDIF.
      MESSAGE ID lwa_errors-msg_id TYPE lwa_errors-msg_type
      NUMBER lwa_errors-msg_number WITH lwa_errors-msg_var1
      lwa_errors-msg_var2 lwa_errors-msg_var3 lwa_errors-msg_var4 INTO
      sy-msgli.
      message_simple space.
    ENDLOOP.

    IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                        7
  messages_all_display.
*}   INSERT
      RAISE creation_failed.
    ELSE.
      gs_variables-addr_data_changed = c_true.
    ENDIF.
  ENDIF.

  LOOP AT it_flown INTO lwa_flown.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_flown-owner
      IMPORTING
        output = lwa_flown-owner.

    lwa_flown-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
    lwa_flown-updkz = c_updkz_new.

    PERFORM owner_data_check USING lwa_flown
                          CHANGING lv_subrc.
    IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                        8
  messages_all_display.
*}   INSERT
      RAISE creation_failed.
    ENDIF.
    APPEND lwa_flown TO gs_fldoc_infocus-x-flown.
  ENDLOOP.

  PERFORM badi_document_check CHANGING lv_stop_save.
  IF NOT lv_stop_save IS INITIAL.
*{   INSERT         C4DK903073                                        9
  messages_all_display.
*}   INSERT
    RAISE creation_failed.
  ENDIF.

  PERFORM owners_data_check USING c_true
                         CHANGING lv_subrc.
  IF lv_subrc NE 0.
*{   INSERT         C4DK903073                                       10
  messages_all_display.
*}   INSERT
    RAISE creation_failed.
  ENDIF.
  PERFORM address_number_get.

****Partners
  lwa_ihpa-updkz = c_updkz_new.
  MODIFY gs_fldoc_infocus-x-ihpa FROM lwa_ihpa
                     TRANSPORTING updkz
                            WHERE parvw NE space.

  CLEAR: gs_variables-data_changed.
  CALL FUNCTION '/AGRI/GLFL_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK  = 'X'
      i_commit_work      = i_commit_work
*     IREF_TEXT          =
    CHANGING
      cs_fldoc           = gs_fldoc_infocus
      ct_messages        = lt_messages
    EXCEPTIONS
      no_change          = 1
      error_while_saving = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  lv_subrc = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
        ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      CLEAR: gs_fldoc_infocus.
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-error NUMBER '009'
                                                        INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    PERFORM document_infocus_read USING gs_fldoc_infocus-tplnr_fl.
    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '007'
                            WITH gs_fldoc_infocus-x-flhdr-tplnr_fl INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

  messages_all_display.
  es_glfl_doc = gs_fldoc_infocus.

ENDFUNCTION.                                             "#EC CI_VALPAR
