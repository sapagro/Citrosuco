FUNCTION /agri/glfl_change.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MESSAGES_DISPLAY) TYPE  SY-DATAR DEFAULT SPACE
*"     VALUE(I_SAVE_MESSAGES) TYPE  SY-DATAR DEFAULT SPACE
*"     VALUE(IS_FLHDR) TYPE  /AGRI/S_GLFLOT
*"     VALUE(IS_IFLOT) TYPE  /AGRI/S_GLIFLOT OPTIONAL
*"     VALUE(IS_ADRC) TYPE  BAPIADDR1 OPTIONAL
*"     VALUE(IS_ILOA) TYPE  /AGRI/S_GLILOA OPTIONAL
*"     VALUE(IT_IFLOTX) TYPE  /AGRI/T_GLIFLOTX OPTIONAL
*"     VALUE(IT_IHPA) TYPE  /AGRI/T_GLIHPA OPTIONAL
*"     VALUE(IT_FLATG) TYPE  /AGRI/T_GLFLATG OPTIONAL
*"     VALUE(IT_FLATV) TYPE  /AGRI/T_GLFLATV OPTIONAL
*"     VALUE(IT_FLCMA) TYPE  /AGRI/T_GLFLCMA OPTIONAL
*"     VALUE(IT_FLOWN) TYPE  /AGRI/T_GLFLOWN OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_GLFL_DOC) TYPE  /AGRI/S_GLFL_DOC
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"     REFERENCE(E_LOG_NUMBER) TYPE  BALOGNR
*"  CHANGING
*"     REFERENCE(CS_GLFL_DOC) TYPE  /AGRI/S_GLFL_DOC OPTIONAL
*"  EXCEPTIONS
*"      NO_DOCUMENTS_TO_PROCESS
*"      NO_AUTHORIZATION
*"      CHANGE_FAILED
*"      TERRAIN_LOCKED
*"----------------------------------------------------------------------

  DATA: lv_subrc             TYPE sy-subrc,
        lv_ktonr             TYPE ktonr,
        ls_variant           TYPE disvariant,
        lwa_tpar             TYPE tpar,
        lwa_fldesc           TYPE /agri/s_gliflotx,
        lwa_flown            TYPE /agri/s_glflown,
        lwa_flcma            TYPE /agri/s_glflcma,
        lwa_flos             TYPE /agri/s_glflos,
        lwa_flatv            TYPE /agri/s_glflatv,
        lwa_flppl            TYPE /agri/s_glflppl,
        lwa_flatv_old        TYPE /agri/s_glflatv,
        lwa_bapiaddr1        TYPE bapiaddr1,
        lwa_flatg            TYPE /agri/s_glflatg,
        lwa_mod_row          TYPE lvc_s_modi,
        lwa_ihpa             TYPE /agri/s_glihpa,
        ls_ihpa              TYPE /agri/s_glihpa,
        ls_xihpa             TYPE /agri/s_glihpa,
        lwa_errors           TYPE addr_error,
        lt_errors            TYPE TABLE OF addr_error,
        lwa_address_complete TYPE szadr_addr1_complete,
        addr1_wa             TYPE szadr_addr1_line,
        adtel_wa             TYPE szadr_adtel_line,
        adfax_wa             TYPE szadr_adfax_line,
        lwa_fldesc_fcat      TYPE /agri/s_gliflotx_fcat,
        lwa_flatg_fcat       TYPE /agri/s_glflatg_fcat,
        lv_tabix             TYPE sy-tabix,
        lv_counter           TYPE sy-tabix,
        lt_tab               TYPE TABLE OF ihpavb,
        ls_tab               TYPE ihpavb.

  FIELD-SYMBOLS: <lwa_fldesc> TYPE /agri/s_gliflotx,
                 <lwa_flown>  TYPE /agri/s_glflown,
                 <lwa_flcma>  TYPE /agri/s_glflcma,
                 <lwa_flatg>  TYPE /agri/s_glflatg,
                 <lwa_flatv>  TYPE /agri/s_glflatv,
                 <lwa_ihpa>   TYPE /agri/s_glihpa,
                 <lwa_flppl>  TYPE /agri/s_glflppl.

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

  CHECK cs_glfl_doc IS NOT INITIAL OR
        is_flhdr IS NOT INITIAL." OR
*        is_iflot IS NOT INITIAL.

****Init messages
  gs_variables-initiator = c_log_initiator-change.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-change
                                    is_flhdr.

  gs_variables-overview_mode = c_mode_change.
  gs_variables-document_mode = c_mode_change.

  CLEAR lv_subrc.
  PERFORM authority_check USING is_flhdr
                                c_authorization_activity-change
                                c_msg_type-error
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    gs_variables-overview_mode = gs_variables-document_mode
                               = c_mode_display.
    gs_variables-errors = c_true.
    PERFORM document_data_initialize USING c_true.
    messages_all_display.
    EXIT.
  ENDIF.

  PERFORM document_infocus_read USING is_flhdr-tplnr_fl.
  IF is_flhdr-tplkz NE gs_fldoc_infocus-x-flhdr-tplkz OR
     is_flhdr-fltyp NE gs_fldoc_infocus-x-flhdr-fltyp OR
     is_flhdr-strno NE gs_fldoc_infocus-x-flhdr-strno.
    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-error NUMBER '028'
                            INTO sy-msgli.
    message_simple space.
    messages_all_display.
    RAISE no_documents_to_process.
  ENDIF.

  PERFORM partners_init.

  PERFORM funloc_control_read USING gs_fldoc_infocus-x-flhdr-tplkz
                                    gs_fldoc_infocus-x-flhdr-tplvl.

  IF gs_variables-errors IS NOT INITIAL.
    messages_all_display.
    RAISE change_failed.
  ENDIF.

  PERFORM document_infocus_lock USING is_flhdr-tplnr_fl
                                      is_flhdr-strno
                                      c_msg_type-error
                             CHANGING lv_subrc.
  IF lv_subrc NE 0.
    messages_all_display.
    RAISE terrain_locked.
  ENDIF.

  PERFORM attr_infocus_set.
  MOVE-CORRESPONDING is_flhdr TO /agri/s_glflot.
  MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
*  MOVE-CORRESPONDING is_iflot TO /agri/s_gliflot.
*  MOVE-CORRESPONDING /agri/s_gliflot TO gs_fldoc_infocus-x-iflot.
*  MOVE-CORRESPONDING is_iloa  TO /agri/s_gliloa.
*  MOVE-CORRESPONDING /agri/s_gliloa TO gs_fldoc_infocus-x-iloa.
  PERFORM header_data_update.
  PERFORM funloc_data_update.
*  gs_fldoc_infocus-x-flhdr-bukrs = is_iloa-bukrs.
*  gs_fldoc_infocus-x-iloa-anlnr = is_iloa-anlnr.
*  gs_fldoc_infocus-x-iloa-kostl = is_iloa-kostl.


  PERFORM funloc_data_check.
  IF gs_variables-errors IS NOT INITIAL.
    messages_all_display.
    RAISE change_failed.
  ENDIF.

  IF it_iflotx IS NOT INITIAL.
    LOOP AT it_iflotx INTO lwa_fldesc WHERE updkz IS NOT INITIAL.
      MOVE-CORRESPONDING lwa_fldesc TO lwa_fldesc_fcat.
      lwa_mod_row-row_id = sy-tabix.
      APPEND lwa_mod_row TO gt_desc_mod_rows.
      APPEND lwa_fldesc_fcat TO gt_fl_desc_layout.
    ENDLOOP.

    PERFORM desc_update CHANGING lv_subrc.
    IF lv_subrc NE 0.
      RAISE change_failed.
    ENDIF.

    LOOP AT gs_fldoc_infocus-y-iflotx ASSIGNING <lwa_fldesc>.
      READ TABLE it_iflotx WITH KEY tplnr_fl = <lwa_fldesc>-tplnr_fl
                                    spras = <lwa_fldesc>-spras
                   TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        <lwa_fldesc>-updkz = c_updkz_delete.
      ENDIF.
    ENDLOOP.
  ENDIF.
***** Address
****  Update
  IF is_adrc IS NOT INITIAL AND cs_glfl_doc-x-adrc-updkz EQ c_updkz_update.
    MOVE-CORRESPONDING is_adrc TO lwa_bapiaddr1.

    CALL FUNCTION 'ADDR_CONVERT_FROM_BAPIADDR1'
      EXPORTING
        addr1_complete_bapi = lwa_bapiaddr1
      IMPORTING
        addr1_complete      = lwa_address_complete.

    gs_fldoc_infocus-x-flhdr-adrnr = lwa_address_complete-addrnumber.
    READ TABLE lwa_address_complete-addr1_tab INTO addr1_wa INDEX 1.
    READ TABLE lwa_address_complete-adtel_tab INTO adtel_wa INDEX 1.
    READ TABLE lwa_address_complete-adfax_tab INTO adfax_wa  INDEX 1.
    MOVE-CORRESPONDING addr1_wa-data TO gs_fldoc_infocus-x-adrc.
    MOVE-CORRESPONDING adtel_wa-adtel TO gs_fldoc_infocus-x-adrc.
    MOVE-CORRESPONDING adfax_wa-adfax TO gs_fldoc_infocus-x-adrc.

    PERFORM address_handle_prepare.
    PERFORM address_maintain TABLES lt_errors
                              USING gv_address_handle
                                    c_true
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
    gs_fldoc_infocus-x-adrc-updkz = cs_glfl_doc-x-adrc-updkz.
    IF lv_subrc NE 0.
      messages_all_display.
      RAISE change_failed.
    ELSE.
      gs_variables-addr_data_changed = c_true.
    ENDIF.
  ENDIF.
****** Owner Update/Create
  LOOP AT it_flown INTO lwa_flown WHERE updkz IS NOT INITIAL
                                    AND updkz NE c_updkz_delete.
    lwa_flown-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
    PERFORM owner_data_check USING lwa_flown
                          CHANGING lv_subrc.
    IF lv_subrc NE 0.
      messages_all_display.
      RAISE change_failed.
    ENDIF.
    READ TABLE gs_fldoc_infocus-x-flown ASSIGNING <lwa_flown>
                               WITH KEY tplnr_fl = lwa_flown-tplnr_fl
                                        owner    = lwa_flown-owner.
    IF sy-subrc EQ 0.
      IF lwa_flown NE <lwa_flown>.
        MOVE-CORRESPONDING lwa_flown TO <lwa_flown>.
        <lwa_flown>-updkz = lwa_flown-updkz."c_updkz_update.
      ENDIF.
    ELSE.
      lwa_flown-updkz = c_updkz_new.
      APPEND lwa_flown TO gs_fldoc_infocus-x-flown.
    ENDIF.
  ENDLOOP.
*********  Delete
  LOOP AT it_flown INTO lwa_flown
         WHERE updkz EQ c_updkz_delete.
    READ TABLE gs_fldoc_infocus-x-flown TRANSPORTING NO FIELDS
               WITH KEY tplnr_fl = lwa_flown-tplnr_fl
                        owner    = lwa_flown-owner.

    IF sy-subrc EQ 0.
      DELETE gs_fldoc_infocus-x-flown INDEX sy-tabix.
      READ TABLE gs_fldoc_infocus-y-flown ASSIGNING <lwa_flown>
                 WITH KEY tplnr_fl = lwa_flown-tplnr_fl
                          owner    = lwa_flown-owner.
      IF sy-subrc EQ 0.
        <lwa_flown>-updkz = c_updkz_delete.
      ELSE.
        APPEND lwa_flown TO gs_fldoc_infocus-y-flown.
      ENDIF.
    ENDIF.
  ENDLOOP.
************************  Crop Season
*********************  Update/Change

  LOOP AT it_flcma INTO lwa_flcma WHERE updkz IS NOT INITIAL
                                  AND updkz NE c_updkz_delete.
    lwa_flown-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
    IF lv_subrc NE 0.
      messages_all_display.
      RAISE change_failed.
    ENDIF.
    READ TABLE gs_fldoc_infocus-x-flcma ASSIGNING <lwa_flcma>
                               WITH KEY tplnr_fl = lwa_flcma-tplnr_fl
                                        contr    = lwa_flcma-contr.
    IF sy-subrc EQ 0.
      IF lwa_flcma NE <lwa_flcma>.
        MOVE-CORRESPONDING lwa_flcma TO <lwa_flcma>.
        <lwa_flcma>-mandt = sy-mandt.
        <lwa_flcma>-updkz = lwa_flcma-updkz."c_updkz_update.
      ENDIF.
    ELSE.
      lwa_flcma-updkz = c_updkz_new.
      APPEND lwa_flcma TO gs_fldoc_infocus-x-flcma.
    ENDIF.
  ENDLOOP.
*********  Delete
  LOOP AT it_flcma INTO lwa_flcma
         WHERE updkz EQ c_updkz_delete.
    READ TABLE gs_fldoc_infocus-x-flcma TRANSPORTING NO FIELDS
               WITH KEY tplnr_fl = lwa_flcma-tplnr_fl
                        contr    = lwa_flcma-contr.

    IF sy-subrc EQ 0.
      DELETE gs_fldoc_infocus-x-flcma INDEX sy-tabix.
      READ TABLE gs_fldoc_infocus-y-flcma ASSIGNING <lwa_flcma>
                 WITH KEY tplnr_fl = lwa_flcma-tplnr_fl
                          contr    = lwa_flcma-contr.
      IF sy-subrc EQ 0.
        <lwa_flcma>-updkz = c_updkz_delete.
      ELSE.
        APPEND lwa_flcma TO gs_fldoc_infocus-y-flcma.
      ENDIF.
    ENDIF.
  ENDLOOP.

****************Attribute Group
  IF it_flatg IS NOT INITIAL.
    LOOP AT it_flatg INTO lwa_flatg WHERE updkz IS NOT INITIAL
                                      AND updkz NE c_updkz_delete.
      READ TABLE gs_fldoc_infocus-x-flatg ASSIGNING <lwa_flatg>
                 WITH KEY tplnr_fl = lwa_flatg-tplnr_fl
                          class    = lwa_flatg-class.
      IF sy-subrc NE 0.
        lwa_flatg-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
        APPEND lwa_flatg TO gs_fldoc_infocus-x-flatg.
        PERFORM class_attributes_read USING c_agtyp-functional_location
                                            lwa_flatg-clint
                                            lwa_flatg-class
                                   CHANGING gt_athdr.
      ELSEIF lwa_flatg-updkz NE c_updkz_delete.
        <lwa_flatg>-updkz = lwa_flatg-updkz.
      ENDIF.
    ENDLOOP.
*-Delete
    LOOP AT it_flatg INTO lwa_flatg WHERE updkz EQ c_updkz_delete.
      READ TABLE gs_fldoc_infocus-x-flatg TRANSPORTING NO FIELDS
                 WITH KEY tplnr_fl = lwa_flatg-tplnr_fl
                          class    = lwa_flatg-class.
      IF sy-subrc EQ 0.
        DELETE gs_fldoc_infocus-x-flatg INDEX sy-tabix.
        READ TABLE gs_fldoc_infocus-y-flatg ASSIGNING <lwa_flatg>
                   WITH KEY tplnr_fl = lwa_flatg-tplnr_fl
                            class    = lwa_flatg-class.
        IF sy-subrc EQ 0.
          <lwa_flatg>-updkz = c_updkz_delete.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*****************Partner
*  LOOP AT cs_glfl_doc-x-ihpa INTO ls_ihpa WHERE updkz IS NOT INITIAL
*                                         AND updkz NE c_updkz_update.
*
*    IF ls_ihpa-updkz EQ c_updkz_delete.
**      CLEAR <lwa_ihpa>.
*      READ TABLE gs_fldoc_infocus-x-ihpa ASSIGNING <lwa_ihpa>
*                                         WITH KEY posnr = ls_ihpa-counter.
*
*      IF ls_ihpa NE lwa_ihpa.
*        MOVE-CORRESPONDING ls_ihpa TO ls_tab.
**READ TABLE lt_tab INTO ls_tab INDEX 1.
*        ls_tab-kzloesch = 'X'.
**        ls_tab-aedat = sy-uzeit.
**        ls_tab-aenam = sy-uname.
*        CLEAR lt_tab.
*        APPEND ls_tab TO lt_tab.
**        CALL FUNCTION 'PM_PARTNER_UPDATE'
***       EXPORTING
***         IT_IHPA_TIME       =
**          TABLES
**            fxihpa             = lt_tab
**                  .
**        IF sy-subrc EQ 0.
**          COMMIT WORK.
**        ENDIF.
*
*        CALL FUNCTION '/AGRI/G_PM_PARTNER_UPDATE'
**        exporting
**          it_ihpa_time       =
*         TABLES
*           t_fxihpa           = lt_tab.
*        IF sy-subrc EQ 0.
*          COMMIT WORK.
*        ENDIF.
*
*        READ TABLE lt_tab INTO ls_tab INDEX 1.
**        <lwa_ihpa> = ls_tab.
*        <lwa_ihpa>-mandt = sy-mandt.
*        <lwa_ihpa>-kzloesch = 'X'.
*        <lwa_ihpa>-updkz = c_updkz_update.
*      ENDIF.
*    ENDIF.
*    CLEAR :ls_tab.
*  ENDLOOP.
*******************************  try
*  LOOP AT cs_glfl_doc-x-ihpa INTO ls_ihpa WHERE updkz IS NOT INITIAL
*                                          AND updkz NE c_updkz_delete.
*
*    IF ls_ihpa-updkz EQ c_updkz_update.
*      READ TABLE gs_fldoc_infocus-x-ihpa ASSIGNING <lwa_ihpa>
*                                         WITH KEY counter = ls_ihpa-counter.
*
*      IF ls_ihpa NE <lwa_ihpa>.
*        MOVE-CORRESPONDING ls_ihpa TO ls_tab.
*        CLEAR lt_tab.
*        APPEND ls_tab TO lt_tab.
**        CALL FUNCTION 'PM_PARTNER_UPDATE'
***       EXPORTING
***         IT_IHPA_TIME       =
**          TABLES
**            fxihpa             = lt_tab
**                  .
**        IF sy-subrc EQ 0.
**          COMMIT WORK.
**        ENDIF.
*
*        CALL FUNCTION '/AGRI/G_PM_PARTNER_UPDATE'
**        exporting
**          it_ihpa_time       =
*         TABLES
*           t_fxihpa           = lt_tab.
*        IF sy-subrc EQ 0.
*          COMMIT WORK.
*        ENDIF.
*
*        READ TABLE lt_tab INTO ls_tab INDEX 1.
*        <lwa_ihpa> = ls_tab.
*        <lwa_ihpa>-updkz = c_updkz_update.
*      ENDIF.
*    ENDIF.
*    CLEAR :ls_tab.
*  ENDLOOP.
*********************************************  ,,end try
***************Update
*  CLEAR : lv_tabix,lwa_ihpa.
*  LOOP AT cs_glfl_doc-x-ihpa INTO ls_ihpa WHERE updkz IS NOT INITIAL
*                                           AND updkz EQ c_updkz_new  .
*
***********    IF ls_ihpa-updkz EQ c_updkz_update.
***********      READ TABLE gs_fldoc_infocus-x-ihpa INTO lwa_ihpa
***********                                         WITH KEY counter = ls_ihpa-counter.
***********      IF sy-subrc EQ 0.
***********        CALL FUNCTION 'PM_PARTNER_GET'
***********          EXPORTING
***********            objnr    = gs_fldoc_infocus-x-flhdr-objnr
***********          TABLES
***********            ihpa_tab = lt_tab.
***********        LOOP AT lt_tab INTO ls_tab WHERE parnr = lwa_ihpa-parnr
***********                                     AND parvw = lwa_ihpa-parvw.
***********          lv_counter = sy-tabix.
***********          EXIT.
***********        ENDLOOP.
***********      ELSE.
***********        CONTINUE.
***********      ENDIF.
***********    ENDIF.
*
*    CALL FUNCTION 'PM_PARTNER_MAINTAIN'
*      EXPORTING
*        objnr               = gs_fldoc_infocus-x-flhdr-objnr
*        parnr               = ls_ihpa-parnr
*        parvw               = ls_ihpa-parvw
*        adrnr               = ls_ihpa-adrnr
**       ADR_HANDLE          =
*        tabix               = lv_counter
**       tabix_objnr         = lv_counter
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
*      RAISE change_failed.
*    ENDIF.
*    CLEAR: lv_counter,lv_tabix.
*
*  ENDLOOP.
*
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
**********  IF sy-subrc <> 0 OR
**********     gs_variables-errors IS NOT INITIAL.
**********    MESSAGE e025(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
**********                             INTO sy-msgli.
**********    message_simple space.
**********    RAISE change_failed.
**********  ENDIF.
***  CLEAR : lv_tabix,lwa_ihpa.
***  LOOP AT cs_glfl_doc-x-ihpa INTO ls_ihpa WHERE updkz IS NOT INITIAL.
***
***    IF ls_ihpa-updkz EQ c_updkz_update.
***      READ TABLE gs_fldoc_infocus-x-ihpa INTO lwa_ihpa
***                                         WITH KEY counter = ls_ihpa-counter.
***      IF sy-subrc EQ 0.
***        CALL FUNCTION 'PM_PARTNER_GET'
***          EXPORTING
***            objnr    = gs_fldoc_infocus-x-flhdr-objnr
***          TABLES
***            ihpa_tab = lt_tab.
***        LOOP AT lt_tab INTO ls_tab WHERE parnr = lwa_ihpa-parnr
***                                     AND parvw = lwa_ihpa-parvw.
***          lv_counter = sy-tabix.
***          EXIT.
***        ENDLOOP.
***      ELSE.
***        CONTINUE.
***      ENDIF.
***    ENDIF.
****ENDLOOP.
***
***    CALL FUNCTION 'PM_PARTNER_MAINTAIN'
***      EXPORTING
***        objnr               = gs_fldoc_infocus-x-flhdr-objnr
***        parnr               = ls_ihpa-parnr
***        parvw               = ls_ihpa-parvw
***        adrnr               = ls_ihpa-adrnr
****       ADR_HANDLE          =
***        tabix               = lv_counter
****       tabix_objnr         = lv_counter
****       MSGTY               = 'E'
***        cpd_pop_up          = c_false
****       CHECK_DEBITOR       = 'X'
****     IMPORTING
****       COUNTER             =
***      EXCEPTIONS
***        invalid_parnr       = 1
***        cpd_customer        = 2
***        invalid_parvw       = 3
***        parvw_unique        = 4
***        OTHERS              = 5.
***    IF sy-subrc <> 0.
***      MESSAGE e025(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
***                         INTO sy-msgli.
***      message_simple space.
***      RAISE change_failed.
***    ENDIF.
***    CLEAR: lv_counter,lv_tabix.
***  ENDLOOP.
***  CALL FUNCTION 'PM_PARTNER_COMPLETE'
***    EXPORTING
****     EQUNR              = ' '
****     EXIT               = ' '
***      objnr              = gs_fldoc_infocus-x-flhdr-objnr
***      obtyp              = 'IFL'
***      pargr              = gs_fldoc_infocus-x-flhdr-pargr
****     SAVE               = ' '
****     TPLNR              = ' '
****     SUBSCREEN_EXT      = 0000
****     SUBSCREEN_PROG_EXT = '        '
***      handling           = 'E'
****     IV_TRANS_TEXT      =
***    IMPORTING
***      partner_missing    = gs_variables-errors
***    EXCEPTIONS
***      cancel             = 1
***      exit               = 2
***      save               = 3
***      OTHERS             = 4.
***  IF sy-subrc <> 0 OR
***     gs_variables-errors IS NOT INITIAL.
***    MESSAGE e025(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
***                             INTO sy-msgli.
***    message_simple space.
***    RAISE change_failed.
***  ENDIF.
*  ENDIF.
*  ENDLOOP.
******************  Delete

*  CLEAR : lv_tabix,lwa_ihpa.
*******  LOOP AT cs_glfl_doc-x-ihpa INTO ls_ihpa WHERE updkz IS NOT INITIAL.
*******
*******    IF ls_ihpa-updkz EQ c_updkz_delete.
********      CLEAR <lwa_ihpa>.
*******      READ TABLE gs_fldoc_infocus-x-ihpa ASSIGNING <lwa_ihpa>
*******                                         WITH KEY counter = ls_ihpa-counter.
*******
*******      IF ls_ihpa NE lwa_ihpa.
*******        MOVE-CORRESPONDING ls_ihpa TO ls_tab.
********READ TABLE lt_tab INTO ls_tab INDEX 1.
*******        ls_tab-kzloesch = 'X'.
********        ls_tab-aedat = sy-uzeit.
********        ls_tab-aenam = sy-uname.
*******        CLEAR lt_tab.
*******        APPEND ls_tab TO lt_tab.
*******        CALL FUNCTION 'PM_PARTNER_UPDATE'
********       EXPORTING
********         IT_IHPA_TIME       =
*******          TABLES
*******            fxihpa             = lt_tab
*******                  .
*******        IF sy-subrc EQ 0.
*******          COMMIT WORK.
*******        ENDIF.
*******        READ TABLE lt_tab INTO ls_tab INDEX 1.
********        <lwa_ihpa> = ls_tab.
*******        <lwa_ihpa>-mandt = sy-mandt.
*******        <lwa_ihpa>-kzloesch = 'X'.
*******        <lwa_ihpa>-updkz = c_updkz_update.
*******      ENDIF.
*******    ENDIF.
*******    CLEAR :ls_tab.
*******  ENDLOOP.
**********
*       IF lwa_flppl NE <lwa_flppl>.
*        <lwa_flppl> = lwa_flppl.
*        <lwa_flppl>-updkz = c_updkz_update.
*      IF sy-subrc EQ 0.
*        CALL FUNCTION 'PM_PARTNER_GET'
*          EXPORTING
*            objnr    = gs_fldoc_infocus-x-flhdr-objnr
*          TABLES
*            ihpa_tab = lt_tab.
*
*      ENDIF.
*      IF lt_tab IS NOT INITIAL.
*        READ TABLE lt_tab INTO ls_tab INDEX 1.
*        ls_tab-kzloesch = 'X'.
*        CLEAR :lt_tab.
*        APPEND ls_tab TO lt_tab.
*        CALL FUNCTION 'PM_PARTNER_UPDATE'
**         EXPORTING
**           IT_IHPA_TIME       =
*          TABLES
*            fxihpa             = lt_tab
*                  .
*        IF sy-subrc EQ 0.
*          COMMIT WORK.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

****Deleted enteris
  LOOP AT cs_glfl_doc-y-ihpa INTO ls_xihpa
                                    WHERE updkz EQ c_updkz_delete.
    IF ls_xihpa-parvw EQ 'AA' OR ls_xihpa-parvw EQ 'AW'.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                       parvw = ls_xihpa-parvw kunnr = ls_xihpa-kunnr.
    ELSE.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                               parvw = ls_xihpa-parvw.
    ENDIF.
    IF sy-subrc EQ 0.
****delete X entry
      DELETE gs_fldoc_infocus-x-ihpa INDEX sy-tabix.
****Update y
      IF ls_xihpa-parvw EQ 'AA' OR ls_xihpa-parvw EQ 'AW'.
        READ TABLE gs_fldoc_infocus-y-ihpa INTO ls_ihpa WITH KEY
                        parvw = ls_xihpa-parvw kunnr = ls_xihpa-kunnr.
      ELSE.
        READ TABLE gs_fldoc_infocus-y-ihpa INTO ls_ihpa WITH KEY
                                 parvw = ls_xihpa-parvw.
      ENDIF.
      ls_ihpa-updkz = c_updkz_delete.
      MODIFY gs_fldoc_infocus-y-ihpa FROM ls_ihpa INDEX sy-tabix
                                              TRANSPORTING updkz.
    ENDIF.
  ENDLOOP.

LOOP AT cs_glfl_doc-x-ihpa INTO ls_xihpa.     "Partner's Update
    IF gs_fldoc_infocus-x-flhdr-pargr EQ space.
      EXIT.
    ENDIF.
    CLEAR: ls_ihpa, lv_ktonr.

    IF ls_xihpa-parvw EQ 'AA' OR ls_xihpa-parvw EQ 'AW'.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                       parvw = ls_xihpa-parvw kunnr = ls_xihpa-kunnr.
    ELSE.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                               parvw = ls_xihpa-parvw.
    ENDIF.
    IF sy-subrc EQ 0.
      IF ls_xihpa-parvw EQ ls_ihpa-parvw AND
         ls_xihpa-kunnr EQ ls_ihpa-kunnr AND
         ls_xihpa-lifnr EQ ls_ihpa-lifnr AND
         ls_xihpa-pernr EQ ls_ihpa-pernr AND
         ls_xihpa-parnr EQ ls_ihpa-parnr AND
***E SP2 Business Partner.
          ls_xihpa-ablad EQ ls_ihpa-ablad.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM tpar INTO lwa_tpar WHERE parvw EQ "#EC CI_SEL_NESTED
                                             ls_xihpa-parvw.
    CASE lwa_tpar-nrart.
      WHEN 'KU'.
        IF NOT ls_ihpa-kunnr IS INITIAL.
          lv_ktonr = ls_ihpa-kunnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-kunnr.
      WHEN 'LI'.
        IF NOT ls_ihpa-lifnr IS INITIAL.
          lv_ktonr = ls_ihpa-lifnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-lifnr.
      WHEN 'PE'.
        IF NOT ls_ihpa-pernr IS INITIAL.
          lv_ktonr = ls_ihpa-pernr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-pernr.
      WHEN 'AP'.
        IF NOT ls_ihpa-parnr IS INITIAL.
          lv_ktonr = ls_ihpa-parnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-parnr.
***E SP2 Business Partner
      WHEN 'XP'.
        IF NOT ls_ihpa-ablad IS INITIAL.
          lv_ktonr = ls_ihpa-ablad.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-ablad.
    ENDCASE.
** Commented due to OData issue's while deleting Partner
*    PERFORM partners_maintain TABLES gs_fldoc_infocus-x-ihpa
*                                     gs_fldoc_infocus-y-ihpa
*                               USING ls_xihpa-parvw
*                                     ls_xihpa-ktonr
*                                     lv_ktonr
**                                     cs_agreement-x-kona-vkorg
**                                     cs_agreement-x-kona-vtweg
**                                     cs_agreement-x-kona-spart
*                                     ls_xihpa-adrnr
*                                     ls_xihpa-adrda
*                                     c_true
*                            CHANGING lv_subrc.
*
*    IF lv_subrc NE 0.
*      messages_all_display.
*      RAISE change_failed.
*    ENDIF.
    APPEND ls_xihpa TO gs_fldoc_infocus-x-ihpa.
  ENDLOOP.

  CALL FUNCTION 'PM_PARTNER_COMPLETE'
    EXPORTING
*     EQUNR           = ' '
*     EXIT            = ' '
      objnr           = gs_fldoc_infocus-x-flhdr-objnr
      obtyp           = 'IFL'
      pargr           = gs_fldoc_infocus-x-flhdr-pargr
*     SAVE            = ' '
*     TPLNR           = ' '
*     SUBSCREEN_EXT   = 0000
*     SUBSCREEN_PROG_EXT = '        '
      handling        = 'E'
*     IV_TRANS_TEXT   =
    IMPORTING
      partner_missing = gs_variables-errors
    EXCEPTIONS
      cancel          = 1
      exit            = 2
      save            = 3
      OTHERS          = 4.

  LOOP AT cs_glfl_doc-x-ihpa INTO ls_xihpa.     "Partner's Update
    IF gs_fldoc_infocus-x-flhdr-pargr EQ space.
      EXIT.
    ENDIF.
    CLEAR: ls_ihpa, lv_ktonr.

    IF ls_xihpa-parvw EQ 'AA' OR ls_xihpa-parvw EQ 'AW'.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                       parvw = ls_xihpa-parvw kunnr = ls_xihpa-kunnr.
    ELSE.
      READ TABLE gs_fldoc_infocus-x-ihpa INTO ls_ihpa WITH KEY
                               parvw = ls_xihpa-parvw.
    ENDIF.
    IF sy-subrc EQ 0.
      IF ls_xihpa-parvw EQ ls_ihpa-parvw AND
         ls_xihpa-kunnr EQ ls_ihpa-kunnr AND
         ls_xihpa-lifnr EQ ls_ihpa-lifnr AND
         ls_xihpa-pernr EQ ls_ihpa-pernr AND
         ls_xihpa-parnr EQ ls_ihpa-parnr AND
***E SP2 Business Partner.
          ls_xihpa-ablad EQ ls_ihpa-ablad.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM tpar INTO lwa_tpar WHERE parvw EQ "#EC CI_SEL_NESTED
                                             ls_xihpa-parvw.
    CASE lwa_tpar-nrart.
      WHEN 'KU'.
        IF NOT ls_ihpa-kunnr IS INITIAL.
          lv_ktonr = ls_ihpa-kunnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-kunnr.
      WHEN 'LI'.
        IF NOT ls_ihpa-lifnr IS INITIAL.
          lv_ktonr = ls_ihpa-lifnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-lifnr.
      WHEN 'PE'.
        IF NOT ls_ihpa-pernr IS INITIAL.
          lv_ktonr = ls_ihpa-pernr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-pernr.
      WHEN 'AP'.
        IF NOT ls_ihpa-parnr IS INITIAL.
          lv_ktonr = ls_ihpa-parnr.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-parnr.
***E SP2 Business Partner
      WHEN 'XP'.
        IF NOT ls_ihpa-ablad IS INITIAL.
          lv_ktonr = ls_ihpa-ablad.
        ENDIF.
        ls_xihpa-ktonr = ls_xihpa-ablad.
    ENDCASE.
** Commented due to OData issue's while deleting Partner
*    PERFORM partners_maintain TABLES gs_fldoc_infocus-x-ihpa
*                                     gs_fldoc_infocus-y-ihpa
*                               USING ls_xihpa-parvw
*                                     ls_xihpa-ktonr
*                                     lv_ktonr
**                                     cs_agreement-x-kona-vkorg
**                                     cs_agreement-x-kona-vtweg
**                                     cs_agreement-x-kona-spart
*                                     ls_xihpa-adrnr
*                                     ls_xihpa-adrda
*                                     c_true
*                            CHANGING lv_subrc.
*
*    IF lv_subrc NE 0.
*      messages_all_display.
*      RAISE change_failed.
*    ENDIF.
    APPEND ls_xihpa TO gs_fldoc_infocus-x-ihpa.
  ENDLOOP.

  LOOP AT gs_fldoc_infocus-x-ihpa ASSIGNING <lwa_ihpa>.
    READ TABLE cs_glfl_doc-x-ihpa INTO ls_xihpa INDEX 1.
    <lwa_ihpa>-ptrno = ls_xihpa-ptrno.
  ENDLOOP.

  IF it_flatv IS NOT INITIAL.
    LOOP AT it_flatv INTO lwa_flatv WHERE updkz IS NOT INITIAL
                                      AND updkz NE c_updkz_delete.
      READ TABLE gs_fldoc_infocus-x-flatv INTO lwa_flatv_old
                                 WITH KEY clint = lwa_flatv-clint
                                          atinn = lwa_flatv-atinn.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        lwa_flatv_old-updkz = lwa_flatv-updkz.
*        lwa_flatv = lwa_flatv_old.
      ENDIF.

      PERFORM attribute_update USING lwa_flatv_old
                            CHANGING lwa_flatv
                                     lv_subrc.
      IF lv_subrc NE 0.
        CONTINUE.
      ELSE.
        IF lv_tabix IS NOT INITIAL.
          READ TABLE gs_fldoc_infocus-x-flatv ASSIGNING <lwa_flatv>
                                INDEX lv_tabix.
          MOVE-CORRESPONDING lwa_flatv TO <lwa_flatv>.
        ELSE.
          lwa_flatv-updkz    = c_updkz_new.
          lwa_flatv-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
          APPEND lwa_flatv TO gs_fldoc_infocus-x-flatv.
        ENDIF.
      ENDIF.
    ENDLOOP.

*-Delete
    LOOP AT it_flatv INTO lwa_flatv WHERE updkz EQ c_updkz_delete.
      READ TABLE gs_fldoc_infocus-x-flatv TRANSPORTING NO FIELDS
                                 WITH KEY clint = lwa_flatv-clint
                                          atinn = lwa_flatv-atinn.
      IF sy-subrc EQ 0.
        DELETE gs_fldoc_infocus-x-flatv INDEX sy-tabix.
        READ TABLE gs_fldoc_infocus-y-flatv ASSIGNING <lwa_flatv>
                   WITH KEY clint = lwa_flatv-clint
                            atinn = lwa_flatv-atinn.
        IF sy-subrc EQ 0.
          <lwa_flatv>-updkz = c_updkz_delete.
        ELSE.
          APPEND lwa_flatv TO gs_fldoc_infocus-y-flatv.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

***Plants
*-Delete
  LOOP AT cs_glfl_doc-x-flppl INTO lwa_flppl
           WHERE updkz EQ c_updkz_delete.
    READ TABLE gs_fldoc_infocus-x-flppl TRANSPORTING NO FIELDS
               WITH KEY pwerk = lwa_flppl-pwerk.
    IF sy-subrc EQ 0.
      DELETE gs_fldoc_infocus-x-flppl INDEX sy-tabix.
      READ TABLE gs_fldoc_infocus-y-flppl ASSIGNING <lwa_flppl>
                 WITH KEY pwerk = lwa_flppl-pwerk.
      IF sy-subrc EQ 0.
        <lwa_flppl>-updkz = c_updkz_delete.
      ELSE.
        APPEND lwa_flppl TO gs_fldoc_infocus-y-flppl.
      ENDIF.
    ENDIF.
  ENDLOOP.
*-Insert/Update
  LOOP AT cs_glfl_doc-x-flppl INTO lwa_flppl
                                 WHERE updkz IS NOT INITIAL
                                   AND updkz NE c_updkz_delete.

    PERFORM plant_data_check USING lwa_flppl
                                   lv_subrc.
    IF lv_subrc NE 0.
      messages_all_display.
      RAISE change_failed.
    ENDIF.

    READ TABLE gs_fldoc_infocus-x-flppl ASSIGNING <lwa_flppl>
                                         WITH KEY pwerk = lwa_flppl-pwerk
                                                  route = lwa_flppl-route.
    IF sy-subrc EQ 0.
*      <lwa_flppl>-updkz = c_updkz_update.
      IF lwa_flppl NE <lwa_flppl>.
        <lwa_flppl> = lwa_flppl.
        <lwa_flppl>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      lwa_flppl-updkz = c_updkz_new.
      APPEND lwa_flppl TO gs_fldoc_infocus-x-flppl.
    ENDIF.
  ENDLOOP.
  CLEAR lv_subrc.
  PERFORM funloc_infocus_save CHANGING lv_subrc.
  IF lv_subrc NE 0 AND lv_subrc NE 1.
    messages_all_display.
    RAISE change_failed.
  ENDIF.
  cs_glfl_doc = gs_fldoc_infocus.
  messages_all_display.

ENDFUNCTION.                                             "#EC CI_VALPAR
