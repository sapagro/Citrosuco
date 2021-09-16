*&---------------------------------------------------------------------*
*& Report ZFMFP_EVALUATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfp_evaluate.

TABLES: qals,
        /agri/glcmhdr,
        /agri/glflcma,
        /agri/glflot,
        qasr.

*--Includes
INCLUDE: /agri/global_constants,
         /agri/global_macros,
         /agri/gprolog_macros.

CLASS lcl_event_handler DEFINITION DEFERRED.

*--Constants
CONSTANTS: c_program_rq_list     TYPE sy-repid
                                 VALUE '/AGRI/FMQS_EVALUATE',
           c_custom_control_name TYPE scrfname
                                 VALUE '/AGRI/QLTY_SAMPLE_2000_CC'.

CONSTANTS: BEGIN OF c_structure_name,
             wo_fl TYPE dd02l-tabname
*-- BOC-T_T.KONNO
*                              VALUE '/AGRI/S_FMQS_EVAL_FCAT',
                              VALUE 'ZSC_FMQS_EVAL_FCAT',
*-- EOC-T_T.KONNO
           END OF c_structure_name.

TYPES: BEGIN OF ty_insc,
         prueflos   TYPE qplos,
         vorglfnr   TYPE qlfnkn,
         probenr    TYPE qprobenrpp,
         merknr     TYPE qmerknrp,
         verwmerkm  TYPE qmerknr,
         kurztext   TYPE qmkkurztxt,
         mittelwert TYPE qmittelwrt,
         katalgart1 TYPE qkatausw,
         gruppe1    TYPE qcodegrp,
         code1      TYPE qcode,
         version1   TYPE qversnr,
         mbewertg   TYPE qmbewertg,
         userc1     TYPE qusrchar18,
       END OF ty_insc.

TYPES: BEGIN OF gs_prnum,
         prnum TYPE /agri/fmprnum,
       END OF gs_prnum.

DATA: BEGIN OF gs_variables,
        refresh_grid,
      END OF gs_variables.

DATA: gt_insc            TYPE TABLE OF ty_insc,
      gt_fieldcat_fl     TYPE lvc_t_fcat,
*-- BOC-T_T.KONNO-01/29/20
*      gt_practqlt        TYPE TABLE OF /agri/s_fmpractqlt_fcat,
      gt_practqlt        TYPE TABLE OF zabs_str_fmpractqlt_fcat,
*-- EOC-T_T.KONNO-01/29/20
      gt_qpct            TYPE TABLE OF qpct,
      gv_ok101           TYPE syucomm,
      fcode              TYPE syucomm,
      ref_container_list TYPE REF TO cl_gui_custom_container,
      ref_grid_list      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_event_handler  TYPE REF TO lcl_event_handler,
      ref_text           TYPE REF TO cl_dd_document.

FIELD-SYMBOLS: <gt_dyntable> TYPE STANDARD TABLE,
               <gs_dyntable>.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_user_command.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*--Basic selections
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_werk  FOR qals-werk               NO INTERVALS NO-EXTENSION,
                so_art   FOR qals-art                NO INTERVALS NO-EXTENSION,
                so_matnr FOR qals-matnr,
                so_fldty FOR /agri/glcmhdr-fldty     NO INTERVALS NO-EXTENSION,
                so_cmnum FOR /agri/glcmhdr-cmnum     NO INTERVALS NO-EXTENSION,
*                so_tplnr FOR /agri/glflcma-tplnr_fl,
                so_tplnr FOR /agri/glflot-strno,
                so_charg FOR qals-charg,
                so_inslt FOR qasr-prueflos,
                so_sdate FOR qals-enstehdat.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  DATA: lv_subrc TYPE sy-subrc.
  config_tcode_authority_check '/AGRI/FMPRQS' '03' lv_subrc.
  IF lv_subrc <> 0.
    MESSAGE ID '/AGRI/FMPR' TYPE 'E' NUMBER '193' INTO sy-msgli.
    message_simple space.
  ENDIF.

START-OF-SELECTION.

  PERFORM data_actqlt_get.

END-OF-SELECTION.

  PERFORM data_display.

*&---------------------------------------------------------------------*
*&      Form  DATA_ACTQLT_GET
*&---------------------------------------------------------------------*
FORM data_actqlt_get.

  TYPES: BEGIN OF ls_makt,
           matnr TYPE matnr,
           maktx TYPE maktx,
         END OF ls_makt.

  DATA: lt_qals    TYPE TABLE OF qals,
        lt_qasr    TYPE TABLE OF qasr,
        lt_qapp    TYPE TABLE OF qapp,
        lt_qamv    TYPE TABLE OF qamv,
        lt_afpo    TYPE TABLE OF afpo,
*        lt_prnum   TYPE TABLE OF gs_prnum,
        lt_prnum   TYPE RANGE OF /agri/fmprnum,
        lt_makt    TYPE TABLE OF ls_makt,
        lt_pritm   TYPE /agri/t_fmpritm,
        lt_flcma   TYPE /agri/t_glflcma,
        lt_cmhdr   TYPE /agri/t_glcmhdr,
*-- BOC-T_T.KONNO-01/29/20
*        lwa_actqlt TYPE /agri/s_fmpractqlt_fcat,
        lwa_actqlt TYPE zabs_str_fmpractqlt_fcat,
*-- EOC-T_T.KONNO-01/29/20
        lwa_qals   TYPE qals,
        lwa_qapp   TYPE qapp,
        lwa_qamv   TYPE qamv,
        lwa_afpo   TYPE afpo,
        lwa_pritm  TYPE /agri/fmpritm,
        lwa_flcma  TYPE /agri/glflcma,
        lwa_cmhdr  TYPE /agri/glcmhdr,
        lwa_insc   TYPE ty_insc,
        lwa_makt   TYPE ls_makt,
        lv_userc1  TYPE qusrchar18.

  FIELD-SYMBOLS: <ls_qasr>  TYPE qasr.

  SELECT * FROM qals
    INTO TABLE lt_qals
    WHERE werk      IN so_werk
      AND art       IN so_art
      AND matnr     IN so_matnr
      AND charg     IN so_charg
      AND prueflos  IN so_inslt
      AND enstehdat IN so_sdate.
  SORT lt_qals BY prueflos.

  IF lt_qals IS NOT INITIAL.
    SELECT matnr, werks, plnty,
           plnnr, plnal, zaehl
      FROM mapl
      INTO TABLE @DATA(lt_mapl)
      FOR ALL ENTRIES IN @lt_qals
     WHERE matnr EQ @lt_qals-selmatnr
       AND werks EQ @lt_qals-werk
       AND plnty EQ 'N' "c_task_list_type-inspection_plan "'Q'
       AND loekz EQ @space. "deletion indicator.

    IF sy-subrc EQ 0.
      SORT lt_mapl BY matnr.

      SELECT plnty, plnnr, plnal, zaehl,
             loekz, verwe, werks
        FROM plko
        INTO TABLE @DATA(lt_plko)
        FOR ALL ENTRIES IN @lt_mapl
       WHERE plnty EQ @lt_mapl-plnty
         AND plnnr EQ @lt_mapl-plnnr
         AND plnal EQ @lt_mapl-plnal
         AND datuv LE @sy-datum
         AND verwe EQ '1'
         AND werks EQ @lt_mapl-werks
         AND statu EQ '4'. "Released Status for General

      DELETE lt_plko WHERE loekz = abap_true.

      IF lt_plko[] IS NOT INITIAL.
        DATA lr_steus TYPE RANGE OF steus.
        APPEND VALUE #( low = 'QM01' sign = 'I' option = 'EQ' )
          TO lr_steus.
        APPEND VALUE #( low = 'QM03' sign = 'I' option = 'EQ' )
          TO lr_steus.

*-- Task list - operation/activity
        SELECT plnty, plnnr, plnkn, zaehl,
               vornr, steus, werks
          FROM plpo
          INTO TABLE @DATA(lt_plpo)
          FOR ALL ENTRIES IN @lt_plko
         WHERE plnty EQ @lt_plko-plnty
           AND plnnr EQ @lt_plko-plnnr
           AND steus IN @lr_steus
           AND datuv LE @sy-datum
           AND loekz EQ @space
           AND werks EQ @lt_plko-werks.
      ENDIF.
    ENDIF.

    SELECT * FROM afpo
      INTO TABLE lt_afpo
      FOR ALL ENTRIES IN lt_qals
      WHERE aufnr EQ lt_qals-aufnr.

    SORT lt_afpo BY aufnr.

    SELECT matnr maktx
      FROM makt
      INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_qals
      WHERE matnr EQ lt_qals-matnr
        AND spras EQ sy-langu.
    SORT lt_makt BY matnr.

    IF lt_afpo IS NOT INITIAL.
      SELECT * FROM /agri/glflcma
         INTO TABLE lt_flcma
       FOR ALL ENTRIES IN lt_afpo
        WHERE proid    EQ lt_afpo-projn
          AND tplnr_fl IN so_tplnr
          AND cmnum    IN so_cmnum.
      IF sy-subrc = 0.
        SORT lt_flcma BY proid tplnr_fl cmnum.
      ENDIF.
    ENDIF.

    SELECT * FROM qasr
      INTO TABLE lt_qasr
      FOR ALL ENTRIES IN lt_qals
      WHERE prueflos EQ lt_qals-prueflos.
    SORT lt_qasr BY prueflos merknr.

    SELECT * FROM qapp
       INTO TABLE lt_qapp
      FOR ALL ENTRIES IN lt_qals
      WHERE prueflos EQ lt_qals-prueflos.

    SORT lt_qapp BY prueflos probenr.
  ENDIF.

  IF lt_qasr IS NOT INITIAL.
    SELECT * FROM qamv
      INTO TABLE lt_qamv
      FOR ALL ENTRIES IN lt_qasr
     WHERE prueflos EQ lt_qasr-prueflos
       AND merknr   EQ lt_qasr-merknr.

    DATA(lt_qasr_aux) = lt_qasr[].
    SELECT * FROM qamv
      INTO TABLE @DATA(lt_qamv_aux)
      FOR ALL ENTRIES IN @lt_qasr
     WHERE prueflos EQ @lt_qasr-prueflos.

    SORT lt_qasr_aux BY prueflos vorglfnr merknr.
    LOOP AT lt_qamv_aux INTO DATA(ls_qamv_aux).
      DATA(lv_tabix) = sy-tabix.
      READ TABLE lt_qasr_aux INTO DATA(ls_qasr_aux)
        WITH KEY prueflos = ls_qamv_aux-prueflos
                 vorglfnr = ls_qamv_aux-vorglfnr
                 merknr   = ls_qamv_aux-merknr BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE lt_qamv_aux INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    SELECT * FROM qpct
      INTO TABLE gt_qpct
      FOR ALL ENTRIES IN lt_qasr
      WHERE katalogart EQ lt_qasr-katalgart1
        AND codegruppe EQ lt_qasr-gruppe1
        AND code       EQ lt_qasr-code1
        AND version    EQ lt_qasr-version1
        AND sprache    EQ sy-langu.          ""#EC CI_GENBUFF
    SORT gt_qpct BY katalogart codegruppe code version.
  ENDIF.

  SELECT * FROM /agri/glcmhdr                 "#EC CI_ALL_FIELDS_NEEDED
    INTO TABLE lt_cmhdr
    WHERE cmnum IN so_cmnum
      AND fldty IN so_fldty.
  SORT lt_cmhdr BY cmnum.

  IF lt_qapp IS NOT INITIAL.

    PERFORM ticket_convert TABLES   lt_qapp
                                    lt_prnum
                           CHANGING lv_userc1.
    SORT lt_prnum BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_prnum COMPARING low.
    IF lt_prnum IS NOT  INITIAL.
      SELECT * FROM /agri/fmpritm             "#EC CI_FAE_LINES_ENSURED
        INTO TABLE lt_pritm
       FOR ALL ENTRIES IN lt_cmhdr
        WHERE prnum IN lt_prnum
          AND cmnum EQ lt_cmhdr-cmnum.
    ENDIF.
  ENDIF.

  SORT lt_qamv BY prueflos vorglfnr merknr.
  LOOP AT lt_qasr ASSIGNING <ls_qasr>.
    READ TABLE lt_qals INTO lwa_qals
      WITH KEY prueflos = <ls_qasr>-prueflos BINARY SEARCH.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE lt_makt INTO lwa_makt
      WITH KEY matnr = lwa_qals-matnr BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR lwa_makt.
    ENDIF.

    READ TABLE lt_qamv INTO lwa_qamv
      WITH KEY prueflos = <ls_qasr>-prueflos
               vorglfnr = <ls_qasr>-vorglfnr
               merknr   = <ls_qasr>-merknr BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE lt_qapp INTO lwa_qapp                     "#EC CI_SORTED
      WITH KEY prueflos = <ls_qasr>-prueflos
               probenr  = <ls_qasr>-probenr BINARY SEARCH.

    IF lwa_qals-aufnr IS NOT INITIAL.
      READ TABLE lt_afpo INTO lwa_afpo
        WITH KEY aufnr = lwa_qals-aufnr BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      PERFORM ticket_convert TABLES   lt_qapp
                                      lt_prnum
                             CHANGING lwa_qapp-userc1.
    ELSE.
      PERFORM ticket_convert TABLES   lt_qapp
                                      lt_prnum
                             CHANGING lwa_qapp-userc1.

      READ TABLE lt_pritm INTO lwa_pritm
        WITH KEY prnum = lwa_qapp-userc1.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_cmhdr INTO lwa_cmhdr
        WITH KEY cmnum = lwa_pritm-cmnum BINARY SEARCH.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_flcma INTO lwa_flcma
        WITH KEY tplnr_fl = lwa_pritm-tplnr
                 cmnum    = lwa_pritm-cmnum.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING lwa_flcma TO lwa_actqlt.
    MOVE-CORRESPONDING lwa_pritm TO lwa_actqlt.
    MOVE-CORRESPONDING lwa_makt  TO lwa_actqlt.
    MOVE-CORRESPONDING lwa_qals  TO lwa_actqlt.
    MOVE-CORRESPONDING lwa_qamv  TO lwa_actqlt.
    MOVE-CORRESPONDING lwa_qamv  TO lwa_insc.
    MOVE-CORRESPONDING lwa_qapp  TO lwa_actqlt.
    MOVE-CORRESPONDING <ls_qasr> TO lwa_actqlt.
    MOVE-CORRESPONDING <ls_qasr> TO lwa_insc.
    MOVE-CORRESPONDING lwa_cmhdr TO lwa_actqlt.
    lwa_actqlt-pruefer    = lwa_qapp-pruefer.
    lwa_actqlt-entstehdat = lwa_qapp-entstehdat.
    lwa_actqlt-entstehzt  = lwa_qapp-entstehzt.
    lwa_insc-userc1       = lwa_qapp-userc1.
    APPEND lwa_actqlt TO gt_practqlt.
    APPEND lwa_insc   TO gt_insc.
    CLEAR: lwa_qals, lwa_qamv, lwa_qapp, lwa_flcma,
           lwa_cmhdr, lwa_pritm, lwa_actqlt.
  ENDLOOP.

  SORT: gt_practqlt BY prueflos ppsortkey vorglfnr merknr,
        gt_insc BY prueflos vorglfnr merknr.

ENDFORM.                    " DATA_ACTQLT_GET

*&---------------------------------------------------------------------*
*&      Form  TICKET_CONVERT
*&---------------------------------------------------------------------*
FORM ticket_convert TABLES   lt_qapp
                             lt_prnum
                    CHANGING lv_userc1 TYPE qusrchar18.

  DATA: "lv_prnum     TYPE /agri/fmprnum,
    lt_prnum_tmp TYPE RANGE OF /agri/fmprnum,
    ls_prnum     LIKE LINE OF lt_prnum_tmp,
    lv_string_in TYPE string,
    lv_length    TYPE i.

  FIELD-SYMBOLS: <lwa_qapp> TYPE qapp.
  ls_prnum-sign = 'I'.
  ls_prnum-option = 'EQ'.

  IF lv_userc1 IS INITIAL.
    LOOP AT lt_qapp ASSIGNING <lwa_qapp>.

      MOVE <lwa_qapp>-userc1 TO lv_string_in.
      lv_length = strlen( lv_string_in ).
      IF lv_string_in+0(lv_length) CN '1234567890'.
        CONTINUE.
      ENDIF.
      CHECK <lwa_qapp>-userc1 NE 0.
      IF <lwa_qapp>-userc1 > 9999.
        MOVE <lwa_qapp>-userc1+4(10) TO ls_prnum-low.   "lv_prnum
        APPEND ls_prnum TO lt_prnum.
      ELSE.
*        APPEND <lwa_qapp>-userc1+8(10) TO lt_prnum.
        MOVE <lwa_qapp>-userc1+8(10) TO ls_prnum-low.
        APPEND ls_prnum TO lt_prnum.
      ENDIF.
    ENDLOOP.
  ELSE.
    MOVE lv_userc1 TO lv_string_in.
    lv_length = strlen( lv_string_in ).
    IF lv_string_in+0(lv_length) CN '1234567890'.
      EXIT.
    ENDIF.
    CHECK lv_userc1 NE 0.
    IF lv_userc1 > 9999.
      lv_userc1 = lv_userc1+4(10).
    ELSE.
      lv_userc1 = lv_userc1+8(10).
    ENDIF.
  ENDIF.

ENDFORM.                    " TICKET_CONVERT

*&---------------------------------------------------------------------*
*&      Form  data_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_display.

*  CHECK gt_practqlt IS NOT INITIAL.
  CALL SCREEN 2000.

ENDFORM.                    "data_display

*&---------------------------------------------------------------------*
*&      Module  pf_status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pf_status OUTPUT.
  PERFORM pf_status.
ENDMODULE.                    "pf_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pf_status.

  SET PF-STATUS 'S2000'.

ENDFORM.                    "pf_status

*&---------------------------------------------------------------------*
*&      Module  title_set  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE title_set OUTPUT.

  PERFORM title_set.
ENDMODULE.                 " title_set  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  title_set
*&---------------------------------------------------------------------*
FORM title_set.

  PERFORM header_display.
ENDFORM.                    "title_set

*&---------------------------------------------------------------------*
*&      Module  exit_processing  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_processing INPUT.
  PERFORM exit_processing.
ENDMODULE.                 " exit_processing  INPUT

*&---------------------------------------------------------------------*
*&      Form  exit_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_processing.

  fcode = gv_ok101.
  CLEAR gv_ok101.

  CASE fcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.                    "exit_processing

*&---------------------------------------------------------------------*
*&      Module  fcode_processing  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.                    "fcode_processing INPUT

*&---------------------------------------------------------------------*
*&      Form  HEADER_DISPLAY
*&---------------------------------------------------------------------*
FORM header_display.

  SET TITLEBAR 'T100'.

ENDFORM.                    " HEADER_DISPLAY

*&---------------------------------------------------------------------*
*&      Module  CONTROLS_DISPLAY  OUTPUT
*----------------------------------------------------------------------*
MODULE controls_display OUTPUT.

  IF gt_practqlt IS INITIAL.
    MESSAGE TEXT-002 TYPE c_msg_type-success
          DISPLAY LIKE c_msg_type-error.
    LEAVE TO SCREEN 0.
  ENDIF.

  PERFORM controls_display.

ENDMODULE.                    "controls_display OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
FORM controls_display.

  IF ref_container_list IS INITIAL.

    CREATE OBJECT ref_container_list
      EXPORTING
        container_name              = c_custom_control_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

  PERFORM requirements_grid_build.

ENDFORM.                    " CONTROLS_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  REQUIREMENTS_GRID_BUILD
*&---------------------------------------------------------------------*
FORM requirements_grid_build.

  DATA: lt_fcat              TYPE lvc_t_fcat,
        lt_toolbar_excludes  TYPE ui_functions,
        lwa_layout           TYPE lvc_s_layo,
        lwa_variant          TYPE disvariant,
        lwa_toolbar_excludes TYPE ui_func,
        lwa_environment      TYPE /agri/s_glvc_environment.

  IF ref_grid_list IS INITIAL.

    lwa_environment-switchoff_performance = c_true.
    lwa_environment-edit_after_shuffle    = c_true.
    lwa_environment-enable_shuffle        = c_true.

    CREATE OBJECT ref_grid_list
      EXPORTING
        i_parent           = ref_container_list
        is_lvc_environment = lwa_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
    ENDIF.

    PERFORM control_events_register.

    PERFORM dynamic_catalog_fill.

    PERFORM dynamic_table_create.

    PERFORM dynamic_warea_create.

    lwa_variant-report = c_program_rq_list.
    lwa_variant-handle = 'GRID'.

    lwa_layout-cwidth_opt  = c_true.
    lwa_layout-sel_mode    = 'A'.
    lwa_layout-info_fname  = 'ROWCOLOR'.
    lwa_layout-stylefname  = 'STYLES'.
    lwa_layout-ctab_fname  = 'T_CELLCOLORS'.

    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.
    lwa_toolbar_excludes = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND lwa_toolbar_excludes TO lt_toolbar_excludes.


    CALL METHOD ref_grid_list->set_table_for_first_display
      EXPORTING
        is_variant                    = lwa_variant
        i_save                        = 'A'
        is_layout                     = lwa_layout
*       IS_PRINT                      =
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = <gt_dyntable>
        it_fieldcatalog               = gt_fieldcat_fl
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSEIF gs_variables-refresh_grid IS NOT INITIAL.

*    PERFORM field_catalog_prepare TABLES lt_fcat.

    CALL METHOD ref_grid_list->frontend_fieldcatalog_set
      EXPORTING
        it_fieldcatalog = lt_fcat.

    CALL METHOD ref_grid_list->get_frontend_layout
      IMPORTING
        es_layout = lwa_layout.

    lwa_layout-cwidth_opt = c_true.

    CALL METHOD ref_grid_list->set_frontend_layout
      EXPORTING
        is_layout = lwa_layout.

    CALL METHOD ref_grid_list->refresh_table_display.
    CLEAR: gs_variables-refresh_grid.

  ENDIF.

ENDFORM.                    " REQUIREMENTS_GRID_BUILD

*&---------------------------------------------------------------------*
*&      Form  CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
FORM control_events_register.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  SET HANDLER ref_event_handler->on_user_command FOR ref_grid_list.

ENDFORM.                    " CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing .

ENDFORM.                    " FCODE_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_CATALOG_FILL
*&---------------------------------------------------------------------*
FORM dynamic_catalog_fill.

  DATA: lwa_fcat TYPE lvc_s_fcat,
        lwa_insc TYPE ty_insc,
        lv_lines TYPE i.

*-- BOC-T_T.KONNO-01/29/20
*  FIELD-SYMBOLS: <ls_practqlt> TYPE /agri/s_fmpractqlt_fcat.
  FIELD-SYMBOLS: <ls_practqlt> TYPE zabs_str_fmpractqlt_fcat.
*-- EOC-T_T.KONNO-01/29/20

  PERFORM field_catalog_prepare USING    c_structure_name-wo_fl
                                CHANGING gt_fieldcat_fl.
  DESCRIBE TABLE gt_fieldcat_fl LINES lv_lines.
  LOOP AT gt_practqlt ASSIGNING <ls_practqlt>.
    READ TABLE gt_insc INTO lwa_insc
      WITH KEY prueflos = <ls_practqlt>-prueflos
               vorglfnr = <ls_practqlt>-vorglfnr
               merknr   = <ls_practqlt>-merknr BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    READ TABLE gt_fieldcat_fl INTO lwa_fcat
           WITH KEY fieldname = lwa_insc-verwmerkm.
    IF sy-subrc <> 0.
      lwa_fcat-fieldname = lwa_insc-verwmerkm.
      lwa_fcat-scrtext_l = lwa_insc-kurztext.
      lwa_fcat-col_pos   = lv_lines + 1.
*-- BOC-T_T.KONNO
*      lwa_fcat-datatype  = 'QMITTELWRT'.
*      lwa_fcat-intlen    = 18.
      IF lwa_fcat-fieldname EQ 'F_DATAAN'. "Data da Análise
        lwa_fcat-datatype = 'DATS'.
        lwa_fcat-inttype  = 'D'.
        lwa_fcat-intlen   = 8.
        lwa_fcat-domname  = 'DATUM'.
      ELSE.
        lwa_fcat-datatype = 'QMITTELWRT'.
        lwa_fcat-intlen   = 18.
      ENDIF.
*-- EOC-T_T.KONNO
      APPEND lwa_fcat TO gt_fieldcat_fl.
      CLEAR: lwa_fcat.
    ENDIF.
  ENDLOOP.

*-- BOC-T_T.KONNO
  READ TABLE gt_fieldcat_fl ASSIGNING FIELD-SYMBOL(<lwa_fieldcat_fl>)
    WITH KEY fieldname = 'MERKNR'.
  IF sy-subrc EQ 0.
    <lwa_fieldcat_fl>-no_out = abap_true.
  ENDIF.
*-- EOC-T_T.KONNO

ENDFORM.                    "dynamic_catalog_fill

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
FORM field_catalog_prepare USING    lv_structure_name
                           CHANGING lt_fcat TYPE lvc_t_fcat.

  DATA: lwa_fcat   TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = lv_structure_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
    CHANGING
      ct_fieldcat            = lt_fcat[]
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc NE 0.
  ENDIF.

  lwa_fcat-fieldname = 'T_CELLCOLORS'.
  lwa_fcat-ref_field = 'TABCOL'.
  lwa_fcat-ref_table = 'SDV45L7_ALV'.
  APPEND lwa_fcat TO lt_fcat.

ENDFORM.                    " FIELD_CATALOG_PREPARE

*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_TABLE_CREATE
*&---------------------------------------------------------------------*
FORM dynamic_table_create.

  DATA: lref_newtable TYPE REF TO data.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fieldcat_fl
    IMPORTING
      ep_table        = lref_newtable.

  ASSIGN lref_newtable->* TO <gt_dyntable>.

ENDFORM.                    " DYNAMIC_TABLE_CREATE
*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_WAREA_CREATE
*&---------------------------------------------------------------------*
FORM dynamic_warea_create.

  DATA: lref_newline        TYPE REF TO data,
        lwa_insc            TYPE ty_insc,
        lwa_scol            TYPE lvc_s_scol,
        lwa_qpct            TYPE qpct,
        lv_prueflos         TYPE qplos,
        lv_vorglfnr         TYPE qlfnkn,
        lv_descr            TYPE c LENGTH 18,
        lv_value            TYPE qsollwertc,
        lv_number_of_digits TYPE t006-decan.

*-- BOC-T_T.KONNO-01/29/20
  DATA: lv_ppsortkey TYPE qppsortkey.
*-- EOC-T_T.KONNO-01/29/20

*-- BOC-T_T.KONNO-01/29/20
*  FIELD-SYMBOLS: <ls_practqlt> TYPE /agri/s_fmpractqlt_fcat,
  FIELD-SYMBOLS: <ls_practqlt> TYPE zabs_str_fmpractqlt_fcat,
*-- EOC-T_T.KONNO-01/29/20
                 <lwa_qpct>    TYPE qpct,
                 <gwa_data>,
                 <lf_value>,
                 <lt_scol>     TYPE lvc_t_scol.

  CREATE DATA lref_newline LIKE LINE OF <gt_dyntable>.
  ASSIGN lref_newline->* TO <gs_dyntable>.
  SORT: gt_practqlt BY prueflos ppsortkey vorglfnr merknr,
        gt_insc BY prueflos vorglfnr merknr.

  LOOP AT gt_practqlt ASSIGNING <ls_practqlt>.
    IF lv_prueflos IS INITIAL
    OR lv_prueflos <> <ls_practqlt>-prueflos
    OR lv_ppsortkey <> <ls_practqlt>-ppsortkey
    OR lv_vorglfnr <> <ls_practqlt>-vorglfnr.
      lv_ppsortkey = <ls_practqlt>-ppsortkey.
      lv_vorglfnr = <ls_practqlt>-vorglfnr.
      lv_prueflos = <ls_practqlt>-prueflos.
      READ TABLE gt_insc INTO lwa_insc
        WITH KEY prueflos = <ls_practqlt>-prueflos
                 vorglfnr = <ls_practqlt>-vorglfnr
                 merknr   = <ls_practqlt>-merknr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO <gt_dyntable> ASSIGNING <gwa_data>.
      MOVE-CORRESPONDING <ls_practqlt> TO <gwa_data>.

      ASSIGN COMPONENT lwa_insc-verwmerkm OF STRUCTURE <gwa_data> TO <lf_value>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'T_CELLCOLORS' OF STRUCTURE <gwa_data> TO <lt_scol>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF lwa_insc-katalgart1 IS NOT INITIAL.
        READ TABLE gt_qpct ASSIGNING <lwa_qpct>
          WITH KEY katalogart = lwa_insc-katalgart1
                   codegruppe = lwa_insc-gruppe1
                   code       = lwa_insc-code1
                   version    = lwa_insc-version1 BINARY SEARCH.

        CONCATENATE <lwa_qpct>-code <lwa_qpct>-kurztext
          INTO lv_descr SEPARATED BY space.

        <lf_value> = lv_descr.
      ELSE.
        lv_number_of_digits = 2.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION'
          EXPORTING
            decim = lv_number_of_digits
            input = lwa_insc-mittelwert
            ivalu = 'X'
          IMPORTING
            flstr = lv_value.

        <lf_value> = lv_value.
      ENDIF.
      CASE lwa_insc-mbewertg.
        WHEN 'A'.
          IF <ls_practqlt>-aufnr IS NOT INITIAL.
            lwa_scol-fname     = 'AUFNR'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
            lwa_scol-fname     = 'EBELN'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ENDIF.
          lwa_scol-fname     = lwa_insc-verwmerkm.
          lwa_scol-color-col = 5.
          APPEND lwa_scol TO <lt_scol>.
        WHEN 'R'.
          IF <ls_practqlt>-aufnr IS NOT INITIAL.
            lwa_scol-fname     = 'AUFNR'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
            lwa_scol-fname     = 'EBELN'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ENDIF.
          lwa_scol-fname     = lwa_insc-verwmerkm.
          lwa_scol-color-col = 6.
          APPEND lwa_scol TO <lt_scol>.
        WHEN 'F'.
          IF <ls_practqlt>-aufnr IS NOT INITIAL.
            lwa_scol-fname     = 'AUFNR'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
            lwa_scol-fname     = 'EBELN'.
            lwa_scol-color-col = 3.
            APPEND lwa_scol TO <lt_scol>.
          ENDIF.
          lwa_scol-fname     = lwa_insc-verwmerkm.
          lwa_scol-color-col = 7.
          APPEND lwa_scol TO <lt_scol>.
      ENDCASE.
    ELSE.
      READ TABLE gt_insc INTO lwa_insc
        WITH KEY prueflos = <ls_practqlt>-prueflos
                 vorglfnr = <ls_practqlt>-vorglfnr
                 merknr   = <ls_practqlt>-merknr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT lwa_insc-verwmerkm OF STRUCTURE <gwa_data> TO <lf_value>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'T_CELLCOLORS' OF STRUCTURE <gwa_data> TO <lt_scol>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF lwa_insc-katalgart1 IS NOT INITIAL.
        READ TABLE gt_qpct INTO lwa_qpct
          WITH KEY katalogart = lwa_insc-katalgart1
                   codegruppe = lwa_insc-gruppe1
                   code       = lwa_insc-code1
                   version    = lwa_insc-version1 BINARY SEARCH.
        CONCATENATE lwa_qpct-code lwa_qpct-kurztext INTO lv_descr
                    SEPARATED BY space.
        <lf_value> = lv_descr.
      ELSE.
        lv_number_of_digits = 2.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION'
          EXPORTING
            decim = lv_number_of_digits
            input = lwa_insc-mittelwert
            ivalu = 'X'
          IMPORTING
            flstr = lv_value.

        <lf_value> = lv_value.
        IF lwa_insc-verwmerkm EQ 'F_DATAAN'.
          DATA: lv_int  TYPE i,
                lv_date TYPE d.

          lv_int = lwa_insc-mittelwert.
          WRITE lv_int TO lv_date.
          CONCATENATE lv_date+4(4) lv_date+2(2) lv_date(2) INTO lv_date.
          <lf_value> = lv_date.
        ENDIF.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'PRUEFER' OF STRUCTURE <gwa_data> TO FIELD-SYMBOL(<lv_inspetor>).
    IF sy-subrc EQ 0.
      <lv_inspetor> = lwa_insc-userc1.
    ENDIF.

    CASE lwa_insc-mbewertg.
      WHEN 'A'.
        IF <ls_practqlt>-aufnr IS NOT INITIAL.
          lwa_scol-fname     = 'AUFNR'.
          lwa_scol-color-col = 3.
          APPEND lwa_scol TO <lt_scol>.
        ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
          lwa_scol-fname     = 'EBELN'.
          lwa_scol-color-col = 3.
          APPEND lwa_scol TO <lt_scol>.
        ENDIF.
        lwa_scol-fname     = lwa_insc-verwmerkm.
        lwa_scol-color-col = 5.
        APPEND lwa_scol TO <lt_scol>.
      WHEN 'R'.
        IF <ls_practqlt>-aufnr IS NOT INITIAL.
          lwa_scol-fname     = 'AUFNR'.
          lwa_scol-color-col = 3.
          APPEND lwa_scol TO <lt_scol>.
        ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
          lwa_scol-fname     = 'EBELN'.
          lwa_scol-color-col = 6.
          APPEND lwa_scol TO <lt_scol>.
        ENDIF.
        lwa_scol-fname     = lwa_insc-verwmerkm.
        lwa_scol-color-col = 3.
        APPEND lwa_scol TO <lt_scol>.
      WHEN 'F'.
        IF <ls_practqlt>-aufnr IS NOT INITIAL.
          lwa_scol-fname     = 'AUFNR'.
          lwa_scol-color-col = 3.
          APPEND lwa_scol TO <lt_scol>.
        ELSEIF <ls_practqlt>-ebeln IS NOT INITIAL.
          lwa_scol-fname     = 'EBELN'.
          lwa_scol-color-col = 3.
          APPEND lwa_scol TO <lt_scol>.
        ENDIF.
        lwa_scol-fname     = lwa_insc-verwmerkm.
        lwa_scol-color-col = 7.
        APPEND lwa_scol TO <lt_scol>.
    ENDCASE.
  ENDLOOP.

  DELETE gt_fieldcat_fl WHERE fieldname EQ 'MERKNR'
                        AND   fieldname EQ 'T_CELLCOLORS'.

ENDFORM.                    " DYNAMIC_WAREA_CREATE
