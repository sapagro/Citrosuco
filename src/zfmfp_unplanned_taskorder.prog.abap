*&---------------------------------------------------------------------*
*& Report  /AGRI/FM_UNPLANNED_TASKORDER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfmfp_unplanned_taskorder.

TYPE-POOLS slis.

INCLUDE: /agri/gprolog_macros,
         /agri/glprolog_macros,
         /agri/global_constants,
         /agri/global_macros,
         /agri/abgl_constants.

TABLES: /agri/glflot,
        /agri/glflcma,
        sscrfields.

CLASS: lcl_event_handler DEFINITION DEFERRED,
       lcl_log_handler   DEFINITION DEFERRED.

DATA: gt_taskorder      TYPE /agri/t_fmfp_uptask_mass,
      gt_taskorder_fcat TYPE /agri/t_fmfp_uptask_mass_fcat,
      gt_csdoc_infocus  TYPE /agri/t_glcs_doc,
      gt_flcma          TYPE /agri/t_glflcma,
      gt_items_mod_rows TYPE lvc_t_modi,
      gt_messtab        TYPE tab_bdcmsgcoll,
      gt_tplnr          TYPE /agri/t_gltplnr,
      wa_parameters     TYPE zsc_fmfp_task_upload,
      wa_fmfphdr        TYPE /agri/fmfphdr.

DATA: gt_fpbom_fcat   TYPE /agri/t_fmfpbom_fcat,
      gt_fpbom_temp   TYPE /agri/t_fmfpbom_fcat,
      gt_bom_mod_rows TYPE lvc_t_modi.

DATA: BEGIN OF gs_unit,
        tomng    TYPE /agri/fmtomng,
        meinh    TYPE vorme,
        tplnr_fl TYPE /agri/gltplnr_fl,
        cmnum    TYPE /agri/glcmnum,
        iwerk    TYPE iwerk,
      END OF gs_unit.

DATA: BEGIN OF gt_verid OCCURS 0,
        verid TYPE verid,
      END OF gt_verid.
DATA: gt_return TYPE ddshretval OCCURS 0 WITH HEADER LINE.
DATA: field_tab LIKE dfies  OCCURS 0 WITH HEADER LINE.
DATA: ref_assignments_grid      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_assignments_container TYPE REF TO cl_gui_custom_container,
      ref_event_handler         TYPE REF TO lcl_event_handler,
      ref_log_handler           TYPE REF TO lcl_log_handler,
      ref_grid_bom              TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_bom         TYPE REF TO cl_gui_custom_container.

DATA: ok_code TYPE sy-ucomm,
      fcode   LIKE ok_code,
      functxt TYPE smp_dyntxt.

DATA: lv_activity(2) TYPE c,
      lv_subrc       TYPE sy-subrc.

DATA: BEGIN OF gs_variables,
        initiator          TYPE /agri/gdescr,
        refresh_bom,
        refresh_colum_bom,
        document_mode,
        refresh_items_grid,
        manual_changes,
      END OF gs_variables.

CONSTANTS: BEGIN OF c_object,
             log LIKE balobj-object  VALUE '/AGRI/FMFP',
           END OF c_object.

CONSTANTS : BEGIN OF c_log_subobject,
              save TYPE balsubobj VALUE 'SAVE',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_structure_name,
             task_order    TYPE dd02l-tabname
                           VALUE '/AGRI/S_FMFP_UPTASK_MASS',
             bill_material TYPE dd02l-tabname
                           VALUE '/AGRI/S_FMFPBOM_FCAT',
           END OF c_structure_name.

CONSTANTS: BEGIN OF c_program_name,
             task_orders TYPE sy-repid
                        VALUE '/AGRI/FMFP_UNPLANNED_TASKORDER',
           END OF c_program_name.

CONSTANTS: BEGIN OF c_screen,
             main_screen   TYPE sy-repid VALUE '0100',
             bill_material TYPE sy-dynnr VALUE '0406',
           END OF c_screen.

CONSTANTS: BEGIN OF c_fcode,
             enter            TYPE sy-ucomm VALUE 'ENTR',
             insert_bom       TYPE sy-ucomm VALUE 'BINS',
             delete_bom       TYPE sy-ucomm VALUE 'BDEL',
             change_colum_bom TYPE sy-ucomm VALUE 'BCOL',
             create_cat       TYPE sy-ucomm VALUE 'CRCA',
           END OF c_fcode.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE /agri/glcmwrk-werks,
            p_cmnum TYPE /agri/glcmhdr-cmnum,
            p_matnr TYPE mara-matnr,
            p_basoq TYPE /agri/glbasoq AS LISTBOX VISIBLE LENGTH 20.
*SELECT-OPTIONS: so_tplnr FOR  /agri/glflcma-tplnr_fl.
SELECT-OPTIONS: so_strno FOR  /agri/glflot-strno.
PARAMETERS:     p_varia TYPE /agri/glvaria AS LISTBOX
                                    VISIBLE LENGTH 30 USER-COMMAND vari,
                p_cpros TYPE /agri/glcpros AS LISTBOX
                                    VISIBLE LENGTH 30 USER-COMMAND pros,
                p_date  TYPE sy-datum DEFAULT sy-datum,
                p_verid TYPE mkal-verid,
                gv_qty  TYPE /agri/glqunt NO-DISPLAY,
                p_res1  TYPE qeinfeld,
                p_res2  TYPE qeinfeld,
                p_res3  TYPE qeinfeld,
                p_res4  TYPE qeinfeld,
                p_res5  TYPE qeinfeld,
                p_res6  TYPE qeinfeld,
                p_res7  TYPE qeinfeld,
                p_res8  TYPE qeinfeld,
                p_res9  TYPE qeinfeld,
                p_res10 TYPE qeinfeld.

SELECTION-SCREEN END OF BLOCK b1.

DEFINE zabs_message_simple.
  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  IF NOT &1 EQ space.
    gs_message-level = &1.
  ENDIF.

  CALL METHOD ref_process_log->zabs_message_add
    EXPORTING
      i_level                 = gs_message-level
      i_msgid                 = sy-msgid
      i_msgty                 = sy-msgty
      i_msgno                 = sy-msgno
      i_msgv1                 = sy-msgv1
      i_msgv2                 = sy-msgv2
      i_msgv3                 = sy-msgv3
      i_msgv4                 = sy-msgv4
    EXCEPTIONS
      message_to_be_displayed = 1
      OTHERS                  = 2.
  IF sy-subrc EQ 1
  AND NOT sy-msgid IS INITIAL
  AND NOT sy-msgty IS INITIAL.
*  and not sy-msgno is initial.
****Rel E SP1
    IF sy-msgid EQ '/AGRI/GMSG1' OR
       sy-msgid EQ '/AGRI/GMSG2'.
      CLEAR gs_log_variables-text.
      CALL FUNCTION '/AGRI/GMSG_TEXTGET'
        EXPORTING
          i_msgcls             = sy-msgid
          i_msgno              = sy-msgno
          i_placeholder1       =  sy-msgv1
          i_placeholder2       =  sy-msgv2
          i_placeholder3       =  sy-msgv3
          i_placeholder4       =  sy-msgv4
       IMPORTING
         e_message           = gs_log_variables-text.

      MESSAGE gs_log_variables-text TYPE sy-msgty.
    ELSE.
***
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*** Rel E SP1
    ENDIF.
***
  ENDIF.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS: on_hotspot_click    FOR EVENT hotspot_click
                  OF /agri/cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no,

      on_data_changed     FOR EVENT data_changed
                    OF /agri/cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before
                    e_onf4_after e_ucomm sender,

      on_toolbar_grid      FOR EVENT toolbar
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,

      on_user_command_grid FOR EVENT user_command
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4_request_grid   FOR EVENT onf4
                    OF /agri/cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue
                    es_row_no er_event_data
                    et_bad_cells e_display
                    sender.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_log_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_log_handler DEFINITION.

  PUBLIC SECTION.

    METHODS: on_log_display_profile
      FOR EVENT log_display_profile
                  OF /agri/cl_process_log_manager
      IMPORTING eref_event_data.

ENDCLASS.                    "lcl_log_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.
    DATA: lwa_taskorder LIKE LINE OF gt_taskorder.

    READ TABLE gt_taskorder INTO lwa_taskorder INDEX e_row_id-index. "#EC CI_NOORDER
    IF sy-subrc = 0 .
      IF e_column_id-fieldname = 'SELECTION'.
      ELSEIF e_column_id-fieldname EQ 'TPLNR_FL'.
        PERFORM display_terrain USING lwa_taskorder-tplnr_fl.
      ELSEIF e_column_id-fieldname EQ 'CMNUM'.
        PERFORM display_corp USING lwa_taskorder-cmnum.
      ELSEIF e_column_id-fieldname = 'AUFNR'
         AND lwa_taskorder-aufnr IS NOT INITIAL .
        PERFORM order_display USING lwa_taskorder-aufnr.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "ON_HOTSPOT_CLICK

  METHOD on_data_changed.

    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
      CASE sender.

        WHEN ref_assignments_grid.
          gs_variables-manual_changes = c_true.
          APPEND LINES OF er_data_changed->mt_mod_cells[]
                       TO gt_items_mod_rows.
          SORT gt_items_mod_rows BY row_id.
          DELETE ADJACENT DUPLICATES FROM gt_items_mod_rows
                           COMPARING row_id.
        WHEN ref_grid_bom.
          gs_variables-manual_changes = c_true.
          APPEND LINES OF er_data_changed->mt_mod_cells[] TO gt_bom_mod_rows.

          SORT gt_bom_mod_rows BY row_id.
          DELETE ADJACENT DUPLICATES FROM gt_bom_mod_rows COMPARING row_id.
        WHEN OTHERS.

      ENDCASE.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed
  METHOD: on_toolbar_grid.

    DATA : lwa_button TYPE stb_button.

    CASE sender.
      WHEN ref_grid_bom.
        toolbar_button_insert e_object->mt_toolbar lwa_button 3
                                  space space space space.
        toolbar_button_insert e_object->mt_toolbar lwa_button space
                              c_fcode-delete_bom icon_delete_row
                              TEXT-009 space.
        toolbar_button_insert e_object->mt_toolbar lwa_button space
                              c_fcode-insert_bom icon_insert_row
                              TEXT-008 space.
        toolbar_button_insert e_object->mt_toolbar lwa_button space
                              c_fcode-change_colum_bom icon_choose_columns
                              TEXT-010 space.
        toolbar_button_insert e_object->mt_toolbar lwa_button space
                              c_fcode-create_cat icon_create
                              TEXT-011 space.
    ENDCASE.

  ENDMETHOD.                    "on_toolbar_grid
  METHOD on_user_command_grid.
    IF e_ucomm EQ c_fcode-change_colum_bom.
      IF gs_variables-refresh_colum_bom EQ c_false.
        gs_variables-refresh_colum_bom = c_true.
      ELSE.
        CLEAR: gs_variables-refresh_colum_bom.
      ENDIF.
      gs_variables-refresh_bom = c_true.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_user_command
  METHOD on_f4_request_grid.

    DATA: lv_tabix            TYPE sy-tabix,
          lv_matnr            TYPE matnr,
          lv_charg            TYPE charg_d,
          lv_lgort            TYPE lgort_d,
          lwa_modified_values TYPE lvc_s_modi.

    FIELD-SYMBOLS: <lt_modified_values> TYPE lvc_t_modi.
    lv_tabix = es_row_no-row_id.

    IF e_fieldname = 'MATNR'.
*
      PERFORM material_cat_values_get USING lv_tabix
                                      CHANGING lv_matnr.

      IF lv_matnr IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        IF <lt_modified_values> IS ASSIGNED.
          lwa_modified_values-row_id = es_row_no-row_id.
          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
          lwa_modified_values-fieldname = e_fieldname.
          lwa_modified_values-value = lv_matnr.
          APPEND lwa_modified_values TO <lt_modified_values>.
        ENDIF.

      ENDIF.
    ENDIF.
    IF e_fieldname = 'CHARG'.
*
      PERFORM bacth_cat_values_get USING lv_tabix
                                   CHANGING lv_charg.

      IF lv_charg IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        IF <lt_modified_values> IS ASSIGNED.
          lwa_modified_values-row_id = es_row_no-row_id.
          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
          lwa_modified_values-fieldname = e_fieldname.
          lwa_modified_values-value = lv_charg.
          APPEND lwa_modified_values TO <lt_modified_values>.
        ENDIF.

      ENDIF.
    ENDIF.
    IF e_fieldname = 'LGORT'.
*
      PERFORM storage_cat_values_get USING lv_tabix
                                   CHANGING lv_lgort.

      IF lv_lgort IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <lt_modified_values>.
        IF <lt_modified_values> IS ASSIGNED.
          lwa_modified_values-row_id = es_row_no-row_id.
          lwa_modified_values-sub_row_id = es_row_no-sub_row_id.
          lwa_modified_values-fieldname = e_fieldname.
          lwa_modified_values-value = lv_lgort.
          APPEND lwa_modified_values TO <lt_modified_values>.
        ENDIF.

      ENDIF.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

    er_event_data->m_event_handled = c_true.

  ENDMETHOD.                    "on_f4_request_grid

ENDCLASS.               "lcl_event_handler

*----------------------------------------------------------------------*
*       CLASS lcl_log_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_log_handler IMPLEMENTATION.

  METHOD on_log_display_profile.

    DATA: ls_fcat TYPE bal_s_fcat.

    READ TABLE eref_event_data->ms_display_profile-mess_fcat
    WITH KEY ref_field = 'TPLNR_FL' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_fcat-ref_table  = '/AGRI/S_GLCS_CONTEXT'.
      ls_fcat-ref_field  = 'TPLNR_FL'.
      ls_fcat-outputlen  = 30.
      ls_fcat-coltext    = TEXT-003.
      ls_fcat-col_pos    = 1.
      APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
    ENDIF.

    READ TABLE eref_event_data->ms_display_profile-mess_fcat
    WITH KEY ref_field = 'CMNUM' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_fcat-ref_table  = '/AGRI/S_GLCS_CONTEXT'.
      ls_fcat-ref_field  = 'CMNUM'.
      ls_fcat-outputlen  = 10.
      ls_fcat-coltext    = TEXT-004.
      ls_fcat-col_pos    = 1.
      APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
    ENDIF.

  ENDMETHOD.                    "on_log_display_profile

ENDCLASS.                    "lcl_log_handler IMPLEMENTATION

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.

  config_tcode_authority_check '/AGRI/FMUT23' '01' lv_subrc.
  IF lv_subrc <> 0.
    MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '023' INTO sy-msgli.
*    zabs_message_simple space.
    PERFORM zabs_message_simple.
  ENDIF.

  functxt-icon_id       = '@2Q@'.
  functxt-icon_text     = TEXT-005.
  functxt-quickinfo     = TEXT-005.
  sscrfields-functxt_01 = functxt.

AT SELECTION-SCREEN OUTPUT.

  PERFORM dropdown_tables_display.

  IF ok_code NE 'FC01'.

    PERFORM screen_modify.

  ENDIF.

AT SELECTION-SCREEN.
  IF NOT gt_csdoc_infocus IS INITIAL.
    ok_code = sy-ucomm.
    IF ok_code = 'FC01'.
      LOOP AT SCREEN.
        screen-input = 1.
        MODIFY SCREEN.
      ENDLOOP.
      CLEAR: p_varia,p_cpros.
    ENDIF.
  ENDIF.

  PERFORM selection_validations.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_verid.
*---Replace Unreleased Interfaces
*  CALL FUNCTION 'CO_SD_GET_PROD_VERS'
*    EXPORTING
*      imp_werks        = p_werks
*      imp_matnr        = p_matnr
*      imp_gamng        = ' '
*      imp_date         = sy-datum
**     KZ_FAUF          =
*      kz_f4_help       = c_true
*    IMPORTING
*      exp_verid        = p_verid
**     EXP_VERTO        =
**     EXP_LGORT        =
**     EXP_CSPLT        =
*    EXCEPTIONS
*      no_version_found = 1
*      OTHERS           = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
  CALL FUNCTION '/AGRI/G_CO_SD_GET_PROD_VERS'
    EXPORTING
      i_werks          = p_werks
      i_matnr          = p_matnr
      i_gamng          = ' '
      i_date           = sy-datum
*     I_KZ_FAUF        =
      i_kz_f4_help     = c_true
    IMPORTING
      e_verid          = p_verid
    EXCEPTIONS
      no_version_found = 1
      OTHERS           = 2.

START-OF-SELECTION.

  lv_activity = c_authorization_activity-change.
  PERFORM authority_check USING lv_activity CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM get_assignments_data.
  PERFORM transfer_parameters.

END-OF-SELECTION.
  IF gt_taskorder[] IS INITIAL.
    MESSAGE s751(/agri/global) INTO sy-msgli.
    PERFORM zabs_message_simple.
  ELSE.
    sy-dynnr = '0100'.
    PERFORM assignments_data_dispaly.
    PERFORM fcode_create_task.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_ASSIGNMENTS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_assignments_data .

  IF NOT gt_tplnr[] IS INITIAL.
    SELECT * FROM /agri/glflcma               "#EC CI_ALL_FIELDS_NEEDED
      INTO CORRESPONDING FIELDS OF TABLE gt_flcma
       FOR ALL ENTRIES IN gt_tplnr
      WHERE tplnr_fl EQ gt_tplnr-tplnr_fl
        AND cmnum    EQ p_cmnum
        AND varia    EQ p_varia
        AND datab    LE p_date
        AND datbi    GE p_date
        AND astat    EQ 'A'  "Active
        AND iwerk    EQ p_werks
        AND loevm    EQ space.

    IF sy-subrc EQ 0.
      PERFORM assignments_data_prepare USING gt_flcma.
    ELSE.
*-- Atenção: Verificar Épocas de Cultura!
      MESSAGE i110(zfmfp) INTO sy-msgli.
      PERFORM zabs_message_simple.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ASSIGNMENTS_DATA

*&---------------------------------------------------------------------*
*&      Module  CONTROLS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE controls_display OUTPUT.

  PERFORM controls_display.

ENDMODULE.                 " CONTROLS_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display .

  DATA: lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

*  PERFORM (lv_routine) IN PROGRAM (c_program_name-task_orders) IF FOUND.
  PERFORM (lv_routine) IN PROGRAM zfmfp_unplanned_taskorder IF FOUND.


ENDFORM.                    " CONTROLS_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CONTROLS_DISPLAY_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0100.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        ls_environment      TYPE /agri/s_glvc_environment,
        lt_toolbar_excludes TYPE ui_functions,
        ls_layout           TYPE lvc_s_layo,
        ls_variant          TYPE disvariant.

  IF ref_assignments_container IS INITIAL.

    CREATE OBJECT ref_assignments_container
      EXPORTING
*       PARENT                      =
        container_name              = '/AGRI/FMUT_0100_CC'
*       STYLE                       =
*       LIFETIME                    = lifetime_default
*       REPID                       =
*       DYNNR                       =
*       NO_AUTODEF_PROGID_DYNNR     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  ls_environment-switchoff_performance = c_true.
  ls_layout-sel_mode                   = 'A'.
  ls_layout-stylefname                 = 'STYLES'.
  ls_layout-info_fname                 = 'ROWCOLOR'.
  ls_layout-cwidth_opt                 = c_true.

  IF ref_assignments_grid IS INITIAL.

    CREATE OBJECT ref_assignments_grid
      EXPORTING
*       I_SHELLSTYLE       = 0
*       I_LIFETIME         =
        i_parent           = ref_assignments_container
*       I_APPL_EVENTS      = space
*       I_PARENTDBG        =
*       I_APPLOGPARENT     =
*       I_GRAPHICSPARENT   =
*       I_USE_VARIANT_CLASS = SPACE
*       I_NAME             =
        is_lvc_environment = ls_environment
*       I_NO_AUTO_DETAILS  =
*       I_NOOUT_FIELDS_HIDE = 'X'
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM field_catalog_prepare USING c_structure_name-task_order
                               CHANGING lt_fcat.

    PERFORM control_events_register.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes
                                    USING  c_structure_name-task_order.

    ls_variant-report    = 'ZFMFP_UNPLANNED_TASKORDER'.
*    ls_variant-report    = c_program_name-task_orders.
    ls_variant-handle    = c_screen-main_screen.

    CALL METHOD ref_assignments_grid->set_table_for_first_display
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_BYPASSING_BUFFER            =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
        is_variant                    = ls_variant
        i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = ls_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
        it_toolbar_excluding          = lt_toolbar_excludes
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
*       IT_EXCEPT_QINFO               =
      CHANGING
        it_outtab                     = gt_taskorder_fcat
        it_fieldcatalog               = lt_fcat
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD ref_assignments_grid->set_toolbar_interactive.
  ELSEIF gs_variables-refresh_items_grid IS NOT INITIAL."--27/10/2018

    CALL METHOD ref_assignments_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    "CONTROLS_DISPLAY_0100

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_catalog_prepare  USING    lv_structure
                            CHANGING lt_fcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <lwa_fcat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = lv_structure
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CASE lv_structure. "26/10/2016

    WHEN c_structure_name-task_order."26/10/2016

      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'TPLNR_FL' OR 'CMNUM'.
            <lwa_fcat>-hotspot      = c_true.
          WHEN 'AUFNR'.
            <lwa_fcat>-hotspot      = c_true.
          WHEN 'GSTRP' OR 'AAREA'.
            <lwa_fcat>-edit         = c_true.
          WHEN 'CONTR'.
            <lwa_fcat>-no_out       = c_true.
        ENDCASE.
      ENDLOOP.

****26/10/2016
    WHEN  c_structure_name-bill_material.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'MATNR' OR 'PLAN_QTY'
            OR 'RATE' OR 'MEINS'.
            <lwa_fcat>-edit = c_true.
          WHEN 'AUFNR' OR 'POSTP'.
            <lwa_fcat>-tech = c_true.
          WHEN 'CHARG' OR 'LGORT'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
        ENDCASE.
      ENDLOOP.
****26/12/2016
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " FIELD_CATALOG_PREPARE

*&---------------------------------------------------------------------*
*&      Form  CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register .

  DATA : lv_routine(28) VALUE 'CONTROL_EVENTS_REGISTER_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.

  PERFORM (lv_routine) IN PROGRAM (c_program_name-task_orders) IF FOUND.

ENDFORM.                    "control_events_register

*&---------------------------------------------------------------------*
*&      Form  CONTROL_EVENTS_REGISTER_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM control_events_register_0100.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  CALL METHOD ref_assignments_grid->register_edit_event
    EXPORTING
      i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  SET HANDLER:  ref_event_handler->on_data_changed
                FOR ref_assignments_grid,
                ref_event_handler->on_hotspot_click
                FOR ref_assignments_grid.

ENDFORM.                    "CONTROL_EVENTS_REGISTER_0100

*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_buttons_exclude TABLES lt_toolbar_excludes TYPE ui_functions
                             USING  lv_structure.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_check TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_refresh
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_mb_paste TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_append_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_cut
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_delete_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_move_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste_new_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_undo
                                           TO lt_toolbar_excludes.

ENDFORM.                    " TOOLBAR_BUTTONS_EXCLUDE

*&---------------------------------------------------------------------*
*&      Module  STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_set OUTPUT.

  PERFORM status_set.

ENDMODULE.                 " STATUS_SET  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_set .

*SET PF-STATUS 'S100'.
  CASE sy-dynnr.
    WHEN c_screen-main_screen.
      SET PF-STATUS 'S100'.
    WHEN c_screen-bill_material.
      SET PF-STATUS 'S406'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " STATUS_SET

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.

  PERFORM fcode_processing.

ENDMODULE.                 " FCODE_PROCESSING  INPUT

*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing .

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.

  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (c_program_name-task_orders)
                          IF FOUND.

ENDFORM.                    " FCODE_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  fcode_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_back.

  LEAVE TO TRANSACTION sy-tcode.

ENDFORM.                    "fcode_back

*&---------------------------------------------------------------------*
*&      Form  FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_exit.

  SET SCREEN 0.

ENDFORM.                    "FCODE_EXIT

*&---------------------------------------------------------------------*
*&      Form  FCODE_CANC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_canc.

  SET SCREEN 0.

ENDFORM.                    "FCODE_CANC

*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_crea.

  DATA: lt_fphdr      TYPE /agri/t_fmfphdr,
        lt_dflow      TYPE /agri/t_glcsdfl,
        lwa_fpdoc     TYPE /agri/s_fmfp_doc,
        lwa_taskorder TYPE /agri/s_fmfp_uptask_mass,
        lwa_fpitm     TYPE /agri/s_fmfpitm,
        lwa_cskey     TYPE /agri/s_glcs_key,
        lwa_fphdr     TYPE /agri/s_fmfphdr.

  DATA: lt_rows            TYPE lvc_t_row,
        lt_messages        TYPE /agri/t_gprolog,
        lwa_rows           TYPE lvc_s_row,
        ls_message         TYPE /agri/s_gprolog,
        lwa_items_mod_rows LIKE LINE OF gt_items_mod_rows,
        lv_subrc           TYPE sysubrc,
        lv_valid,
        lv_answer.

  DATA: lv_lines TYPE sy-tabix,
        lv_cont  TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_taskorder> LIKE LINE OF gt_taskorder.

  CALL METHOD ref_assignments_grid->check_changed_data
    IMPORTING
      e_valid = lv_valid.

  IF lv_valid IS INITIAL.
    CLEAR ok_code.
    EXIT.
  ENDIF.

  CALL METHOD ref_assignments_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows IS INITIAL.
    MESSAGE e006(/agri/fmfp) INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  DESCRIBE TABLE lt_rows LINES lv_lines.

*  LOOP AT lt_rows INTO lwa_rows.
*    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder> INDEX lwa_rows-index. "#EC CI_NOORDER
*
*    IF sy-subrc EQ 0.
*      IF  <lwa_taskorder>-sappl EQ c_special_appl-chemical.
*        lv_cont = lv_cont + 1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  IF lv_lines EQ lv_cont.
*
*    gs_variables-initiator =  c_log_initiator-check.
*    PERFORM messages_initialize USING  gs_variables-initiator
*                                       c_log_subobject-save
*                                       lwa_fpdoc-x-fphdr.
*    REFRESH: gt_fpbom_fcat, gt_fpbom_temp.
*    CLEAR: gs_unit.
*    PERFORM bom_items_display CHANGING lv_subrc.
*
*    IF lv_subrc EQ 0.
*
*      gs_variables-refresh_bom = c_true.
*      CALL SCREEN 406 STARTING AT 18 1 ENDING AT 120 16.
*
*    ENDIF.
*
*    PERFORM messages_display USING gs_variables-initiator.
**    gs_variables-refresh_items_grid = c_true.
*    EXIT.
*
*  ENDIF.

  popup_to_confirm TEXT-006 TEXT-007 c_true lv_answer.
  IF lv_answer EQ '1'.

    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save
                                      lwa_fpdoc-x-fphdr.

    LOOP AT lt_rows INTO lwa_rows.

      REFRESH: lt_dflow,lt_fphdr.
      CLEAR: lwa_cskey,lwa_fpitm,lwa_fpdoc.
      REFRESH: lt_messages.

      READ TABLE gt_taskorder ASSIGNING <lwa_taskorder> INDEX lwa_rows-index. "#EC CI_NOORDER
      MOVE-CORRESPONDING <lwa_taskorder> TO lwa_taskorder.

      PERFORM messages_context_set USING lwa_taskorder.
      PERFORM assginments_data_check USING lwa_taskorder
                                           lv_subrc.
      CHECK lv_subrc IS INITIAL.
      MOVE-CORRESPONDING <lwa_taskorder> TO lwa_cskey.
      lwa_fpitm-matnr    = p_matnr.
      lwa_fpitm-verid    = p_verid.
      lwa_fpitm-actdt    = <lwa_taskorder>-gstrp.
      lwa_fpitm-tomng    = <lwa_taskorder>-aarea.
      lwa_fpitm-meinh    = <lwa_taskorder>-meins.
      lwa_fpitm-confm    = c_true.
      lwa_cskey-tplnr_fl = <lwa_taskorder>-tplnr_fl.
      lwa_cskey-contr    = <lwa_taskorder>-contr.

      IF p_cpros IS NOT INITIAL.
        SELECT * FROM /agri/glcsdfl                  "#EC CI_SEL_NESTED
             INTO CORRESPONDING FIELDS OF TABLE lt_dflow
                  WHERE tplnr_fl EQ lwa_cskey-tplnr_fl
                    AND contr    EQ lwa_cskey-contr
                    AND autyp    EQ c_document_category-production_order.

        IF lt_dflow IS NOT INITIAL.
          SELECT * FROM /agri/fmfphdr "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_NESTED
            INTO CORRESPONDING FIELDS OF TABLE lt_fphdr
            FOR ALL ENTRIES IN lt_dflow            "#EC CI_NO_TRANSFORM
            WHERE aufnr EQ lt_dflow-aufnr
              AND cpros EQ p_cpros
              AND tecom EQ space.
          IF sy-subrc EQ 0.
            SORT lt_fphdr BY aufnr DESCENDING.
            READ TABLE lt_fphdr INTO lwa_fphdr INDEX 1. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_NOORDER
            IF sy-subrc EQ 0.
              lwa_fpitm-aufnr = lwa_fphdr-aufnr. "#EC CI_ALL_FIELDS_NEEDED
            ELSE.
              MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                       INTO sy-msgli.
              PERFORM zabs_message_simple.
              CONTINUE.
            ENDIF.
          ELSE.
            MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                     INTO sy-msgli.
            PERFORM zabs_message_simple.
            CONTINUE.
          ENDIF.
        ELSE.
          MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                   INTO sy-msgli.
          PERFORM zabs_message_simple.
          CONTINUE.
        ENDIF.
      ENDIF.

      CALL FUNCTION '/AGRI/FMFP_UP_TASK_CREATE'
        EXPORTING
*         I_SAVE_MESSAGES   = ' '
*         I_COMMIT_WORK     = 'X'
          i_matnr           = p_matnr
          is_cskey          = lwa_cskey
          is_fpitm          = lwa_fpitm
        IMPORTING
          es_fpdoc          = lwa_fpdoc
          et_messages       = lt_messages
        EXCEPTIONS
          inconsistent_data = 1
          no_data           = 2
          OTHERS            = 3.

      lv_subrc  = sy-subrc.
      LOOP AT lt_messages INTO ls_message.
        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
           NUMBER ls_message-msgno WITH ls_message-msgv1
           ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                      INTO sy-msgli.
        PERFORM zabs_message_simple.
      ENDLOOP.

      SORT lwa_fpdoc-x-fpitm BY posnr DESCENDING.
      READ TABLE lwa_fpdoc-x-fpitm INTO lwa_fpitm INDEX 1. "#EC CI_NOORDER
      <lwa_taskorder>-aufnr = lwa_fpitm-aufnr_to.

    ENDLOOP.
    PERFORM messages_display USING  gs_variables-initiator.
    gs_variables-refresh_items_grid = c_true. "27/10/2016
  ENDIF.

ENDFORM.                    "fcode_crea

*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_crca.

  DATA: lt_fphdr      TYPE /agri/t_fmfphdr,
        lt_dflow      TYPE /agri/t_glcsdfl,
        lwa_fpdoc     TYPE /agri/s_fmfp_doc,
        lwa_taskorder TYPE /agri/s_fmfp_uptask_mass,
        lwa_fpitm     TYPE /agri/s_fmfpitm,
        lwa_cskey     TYPE /agri/s_glcs_key,
        lwa_fphdr     TYPE /agri/s_fmfphdr.

  DATA: lt_rows            TYPE lvc_t_row,
        lt_messages        TYPE /agri/t_gprolog,
        lwa_rows           TYPE lvc_s_row,
        ls_message         TYPE /agri/s_gprolog,
        lwa_items_mod_rows LIKE LINE OF gt_items_mod_rows,
        lv_subrc           TYPE sysubrc,
        lv_valid,
        lv_answer.

  DATA: lv_lines TYPE sy-tabix,
        lv_cont  TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_taskorder> LIKE LINE OF gt_taskorder.

  CALL METHOD ref_assignments_grid->check_changed_data
    IMPORTING
      e_valid = lv_valid.

  IF lv_valid IS INITIAL.
    CLEAR ok_code.
    EXIT.
  ENDIF.

  CALL METHOD ref_assignments_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows IS INITIAL.
    MESSAGE e006(/agri/fmfp) INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  popup_to_confirm TEXT-006 TEXT-012 c_true lv_answer.
  IF lv_answer EQ '1'.

    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save
                                      lwa_fpdoc-x-fphdr.

    LOOP AT lt_rows INTO lwa_rows.

      REFRESH: lt_dflow,lt_fphdr.
      CLEAR: lwa_cskey,lwa_fpitm,lwa_fpdoc.
      REFRESH: lt_messages.

      READ TABLE gt_taskorder ASSIGNING <lwa_taskorder> INDEX lwa_rows-index. "#EC CI_NOORDER
      MOVE-CORRESPONDING <lwa_taskorder> TO lwa_taskorder.

      PERFORM messages_context_set USING lwa_taskorder.
      PERFORM assginments_data_check USING lwa_taskorder
                                           lv_subrc.
      CHECK lv_subrc IS INITIAL.
      MOVE-CORRESPONDING <lwa_taskorder> TO lwa_cskey.
      lwa_fpitm-matnr    = p_matnr.
      lwa_fpitm-verid    = p_verid.
      lwa_fpitm-actdt    = <lwa_taskorder>-gstrp.
      lwa_fpitm-tomng    = <lwa_taskorder>-aarea.
      lwa_fpitm-meinh    = <lwa_taskorder>-meins.
      lwa_fpitm-confm    = c_true.
      lwa_cskey-tplnr_fl = <lwa_taskorder>-tplnr_fl.
      lwa_cskey-contr    = <lwa_taskorder>-contr.

      IF p_cpros IS NOT INITIAL.
        SELECT * FROM /agri/glcsdfl              "#EC CI_SEL_NESTED2214
             INTO CORRESPONDING FIELDS OF TABLE lt_dflow
                  WHERE tplnr_fl EQ lwa_cskey-tplnr_fl
                    AND contr    EQ lwa_cskey-contr
                    AND autyp    EQ c_document_category-production_order.

        IF lt_dflow IS NOT INITIAL.
          SELECT * FROM /agri/fmfphdr "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_NESTED
            INTO CORRESPONDING FIELDS OF TABLE lt_fphdr
            FOR ALL ENTRIES IN lt_dflow            "#EC CI_NO_TRANSFORM
            WHERE aufnr EQ lt_dflow-aufnr
              AND cpros EQ p_cpros
              AND tecom EQ space.

          IF sy-subrc EQ 0.
            SORT lt_fphdr BY aufnr DESCENDING.
            READ TABLE lt_fphdr INTO lwa_fphdr INDEX 1. "#EC CI_ALL_FIELDS_NEEDED
            IF sy-subrc EQ 0.
              lwa_fpitm-aufnr = lwa_fphdr-aufnr. "#EC CI_ALL_FIELDS_NEEDED
            ELSE.
              MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                       INTO sy-msgli.
              PERFORM zabs_message_simple.
              CONTINUE.
            ENDIF.
          ELSE.
            MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                     INTO sy-msgli.
            PERFORM zabs_message_simple.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION '/AGRI/FMFP_UP_CAT_CREATE'
        EXPORTING
*         I_SAVE_MESSAGES   = ' '
*         I_COMMIT_WORK     = 'X'
          i_matnr           = p_matnr
          is_cskey          = lwa_cskey
          is_fpitm          = lwa_fpitm
          it_fpbom          = gt_fpbom_fcat
          it_fpbom_temp     = gt_fpbom_temp
          i_tomng           = gs_unit-tomng
        IMPORTING
          es_fpdoc          = lwa_fpdoc
          et_messages       = lt_messages
        EXCEPTIONS
          inconsistent_data = 1
          no_data           = 2
          OTHERS            = 3.

      lv_subrc  = sy-subrc.
      LOOP AT lt_messages INTO ls_message.

        IF ls_message-msgty EQ c_msg_type-error.
          lv_subrc  = 4.
        ENDIF.

        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
           NUMBER ls_message-msgno WITH ls_message-msgv1
           ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                      INTO sy-msgli.
        PERFORM zabs_message_simple.
      ENDLOOP.

      SORT lwa_fpdoc-x-fpitm BY posnr DESCENDING.
      READ TABLE lwa_fpdoc-x-fpitm INTO lwa_fpitm INDEX 1. "#EC CI_NOORDER
      <lwa_taskorder>-aufnr = lwa_fpitm-aufnr_to.

    ENDLOOP.
    PERFORM messages_display USING  gs_variables-initiator.
    IF lv_subrc EQ 0.
      gs_variables-refresh_items_grid = c_true.
      SET SCREEN 0.
    ENDIF.

  ENDIF.

ENDFORM.                    "fcode_crea


**&---------------------------------------------------------------------*
**&      Form  fcode_setval
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM fcode_setval.
*
*  DATA:    lv_save,
*           lv_where         TYPE string,
*           lt_rows          TYPE lvc_t_row,
*           lwa_selected_col TYPE lvc_s_col,
*           lt_selected_col  TYPE lvc_t_col,
*           lwa_selected_row TYPE lvc_s_row,
*           lt_selected_rows TYPE lvc_t_row,
*           lv_selected_rows TYPE boole_d.
*
*  CALL METHOD ref_assignments_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_selected_rows.
*
*  CALL METHOD ref_assignments_grid->get_selected_columns
*    IMPORTING
*      et_index_columns = lt_selected_col.
*
*  IF lt_selected_rows IS INITIAL.
*    popup_to_confirm text-002 text-003 c_true lv_save.
*    IF lv_save EQ '1'.
*
*    ELSEIF lv_save EQ '2'.
*      READ TABLE lt_selected_col INTO lwa_selected_col INDEX 1.
*      IF sy-subrc EQ 0 AND
*         lwa_selected_col-fieldname IS NOT INITIAL.
*        CONCATENATE lwa_selected_col-fieldname 'IS INITIAL'
*               INTO lv_where SEPARATED BY space.
*        LOOP AT gt_taskord_fcat TRANSPORTING NO FIELDS
*                              WHERE (lv_where).
*          lwa_selected_row-index = sy-tabix.
*          APPEND lwa_selected_row TO lt_selected_rows.
*        ENDLOOP.
*      ENDIF.
*      IF lt_selected_rows IS INITIAL.
*        EXIT.
*      ENDIF.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  lv_selected_rows = c_true.
*
*  CALL METHOD ref_items_grid->select_and_compute
*    EXPORTING
*      i_only_selected_rows = lv_selected_rows
**     i_init_container     =
**     it_selected_columns  =
**     i_show_dialog        = 'X'
**     i_apply_all_records  =
**     it_conditions        =
**     it_condition_columns =
*      it_selected_rows     = lt_selected_rows
**     is_select_n_compute  =
*    RECEIVING
*      rt_row_indexes       = lt_rows.
*
*  PERFORM item_data_update.
*
*ENDFORM.                    "fcode_setval

*&---------------------------------------------------------------------*
*&      Module  TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE title_set OUTPUT.

  PERFORM title_set.

ENDMODULE.                 " TITLE_SET  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM title_set .
****25/10/2016
*  SET TITLEBAR 'T001'.
  CASE sy-tcode.
    WHEN c_screen-bill_material.
      SET TITLEBAR 'T406'.
    WHEN OTHERS.
      SET TITLEBAR 'T100'.
  ENDCASE.
****25/10/2016

ENDFORM.                    " TITLE_SET

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TERRAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_terrain  USING  lv_tplnr TYPE /agri/gltplnr_fl.

  DATA: lt_tplnr  TYPE /agri/t_gltplnr,
        lwa_tplnr TYPE /agri/s_gltplnr.
  REFRESH: lt_tplnr.

  lwa_tplnr-tplnr_fl = lv_tplnr.
  APPEND lwa_tplnr TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLFL_PROCESS'
    EXPORTING
*     I_MODE        = 'A'
*     I_DISPLAY_ONLY = 'X'
      it_tplnr      = lt_tplnr
    EXCEPTIONS
      enter_terrain = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_TERRAIN

*&---------------------------------------------------------------------*
*&      Form  ORDER_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM order_display  USING lv_aufnr TYPE aufnr.

  DATA: lt_aufnr  TYPE /agri/t_fmaufnr,
        lwa_aufnr TYPE /agri/s_fmaufnr.
  REFRESH: lt_aufnr.

  lwa_aufnr-aufnr = lv_aufnr.
  APPEND lwa_aufnr TO lt_aufnr.

  CALL FUNCTION '/AGRI/FMCO_PROCESS'
    EXPORTING
*     I_MODE                        = 'A'
*     I_DISPLAY_ONLY                = 'X'
*     I_SAVE                        = 'X'
      it_aufnr                      = lt_aufnr
*     IT_FILTER                     =
*     IS_WSLINK                     =
    EXCEPTIONS
      enter_order_number            = 1
      invalid_parameter_combination = 2
      OTHERS                        = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ORDER_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection_validations .

  DATA: lv_cmnum  TYPE /agri/glcmnum,
        lwa_cmnum TYPE /agri/s_glcmnum,
        lt_cmnum  TYPE /agri/t_glcmnum.

  IF p_werks IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '059' INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  IF p_cmnum IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '060' INTO sy-msgli.
    PERFORM zabs_message_simple.
  ELSE.
    SELECT SINGLE cmnum
             FROM /agri/glcmhdr
             INTO lv_cmnum
            WHERE cmnum EQ p_cmnum.

    IF lv_cmnum IS INITIAL.
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
                 NUMBER '066' WITH p_cmnum INTO sy-msgli.
      PERFORM zabs_message_simple.
    ELSE.
      lwa_cmnum-cmnum = lv_cmnum.
      APPEND lwa_cmnum TO lt_cmnum.
    ENDIF.
  ENDIF.

  IF p_matnr IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '042' INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  IF p_date IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '061' INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  IF p_verid IS NOT INITIAL.

    READ TABLE gt_verid WITH KEY verid = p_verid.
    IF sy-subrc NE 0.
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
                 NUMBER '070' INTO sy-msgli.
      PERFORM zabs_message_simple.
    ENDIF.
  ENDIF.

  IF  p_basoq IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '071' INTO sy-msgli.
    PERFORM zabs_message_simple.
  ENDIF.

  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE gt_tplnr
   WHERE strno IN so_strno.

*  PERFORM valid_assignments_read USING p_date
*                                       lt_cmnum.

  PERFORM valid_assignments_read USING p_date.

ENDFORM.                    " SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_initialize  USING  lv_initiator TYPE /agri/gdescr
                                 lv_subobject TYPE balsubobj
                                 ls_fphdr     TYPE /agri/s_fmfphdr.
  messages_init.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

  CREATE OBJECT ref_log_handler.

  message_log_event_handler_set ref_log_handler
                                on_log_display_profile.


ENDFORM.                    " MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_CONTEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_context_set   USING lwa_orders TYPE /agri/s_fmfp_uptask_mass .

  DATA: lwa_context TYPE /agri/s_glcs_context.

  MOVE-CORRESPONDING lwa_orders TO lwa_context.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->context_data_get
    EXPORTING
      es_context_data = gs_context_data.

  gs_context_data-document        = lwa_orders-tplnr_fl.
  gs_context_data-docvariant      = lwa_orders-tplnr_fl.
  gs_context_data-context-tabname = '/AGRI/S_GLCS_CONTEXT'.
  gs_context_data-context-value   = lwa_context.

  gs_context_data-document        = lwa_orders-cmnum.
  gs_context_data-docvariant      = lwa_orders-cmnum.
  gs_context_data-context-tabname = '/AGRI/S_GLCS_CONTEXT'.
  gs_context_data-context-value   = lwa_context.

  CALL METHOD ref_process_log->context_data_set
    EXPORTING
      is_context_data = gs_context_data.

ENDFORM.                    " MESSAGES_CONTEXT_SET
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_display  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report = c_program_name.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CALL METHOD ref_process_log->context_data_set
    EXPORTING
      is_context_data = gs_context_data.
  CLEAR: gs_variables-initiator,gs_context_data.

ENDFORM.                    " MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CORP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_corp  USING lv_cmnum TYPE /agri/glcmnum .

  DATA: lt_cmnum  TYPE /agri/t_glcmnum,
        lwa_cmnum TYPE /agri/s_glcmnum.
  REFRESH: lt_cmnum.

  lwa_cmnum-cmnum = lv_cmnum.
  APPEND lwa_cmnum TO lt_cmnum.

  CALL FUNCTION '/AGRI/GLCM_PROCESS'
    EXPORTING
*     I_MODE                        = 'A'
*     I_DISPLAY_ONLY                = 'X'
*     I_SAVE                        = 'X'
      it_cmnum                      = lt_cmnum
*     IT_FILTER                     =
*     IS_WSLINK                     =
    EXCEPTIONS
      enter_crop_master             = 1
      invalid_parameter_combination = 2
      OTHERS                        = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_CORP
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_data_prepare  USING lt_flcma TYPE /agri/t_glflcma. "#EC CI_ALL_FIELDS_NEEDED

  DATA: lwa_taskorder TYPE /agri/s_fmfp_uptask_mass,
        lwa_flcma     TYPE /agri/glflcma.
*        lt_cskey      TYPE /agri/t_glcs_key,
*        lt_dflow      TYPE /agri/t_glcsdfl,
*        lwa_dflow     TYPE /agri/s_glcsdfl,
*        lt_fphdr      TYPE /agri/t_fmfphdr.

*  IF p_cpros IS NOT INITIAL.
*    SELECT * FROM /agri/glcsdfl
*         INTO CORRESPONDING FIELDS OF TABLE lt_dflow
*         FOR ALL ENTRIES IN lt_cskey
*              WHERE tplnr_fl EQ lt_cskey-tplnr_fl
*                AND contr    EQ lt_cskey-contr
*                AND autyp    EQ c_document_category-production_order.
*
*    IF lt_dflow IS NOT INITIAL.
*      SELECT * FROM /agri/fmfphdr
*        INTO CORRESPONDING FIELDS OF TABLE lt_fphdr
*        FOR ALL ENTRIES IN lt_dflow
*        WHERE aufnr EQ lt_dflow-aufnr
*          AND cpros EQ p_cpros
*          AND tecom EQ space.
*      IF sy-subrc NE 0.
**        MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
**                                 INTO sy-msgli.
**        message_simple space.
**        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  LOOP AT lt_flcma INTO lwa_flcma.            "#EC CI_ALL_FIELDS_NEEDED
    lwa_taskorder-gstrp     = sy-datum.
    lwa_taskorder-tplnr_fl  = lwa_flcma-tplnr_fl. "#EC CI_ALL_FIELDS_NEEDED
    lwa_taskorder-cmnum     = lwa_flcma-cmnum. "#EC CI_ALL_FIELDS_NEEDED
    lwa_taskorder-contr     = lwa_flcma-contr. "#EC CI_ALL_FIELDS_NEEDED
    lwa_taskorder-matnr     = p_matnr.
    lwa_taskorder-verid     = p_verid.

****25/10/2016 New configuration abspro workorder type only CAT material.
    PERFORM special_application_change CHANGING lwa_taskorder.

****25/10/2016

    IF p_basoq = 'A'." IS NOT INITIAL.
      lwa_taskorder-aarea   = lwa_flcma-aarea. "#EC CI_ALL_FIELDS_NEEDED
      lwa_taskorder-meins   = lwa_flcma-msehi. "#EC CI_ALL_FIELDS_NEEDED
    ELSEIF p_basoq = 'G'." IS NOT INITIAL.
      lwa_taskorder-aarea   = lwa_flcma-garea. "#EC CI_ALL_FIELDS_NEEDED
      lwa_taskorder-meins   = lwa_flcma-msehi. "#EC CI_ALL_FIELDS_NEEDED
    ELSEIF p_basoq = 'E'." IS NOT INITIAL.
      lwa_taskorder-aarea   = lwa_flcma-eston. "#EC CI_ALL_FIELDS_NEEDED
      lwa_taskorder-meins   = lwa_flcma-esuom. "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.
    APPEND lwa_taskorder TO gt_taskorder.
  ENDLOOP.

ENDFORM.                    " ASSIGNMENTS_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Module  ASSIGNMENTS_DATA_DISPALY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE assignments_data_dispaly OUTPUT.

  PERFORM assignments_data_dispaly.

ENDMODULE.                 " ASSIGNMENTS_DATA_DISPALY  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_DATA_DISPALY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_data_dispaly .

  DATA: lwa_taskorder      TYPE /agri/s_fmfp_uptask_mass,
        lwa_taskorder_fcat TYPE /agri/s_fmfp_uptask_mass_fcat,
        lwa_makt           TYPE makt,
        lwa_cmhdrt         TYPE /agri/glcmhdrt.


*  CHECK ref_assignments_grid IS INITIAL OR
*        gs_variables-refresh_items_grid IS NOT INITIAL.

  REFRESH: gt_taskorder_fcat.

  LOOP AT gt_taskorder INTO lwa_taskorder.
    MOVE-CORRESPONDING lwa_taskorder TO lwa_taskorder_fcat.
    key_text_get '/AGRI/GLCMHDRT' 'CMNUM' lwa_taskorder_fcat-cmnum
                 lwa_cmhdrt lwa_taskorder_fcat-descr.
    key_text_get 'MAKT' 'MATNR' lwa_taskorder_fcat-matnr
                 lwa_makt lwa_taskorder_fcat-maktx.
    APPEND lwa_taskorder_fcat TO gt_taskorder_fcat.
  ENDLOOP.

ENDFORM.                    " ASSIGNMENTS_DATA_DISPALY
*&---------------------------------------------------------------------*
*&      Module  ASSIGNMENTS_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE assignments_data_update INPUT.

  PERFORM assignments_data_update.

ENDMODULE.                 " ASSIGNMENTS_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_data_update .

  DATA: lwa_taskorder_fcat TYPE /agri/s_fmfp_uptask_mass_fcat,
        lwa_taskorder      TYPE /agri/s_fmfp_uptask_mass,
        lwa_fpdoc          TYPE /agri/s_fmfp_doc,
        lv_modified,
        lv_valid.

  FIELD-SYMBOLS: <lwa_taskorder> TYPE /agri/s_fmfp_uptask_mass.

  lv_modified = ref_assignments_grid->data_modified_check( ).

  IF lv_modified EQ c_true.
    CALL METHOD ref_assignments_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_items_grid = c_true.
    ENDIF.
  ENDIF.

  LOOP AT gt_taskorder_fcat INTO lwa_taskorder_fcat.
    MOVE-CORRESPONDING lwa_taskorder_fcat TO lwa_taskorder.
    UNASSIGN <lwa_taskorder>.
    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder>
                         WITH KEY tplnr_fl = lwa_taskorder-tplnr_fl
                                  cmnum    = lwa_taskorder-cmnum.
    IF <lwa_taskorder> IS ASSIGNED.
      IF lwa_taskorder NE <lwa_taskorder>.
        <lwa_taskorder> = lwa_taskorder.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ASSIGNMENTS_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  DROPDOWN_TABLES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_tables_display .

  DATA: lt_vrm_values  TYPE vrm_values,
        lwa_fmpodoc    TYPE /agri/s_glcs_doc,
        lwa_csprs      TYPE /agri/s_glcsprs,
        lwa_cmvar      TYPE /agri/glflcma,
        lwa_csprst     TYPE /agri/s_glcsprst,
        lwa_vrm_values TYPE vrm_value.

  STATICS: lt_cmvar TYPE TABLE OF /agri/glflcma,
           lv_cmnum TYPE /agri/glcmnum.


  IF p_cmnum IS NOT INITIAL AND
     p_cmnum NE lv_cmnum.

    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
      FROM /agri/glflcma
        INTO TABLE lt_cmvar
          WHERE cmnum = p_cmnum.
    lv_cmnum = p_cmnum.

  ENDIF.

  LOOP AT gt_csdoc_infocus INTO lwa_fmpodoc.

    lwa_vrm_values-key =  lwa_fmpodoc-x-cshdr-varia.
    READ TABLE lt_cmvar INTO lwa_cmvar        "#EC CI_ALL_FIELDS_NEEDED
                        WITH KEY varia = lwa_fmpodoc-x-cshdr-varia.
    IF sy-subrc EQ 0.
*      lwa_vrm_values-text = lwa_cmvar-descr.
    ENDIF.
    COLLECT lwa_vrm_values INTO lt_vrm_values.
    CLEAR lwa_vrm_values.
  ENDLOOP.

  dropdowns_fill lt_vrm_values 'P_VARIA'.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id              = 'P_VARIA'
*      values          = lt_vrm_values
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
  REFRESH: lt_vrm_values.

  IF p_varia IS NOT INITIAL.
    READ TABLE gt_csdoc_infocus INTO lwa_fmpodoc
                                WITH KEY x-cshdr-varia = p_varia." BINARY SEARCH.
    IF sy-subrc EQ 0.
      SORT lwa_fmpodoc-x-csprs BY seqnr.
      LOOP AT lwa_fmpodoc-x-csprs INTO lwa_csprs.
        lwa_vrm_values-key =  lwa_csprs-cpros.
        READ TABLE lwa_fmpodoc-x-csprst INTO lwa_csprst
                                        WITH KEY cpros = lwa_csprs-cpros
                                                 spras = sy-langu BINARY SEARCH.
        IF sy-subrc EQ 0.
          lwa_vrm_values-text = lwa_csprst-descr.
        ENDIF.
        APPEND lwa_vrm_values TO lt_vrm_values.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_CPROS'
      values          = lt_vrm_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF p_matnr IS NOT INITIAL.
    SELECT  verid FROM mkal
                 INTO TABLE gt_verid
                      WHERE werks EQ p_werks
                        AND matnr EQ p_matnr
                        AND bdatu GE sy-datum
                        AND adatu LE sy-datum.
  ENDIF.
ENDFORM.                    " DROPDOWN_TABLES_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  VALID_ASSIGNMENTS_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM valid_assignments_read  USING lv_date TYPE datab.
*                                   lt_cmnum TYPE /agri/t_glcmnum.

  DATA: lt_cskey  TYPE /agri/t_glcs_key.

*  IF lt_cmnum[] IS NOT INITIAL.
  IF gt_tplnr[] IS NOT INITIAL.
    SELECT * FROM /agri/glflcma
             INTO CORRESPONDING FIELDS OF TABLE lt_cskey
*              FOR ALL ENTRIES IN lt_cmnum
              FOR ALL ENTRIES IN gt_tplnr
*            WHERE tplnr_fl IN so_tplnr
            WHERE tplnr_fl EQ gt_tplnr-tplnr_fl
*              AND cmnum EQ lt_cmnum-cmnum
              AND cmnum EQ p_cmnum
              AND datab LE lv_date
              AND datbi GE lv_date
              AND class EQ c_application-farming
              AND astat EQ 'A'
              AND loevm EQ space.
  ENDIF.

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
      i_mode         = c_mode_display
      it_cskey       = lt_cskey
    IMPORTING
      et_csdoc       = gt_csdoc_infocus
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '002'
                             INTO sy-msgli.
    PERFORM zabs_message_simple.
    EXIT.
  ENDIF.

ENDFORM.                    " VALID_ASSIGNMENTS_READ
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify .

  DATA: lv_perform(30) TYPE c VALUE 'SCREEN_MODIFY_'.
  CONCATENATE lv_perform sy-dynnr INTO lv_perform.

  LOOP AT SCREEN.
    PERFORM (lv_perform) IN PROGRAM (c_program_name)
                         IF FOUND.
*    IF gs_variables-overview_mode EQ c_mode_display.
*      CHECK screen-group1 NE c_screen_group-display_only.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY

*&---------------------------------------------------------------------*
*&      Form  screen_modify_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_modify_1000.

  IF gt_csdoc_infocus IS NOT INITIAL.

    IF screen-name     EQ 'P_WERKS'.
      screen-input = 0.
    ELSEIF screen-name CS 'SO_TPLNR'.
      screen-input = 0.
    ELSEIF screen-name EQ 'P_CMNUM'.
      screen-input = 0.
    ELSEIF screen-name EQ 'P_DATE'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                    "screen_modify_1000
*&---------------------------------------------------------------------*
*&      Form  ASSGINMENTS_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assginments_data_check  USING lwa_taskorder TYPE /agri/s_fmfp_uptask_mass
                                   lv_subrc      TYPE sy-subrc.

  DATA: lwa_flcma TYPE /agri/s_glflcma.

  READ TABLE gt_flcma INTO lwa_flcma WITH KEY tplnr_fl = lwa_taskorder-tplnr_fl
                                              cmnum    = lwa_taskorder-cmnum
                                              contr    = lwa_taskorder-contr.
  IF sy-subrc EQ 0.
    CASE p_basoq.
      WHEN 'A'.
        IF lwa_taskorder-aarea GT lwa_flcma-aarea.
          lv_subrc = 4.
          MESSAGE e048(/agri/fmfp) INTO sy-msgli.
          PERFORM zabs_message_simple.
        ENDIF.
      WHEN 'G'.
        IF lwa_taskorder-aarea GT lwa_flcma-garea.
          lv_subrc = 4.
          MESSAGE e048(/agri/fmfp) INTO sy-msgli.
          PERFORM zabs_message_simple.
        ENDIF.
      WHEN 'E'.
        IF lwa_taskorder-aarea GT lwa_flcma-eston.
          lv_subrc = 4.
          MESSAGE e048(/agri/fmfp) INTO sy-msgli.
          PERFORM zabs_message_simple.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " ASSGINMENTS_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING    lv_activity
                      CHANGING lv_subrc TYPE sy-subrc.

  config_tcode_authority_check sy-tcode lv_activity lv_subrc.

  IF lv_subrc IS NOT INITIAL.
    CASE lv_activity.
      WHEN c_authorization_activity-change.
        MESSAGE ID '/AGRI/FMUT' TYPE 'S' NUMBER '008'
           INTO sy-msgli.
*      WHEN c_authorization_activity-create.
*        MESSAGE ID '/AGRI/FMUT' TYPE 'S' NUMBER '008'
*           INTO sy-msgli.
*      WHEN c_authorization_activity-display.
*        MESSAGE ID '/AGRI/FMUT' TYPE 'S' NUMBER '008'
*           INTO sy-msgli.
    ENDCASE.
    PERFORM zabs_message_simple.
  ENDIF.
*****Authorization check (badi)

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  SPECIAL_APPLICATION_CHANGE
*&---------------------------------------------------------------------*
*       text 25/10/2016
*----------------------------------------------------------------------*
*      -->P_LWA_TASKORDER_MATNR  text
*      <--P_LWA_TASKORDER_SAPPL  text
*----------------------------------------------------------------------*
FORM special_application_change CHANGING  lwa_taskorder TYPE /agri/s_fmfp_uptask_mass.

  DATA: lv_mtart      TYPE mtart,
        lv_mtart_temp TYPE mtart,
        ls_glflot     TYPE /agri/glflot.

*--terrain location
  SELECT SINGLE stort INTO lwa_taskorder-stort
  FROM /agri/glflot
  WHERE tplnr_fl = lwa_taskorder-tplnr_fl.

  SELECT SINGLE mtart
  FROM mara
  INTO lv_mtart_temp
  WHERE matnr EQ lwa_taskorder-matnr.

  IF lv_mtart_temp IS NOT INITIAL.

*--New configuration abspro workorder type only CAT material.

    SELECT SINGLE mtart
           FROM /agri/tfmwotma
           INTO lv_mtart
           WHERE mtart EQ lv_mtart_temp.                "#EC CI_NOORDER

    IF lv_mtart IS NOT INITIAL.

      lwa_taskorder-sappl = c_special_appl-chemical.

    ENDIF.

  ENDIF.



ENDFORM.                    " SPECIAL_APPLICATION_CHANGE
*&---------------------------------------------------------------------*
*&      Form  BOM_ITEMS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM bom_items_display CHANGING lv_subrc  TYPE sy-subrc.

*  DATA:  gt_fpbom_fcat      TYPE /agri/t_fmfpbom_fcat, "--10/10/2016

  TYPES: BEGIN OF ls_stors,
           stort TYPE /agri/glstort,
         END OF ls_stors.

  DATA: lv_lines  TYPE sy-subrc,
        lwa_fpitm TYPE /agri/s_fmfpitm,
        lwa_row   TYPE lvc_s_row,
        lt_rows   TYPE lvc_t_row.

  DATA: lt_stpo     TYPE TABLE OF stpo,
        lwa_stpo    TYPE stpo,
        lt_stas     TYPE TABLE OF stas,
        lt_mkal     TYPE TABLE OF mkal,
        lwa_mkal    TYPE mkal,
        lv_stlan    TYPE stlan,
        lv_stlnr    TYPE stnum,
        lv_stlal    TYPE stalt,
        lv_bmein    TYPE basme,
        lv_bmeng    TYPE basmn,
        lwa_fpbom   TYPE /agri/s_fmfpbom_fcat,
        ls_glflot   TYPE /agri/glflot,
        lt_fmcacxm  TYPE /agri/t_fmcacxm,
        lwa_fmcacxm TYPE /agri/s_fmcacxm,
        lwa_makt    TYPE makt,
        lwa_aufnr   TYPE /agri/s_fmaufnr,
        lt_aufnr    TYPE /agri/t_fmaufnr,
        lt_fpdoc    TYPE /agri/t_fmfp_doc,
        lwa_fpdoc   TYPE /agri/s_fmfp_doc.

  DATA: lt_fphdr  TYPE /agri/t_fmfphdr,
        lt_dflow  TYPE /agri/t_glcsdfl,
        lwa_fphdr TYPE /agri/s_fmfphdr,
        lt_stors  TYPE TABLE OF ls_stors.

  FIELD-SYMBOLS: <lwa_fpdoc>     TYPE /agri/s_fmfp_doc,
                 <lwa_fpitm>     TYPE /agri/s_fmfpitm,
                 <lwa_fpbom>     TYPE /agri/s_fmfpbom_fcat,
                 <lwa_taskorder> TYPE /agri/s_fmfp_uptask_mass.

  CALL METHOD ref_assignments_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  IF lt_rows IS INITIAL.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO lwa_row.

    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder>
                                 INDEX lwa_row-index.   "#EC CI_NOORDER

    IF sy-subrc EQ 0.

      APPEND <lwa_taskorder>-stort TO lt_stors.

      IF p_cpros IS NOT INITIAL.
        SELECT * FROM /agri/glcsdfl                  "#EC CI_SEL_NESTED
             INTO CORRESPONDING FIELDS OF TABLE lt_dflow
                  WHERE tplnr_fl EQ <lwa_taskorder>-tplnr_fl
                    AND contr    EQ <lwa_taskorder>-contr
                    AND autyp    EQ c_document_category-production_order.

        IF lt_dflow IS NOT INITIAL.
          SELECT * FROM /agri/fmfphdr "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_NESTED
            INTO CORRESPONDING FIELDS OF TABLE lt_fphdr
            FOR ALL ENTRIES IN lt_dflow            "#EC CI_NO_TRANSFORM
            WHERE aufnr EQ lt_dflow-aufnr
              AND cpros EQ p_cpros
              AND tecom EQ space.

          IF sy-subrc EQ 0.
            SORT lt_fphdr BY aufnr DESCENDING.
            READ TABLE lt_fphdr INTO lwa_fphdr INDEX 1. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_NOORDER
            IF sy-subrc EQ 0.
              CLEAR: lt_aufnr[], lt_aufnr.
              lwa_aufnr-aufnr = lwa_fphdr-aufnr. "#EC CI_ALL_FIELDS_NEEDED
              APPEND lwa_aufnr TO lt_aufnr.
            ELSE.
              lv_subrc = 4.
              MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                       INTO sy-msgli.
              PERFORM zabs_message_simple.
              CONTINUE.
            ENDIF.
          ELSE.
            lv_subrc = 4.
            MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                     INTO sy-msgli.
            PERFORM zabs_message_simple.

            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION '/AGRI/FMFP_VIEW'
        EXPORTING
          it_aufnr       = lt_aufnr
        IMPORTING
          et_fpdoc       = lt_fpdoc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.

      IF sy-subrc NE 0.
        lv_subrc = 4.
      ENDIF.
    ELSE.
      lv_subrc = 4.
      CONTINUE.
    ENDIF.

*    CHECK lv_subrc EQ 0.

    READ TABLE lt_fpdoc INTO lwa_fpdoc WITH KEY aufnr = lwa_aufnr-aufnr.

    CHECK sy-subrc EQ 0.

    IF <lwa_taskorder>-matnr IS NOT INITIAL AND
     lwa_fpdoc-x-fphdr-iwerk IS NOT INITIAL.

      gs_unit-tomng = gs_unit-tomng + <lwa_taskorder>-aarea. "--parameter screen 406
      gs_unit-meinh = <lwa_taskorder>-meins. "--parameter screen 406
      gs_unit-tplnr_fl = lwa_fpdoc-x-fphdr-tplnr_fl.
      gs_unit-iwerk =  lwa_fpdoc-x-fphdr-iwerk.
      gs_unit-cmnum =  lwa_fpdoc-x-fphdr-cmnum.

      IF <lwa_taskorder>-verid IS INITIAL.

        SELECT * INTO TABLE lt_mkal                  "#EC CI_SEL_NESTED
        FROM mkal
        WHERE matnr =  <lwa_taskorder>-matnr AND
              werks =  gs_unit-iwerk.

      ELSE.

        SELECT * INTO TABLE lt_mkal                  "#EC CI_SEL_NESTED
        FROM mkal
        WHERE matnr =  <lwa_taskorder>-matnr AND
              werks =  gs_unit-iwerk AND
              verid =  <lwa_taskorder>-verid.

      ENDIF.

      DESCRIBE TABLE lt_mkal LINES lv_lines.

      IF lv_lines GT 1.

        lv_subrc = 4.
***Extended additional syntax check 1_3 ATC 1709 PQ
        MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '0084'
*                                WITH lwa_aufnr-aufnr
                                INTO sy-msgli. "#EC*
*****
        PERFORM zabs_message_simple.
        CONTINUE.

      ENDIF.

**** CAT master data
      SELECT SINGLE * INTO ls_glflot                 "#EC CI_SEL_NESTED
        FROM /agri/glflot
        WHERE tplnr_fl = gs_unit-tplnr_fl.

      IF ls_glflot IS NOT INITIAL.

        SELECT * INTO TABLE lt_fmcacxm "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_NESTED
          FROM /agri/fmcacxm
          WHERE stort = ls_glflot-stort AND
                cmnum = gs_unit-cmnum.

      ENDIF.

****Material to BOM Link
      IF lt_mkal[] IS NOT INITIAL.

        READ TABLE lt_mkal INTO lwa_mkal INDEX 1.       "#EC CI_NOORDER
        IF sy-subrc EQ 0.
          SELECT SINGLE stlnr stlal INTO (lv_stlnr, lv_stlal) "#EC CI_SEL_NESTED
          FROM mast
          WHERE matnr =  <lwa_taskorder>-matnr AND      "#EC CI_NOORDER
                werks =  gs_unit-iwerk AND
                stlan = lwa_mkal-stlan AND
                stlal = lwa_mkal-stlal.
        ENDIF.
      ELSE.
        SELECT SINGLE stlnr stlal INTO (lv_stlnr, lv_stlal) "#EC CI_SEL_NESTED
        FROM mast
        WHERE matnr = <lwa_taskorder>-matnr AND         "#EC CI_NOORDER
              werks =  gs_unit-iwerk.
      ENDIF.

      IF lv_stlnr IS NOT INITIAL AND lv_stlal IS NOT INITIAL.

****BOM Header
        SELECT SINGLE bmein bmeng INTO (lv_bmein, lv_bmeng) "#EC CI_SEL_NESTED
        FROM stko
        WHERE stlnr = lv_stlnr AND                      "#EC CI_NOORDER
              stlal = lv_stlal AND
              lkenz = c_false.

****BOMs - Item Selection
        SELECT * INTO TABLE lt_stas                  "#EC CI_SEL_NESTED
        FROM stas
        WHERE stlnr = lv_stlnr AND
              stlal = lv_stlal AND
              lkenz = c_false.

        IF lt_stas[] IS NOT INITIAL.
****BOM item
          SELECT * INTO TABLE lt_stpo                "#EC CI_SEL_NESTED
            FROM stpo
            FOR ALL ENTRIES IN lt_stas             "#EC CI_NO_TRANSFORM
            WHERE stlnr = lv_stlnr AND
                  stlkn = lt_stas-stlkn AND
                  lkenz = c_false.

          IF lt_stpo[] IS NOT INITIAL.
            LOOP AT lt_stpo INTO lwa_stpo.
              lwa_fpbom-matnr = lwa_stpo-idnrk.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lwa_fpbom-matnr
                IMPORTING
                  output = lwa_fpbom-matnr.

              READ TABLE lt_fmcacxm INTO lwa_fmcacxm WITH KEY matnr = lwa_fpbom-matnr. "#EC CI_ALL_FIELDS_NEEDED

              IF sy-subrc EQ 0.

                lwa_fpbom-werks = gs_unit-iwerk.
                lwa_fpbom-unit = lwa_stpo-meins.
                lwa_fpbom-lgort = lwa_stpo-lgort.
                lwa_fpbom-maxperarea = lwa_fmcacxm-maxperarea. "#EC CI_ALL_FIELDS_NEEDED
                lwa_fpbom-rate  = lwa_stpo-menge. "Rate Material BOM
                lwa_fpbom-postp = lwa_stpo-postp.
                key_text_get 'MAKT' 'MATNR' lwa_fpbom-matnr
                              lwa_makt lwa_fpbom-maktx.

                IF lwa_fpbom-lgort IS INITIAL.
                  PERFORM storage_location_change USING lwa_fpbom-matnr
                                                  CHANGING lwa_fpbom-lgort.
                ENDIF.
**** calculate rate
                IF lv_bmeng IS NOT INITIAL.
                  lwa_fpbom-rate = lwa_fpbom-rate / lv_bmeng.
                ENDIF.
***** calculate plan quantity
*                IF <lwa_taskorder>-aarea IS NOT INITIAL.
*                  lwa_fpbom-plan_qty =  lwa_fpbom-rate * <lwa_taskorder>-aarea.
*                ENDIF.
                APPEND lwa_fpbom TO gt_fpbom_fcat.
              ELSE.
                IF lwa_stpo-postp NE 'L'.
                  lwa_fpbom-werks = gs_unit-iwerk.
                  lwa_fpbom-unit = lwa_stpo-meins.
                  lwa_fpbom-lgort = lwa_stpo-lgort.
                  lwa_fpbom-rate  = lwa_stpo-menge. "Rate Material BOM
                  lwa_fpbom-postp = lwa_stpo-postp.
                  IF lwa_fpbom-lgort IS INITIAL.
                    PERFORM storage_location_change USING lwa_fpbom-matnr
                                                    CHANGING lwa_fpbom-lgort.
                  ENDIF.
                  APPEND lwa_fpbom TO gt_fpbom_temp.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_fpbom_fcat BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_fpbom_fcat COMPARING matnr.

  LOOP AT gt_fpbom_fcat ASSIGNING <lwa_fpbom>.
**** calculate plan quantity
    IF gs_unit-tomng IS NOT INITIAL.
      <lwa_fpbom>-plan_qty =  <lwa_fpbom>-rate * gs_unit-tomng.
    ENDIF.
  ENDLOOP.

  IF lt_stors[] IS NOT INITIAL.
    CLEAR: lv_lines.
    DELETE ADJACENT DUPLICATES FROM lt_stors COMPARING stort.
    DESCRIBE TABLE lt_stors LINES lv_lines.
    IF lv_lines GT 1.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '0089'
                              INTO sy-msgli.
      PERFORM zabs_message_simple.
    ENDIF.

  ENDIF.


ENDFORM.                    " BOM_ITEMS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  storage_location_change
*&---------------------------------------------------------------------*
*       text 25/10/2016
*----------------------------------------------------------------------*
*      -->LV_MATNR   text
*      -->LV_LGORT   text
*----------------------------------------------------------------------*
FORM storage_location_change  USING lv_matnr TYPE matnr
                              CHANGING lv_lgort TYPE lgort_d.

  SELECT SINGLE lgort INTO lv_lgort
    FROM mard
    WHERE matnr = lv_matnr                              "#EC CI_NOORDER
      AND werks = gs_unit-iwerk.

ENDFORM.                    " STORAGE_LOCATION_CHANGE
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_CAT_VALUES_GET
*&---------------------------------------------------------------------*
*       text 26/10/2016
*----------------------------------------------------------------------*
*      <--P_LV_MATNR  text
*----------------------------------------------------------------------*
FORM material_cat_values_get USING lv_tabix TYPE sy-tabix
                             CHANGING lv_matnr TYPE matnr.

  DATA: lt_return   TYPE STANDARD TABLE OF ddshretval,
        lwa_return  TYPE ddshretval,
        lt_glflot   TYPE TABLE OF /agri/glflot,
        ls_glflot   TYPE /agri/glflot,
        lt_fmcacxm  TYPE /agri/t_fmcacxm,
        lwa_fmcacxm TYPE /agri/s_fmcacxm.

  IF gt_taskorder IS NOT INITIAL.
    SELECT * INTO TABLE lt_glflot             "#EC CI_FAE_LINES_ENSURED
       FROM /agri/glflot
       FOR ALL ENTRIES IN gt_taskorder
       WHERE tplnr_fl = gt_taskorder-tplnr_fl.
  ENDIF.

  IF lt_glflot IS NOT INITIAL.

    SELECT * INTO TABLE lt_fmcacxm                 "#EC CI_NO_TRANSFORM
      FROM /agri/fmcacxm
      FOR ALL ENTRIES IN lt_glflot
      WHERE stort = lt_glflot-stort AND
            cmnum = gs_unit-cmnum.

  ENDIF.

  CHECK lt_fmcacxm IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'MATNR'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_fmcacxm
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE lt_return INTO lwa_return WITH  KEY retfield = 'MATNR'.
  IF sy-subrc = 0.
    lv_matnr = lwa_return-fieldval.
  ENDIF.

ENDFORM.                    " MATERIAL_CAT_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  bacth_cat_values_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_TABIX   text
*      -->LV_CHARG   text
*----------------------------------------------------------------------*
FORM bacth_cat_values_get  USING    lv_tabix TYPE sy-tabix
                           CHANGING lv_charg TYPE charg_d.

  TYPES: BEGIN OF ls_charg,
           charg TYPE charg_d,
         END OF ls_charg.

  DATA: lt_return  TYPE STANDARD TABLE OF ddshretval,
        lwa_return TYPE ddshretval,
        ls_glflot  TYPE /agri/glflot,
        lt_charg   TYPE STANDARD TABLE OF ls_charg,
        lwa_style  TYPE lvc_s_styl,
        lwa_fpbom  TYPE /agri/s_fmfpbom_fcat.


  READ TABLE gt_fpbom_fcat INTO lwa_fpbom INDEX lv_tabix. "#EC CI_NOORDER

  CHECK sy-subrc EQ 0.
*
*  READ TABLE lwa_fpbom-styles INTO lwa_style WITH KEY
*                                     fieldname = 'CHARG'.
*  CHECK sy-subrc NE 0.

  SELECT charg INTO TABLE lt_charg
    FROM mchb
    WHERE matnr = lwa_fpbom-matnr
      AND werks = lwa_fpbom-werks.

  CHECK lt_charg IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CHARG'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CHARG'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_charg
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE lt_return INTO lwa_return WITH  KEY retfield = 'CHARG'.
  IF sy-subrc = 0.
    lv_charg = lwa_return-fieldval.
  ENDIF.

ENDFORM.                    " BACTH_CAT_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  STORAGE_CAT_VALUES_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*      <--P_LV_LGORT  text
*----------------------------------------------------------------------*
FORM storage_cat_values_get  USING    lv_tabix TYPE sy-tabix
                             CHANGING lv_lgort TYPE lgort_d.


  TYPES: BEGIN OF ls_lgort,
           lgort TYPE lgort_d,
         END OF ls_lgort.

  DATA: lt_return  TYPE STANDARD TABLE OF ddshretval,
        lwa_return TYPE ddshretval,
        ls_glflot  TYPE /agri/glflot,
        lt_lgort   TYPE STANDARD TABLE OF ls_lgort,
        lwa_style  TYPE lvc_s_styl,
        lwa_fpbom  TYPE /agri/s_fmfpbom_fcat.


  READ TABLE gt_fpbom_fcat INTO lwa_fpbom INDEX lv_tabix. "#EC CI_NOORDER

  CHECK sy-subrc EQ 0.

  SELECT lgort INTO TABLE lt_lgort
    FROM mard
    WHERE matnr = lwa_fpbom-matnr
      AND werks = lwa_fpbom-werks.

  CHECK lt_lgort IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CHARG'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'LGORT'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_lgort
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE lt_return INTO lwa_return WITH  KEY retfield = 'LGORT'.
  IF sy-subrc = 0.
    lv_lgort = lwa_return-fieldval.
  ENDIF.

ENDFORM.                    " STORAGE_CAT_VALUES_GET
*&---------------------------------------------------------------------*
*&      Module  BOM_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bom_data_display OUTPUT.
  PERFORM bom_data_display.
ENDMODULE.                 " BOM_DATA_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BOM_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_data_display .


  FIELD-SYMBOLS: <lwa_fpbom>       TYPE /agri/s_fmfpbom_fcat.

  CHECK: ref_container_bom IS INITIAL OR
         gs_variables-refresh_bom IS NOT INITIAL.


  LOOP AT gt_fpbom_fcat ASSIGNING <lwa_fpbom>.

    PERFORM bom_styles_prepare CHANGING <lwa_fpbom>.

  ENDLOOP.


ENDFORM.                    " BOM_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BOM_STYLES_PREPARE
*&---------------------------------------------------------------------*
*       text 26/10/2016
*----------------------------------------------------------------------*
*      <--P_<LWA_FPBOM>  text
*----------------------------------------------------------------------*
FORM bom_styles_prepare  CHANGING lwa_fpbom TYPE /agri/s_fmfpbom_fcat.

  DATA: lwa_style TYPE lvc_s_styl,
        lwa_edit  TYPE lvc_s_styl,
        lv_charg  TYPE charg_d.

  REFRESH: lwa_fpbom-styles.

  IF lwa_fpbom-matnr IS NOT INITIAL.

    IF lwa_fpbom-werks IS NOT INITIAL.

      SELECT SINGLE charg INTO lv_charg
        FROM mchb
        WHERE matnr = lwa_fpbom-matnr                   "#EC CI_NOORDER
        AND werks = lwa_fpbom-werks.

      IF lv_charg IS INITIAL.
        lwa_style-fieldname = 'CHARG'.
        lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT lwa_style INTO TABLE lwa_fpbom-styles.
      ENDIF.

    ENDIF.

    lwa_style-fieldname = 'MAKTX'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpbom-styles.

*    lwa_style-fieldname = 'LGORT'.
*    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*    INSERT lwa_style INTO TABLE lwa_fpbom-styles.

    lwa_style-fieldname = 'MEINS'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpbom-styles.


  ENDIF.

  IF gs_variables-refresh_colum_bom EQ c_false.

    lwa_style-fieldname = 'PLAN_QTY'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpbom-styles.

  ELSE.

    lwa_style-fieldname = 'RATE'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpbom-styles.

  ENDIF.

ENDFORM.                    " BOM_STYLES_PREPARE

*&---------------------------------------------------------------------*
*&      Form  controls_display_0406
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM controls_display_0406.

  DATA: lv_title 	          TYPE lvc_title,
        lv_itm_indx         TYPE i,
        lv_itm_indx_tmp(20) TYPE n,
        lv_tot_itms(20)     TYPE n,
        ls_layout           TYPE lvc_s_layo,
        lv_input            TYPE i,
        ls_variant          TYPE disvariant.

  DATA: lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions.

  DATA: ls_row_info           TYPE lvc_s_row,
        ls_col_info           TYPE lvc_s_col,
        ls_rowid_current_cell TYPE lvc_s_row,
        ls_colid_current_cell TYPE lvc_s_col,
        ls_environment        TYPE /agri/s_glvc_environment.

*  PERFORM bom_items_display.

  IF ref_container_bom IS INITIAL.

    CREATE OBJECT ref_container_bom
      EXPORTING
        container_name              = '/AGRI/SAPLFMFPM_0406_CC'
        repid                       = c_program_name-task_orders
        dynnr                       = c_screen-bill_material
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

    ls_environment-switchoff_performance = c_true.
    CREATE OBJECT ref_grid_bom
      EXPORTING
        i_parent           = ref_container_bom
        is_lvc_environment = ls_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    PERFORM control_events_register.

    PERFORM field_catalog_prepare  USING c_structure_name-bill_material
                                CHANGING lt_fcat.

    ls_layout-info_fname = 'DDHDL'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.
    ls_layout-sel_mode   = 'D'.
    ls_layout-smalltitle = c_true.

    ls_variant-report = c_program_name-task_orders.
    ls_variant-handle = c_screen-bill_material.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes
                                     USING c_structure_name-bill_material.

    CALL METHOD ref_grid_bom->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = gt_fpbom_fcat
        it_fieldcatalog               = lt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD ref_grid_bom->set_toolbar_interactive.

    CLEAR: gs_variables-refresh_bom.

  ELSEIF gs_variables-refresh_bom NE space.

    CLEAR gs_variables-refresh_bom.

    PERFORM field_catalog_prepare  USING c_structure_name-bill_material
                                    CHANGING lt_fcat.

    ls_layout-sel_mode = 'A'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = c_true.
    ls_layout-info_fname = 'ROWCOLOR'.
    ls_layout-smalltitle = c_true.
    ls_layout-grid_title = TEXT-038.

    CALL METHOD ref_grid_bom->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

    CALL METHOD ref_grid_bom->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat.

    CALL METHOD ref_grid_bom->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_input = 0.
  ELSE.
    lv_input = 1.
  ENDIF.

  CALL METHOD ref_grid_bom->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

ENDFORM.                    "controls_display_0406
*&---------------------------------------------------------------------*
*&      Form  control_events_register_0406
*&---------------------------------------------------------------------*
*       text 24/10/2016
*----------------------------------------------------------------------*
FORM control_events_register_0406.

  DATA: lwa_f4 TYPE lvc_s_f4,
        lt_f4  TYPE lvc_t_f4.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  IF ref_grid_bom IS NOT INITIAL.
    CALL METHOD ref_grid_bom->register_edit_event
      EXPORTING
        i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ENDIF.


****Register the F4 event for grid
  lwa_f4-fieldname = 'CHARG'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.
  lwa_f4-fieldname = 'MATNR'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.
  lwa_f4-fieldname = 'LGORT'.
  lwa_f4-register = c_true.
  INSERT lwa_f4 INTO TABLE lt_f4.

  CALL METHOD ref_grid_bom->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  SET HANDLER: ref_event_handler->on_toolbar_grid
               FOR ref_grid_bom,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_bom,
               ref_event_handler->on_data_changed
               FOR ref_grid_bom,
               ref_event_handler->on_f4_request_grid
               FOR ref_grid_bom.

ENDFORM.                    "control_events_register_0406
*&---------------------------------------------------------------------*
*&      Module  ITEMS_TOMNG_CHANGE  INPUT
*&---------------------------------------------------------------------*
*       text 26/10/2016
*----------------------------------------------------------------------*
MODULE items_tomng_change INPUT.
  PERFORM items_tomng_change.
ENDMODULE.                 " ITEMS_TOMNG_CHANGE  INPUT
*&---------------------------------------------------------------------*
*&      Form  ITEMS_TOMNG_CHANGE
*&---------------------------------------------------------------------*
*       text 26/10/2016
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM items_tomng_change.

  DATA: lv_modified,
        lv_valid,
        lwa_fpcom       TYPE /agri/s_fmfpcom,
        lwa_fpitm       TYPE /agri/s_fmfpitm,
        lwa_fpitm_ofcat TYPE /agri/s_fmfpitm_fcat,
        lwa_row         TYPE lvc_s_row,
        lt_rows         TYPE lvc_t_row,
        lv_aarea        TYPE /agri/glqunt,
        lv_dif          TYPE /agri/glqunt.


  DATA: lv_lines TYPE sy-tabix,
        lv_cont  TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_fpbom>     TYPE /agri/s_fmfpbom_fcat,
                 <lwa_fpitm>     TYPE /agri/s_fmfpitm,
                 <lwa_taskorder> TYPE /agri/s_fmfp_uptask_mass.

  CALL METHOD ref_assignments_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  IF lt_rows IS INITIAL.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_rows LINES lv_lines.
  lv_aarea = gs_unit-tomng.
  lv_aarea = lv_aarea / lv_lines.
  lv_dif = gs_unit-tomng - ( lv_aarea * lv_lines ).
  lv_dif = abs( lv_dif ).


  LOOP AT lt_rows INTO lwa_row.
    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder>
                                 INDEX lwa_row-index.   "#EC CI_NOORDER
    IF sy-subrc EQ 0.
      <lwa_taskorder>-aarea = lv_aarea + lv_dif.
      lv_dif = 0.
*      gs_variables-document_changed = c_true.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_fpbom_fcat ASSIGNING <lwa_fpbom>.

    IF gs_unit-tomng IS NOT INITIAL.
      <lwa_fpbom>-plan_qty = gs_unit-tomng * <lwa_fpbom>-rate.
      <lwa_fpbom>-rate =  <lwa_fpbom>-plan_qty / gs_unit-tomng.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ITEMS_TOMNG_CHANGE
*&---------------------------------------------------------------------*
*&      Module  BOM_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bom_data_update INPUT.
  PERFORM bom_data_update.
ENDMODULE.                 " BOM_DATA_UPDATE  INPUT
*&---------------------------------------------------------------------*
*&      Form  BOM_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_data_update.

  DATA: lv_modified,
        lwa_fpdoc   TYPE /agri/s_fmfp_doc,
        lv_valid,
        lwa_mod_row LIKE LINE OF gt_bom_mod_rows.

  FIELD-SYMBOLS: <lwa_fpbom>       TYPE /agri/s_fmfpbom_fcat.


  CHECK gs_variables-document_mode NE c_mode_display AND
        ok_code NE c_fcode-insert_bom AND
        ok_code NE c_fcode-delete_bom  AND
        ok_code NE c_fcode-change_colum_bom.

*  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING  gs_variables-initiator
                                     c_log_subobject-save
                                     lwa_fpdoc-x-fphdr.

  lv_modified = ref_grid_bom->data_modified_check( ).
  IF lv_modified EQ c_true OR
     gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_grid_bom->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_bom = c_true.
    ENDIF.
  ENDIF.

  LOOP AT gt_bom_mod_rows INTO lwa_mod_row.
    READ TABLE gt_fpbom_fcat ASSIGNING <lwa_fpbom>
                                 INDEX lwa_mod_row-row_id. "#EC CI_NOORDER
    IF sy-subrc EQ 0.
*      PERFORM bom_data_check USING <lwa_fpbom>.
      PERFORM bom_data_change USING lwa_mod_row-fieldname
                           CHANGING <lwa_fpbom>.
    ENDIF.
    DELETE gt_bom_mod_rows WHERE row_id EQ lwa_mod_row-row_id.
  ENDLOOP.

  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.                    " BOM_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  BOM_DATA_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_MOD_ROW_FIELDNAME  text
*      <--P_<LWA_FPBOM>  text
*----------------------------------------------------------------------*
FORM bom_data_change  USING lv_fieldname
                      CHANGING lwa_bom TYPE /agri/s_fmfpbom_fcat.

  DATA: ls_glflot  TYPE /agri/glflot,
        lt_glflot  TYPE TABLE OF /agri/glflot,
        lt_fmcacxm TYPE /agri/t_fmcacxm,
        ls_fmcacxm TYPE /agri/s_fmcacxm,
        lwa_makt   TYPE makt,
        lv_lgort   TYPE lgort_d.

  DATA: lt_fmcaim      TYPE TABLE OF /agri/fmcaim,
        lwa_fmcaim     TYPE /agri/fmcaim,
        lv_lines       TYPE sy-tabix,
        lwa_messages   TYPE /agri/s_gprolog,
        lwa_fpbom_fcat TYPE /agri/s_fmfpbom_fcat.

  CASE lv_fieldname.
    WHEN 'MATNR'.
      IF lwa_bom-matnr IS NOT INITIAL.
        IF gt_taskorder IS NOT INITIAL.
          SELECT * INTO TABLE lt_glflot       "#EC CI_FAE_LINES_ENSURED
          FROM /agri/glflot
          FOR ALL ENTRIES IN gt_taskorder
          WHERE tplnr_fl = gt_taskorder-tplnr_fl.
        ENDIF.

        IF lt_glflot IS NOT INITIAL.
          SELECT * INTO TABLE lt_fmcacxm           "#EC CI_NO_TRANSFORM
            FROM /agri/fmcacxm
            FOR ALL ENTRIES IN lt_glflot
            WHERE stort = lt_glflot-stort AND
                  cmnum = gs_unit-cmnum AND
                  matnr = lwa_bom-matnr.

          IF lt_fmcacxm IS NOT INITIAL.

            READ TABLE lt_fmcacxm INTO ls_fmcacxm WITH KEY matnr = lwa_bom-matnr
                                                           cmnum = gs_unit-cmnum.
            IF sy-subrc EQ 0.

              MOVE-CORRESPONDING ls_fmcacxm TO lwa_bom.
              lwa_bom-werks = gs_unit-iwerk.
              key_text_get 'MAKT' 'MATNR' lwa_bom-matnr
                                lwa_makt lwa_bom-maktx.
*              PERFORM storage_location_change USING lwa_bom-matnr
*                                           CHANGING lwa_bom-lgort.

              IF gs_unit-tomng IS NOT INITIAL.
                lwa_bom-plan_qty = gs_unit-tomng * lwa_bom-rate.
                IF lwa_bom-rate > lwa_bom-maxperarea.
                  MESSAGE e091(/agri/fmfp)
                  WITH lwa_bom-matnr
                  INTO sy-msgli.
                  PERFORM zabs_message_simple.
                  CLEAR: lwa_bom-rate, lwa_bom-plan_qty.
                ENDIF.
              ENDIF.

            ENDIF.

          ELSE.

            MESSAGE e085(/agri/fmfp)
            WITH gs_unit-cmnum
                 ls_glflot-stort
            INTO sy-msgli.
            PERFORM zabs_message_simple.
            CLEAR: lwa_bom-matnr.
            EXIT.

          ENDIF.
        ELSE.
***Extended additional syntax check 1_3 ATC 1709 PQ
          MESSAGE e085(/agri/fmfp)
*          WITH gs_unit-tplnr_fl
          INTO sy-msgli. "#EC*
****
          PERFORM zabs_message_simple.
          CLEAR: lwa_bom-matnr.
          EXIT.
        ENDIF.


****imcompatibility materials
        SELECT * INTO TABLE lt_fmcaim
        FROM /agri/fmcaim
        WHERE matnr = lwa_bom-matnr
           OR matn1 = lwa_bom-matnr.

        DESCRIBE TABLE gt_fpbom_fcat LINES lv_lines.

        IF lv_lines GT 1.

          LOOP AT lt_fmcaim INTO lwa_fmcaim.

            READ TABLE gt_fpbom_fcat INTO lwa_fpbom_fcat WITH KEY matnr = lwa_fmcaim-matnr.
            IF sy-subrc EQ 0.
              READ TABLE gt_fpbom_fcat INTO lwa_fpbom_fcat WITH KEY matnr = lwa_fmcaim-matn1.
              IF sy-subrc EQ 0.

                CASE lwa_fmcaim-itype.
                  WHEN 'L'.
                    lwa_messages-msgv1 = TEXT-048.
                  WHEN 'M'.
                    lwa_messages-msgv1 = TEXT-049.
                  WHEN 'H'.
                    lwa_messages-msgv1 = TEXT-050.
                  WHEN OTHERS.
                ENDCASE.

                CASE lwa_fmcaim-msgty.
                  WHEN c_msg_type-error.

                    MESSAGE ID '/AGRI/FMFP'
                    TYPE c_msg_type-error
                    NUMBER 093
                    WITH lwa_fmcaim-matnr
                         lwa_messages-msgv1
                         lwa_fmcaim-matn1
                    INTO sy-msgli.                    "#EC CI_FLDEXT_OK
                    PERFORM zabs_message_simple.
                    CLEAR: lwa_bom-matnr.
                    EXIT.                               "#EC CI_NOORDER

                  WHEN OTHERS.

                    MESSAGE ID '/AGRI/FMFP'
                    TYPE lwa_fmcaim-msgty
                    NUMBER 093
                    WITH lwa_fmcaim-matnr
                         lwa_messages-msgv1
                         lwa_fmcaim-matn1
                    INTO sy-msgli.                    "#EC CI_FLDEXT_OK
                    PERFORM zabs_message_simple.

                ENDCASE.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.
    WHEN 'RATE'.
      IF gs_unit-tomng IS NOT INITIAL.
        IF lwa_bom-rate > lwa_bom-maxperarea.
          MESSAGE e091(/agri/fmfp)
          WITH lwa_bom-matnr
          INTO sy-msgli.
          PERFORM zabs_message_simple.
          CLEAR: lwa_bom-rate, lwa_bom-plan_qty.
          EXIT.
        ENDIF.
        lwa_bom-plan_qty = gs_unit-tomng * lwa_bom-rate.
      ENDIF.
    WHEN 'PLAN_QTY'.
      IF gs_unit-tomng IS NOT INITIAL.
        lwa_bom-rate = lwa_bom-plan_qty / gs_unit-tomng.
        IF lwa_bom-rate > lwa_bom-maxperarea.
          MESSAGE e091(/agri/fmfp)
          WITH lwa_bom-matnr
          INTO sy-msgli.
          PERFORM zabs_message_simple.
          CLEAR: lwa_bom-rate, lwa_bom-plan_qty.
          EXIT.
        ENDIF.
      ENDIF.
    WHEN 'LGORT'.
      IF lwa_bom-matnr IS NOT INITIAL AND
         lwa_bom-lgort IS NOT INITIAL.

        SELECT SINGLE lgort INTO lv_lgort
        FROM mard
        WHERE matnr = lwa_bom-matnr
        AND werks = lwa_bom-werks
        AND lgort = lwa_bom-lgort.

        IF lv_lgort IS INITIAL.

          MESSAGE e090(/agri/fmfp)
           WITH lwa_bom-lgort
                lwa_bom-matnr
           INTO sy-msgli.
          PERFORM zabs_message_simple.
          CLEAR: lwa_bom-lgort.
          EXIT.

        ENDIF.

      ELSE.
        MESSAGE e088(/agri/fmfp)
            INTO sy-msgli.
        PERFORM zabs_message_simple.
        CLEAR: lwa_bom-lgort.
        EXIT.

      ENDIF.
  ENDCASE.

ENDFORM.                    " BOM_DATA_CHANGE

*&---------------------------------------------------------------------*
*&      Form  fcode_bins
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_bins.

  DATA: lwa_fpbom_fcat TYPE /agri/s_fmfpbom_fcat,
        lv_lines       TYPE sy-tabix.

  DESCRIBE TABLE gt_fpbom_fcat LINES lv_lines.
  APPEND lwa_fpbom_fcat TO gt_fpbom_fcat.

  CALL METHOD ref_grid_bom->tables_display_refresh
    CHANGING
      it_outtab = gt_fpbom_fcat.

  gs_variables-refresh_bom = c_true.

ENDFORM.                    "fcode_bins
*&---------------------------------------------------------------------*
*&      Form  fcode_bdel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM  fcode_bdel.

  DATA: lwa_fpbom_fcat TYPE /agri/s_fmfpbom_fcat,
        lwa_row        TYPE lvc_s_row,
        lt_rows        TYPE lvc_t_row.

  CALL METHOD ref_grid_bom->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.

  LOOP AT lt_rows INTO lwa_row.

    DELETE gt_fpbom_fcat INDEX lwa_row-index.           "#EC CI_NOORDER

  ENDLOOP.
  gs_variables-refresh_bom = c_true.

ENDFORM.                    "fcode_bdel

*&---------------------------------------------------------------------*
*&      Form  fcode_create_task
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_create_task.

  DATA: lt_fphdr      TYPE /agri/t_fmfphdr,
        lt_dflow      TYPE /agri/t_glcsdfl,
        lwa_fpdoc     TYPE /agri/s_fmfp_doc,
        lwa_taskorder TYPE /agri/s_fmfp_uptask_mass,
        lwa_fpitm     TYPE /agri/s_fmfpitm,
        lwa_cskey     TYPE /agri/s_glcs_key,
        lwa_fphdr     TYPE /agri/s_fmfphdr.

  DATA: lt_rows            TYPE lvc_t_row,
        lt_messages        TYPE /agri/t_gprolog,
        lwa_rows           TYPE lvc_s_row,
        ls_message         TYPE /agri/s_gprolog,
        lwa_items_mod_rows LIKE LINE OF gt_items_mod_rows,
        lv_row_index       TYPE lvc_index,
        lv_subrc           TYPE sysubrc,
        lv_valid,
        lv_answer.

  DATA: lv_lines TYPE sy-tabix,
        lv_cont  TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_taskorder> LIKE LINE OF gt_taskorder.

  IF lt_rows[] IS INITIAL.
    lv_lines = lines( gt_taskorder ).
    DO lv_lines TIMES.
      INSERT INITIAL LINE INTO TABLE lt_rows
        ASSIGNING FIELD-SYMBOL(<lwa_row>).
      IF sy-subrc EQ 0.
        ADD 1 TO lv_row_index.
        <lwa_row>-index = lv_row_index.
      ENDIF.
    ENDDO.
  ENDIF.

  LOOP AT lt_rows INTO lwa_rows.
    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder> INDEX lwa_rows-index. "#EC CI_NOORDER

    IF sy-subrc EQ 0.
      IF  <lwa_taskorder>-sappl EQ c_special_appl-chemical.
        lv_cont = lv_cont + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_lines EQ lv_cont.
    gs_variables-initiator =  c_log_initiator-check.
    PERFORM messages_initialize USING  gs_variables-initiator
                                       c_log_subobject-save
                                       lwa_fpdoc-x-fphdr.
    REFRESH: gt_fpbom_fcat, gt_fpbom_temp.
    CLEAR: gs_unit.
    PERFORM bom_items_display CHANGING lv_subrc.

    IF lv_subrc EQ 0.
      gs_variables-refresh_bom = c_true.
      CALL SCREEN 406 STARTING AT 18 1 ENDING AT 120 16.
    ENDIF.

    PERFORM messages_display USING gs_variables-initiator.

    EXIT.

  ENDIF.

  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_fpdoc-x-fphdr.

  LOOP AT lt_rows INTO lwa_rows.

    REFRESH: lt_dflow,lt_fphdr.
    CLEAR: lwa_cskey,lwa_fpitm,lwa_fpdoc.
    REFRESH: lt_messages.

    READ TABLE gt_taskorder ASSIGNING <lwa_taskorder> INDEX lwa_rows-index. "#EC CI_NOORDER
    MOVE-CORRESPONDING <lwa_taskorder> TO lwa_taskorder.

    PERFORM messages_context_set USING lwa_taskorder.
    PERFORM assginments_data_check USING lwa_taskorder
                                         lv_subrc.
    CHECK lv_subrc IS INITIAL.
    MOVE-CORRESPONDING <lwa_taskorder> TO lwa_cskey.
    lwa_fpitm-matnr    = p_matnr.
    lwa_fpitm-verid    = p_verid.
    lwa_fpitm-actdt    = <lwa_taskorder>-gstrp.
    lwa_fpitm-tomng    = <lwa_taskorder>-aarea.
    lwa_fpitm-meinh    = <lwa_taskorder>-meins.
    lwa_fpitm-confm    = c_true.
    lwa_cskey-tplnr_fl = <lwa_taskorder>-tplnr_fl.
    lwa_cskey-contr    = <lwa_taskorder>-contr.

    IF p_cpros IS NOT INITIAL.
      SELECT * FROM /agri/glcsdfl                    "#EC CI_SEL_NESTED
           INTO CORRESPONDING FIELDS OF TABLE lt_dflow
                WHERE tplnr_fl EQ lwa_cskey-tplnr_fl
                  AND contr    EQ lwa_cskey-contr
                  AND autyp    EQ c_document_category-production_order.

      IF lt_dflow IS NOT INITIAL.
        SELECT * FROM /agri/fmfphdr "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_NESTED
          INTO CORRESPONDING FIELDS OF TABLE lt_fphdr
          FOR ALL ENTRIES IN lt_dflow              "#EC CI_NO_TRANSFORM
          WHERE aufnr EQ lt_dflow-aufnr
            AND cpros EQ p_cpros
            AND tecom EQ space.
        IF sy-subrc EQ 0.
          SORT lt_fphdr BY aufnr DESCENDING.
          READ TABLE lt_fphdr INTO lwa_fphdr INDEX 1. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_NOORDER
          IF sy-subrc EQ 0.
            lwa_fpitm-aufnr = lwa_fphdr-aufnr. "#EC CI_ALL_FIELDS_NEEDED
          ELSE.
            MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                     INTO sy-msgli.
            PERFORM zabs_message_simple.
            CONTINUE.
          ENDIF.
        ELSE.
          MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                   INTO sy-msgli.
          PERFORM zabs_message_simple.
          CONTINUE.
        ENDIF.
      ELSE.
        MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error NUMBER '072' WITH p_cpros
                                 INTO sy-msgli.
        PERFORM zabs_message_simple.
        CONTINUE.
      ENDIF.
    ENDIF.

    wa_fmfphdr = lwa_fphdr.

    CALL FUNCTION '/AGRI/FMFP_UP_TASK_CREATE'
      EXPORTING
        i_matnr           = p_matnr
        is_cskey          = lwa_cskey
        is_fpitm          = lwa_fpitm
      IMPORTING
        es_fpdoc          = lwa_fpdoc
        et_messages       = lt_messages
      EXCEPTIONS
        inconsistent_data = 1
        no_data           = 2
        OTHERS            = 3.

    lv_subrc = sy-subrc.
    WAIT UP TO 2 SECONDS.

    LOOP AT gt_messtab INTO DATA(ls_messtab).
      IF ls_messtab-msgtyp IS INITIAL.
        ls_messtab-msgtyp = 'I'.
      ENDIF.
      IF ls_messtab-msgid IS INITIAL
      OR ls_messtab-msgnr IS INITIAL.
        CONTINUE.
      ENDIF.
      ls_message-msgid = ls_messtab-msgid.
      ls_message-msgty = ls_messtab-msgtyp.
      ls_message-msgno = ls_messtab-msgnr.
      ls_message-msgv1 = ls_messtab-msgv1.
      ls_message-msgv2 = ls_messtab-msgv2.
      ls_message-msgv3 = ls_messtab-msgv3.
      ls_message-msgv4 = ls_messtab-msgv4.
      APPEND ls_message TO lt_messages.
    ENDLOOP.

    LOOP AT lt_messages INTO ls_message.
      CONCATENATE 'NODISP' ls_message-msgid ls_message-msgty
        ls_message-msgno ls_message-msgv1 ls_message-msgv2
        ls_message-msgv3 ls_message-msgv4 INTO sy-msgli
        SEPARATED BY ';'.
      WRITE:/ sy-msgli.
      MESSAGE ID ls_message-msgid TYPE ls_message-msgty
         NUMBER ls_message-msgno WITH ls_message-msgv1
         ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                    INTO sy-msgli.
      PERFORM zabs_message_simple.
      CONCATENATE 'DISP' sy-msgli INTO sy-msgli
        SEPARATED BY ';'.
      WRITE:/ sy-msgli.
    ENDLOOP.

    SORT lwa_fpdoc-x-fpitm BY posnr DESCENDING.
    READ TABLE lwa_fpdoc-x-fpitm INTO lwa_fpitm INDEX 1. "#EC CI_NOORDER
    <lwa_taskorder>-aufnr = lwa_fpitm-aufnr_to.
  ENDLOOP.

  IF gt_taskorder[] IS NOT INITIAL.
    SELECT aufnr, prueflos
      INTO TABLE @DATA(lt_afko)
      FROM afko
      FOR ALL ENTRIES IN @gt_taskorder
     WHERE aufnr = @gt_taskorder-aufnr.
    IF sy-subrc EQ 0.
      SELECT prueflos, vorglfnr, probenr, ppsortkey
        INTO TABLE @DATA(lt_qapp)
        FROM qapp
        FOR ALL ENTRIES IN @lt_afko
       WHERE prueflos = @lt_afko-prueflos."AQUI
    ENDIF.
  ENDIF.

ENDFORM.                    "fcode_create_task

*&---------------------------------------------------------------------*
*& Form TRANSFER_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM transfer_parameters .

  READ TABLE so_strno INTO DATA(lwa_strno) INDEX 1.
  IF sy-subrc EQ 0.
    wa_parameters-strno = lwa_strno-low.
  ENDIF.

  wa_parameters-werks    = p_werks.
  wa_parameters-cmnum    = p_cmnum.
  wa_parameters-matnr    = p_matnr.
  wa_parameters-basoq    = p_basoq.
  wa_parameters-varia    = p_varia.
  wa_parameters-cpros    = p_cpros.
  wa_parameters-verid    = p_verid.
  wa_parameters-result1  = p_res1.
  wa_parameters-result2  = p_res2.
  wa_parameters-result3  = p_res3.
  wa_parameters-result4  = p_res4.
  wa_parameters-result5  = p_res5.
  wa_parameters-result6  = p_res6.
  wa_parameters-result7  = p_res7.
  wa_parameters-result8  = p_res8.
  wa_parameters-result9  = p_res9.
  wa_parameters-result10 = p_res10.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ZABS_MESSAGE_SIMPLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zabs_message_simple .

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->zabs_message_add
    EXPORTING
      i_level                 = gs_message-level
      i_msgid                 = sy-msgid
      i_msgty                 = sy-msgty
      i_msgno                 = sy-msgno
      i_msgv1                 = sy-msgv1
      i_msgv2                 = sy-msgv2
      i_msgv3                 = sy-msgv3
      i_msgv4                 = sy-msgv4
    EXCEPTIONS
      message_to_be_displayed = 1
      OTHERS                  = 2.
  IF sy-subrc EQ 1
  AND NOT sy-msgid IS INITIAL
  AND NOT sy-msgty IS INITIAL.
*  and not sy-msgno is initial.
****Rel E SP1
    IF sy-msgid EQ '/AGRI/GMSG1' OR
       sy-msgid EQ '/AGRI/GMSG2'.
      CLEAR gs_log_variables-text.
      CALL FUNCTION '/AGRI/GMSG_TEXTGET'
        EXPORTING
          i_msgcls       = sy-msgid
          i_msgno        = sy-msgno
          i_placeholder1 = sy-msgv1
          i_placeholder2 = sy-msgv2
          i_placeholder3 = sy-msgv3
          i_placeholder4 = sy-msgv4
        IMPORTING
          e_message      = gs_log_variables-text.

      MESSAGE gs_log_variables-text TYPE sy-msgty.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*** Rel E SP1
    ENDIF.
***
  ENDIF.

ENDFORM.
