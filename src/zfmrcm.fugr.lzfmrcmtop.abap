FUNCTION-POOL zfmrcm.                 "MESSAGE-ID ..

CLASS : lcl_event_handler DEFINITION DEFERRED,
        lcl_log_handler   DEFINITION DEFERRED.

TYPE-POOLS: slis, icon.

TABLES: /agri/s_gtabstrip_captions,
        zsc_fmrchdr, zsc_fmrcnum, sscrfields, zsc_fmrclst, zsc_fmrcbom,
        zsc_fmrcvrs, zsc_zfmrcsim.

DATA: gs_fmrchdr_previous TYPE zsc_fmrchdr.

*--Includes
INCLUDE: zlfmrcmcon,
         zlfmrcmmcr,
         /agri/abgl_constants,
         /agri/global_macros,
         /agri/global_constants,
         /agri/gprolog_macros,
         /agri/global_brf_macros.

TYPES: BEGIN OF s_rctyp,
         rctyp TYPE zfmrctyp,
       END OF s_rctyp,
       t_rctyp TYPE TABLE OF s_rctyp.

*--Fcodes
DATA : ok_code TYPE  sy-ucomm,
       fcode   LIKE  ok_code.

*--Controls
CONTROLS: ts_items TYPE TABSTRIP.

*--Global Data

DATA: BEGIN OF gs_variables,
        overview_mode,
        document_mode,
        object_text(40),
        display_only,
        copy,
        document_changed,
        manual_changes,
        data_changed,
        refresh_worklist,
        worklist_is_visible,
        external,
        mass_action,
        exit_screen,
        messages,
        errors,
        refresh_dose_grid,
        refresh_vrs_grid,
        refresh_bom_grid,
        refresh_sim_grid,
        items_manual_changes,
        transaction_code      TYPE sy-tcode,
        rctyp_in_focus        TYPE zfmrctyp,
        program               TYPE sy-repid,
        subscr_quick_info     TYPE sydynnr,
        subscr_details        TYPE sydynnr,
        exit_after_save,
        user_additional_data  TYPE sydynnr,
        subscr_admin          LIKE sy-dynnr,
        initiator             TYPE /agri/gdescr,
        admin_program         TYPE sy-repid,
        cancelled,
        external_dialog,
        display_mode,                 "TYPE /agri/FMRCmod,
        attribute_description,
        user_structure(30),
        user_function1(40),
        user_function2(40),
        user_function3(40),
        user_function4(40),
        user_function5(40),
        notes_title           TYPE char060,
        header_display,
      END OF gs_variables.

*--Gobal Tables for Grid display
DATA: gt_search_header      TYPE TABLE OF zsc_fmrchdr,
      gs_fmrctyp            TYPE ztfmrctyp,
      gt_search_next_header TYPE TABLE OF zsc_fmrchdr,
      gt_worklist_header    TYPE TABLE OF zsc_fmrchdr_wl,
      gt_selected_docs      TYPE zt_fmrc_key.

****Global Infocus Document Structure
DATA: gs_rcdoc_infocus TYPE zsc_fmrc_doc,
      gs_rckey         TYPE zsc_fmrc_key,
      gv_numocurren    TYPE int4.

DATA: gt_rchdr        TYPE zt_fmrchdr,
      gt_fmrclst_fcat TYPE zt_fmrclst_fcat,
      gt_fmrcbom_fcat TYPE zt_fmrcbom_fcat,
      gt_fmrcvrs_fcat TYPE zt_fmrcvrs_fcat,
      gt_fmrcsim_fcat TYPE zt_zfmrcsim_fcat,
      gs_tfmrctyp     TYPE ztfmrctyp.

DATA: gt_rcdoc              TYPE zt_fmrc_doc.

DATA: gt_fcat TYPE lvc_t_fcat.
DATA: gt_fcat_rclst TYPE lvc_t_fcat.
DATA: gt_fcat_rcbom TYPE lvc_t_fcat.
DATA: gt_fcat_rcvrs TYPE lvc_t_fcat.

****Reference Variables
DATA : ref_worklist_container TYPE REF TO cl_gui_docking_container,
       ref_worklist           TYPE REF TO /agri/cl_worklist_container,
       ref_event_handler      TYPE REF TO lcl_event_handler,
       ref_log_handler        TYPE REF TO lcl_log_handler,
       ref_container_rclst    TYPE REF TO cl_gui_custom_container,
       ref_container_rcvrs    TYPE REF TO cl_gui_custom_container,
       ref_container_rcbom    TYPE REF TO cl_gui_custom_container,
       ref_grid_rclst         TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_rcvrs         TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_rcbom         TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_similar       TYPE REF TO /agri/cl_gui_alv_grid.

****Selected Docs and Rows
DATA :  gt_selected_rows      TYPE lvc_t_row.
DATA :  gt_selected_row_rcbom TYPE lvc_t_row.
DATA :  gt_selected_row_rcvrs TYPE lvc_t_row.

DATA:   gt_dose_modi  TYPE lvc_t_modi.
DATA:   gt_rcbom_modi TYPE lvc_t_modi.
DATA:   gt_rcvrs_modi TYPE lvc_t_modi.

*--Tabstrip
DATA: gs_tabstrip_captions TYPE /agri/s_gtabstrip_captions,
      gt_tabstrip_fcodes   TYPE /agri/t_gtabstrip.

*--Texts
DATA: ref_text             TYPE REF TO /agri/cl_gtext_process.

****Tabstrip settings
DATA: gt_tabstrip_texts    TYPE TABLE OF dd07v.

DATA: gs_rchdr_portal      TYPE zsc_fmrchdr.

*...Produtos Similares
DATA: gt_fieldcat_sim        TYPE lvc_t_fcat,
      gt_similares           TYPE STANDARD TABLE OF zsc_zfmrcsim INITIAL SIZE 0,
      gs_layout_sim          TYPE lvc_s_layo,
      g_container_sim        TYPE scrfname VALUE 'BCALV_GRID_0309',
      gr_grid_sim            TYPE REF TO cl_gui_alv_grid,
      g_custom_container_sim TYPE REF TO cl_gui_custom_container,
      row_id                 TYPE lvc_t_roid WITH HEADER LINE,
      row_i                  TYPE lvc_s_row,
      col_i                  TYPE lvc_s_col.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler_sim DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_sim DEFINITION.

  PUBLIC SECTION.

    METHODS:
      handle_data_changed_finished
                    FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_on_f4
                    FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender e_fieldname e_fieldvalue es_row_no
                    er_event_data et_bad_cells e_display,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.
    DATA : lr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
    METHODS: my_f4
      IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                et_bad_cells  TYPE lvc_t_modi
                es_row_no     TYPE lvc_s_roid
                er_event_data TYPE REF TO cl_alv_event_data
                e_display     TYPE c
                e_fieldname   TYPE lvc_fname
      EXPORTING lt_f4         TYPE ddshretval_table.

ENDCLASS. "lcl_event_handler_sim DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_event_handler_sim IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_sim IMPLEMENTATION .

*---------------------------------------------------------------------*
*       METHOD my_f4  insert here your own f4-help                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
  METHOD my_f4.

    DATA: wa_tab        LIKE LINE OF gt_similares,
          lt_fcat       TYPE lvc_t_fcat,
          lt_index_rows TYPE lvc_t_row,
          l_tabname     TYPE dd03v-tabname,
          l_fieldname   TYPE dd03v-fieldname,
          l_help_valu   TYPE help_info-fldvalue,
          lt_bad_cell   TYPE lvc_t_modi,
          lp_wa         TYPE REF TO data.

    FIELD-SYMBOLS: <l_field_value> TYPE any,
                   <ls_wa>         TYPE any.

    CALL METHOD sender->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = lt_fcat.

    READ TABLE gt_similares INDEX es_row_no-row_id INTO wa_tab.
    CREATE DATA lp_wa LIKE LINE OF gt_similares.
    ASSIGN lp_wa->* TO <ls_wa>.
    <ls_wa> = wa_tab.

    READ TABLE lt_fcat INTO DATA(lwa_fieldcat)
       WITH KEY fieldname = e_fieldname.
    IF sy-subrc EQ 0.
      MOVE lwa_fieldcat-ref_table TO l_tabname.
      MOVE lwa_fieldcat-fieldname TO l_fieldname.
      ASSIGN COMPONENT lwa_fieldcat-fieldname
        OF STRUCTURE wa_tab TO <l_field_value>.
      WRITE <l_field_value> TO l_help_valu.

      IF e_fieldname EQ 'MATNR_INS'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'ZABS_SH_INSUMO' "'MAT1_A'
            shlpparam         = 'MATNR'
            dynpprog          = sy-cprog
            dynpnr            = sy-dynnr
          TABLES
            return_tab        = lt_f4
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
      ELSEIF e_fieldname EQ 'MATNR_SIM'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'MAT1_A'
            shlpparam         = 'MATNR'
            dynpprog          = sy-cprog
            dynpnr            = sy-dynnr
          TABLES
            return_tab        = lt_f4
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
      ELSEIF e_fieldname EQ 'UNITS_INS'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'H_UNIT_OF_MEASURE'
            shlpparam         = 'MSEHI'
            dynpprog          = sy-cprog
            dynpnr            = sy-dynnr
          TABLES
            return_tab        = lt_f4
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
      ELSEIF e_fieldname EQ 'UNITS_SIM'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'H_UNIT_OF_MEASURE'
            shlpparam         = 'MSEHI'
            dynpprog          = sy-cprog
            dynpnr            = sy-dynnr
          TABLES
            return_tab        = lt_f4
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
      ENDIF.
    ENDIF.

  ENDMETHOD.                                                "my_f4
*-------------------------------------------------------------------
  METHOD handle_data_changed_finished .

    DATA: lwa_modi       TYPE lvc_s_modi,
          lv_matnr_input TYPE matnr.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    LOOP AT gt_similares ASSIGNING FIELD-SYMBOL(<lwa_similar>).
*      IF <lwa_similar>-matnr_ins IS INITIAL.
*        CONTINUE.
*      ELSE.
      <lwa_similar>-stlal = zsc_fmrchdr-stlal.
*      ENDIF.
      IF <lwa_similar>-matnr_ins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <lwa_similar>-matnr_ins
          IMPORTING
            output       = <lwa_similar>-matnr_ins
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDIF.
    ENDLOOP.

    IF e_modified = 'X'.

      LOOP AT et_good_cells INTO lwa_modi.
        MOVE lwa_modi-row_id TO row_id-row_id.
        MOVE lwa_modi-row_id TO row_i-index.
        MOVE lwa_modi-fieldname TO col_i-fieldname.

        IF lwa_modi-fieldname EQ 'MATNR_INS'.
          READ TABLE gt_similares ASSIGNING <lwa_similar>
            INDEX lwa_modi-row_id.
          IF sy-subrc EQ 0.
            IF <lwa_similar>-matnr_ins IS INITIAL.
*            CLEAR: <lwa_similar>-maktx_ins,
*                   <lwa_similar>-units_ins.
              CLEAR: <lwa_similar>-maktx_ins.
            ELSE.
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = <lwa_similar>-matnr_ins
                IMPORTING
                  output       = lv_matnr_input
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.
              IF sy-subrc EQ 0.
*              SELECT SINGLE meins
*                INTO @<lwa_similar>-units_ins
*                FROM mara
*               WHERE matnr EQ @lv_matnr_input.

                SELECT SINGLE maktx
                  INTO @<lwa_similar>-maktx_ins
                  FROM makt
                 WHERE matnr = @lv_matnr_input
                   AND spras = @sy-langu.
              ENDIF.

              READ TABLE gs_rcdoc_infocus-x-rclst TRANSPORTING NO FIELDS
                WITH KEY matnr_ins = lv_matnr_input.
              IF sy-subrc NE 0.
                CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                  EXPORTING
                    input  = lv_matnr_input
                  IMPORTING
                    output = lv_matnr_input.
*.............Insumo &1 não utilizado na Receita &2! Revise os dados!
                MESSAGE i039(zfmfp) WITH lv_matnr_input gs_rcdoc_infocus-rcnum.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSEIF lwa_modi-fieldname EQ 'MATNR_SIM'.
          READ TABLE gt_similares ASSIGNING <lwa_similar>
            INDEX lwa_modi-row_id.
          IF sy-subrc EQ 0.
            IF <lwa_similar>-matnr_sim IS INITIAL.
              CLEAR: <lwa_similar>-maktx_sim,
                     <lwa_similar>-units_sim.
            ELSE.
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = <lwa_similar>-matnr_sim
                IMPORTING
                  output       = lv_matnr_input
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.
              IF sy-subrc EQ 0.
                SELECT SINGLE meins
                  INTO @<lwa_similar>-units_sim
                  FROM mara
                 WHERE matnr EQ @lv_matnr_input.

                SELECT SINGLE maktx
                  INTO @<lwa_similar>-maktx_sim
                  FROM makt
                 WHERE matnr = @lv_matnr_input
                   AND spras = @sy-langu.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CALL METHOD gr_grid_sim->refresh_table_display.

      CALL METHOD gr_grid_sim->set_current_cell_via_id
        EXPORTING
          is_row_id    = row_i
          is_column_id = col_i.

      CALL METHOD cl_gui_cfw=>flush.
    ENDIF.

  ENDMETHOD. "handle_data_changed_finished
*-------------------------------------------------------------------
  METHOD handle_toolbar.

    DATA: lwa_toolbar  TYPE stb_button.

    CLEAR lwa_toolbar.
    MOVE 3 TO lwa_toolbar-butn_type.
    APPEND lwa_toolbar TO e_object->mt_toolbar.
* append an icon to show booking table
    CLEAR lwa_toolbar.
    MOVE 'SAVE' TO lwa_toolbar-function.
    MOVE icon_system_save TO lwa_toolbar-icon.
    MOVE 'Gravar Produtos Similares'(111) TO lwa_toolbar-quickinfo.
    MOVE 'Gravar'(112) TO lwa_toolbar-text.
    MOVE ' ' TO lwa_toolbar-disabled.
    APPEND lwa_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
*-------------------------------------------------------------------
  METHOD handle_on_f4.

    DATA: lwa_f4         TYPE ddshretval,
          lt_f4          TYPE TABLE OF ddshretval,
          lv_matnr_input TYPE matnr.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    CALL METHOD my_f4
      EXPORTING
        sender        = sender
        es_row_no     = es_row_no
        er_event_data = er_event_data
        et_bad_cells  = et_bad_cells
        e_display     = e_display
        e_fieldname   = e_fieldname
      IMPORTING
        lt_f4         = lt_f4.

    DATA(lv_fieldname) = e_fieldname.
    READ TABLE gt_similares ASSIGNING FIELD-SYMBOL(<lwa_similar>)
      INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.
      IF e_fieldname EQ 'MATNR_INS'
      OR e_fieldname EQ 'MATNR_SIM'.
        lv_fieldname = 'MATNR'.
      ENDIF.
      READ TABLE lt_f4 INTO lwa_f4 WITH KEY fieldname = lv_fieldname.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT e_fieldname OF STRUCTURE <lwa_similar>
          TO FIELD-SYMBOL(<lv_field>).
        IF sy-subrc EQ 0.
          <lv_field> = lwa_f4-fieldval.
          IF lv_fieldname = 'MATNR'.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <lv_field>
              IMPORTING
                output       = lv_matnr_input
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            IF sy-subrc EQ 0.
              SELECT SINGLE meins
                INTO @DATA(lv_meins)
                FROM mara
               WHERE matnr EQ @lv_matnr_input.

              SELECT SINGLE maktx
                INTO @DATA(lv_maktx)
                FROM makt
               WHERE matnr = @lv_matnr_input
                 AND spras = @sy-langu.

              IF sy-subrc EQ 0.
                CASE e_fieldname.
                  WHEN 'MATNR_INS'.
                    <lwa_similar>-maktx_ins = lv_maktx.
*                    <lwa_similar>-units_ins = lv_meins.
                  WHEN 'MATNR_SIM'.
                    <lwa_similar>-maktx_sim = lv_maktx.
                    <lwa_similar>-units_sim = lv_meins.
                ENDCASE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

    CALL METHOD gr_grid_sim->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDMETHOD.
*-------------------------------------------------------------------
  METHOD handle_user_command.

    DATA: lt_rcdoc TYPE zt_fmrc_doc.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    CASE e_ucomm.
      WHEN 'SAVE'.
        PERFORM save_similar USING lt_rcdoc.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command

ENDCLASS. "lcl_event_handler_sim IMPLEMENTATION

DATA: gr_event_handler_sim TYPE REF TO lcl_event_handler_sim.
