*&---------------------------------------------------------------------*
*& Report ZFMFPGROUP_MANAGEMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfpgroup_management.

INCLUDE <icon>.

TABLES: zfmfpgrouphdr.

TYPES: BEGIN OF type_pa0002,
         pernr TYPE persno,
         cname TYPE pad_cname,
       END OF type_pa0002.

CONSTANTS: BEGIN OF c_object,
             log TYPE balobj-object VALUE '/AGRI/GLFL',
           END OF c_object.

CONSTANTS: BEGIN OF c_log_subobject,
             save TYPE balsubobj VALUE 'SAVE',
           END OF c_log_subobject.

DATA: BEGIN OF gs_variables,
        overview_mode,
        refresh_monitor_grid,
        errors,
        manual_changes,
        initiator            TYPE /agri/gdescr,
      END OF gs_variables.

****Log Initiator
CONSTANTS: BEGIN OF c_log_initiator,
             create  TYPE balobj VALUE 'CREATE',
             change  TYPE balobj VALUE 'CHANGE',
             display TYPE balobj VALUE 'DISPLAY',
             save    TYPE balobj VALUE 'SAVE',
             post    TYPE balobj VALUE 'POST',
             check   TYPE balobj VALUE 'CHECK',
             read    TYPE balobj VALUE 'READ',
             upload  TYPE balobj VALUE 'UPLOAD',
             copy    TYPE balobj VALUE 'COPY',
             mass    TYPE balobj VALUE 'MASS',
           END OF c_log_initiator.

CLASS lcl_log_handler  DEFINITION DEFERRED.

FIELD-SYMBOLS: <gt_dyntable> TYPE STANDARD TABLE.

DATA: gt_message      TYPE /agri/t_gprolog.

DATA: gt_groupitm        TYPE STANDARD TABLE OF zfmfpgroupitm INITIAL SIZE 0,
      gt_outtab          TYPE STANDARD TABLE OF zsc_fmfpgroupitm INITIAL SIZE 0,
      gt_dd07v           TYPE STANDARD TABLE OF dd07v INITIAL SIZE 0,
      gt_pa0002          TYPE STANDARD TABLE OF type_pa0002 INITIAL SIZE 0,
      gt_fieldcat        TYPE lvc_t_fcat,
      gs_layout          TYPE lvc_s_layo,
      gt_taba            TYPE STANDARD TABLE OF dd07v INITIAL SIZE 0,
      gt_tabb            TYPE STANDARD TABLE OF dd07v INITIAL SIZE 0,
      ok_code            LIKE sy-ucomm,
      g_container        TYPE scrfname VALUE 'BCALV_GRID_0100',
      gr_grid            TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      row_id             TYPE lvc_t_roid WITH HEADER LINE,
      row_i              TYPE lvc_s_row,
      col_i              TYPE lvc_s_col,
      v_status_txt       TYPE dd07t-ddtext,
      v_cname            TYPE adrp-name_text,
      v_fazenda1         TYPE /agri/glpltxt,
      v_fazenda2         TYPE /agri/glpltxt,
      v_fazenda3         TYPE /agri/glpltxt,
      v_fazenda4         TYPE /agri/glpltxt,
      v_fazenda5         TYPE /agri/glpltxt,
      v_coordenador      TYPE adrp-name_text,
      v_supervisor       TYPE adrp-name_text,
      v_encarregado      TYPE adrp-name_text,
      "Begin of José Sequeira - 09.02.2021 13:43:51
      v_encarregado_adm  TYPE adrp-name_text,
      "End of José Sequeira - 09.02.2021 13:43:51
      v_lider            TYPE adrp-name_text,
      dummy.

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/glpg_upload_process_cls.

DATA: ref_log_handler TYPE REF TO lcl_log_handler.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.
PARAMETERS: p_new RADIOBUTTON GROUP grp MODIF ID typ DEFAULT 'X'
                    USER-COMMAND grp,
            p_old RADIOBUTTON GROUP grp MODIF ID typ.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_turmax TYPE zfmfpgrouphdr-turma_tipo MODIF ID new,
            p_idx    TYPE zfmfpgrouphdr-turma_id MODIF ID new,
            p_textx  TYPE zfmfpgrouphdr-turma_text MODIF ID new.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_turmay TYPE zfmfpgrouphdr-turma_tipo
              AS LISTBOX VISIBLE LENGTH 30 USER-COMMAND v_turma MODIF ID old,
            p_idy    TYPE zfmfpgrouphdr-turma_id
              AS LISTBOX VISIBLE LENGTH 30 USER-COMMAND v_id MODIF ID old.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

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

ENDCLASS. "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION .

*---------------------------------------------------------------------*
*       METHOD my_f4  insert here your own f4-help                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
  METHOD my_f4.

    DATA: wa_tab        LIKE LINE OF gt_outtab,
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

    READ TABLE gt_outtab INDEX es_row_no-row_id INTO wa_tab.
    CREATE DATA lp_wa LIKE LINE OF gt_outtab.
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

      IF e_fieldname EQ 'PERNR'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'PREM'
            shlpparam         = 'PERNR'
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
      ELSEIF e_fieldname EQ 'IDRESOURCE'.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = space
            fieldname         = space
            searchhelp        = 'ZABS_SH_IDRESOURCE'
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

    DATA: lt_pa0002 TYPE STANDARD TABLE OF pa0002 INITIAL SIZE 0,
          lwa_modi  TYPE lvc_s_modi,
          lv_tipo   TYPE zfmturma_tipo,
          lv_id     TYPE zfmturma_id.

    IF zfmfpgrouphdr-turma_id IS NOT INITIAL.
      UNPACK zfmfpgrouphdr-turma_id TO zfmfpgrouphdr-turma_id.
    ENDIF.
    CHECK e_modified = 'X'.

    LOOP AT et_good_cells INTO lwa_modi.
      MOVE lwa_modi-row_id TO row_id-row_id.
      MOVE lwa_modi-row_id TO row_i-index.
      MOVE lwa_modi-fieldname TO col_i-fieldname.

      IF lwa_modi-fieldname EQ 'TURMA_TIPO'.
        READ TABLE gt_outtab ASSIGNING FIELD-SYMBOL(<lwa_outtab>)
          INDEX lwa_modi-row_id.
        IF sy-subrc EQ 0
        AND <lwa_outtab>-turma_tipo IS INITIAL.
          <lwa_outtab>-turma_tipo = zfmfpgrouphdr-turma_tipo.
        ENDIF.
      ELSEIF lwa_modi-fieldname EQ 'TURMA_ID'.
        READ TABLE gt_groupitm ASSIGNING FIELD-SYMBOL(<lwa_groupitm>)
          INDEX lwa_modi-row_id.
        IF sy-subrc EQ 0
        AND <lwa_groupitm>-turma_id IS INITIAL.
          <lwa_groupitm>-turma_id = zfmfpgrouphdr-turma_id.
        ENDIF.
        READ TABLE gt_outtab ASSIGNING <lwa_outtab>
          INDEX lwa_modi-row_id.
        IF sy-subrc EQ 0
        AND <lwa_outtab>-turma_id IS INITIAL.
          <lwa_outtab>-turma_id = zfmfpgrouphdr-turma_id.
        ENDIF.
      ENDIF.
    ENDLOOP.

    READ TABLE gt_groupitm ASSIGNING <lwa_groupitm>
      WITH KEY turma_id = lv_id.
    IF sy-subrc EQ 0.
      <lwa_groupitm>-turma_id = zfmfpgrouphdr-turma_id.
    ENDIF.

    READ TABLE gt_outtab ASSIGNING <lwa_outtab>
      WITH KEY turma_tipo = lv_tipo.
    IF sy-subrc EQ 0.
      <lwa_outtab>-turma_tipo = zfmfpgrouphdr-turma_tipo.
    ENDIF.

    READ TABLE gt_outtab ASSIGNING <lwa_outtab>
      WITH KEY turma_id = lv_id.
    IF sy-subrc EQ 0.
      <lwa_outtab>-turma_id = zfmfpgrouphdr-turma_id.
    ENDIF.

    READ TABLE gt_outtab ASSIGNING <lwa_outtab>
      INDEX row_id-row_id.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = <lwa_outtab>-pernr
          infty           = '0002'
        TABLES
          infty_tab       = lt_pa0002
        EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

      IF sy-subrc EQ 0.
        READ TABLE lt_pa0002 INTO DATA(lwa_pa0002) INDEX 1.
        IF sy-subrc EQ 0.
          <lwa_outtab>-cname = lwa_pa0002-cname.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD gr_grid->refresh_table_display.
    CALL METHOD gr_grid->set_current_cell_via_id
      EXPORTING
        is_row_id    = row_i
        is_column_id = col_i.
    CALL METHOD cl_gui_cfw=>flush.

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
    MOVE 'Gravar Alterações'(111) TO lwa_toolbar-quickinfo.
    MOVE 'Gravar'(112) TO lwa_toolbar-text.
    MOVE ' ' TO lwa_toolbar-disabled.
    APPEND lwa_toolbar TO e_object->mt_toolbar.

    CLEAR lwa_toolbar.
    MOVE 'DELETE' TO lwa_toolbar-function.
    MOVE icon_delete TO lwa_toolbar-icon.
    MOVE 'Eliminar Turma'(111) TO lwa_toolbar-quickinfo.
    MOVE 'Eliminar'(112) TO lwa_toolbar-text.
    MOVE ' ' TO lwa_toolbar-disabled.
    APPEND lwa_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

*-------------------------------------------------------------------
  METHOD handle_on_f4.

    DATA: lt_pa0002 TYPE STANDARD TABLE OF pa0002 INITIAL SIZE 0,
          lwa_f4    TYPE ddshretval,
          lt_f4     TYPE TABLE OF ddshretval.

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

    READ TABLE gt_outtab ASSIGNING FIELD-SYMBOL(<lwa_outtab>)
      INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.
      READ TABLE lt_f4 INTO lwa_f4 WITH KEY fieldname = e_fieldname.
      IF sy-subrc EQ 0.
        IF e_fieldname EQ 'PERNR'.
          <lwa_outtab>-pernr = lwa_f4-fieldval.
          IF <lwa_outtab>-pernr IS NOT INITIAL.
            CALL FUNCTION 'HR_READ_INFOTYPE'
              EXPORTING
                pernr           = <lwa_outtab>-pernr
                infty           = '0002'
              TABLES
                infty_tab       = lt_pa0002
              EXCEPTIONS
                infty_not_found = 1
                OTHERS          = 2.

            IF sy-subrc EQ 0.
              READ TABLE lt_pa0002 INTO DATA(lwa_pa0002) INDEX 1.
              IF sy-subrc EQ 0.
                <lwa_outtab>-cname = lwa_pa0002-cname.
              ENDIF.
            ENDIF.

            SELECT idresource UP TO 1 ROWS
              FROM /agri/fmacres
              INTO <lwa_outtab>-idresource
             WHERE pernr = <lwa_outtab>-pernr.
            ENDSELECT.
          ELSE.
            CLEAR: <lwa_outtab>-cname,
                   <lwa_outtab>-idresource.
          ENDIF.
        ELSEIF e_fieldname EQ 'IDRESOURCE'.
          <lwa_outtab>-idresource = lwa_f4-fieldval.
        ENDIF.
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

    CALL METHOD gr_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDMETHOD.

*-------------------------------------------------------------------
  METHOD handle_user_command.

    TYPES: BEGIN OF type_pa0002,
             pernr TYPE persno,
             endda TYPE endda,
             begda TYPE begda,
             cname TYPE pad_cname,
           END OF type_pa0002,

           BEGIN OF type_pa0000,
             pernr TYPE persno,
             endda TYPE endda,
             begda TYPE begda,
             stat2 TYPE stat2,
           END OF type_pa0000.

    DATA: lt_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 0,
          lt_groupitm   TYPE STANDARD TABLE OF zfmfpgroupitm INITIAL SIZE 0,
          lt_fmacres    TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
          lt_pa0000     TYPE STANDARD TABLE OF type_pa0000 INITIAL SIZE 0,
          lt_pa0002     TYPE STANDARD TABLE OF type_pa0002 INITIAL SIZE 0,
          lwa_dynpfield LIKE LINE OF lt_dynpfields.

    DATA: g_repid LIKE sy-repid,
          lt_rows TYPE lvc_t_row.

    CONSTANTS: BEGIN OF lc_status,
                 desligado   TYPE stat2 VALUE '1',
                 suspenso    TYPE stat2 VALUE '1',
                 pensionista TYPE stat2 VALUE '2',
                 ativo       TYPE stat2 VALUE '3',
               END OF lc_status.

    DATA(lv_save) = abap_true.

    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-STATUS'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-TURMA_ID'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-COORDENADOR'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-SUPERVISOR'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-ENCARREGADO'.
    APPEND lwa_dynpfield TO lt_dynpfields.
"Begin of José Sequeira - 09.02.2021 13:56:44
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-ENCARREGADO_ADM'.
    APPEND lwa_dynpfield TO lt_dynpfields.
"End of José Sequeira - 09.02.2021 13:56:44
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-LIDER_TURMA'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-FAZENDA1'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-FAZENDA2'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-FAZENDA3'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-FAZENDA4'.
    APPEND lwa_dynpfield TO lt_dynpfields.
    lwa_dynpfield-fieldname = 'ZFMFPGROUPHDR-FAZENDA5'.
    APPEND lwa_dynpfield TO lt_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
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

    g_repid = sy-repid.
    CASE e_ucomm.
      WHEN 'DELETE'.
        IF p_old EQ abap_true.
          DELETE FROM zfmfpgrouphdr
            WHERE turma_tipo = @zfmfpgrouphdr-turma_tipo
              AND turma_id   = @zfmfpgrouphdr-turma_id.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

          DELETE FROM zfmfpgroupitm
            WHERE turma_id = @zfmfpgrouphdr-turma_id.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.

*-- Turma &1 eliminada com sucesso!
        MESSAGE s024(zfmfp) WITH zfmfpgrouphdr-turma_tipo.
        PERFORM add_message.

        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'SAVE'.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-COORDENADOR'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-coordenador = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-SUPERVISOR'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-supervisor = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-ENCARREGADO'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-encarregado = lwa_dynpfield-fieldvalue.
        ENDIF.
"Begin of José Sequeira - 09.02.2021 13:57:04
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-ENCARREGADO_ADM'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-encarregado_adm = lwa_dynpfield-fieldvalue.
        ENDIF.
"End of José Sequeira - 09.02.2021 13:57:04
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-LIDER_TURMA'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-lider_turma = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-FAZENDA1'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-fazenda1 = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-FAZENDA2'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-fazenda2 = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-FAZENDA3'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-fazenda3 = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-FAZENDA4'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-fazenda4 = lwa_dynpfield-fieldvalue.
        ENDIF.
        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-FAZENDA5'.
        IF sy-subrc EQ 0
        AND lwa_dynpfield-fieldvalue IS NOT INITIAL.
          zfmfpgrouphdr-fazenda5 = lwa_dynpfield-fieldvalue.
        ENDIF.

        READ TABLE lt_dynpfields INTO lwa_dynpfield
          WITH KEY fieldname = 'ZFMFPGROUPHDR-STATUS'.
        IF sy-subrc EQ 0.
          IF lwa_dynpfield-fieldvalue IS INITIAL.
*-- Informe o status. Campo de preenchimento obrigatório.
            MESSAGE i018(zfmfp).
            lv_save = abap_false.
          ELSE.
            zfmfpgrouphdr-status = lwa_dynpfield-fieldvalue.
          ENDIF.
        ENDIF.

        IF gt_outtab[] IS INITIAL
        AND lv_save EQ abap_true.
*-- As informações não foram gravadas. É necessário informar um participante.
          MESSAGE i021(zfmfp).
          lv_save = abap_false.
        ENDIF.

        IF lv_save EQ abap_true
        AND gt_outtab[] IS NOT INITIAL.
          SELECT pernr,
                 endda,
                 begda,
                 cname
            FROM pa0002
            INTO TABLE @lt_pa0002
            FOR ALL ENTRIES IN @gt_outtab
           WHERE pernr  = @gt_outtab-pernr
             AND begda <= @sy-datum
             AND endda >= @sy-datum.

          SORT lt_pa0002 BY pernr endda DESCENDING.

          DATA(lt_outtab) = gt_outtab[].
          IF lt_outtab[] IS NOT INITIAL.
            SELECT *
              FROM /agri/fmacres
              INTO TABLE @lt_fmacres
              FOR ALL ENTRIES IN @lt_outtab
             WHERE idresource = @lt_outtab-idresource.

            SORT lt_fmacres BY idresource.
          ENDIF.

          REFRESH gt_message.

          lt_outtab[] = gt_outtab[].
          DATA(lt_outtab2) = gt_outtab[].
          SORT: lt_outtab BY pernr,
                lt_outtab2 BY pernr.

          DATA(lv_rows) = lines( lt_outtab ).
          DATA(lv_count) = 1.
          LOOP AT lt_outtab INTO DATA(lwa_outtab).
            DATA(lv_current) = sy-tabix.
            DATA(lv_next) = sy-tabix + 1.
            DATA(lwa_record_copy) = lwa_outtab.
            AT NEW pernr.
              lv_count = 1.
            ENDAT.
            READ TABLE lt_outtab2 INTO DATA(lwa_outtab2)
              INDEX lv_next.
            IF sy-subrc EQ 0.
              IF lwa_outtab-pernr EQ lwa_outtab2-pernr.
                ADD 1 TO lv_count.
              ENDIF.
            ENDIF.
            AT END OF pernr.
              IF lv_count GT 1.
*-- Não é possível gravar. Existem &1 ocorrências para o nº pessoal &2.
                MESSAGE e022(zfmfp) WITH lv_count lwa_outtab-pernr INTO dummy.
                PERFORM add_message.
                lv_save = abap_false.
              ENDIF.
            ENDAT.
          ENDLOOP.

          LOOP AT gt_outtab INTO lwa_outtab.
            DATA(lv_tabix) = sy-tabix.
            READ TABLE lt_pa0002 TRANSPORTING NO FIELDS
              WITH KEY pernr = lwa_outtab-pernr BINARY SEARCH.
            IF sy-subrc NE 0.
*-- Favor verificar infotipo 0002 p/ nº pessoal &1.
              MESSAGE e047(zfmfp) WITH lwa_outtab-pernr INTO dummy.
              PERFORM add_message.
              lv_save = abap_false.
            ENDIF.
          ENDLOOP.

          IF lv_save EQ abap_true.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = zfmfpgrouphdr-fazenda1
              IMPORTING
                output     = zfmfpgrouphdr-fazenda1
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = zfmfpgrouphdr-fazenda2
              IMPORTING
                output     = zfmfpgrouphdr-fazenda2
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = zfmfpgrouphdr-fazenda3
              IMPORTING
                output     = zfmfpgrouphdr-fazenda3
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = zfmfpgrouphdr-fazenda4
              IMPORTING
                output     = zfmfpgrouphdr-fazenda4
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = zfmfpgrouphdr-fazenda5
              IMPORTING
                output     = zfmfpgrouphdr-fazenda5
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            IF p_old EQ abap_true.
              UPDATE zfmfpgrouphdr
                 SET status            = @zfmfpgrouphdr-status,
                     aedat             = @sy-datum,
                     last_changed_by   = @sy-uname,
                     last_changed_time = @sy-uzeit,
                     coordenador       = @zfmfpgrouphdr-coordenador,
                     supervisor        = @zfmfpgrouphdr-supervisor,
                     encarregado       = @zfmfpgrouphdr-encarregado,
                     "Begin of José Sequeira - 09.02.2021 13:55:20
                     encarregado_adm   = @zfmfpgrouphdr-encarregado_adm,
                     "End of José Sequeira - 09.02.2021 13:55:20
                     lider_turma       = @zfmfpgrouphdr-lider_turma,
                     fazenda1          = @zfmfpgrouphdr-fazenda1,
                     fazenda2          = @zfmfpgrouphdr-fazenda2,
                     fazenda3          = @zfmfpgrouphdr-fazenda3,
                     fazenda4          = @zfmfpgrouphdr-fazenda4,
                     fazenda5          = @zfmfpgrouphdr-fazenda5
               WHERE turma_tipo = @zfmfpgrouphdr-turma_tipo
                 AND turma_id   = @zfmfpgrouphdr-turma_id.
              IF sy-subrc EQ 0.
                COMMIT WORK.
              ELSE.
                ROLLBACK WORK.
              ENDIF.
            ELSEIF p_new EQ abap_true.
              MODIFY zfmfpgrouphdr FROM zfmfpgrouphdr.
              IF sy-subrc EQ 0.
                COMMIT WORK AND WAIT.
              ENDIF.
            ENDIF.

            lt_groupitm[] = CORRESPONDING #( gt_outtab[] ).
            MODIFY zfmfpgroupitm FROM TABLE lt_groupitm.
            IF sy-subrc EQ 0.
              COMMIT WORK AND WAIT.
            ENDIF.

*-- Os dados foram atualizados com sucesso!
            MESSAGE s023(zfmfp).
            PERFORM add_message.
          ENDIF.

          IF gt_message[] IS NOT INITIAL.
            DELETE ADJACENT DUPLICATES FROM gt_message COMPARING ALL FIELDS.
            gs_variables-initiator = c_log_initiator-save.
            PERFORM messages_initialize USING gs_variables-initiator
                                              c_log_subobject-save.
            PERFORM message_add_table.
            PERFORM messages_table_display USING gs_variables-initiator.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command

ENDCLASS. "lcl_event_handler IMPLEMENTATION

DATA: gr_event_handler TYPE REF TO lcl_event_handler.

INITIALIZATION.
  PERFORM dropdown_tables_display.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_update.

AT SELECTION-SCREEN.
  IF sy-ucomm EQ 'ONLI'.
    PERFORM selection_validations.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.

END-OF-SELECTION.
  PERFORM prepare_output.
  PERFORM prepare_catalog.
  PERFORM prepare_layout.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Form DROPDOWN_TABLES_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM dropdown_tables_display .

  DATA: lt_dynpfields TYPE dynpread_tabtype,
        lwa_dynpfield TYPE dynpread,
        lt_values     TYPE vrm_values,
        lv_vrm_id     TYPE vrm_id.

  SELECT turma_id,
         turma_tipo,
         turma_text
    INTO TABLE @DATA(lt_srchp)
    FROM zfmfpgrouphdr.

  DELETE lt_srchp WHERE turma_tipo IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_srchp COMPARING turma_tipo.

  LOOP AT lt_srchp INTO DATA(lwa_srchp).
    INSERT INITIAL LINE INTO TABLE lt_values
      ASSIGNING FIELD-SYMBOL(<lwa_value>).
    IF sy-subrc EQ 0.
      <lwa_value>-key = lwa_srchp-turma_tipo.
    ENDIF.
  ENDLOOP.

  lv_vrm_id = 'P_TURMAY'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_vrm_id
      values          = lt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CLEAR: p_textx, p_turmax.

  lwa_dynpfield-fieldname = 'PTEXTX'.
  APPEND lwa_dynpfield TO lt_dynpfields.
  lwa_dynpfield-fieldname = 'PTURMAX'.
  APPEND lwa_dynpfield TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZFMTURMA_TIPO'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = gt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  DELETE gt_dd07v WHERE domvalue_l IS INITIAL.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCREEN_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_update .

  LOOP AT SCREEN.
    IF p_new EQ abap_true.
      IF screen-group1 EQ 'NEW'.
        screen-active = '1'.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'OLD'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_old EQ abap_true.
      IF screen-group1 EQ 'NEW'.
        screen-active = '0'.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'OLD'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF p_old EQ abap_true
  AND p_turmay IS NOT INITIAL.
    PERFORM update_turma_id.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

  DATA: lt_pa0002  TYPE STANDARD TABLE OF pa0002 INITIAL SIZE 0,
        lr_fazenda TYPE RANGE OF /agri/gltplnr_fl,
        lv_userid  TYPE sysid,
        lv_pernr   TYPE p0105-pernr,
        lv_next    TYPE zfmturma_id,
        lv_index   TYPE c,
        lv_max_id  TYPE zfmturma_id.

  IF p_new EQ abap_true.
    IF p_turmax IS INITIAL.
*...Informe o código da nova turma. Campo de preenchimento obrigatório.
      MESSAGE e016(zfmfp) DISPLAY LIKE 'I'.
    ELSE.
      READ TABLE gt_dd07v TRANSPORTING NO FIELDS
        WITH KEY domvalue_l = p_turmax.
      IF sy-subrc NE 0.
*...Selecione um Tipo de Turma disponível no matchcode!
        MESSAGE e033(zfmfp) DISPLAY LIKE 'I'.
      ELSE.
        IF p_idx IS INITIAL.
*...Informe o ID da nova turma. Campo de preenchimento obrigatório.
          MESSAGE e025(zfmfp) DISPLAY LIKE 'I'.
        ELSE.
          IF p_idx CO ' 0123456789'.
            IF p_idx EQ '000'.
*...ID da turma deve ser diferente de '000'.
              MESSAGE e035(zfmfp) DISPLAY LIKE 'I'.
            ELSE.
              SELECT turma_id,
                     turma_tipo UP TO 1 ROWS
                INTO @DATA(lwa_id_validate)
                FROM zfmfpgrouphdr
               WHERE turma_id EQ @p_idx.
              ENDSELECT.

              IF sy-subrc EQ 0.
                SELECT MAX( turma_id )
                  INTO @lv_max_id
                  FROM zfmfpgrouphdr.

*...Tamanho máximo 3 caracteres
                IF lv_max_id LT '999'.
                  ADD 1 TO lv_max_id.
                  UNPACK lv_max_id TO lv_max_id.
*...Turma &1 utiliza ID &2. Próximo ID disponível: &3.
                  MESSAGE e034(zfmfp) WITH lwa_id_validate-turma_tipo
                    lwa_id_validate-turma_id lv_max_id DISPLAY LIKE 'I'.
                ELSEIF lv_max_id EQ '999'.
                  SELECT turma_id
                    INTO TABLE @DATA(lt_hdr_turma)
                    FROM zfmfpgrouphdr.

                  SORT lt_hdr_turma BY turma_id ASCENDING.

                  DATA(lv_turmas) = lines( lt_hdr_turma ).
                  DO lv_turmas TIMES.
                    lv_next = sy-index.
                    UNPACK lv_next TO lv_next.
                    READ TABLE lt_hdr_turma TRANSPORTING NO FIELDS
                      WITH KEY turma_id = lv_next BINARY SEARCH.
                    IF sy-subrc NE 0.
*...Turma &1 utiliza ID &2. Próximo ID disponível: &3.
                      MESSAGE e034(zfmfp) WITH lwa_id_validate-turma_tipo
                        lwa_id_validate-turma_id lv_next DISPLAY LIKE 'I'.
                      EXIT.
                    ENDIF.
                  ENDDO.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
*...O Campo ID deve conter somente números!
            MESSAGE e027(zfmfp) WITH p_turmax DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_textx IS INITIAL.
*...Informe a descrição da nova turma. Campo de preenchimento obrigatório.
      MESSAGE e015(zfmfp) DISPLAY LIKE 'I'.
    ENDIF.

    zfmfpgrouphdr-ernam = sy-uname.
    zfmfpgrouphdr-erdat = sy-datum.
    zfmfpgrouphdr-erzet = sy-uzeit.
    zfmfpgrouphdr-turma_tipo = p_turmax.
    zfmfpgrouphdr-turma_text = p_textx.
    zfmfpgrouphdr-turma_id = p_idx.
  ELSEIF p_old EQ abap_true.
    IF p_turmay IS INITIAL.
*...Informe a turma que será modificada. Campo de preenchimento obrigatório
      MESSAGE e014(zfmfp) DISPLAY LIKE 'I'.
    ELSE.
      IF p_idy IS INITIAL.
*...Informe o ID que será modificado. Campo de preenchimento obrigatório
        MESSAGE e028(zfmfp) DISPLAY LIKE 'I'.
      ELSE.
        SELECT SINGLE *
          INTO @DATA(lwa_grouphdr)
          FROM zfmfpgrouphdr
        WHERE turma_tipo EQ @p_turmay
          AND turma_id EQ @p_idy.
        IF sy-subrc NE 0.
*...Verifique Turma e ID informados. Turma &1 ID &2 inexistente.
          MESSAGE e013(zfmfp) WITH p_turmay DISPLAY LIKE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.

    zfmfpgrouphdr = lwa_grouphdr.
    PERFORM fill_description.
    IF zfmfpgrouphdr-turma_id IS NOT INITIAL.
      UNPACK zfmfpgrouphdr-turma_id TO zfmfpgrouphdr-turma_id.
    ENDIF.
  ENDIF.

  lv_userid = zfmfpgrouphdr-ernam.
*--Get pernr for the user
  CALL FUNCTION 'RP_GET_PERNR_FROM_USERID'
    EXPORTING
      begda     = sy-datum
      endda     = sy-datum
      usrid     = lv_userid
      usrty     = '0001'
    IMPORTING
      usr_pernr = lv_pernr
    EXCEPTIONS
      retcd     = 1
      OTHERS    = 2.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = lv_pernr
        infty           = '0002'
      TABLES
        infty_tab       = lt_pa0002
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    IF sy-subrc EQ 0.
      READ TABLE lt_pa0002 INTO DATA(lwa_pa0002) INDEX 1.
      IF sy-subrc EQ 0.
        v_cname = lwa_pa0002-cname.
      ENDIF.
    ENDIF.
  ENDIF.

  IF v_cname IS INITIAL.
    SELECT SINGLE adrp~name_text INTO v_cname
      FROM usr21 JOIN adrp
        ON usr21~persnumber = adrp~persnumber
       AND adrp~date_from   = '00010101'
       AND adrp~nation      = ''
     WHERE usr21~bname = zfmfpgrouphdr-ernam.
  ENDIF.

  PERFORM get_domain_fixed_value.

  DO 5 TIMES.
    lv_index = sy-index.
    CONCATENATE 'FAZENDA' lv_index INTO DATA(lv_fieldname).
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE zfmfpgrouphdr
      TO FIELD-SYMBOL(<lv_fazenda>).
    IF sy-subrc EQ 0
    AND <lv_fazenda> IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lr_fazenda
        ASSIGNING FIELD-SYMBOL(<lwa_fazenda>).
      IF sy-subrc EQ 0.
        <lwa_fazenda> = 'IEQ'.
        <lwa_fazenda>-low = <lv_fazenda>.
      ENDIF.
    ENDIF.
  ENDDO.

  IF lr_fazenda[] IS NOT INITIAL.
    SELECT tplnr_fl, pltxt
      FROM /agri/glflot
      INTO TABLE @DATA(lt_fazendas)
     WHERE tplnr_fl IN @lr_fazenda[].

    IF sy-subrc EQ 0.
      SORT lt_fazendas BY tplnr_fl.
      IF zfmfpgrouphdr-fazenda1 IS NOT INITIAL.
        READ TABLE lt_fazendas INTO DATA(lwa_fazenda)
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda1 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda1 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda2 IS NOT INITIAL.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda2 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda2 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda3 IS NOT INITIAL.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda3 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda3 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda4 IS NOT INITIAL.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda4 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda4 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda5 IS NOT INITIAL.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda5 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda5 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  DATA: lt_f4      TYPE lvc_t_f4,
        lwa_f4     TYPE lvc_s_f4,
        lt_buttons TYPE ui_functions,
        lwa_button TYPE ui_func.

  SET TITLEBAR 'MAIN100'.
  SET PF-STATUS 'MAIN100'.
  IF g_custom_container IS INITIAL.

*... Toolbar Button CHECK
    lwa_button = cl_gui_alv_grid=>mc_fc_check.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button REFRESH
    lwa_button = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button UNDO
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button PRINT
    lwa_button = cl_gui_alv_grid=>mc_fc_print_back.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button Print Preview
    lwa_button = cl_gui_alv_grid=>mc_fc_print_prev.
    APPEND lwa_button TO lt_buttons.
*... Toolbar Button INFORMATION
    lwa_button = cl_gui_alv_grid=>mc_fc_info.
    APPEND lwa_button TO lt_buttons.
*... Menu Button SUBTOTAL
    lwa_button = cl_gui_alv_grid=>mc_mb_subtot.
    APPEND lwa_button TO lt_buttons.
*... Menu Button SUM add Menu Item SUM
    lwa_button = cl_gui_alv_grid=>mc_fc_sum.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_mb_view.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_mb_sum.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND lwa_button TO lt_buttons.
    lwa_button = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND lwa_button TO lt_buttons.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = g_custom_container.
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZFMFPGROUPITM'
      CHANGING
*       it_outtab                     = gt_groupitm
        it_outtab                     = gt_outtab
        it_fieldcatalog               = gt_fieldcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT gr_event_handler.
    SET HANDLER:
     gr_event_handler->handle_data_changed_finished FOR gr_grid,
     gr_event_handler->handle_user_command FOR gr_grid,
     gr_event_handler->handle_toolbar FOR gr_grid.

    CALL METHOD gr_grid->set_toolbar_interactive.

    CALL METHOD gr_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD gr_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD cl_gui_control=>set_focus EXPORTING control = gr_grid.

    lwa_f4-fieldname  = 'PERNR'.
    lwa_f4-register   = abap_true.
    lwa_f4-getbefore  = abap_true.
    lwa_f4-chngeafter = abap_true.
    INSERT lwa_f4 INTO TABLE lt_f4.
    lwa_f4-fieldname  = 'IDRESOURCE'.
    INSERT lwa_f4 INTO TABLE lt_f4.

    CALL METHOD gr_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

    SET HANDLER gr_event_handler->handle_on_f4 FOR gr_grid.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      PERFORM clear.
      PERFORM exit_program.
    WHEN OTHERS.
      PERFORM fill_description.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit_program .

  SET SCREEN '0'.
  LEAVE SCREEN.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_DOMAIN_FIXED_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_domain_fixed_value .

  REFRESH: gt_taba, gt_tabb.
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = 'ZFM_STATUS'
      langu         = sy-langu
      withtext      = 'X'
    TABLES
      dd07v_tab_a   = gt_taba
      dd07v_tab_n   = gt_tabb
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0
  AND zfmfpgrouphdr-status IS NOT INITIAL.
    READ TABLE gt_taba INTO DATA(lwa_taba)
      WITH KEY domvalue_l = zfmfpgrouphdr-status.
    IF sy-subrc EQ 0.
      v_status_txt = lwa_taba-ddtext.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module INIT_LISTBOX OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_listbox OUTPUT.

  DATA: name TYPE vrm_id,
        list TYPE vrm_values.

  name = 'ZFMFPGROUPHDR-STATUS'.

  LOOP AT gt_taba INTO DATA(lwa_taba).
    INSERT INITIAL LINE INTO TABLE list
      ASSIGNING FIELD-SYMBOL(<lwa_list>).
    IF sy-subrc EQ 0.
      <lwa_list>-key  = lwa_taba-domvalue_l.
      <lwa_list>-text = lwa_taba-ddtext.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form PREPARE_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_catalog .

  DATA: lt_fieldcat_slis TYPE slis_t_fieldcat_alv.

  REFRESH: gt_fieldcat, gt_groupitm.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSC_FMFPGROUPITM'
    CHANGING
      ct_fieldcat            = lt_fieldcat_slis
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fieldcat_slis
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat[]
    TABLES
      it_data         = gt_groupitm[]
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0.
    LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<lwa_fieldcat>).
      CASE <lwa_fieldcat>-fieldname.
        WHEN 'PERNR'.
          <lwa_fieldcat>-edit = abap_true.
          <lwa_fieldcat>-f4availabl = abap_true.
        WHEN 'IDRESOURCE'.
          <lwa_fieldcat>-edit = abap_true.
          <lwa_fieldcat>-ref_field = 'IDRESOURCE'.
          <lwa_fieldcat>-ref_table = '/AGRI/FMACRES'.
          <lwa_fieldcat>-f4availabl = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_layout .

  gs_layout-zebra = abap_true.
  gs_layout-smalltitle = abap_true.
  gs_layout-cwidth_opt = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
FORM messages_initialize USING lv_initiator TYPE /agri/gdescr
                               lv_subobject TYPE balsubobj.

  messages_init.
  CHECK lv_initiator IS NOT INITIAL.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

  IF ref_log_handler IS INITIAL.
    CREATE OBJECT ref_log_handler.
    message_log_event_handler_set ref_log_handler
                                  on_log_display_profile.
  ENDIF.

ENDFORM.                    " messages_initialize

*&---------------------------------------------------------------------*
*& Form MESSAGES_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM messages_table_display USING lv_initiator TYPE /agri/gdescr.

  CONSTANTS: c_program TYPE sy-repid VALUE 'ZFMFPGROUP_MANAGEMENT'.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator abap_true space space ls_variant.
  messages_init.
  CLEAR lv_initiator.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  message_add_table
*&---------------------------------------------------------------------*
FORM message_add_table.

  DATA: ls_message TYPE /agri/s_gprolog.

  LOOP AT gt_message INTO ls_message.
    PERFORM messages_context_set USING ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

ENDFORM.                    "message_add_table

*&---------------------------------------------------------------------*
*&      Form  messages_context_set
*&---------------------------------------------------------------------*
FORM messages_context_set USING ls_message TYPE /agri/s_gprolog.

  DATA: lref_newtab  TYPE REF TO data,
        lref_newline TYPE REF TO data,
        lwa_sheet    TYPE /agri/s_excel_sheet,
        lwa_data,
        lv_row       TYPE i.

  FIELD-SYMBOLS: <lf_value>    TYPE c,
                 <lt_dyntable> TYPE table,
                 <ls_dynline>  TYPE any,
                 <lwa_sheet>   TYPE /agri/s_excel_sheet.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->context_data_get
    EXPORTING
      es_context_data = gs_context_data.

  gs_context_data-document        = ls_message-context-document.
  gs_context_data-docvariant      = ls_message-context-docvariant.
  gs_context_data-context-tabname = ls_message-context-context-tabname.
  gs_context_data-context-value   = ls_message-context-context-value.

  CALL METHOD ref_process_log->context_data_set
    EXPORTING
      is_context_data = gs_context_data.

ENDFORM.                    "messages_context_set

*&---------------------------------------------------------------------*
*& Form ADD_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_message.

  INSERT INITIAL LINE INTO TABLE gt_message
    ASSIGNING FIELD-SYMBOL(<lwa_message>).
  IF sy-subrc EQ 0.
    <lwa_message>-msgid = sy-msgid.
    <lwa_message>-msgty = sy-msgty.
    <lwa_message>-msgno = sy-msgno.
    <lwa_message>-msgv1 = sy-msgv1.
    <lwa_message>-msgv2 = sy-msgv2.
    <lwa_message>-msgv3 = sy-msgv3.
    <lwa_message>-msgv4 = sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_data .

  REFRESH: gt_groupitm, gt_pa0002, gt_outtab.

  IF p_old EQ abap_true.
    SELECT  *
      INTO TABLE @gt_groupitm
      FROM zfmfpgroupitm
    WHERE turma_id EQ @p_idy.

    IF sy-subrc EQ 0.
      SELECT pernr cname
        FROM pa0002
        INTO TABLE gt_pa0002
        FOR ALL ENTRIES IN gt_groupitm
       WHERE pernr = gt_groupitm-pernr.

      SORT gt_pa0002 BY pernr.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepare_output .

  gt_outtab[] = CORRESPONDING #( gt_groupitm[] ).
  LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<lwa_outtab>).
    IF p_old EQ abap_true.
      <lwa_outtab>-turma_tipo = p_turmay.
    ELSEIF p_new EQ abap_true.
      <lwa_outtab>-turma_tipo = p_turmax.
    ENDIF.
    READ TABLE gt_pa0002 INTO DATA(lwa_pa0002)
      WITH KEY pernr = <lwa_outtab>-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_outtab>-cname = lwa_pa0002-cname.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear.

  DATA: lt_dynpfields TYPE dynpread_tabtype,
        lwa_dynpfield TYPE dynpread.

  REFRESH: gt_groupitm, gt_outtab, gt_pa0002, gt_fieldcat,
           gt_taba, gt_tabb, row_id.

  CLEAR: gs_layout, gs_variables, ok_code, p_turmax, p_textx,
         row_i, col_i, v_status_txt, v_cname.

ENDFORM.

**&---------------------------------------------------------------------*
**&      Module  FILL_DESCRIPTION  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE fill_description INPUT.
*
*  PERFORM fill_description.
*
*ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FILL_DESCRIPTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_description .

  TYPES: BEGIN OF ltype_pa0002,
           pernr TYPE persno,
           cname TYPE pad_cname,
         END OF ltype_pa0002.

  DATA: lt_pa0002  TYPE STANDARD TABLE OF ltype_pa0002 INITIAL SIZE 0,
        lt_pernr   LIKE lt_pa0002,
        lr_fazenda TYPE RANGE OF /agri/gltplnr_fl,
        lv_index   TYPE c.

  CLEAR: v_coordenador, v_supervisor, v_encarregado, v_lider,
         v_fazenda1, v_fazenda2, v_fazenda3, v_fazenda4,
         v_fazenda5,
"Begin of José Sequeira - 09.02.2021 13:52:23
         v_encarregado_adm.
"End of José Sequeira - 09.02.2021 13:52:23

  lt_pernr = VALUE #( ( pernr = zfmfpgrouphdr-coordenador )
                      ( pernr = zfmfpgrouphdr-supervisor )
                      ( pernr = zfmfpgrouphdr-encarregado )
                      "Begin of José Sequeira - 09.02.2021 13:53:45
                      ( pernr = zfmfpgrouphdr-encarregado_adm )
                      "End of José Sequeira - 09.02.2021 13:53:45
                      ( pernr = zfmfpgrouphdr-lider_turma ) ).

  DELETE lt_pernr WHERE pernr IS INITIAL.

  IF lt_pernr[] IS NOT INITIAL.
    SELECT pernr,
           cname
      INTO TABLE @lt_pa0002
      FROM pa0002
      FOR ALL ENTRIES IN @lt_pernr
     WHERE pernr = @lt_pernr-pernr.

    SORT lt_pa0002 BY pernr.

    READ TABLE lt_pa0002 INTO DATA(lwa_pa0002)
      WITH KEY pernr = zfmfpgrouphdr-coordenador BINARY SEARCH.
    IF sy-subrc EQ 0.
      v_coordenador = lwa_pa0002-cname.
    ENDIF.

    READ TABLE lt_pa0002 INTO lwa_pa0002
      WITH KEY pernr = zfmfpgrouphdr-supervisor BINARY SEARCH.
    IF sy-subrc EQ 0.
      v_supervisor = lwa_pa0002-cname.
    ENDIF.

    READ TABLE lt_pa0002 INTO lwa_pa0002
      WITH KEY pernr = zfmfpgrouphdr-encarregado BINARY SEARCH.
    IF sy-subrc EQ 0.
      v_encarregado = lwa_pa0002-cname.
    ENDIF.

"Begin of José Sequeira - 09.02.2021 13:52:55
    READ TABLE lt_pa0002 INTO lwa_pa0002
      WITH KEY pernr = zfmfpgrouphdr-encarregado_adm BINARY SEARCH.
    IF sy-subrc EQ 0.
      v_encarregado_adm = lwa_pa0002-cname.
    ENDIF.
"End of José Sequeira - 09.02.2021 13:52:55

    READ TABLE lt_pa0002 INTO lwa_pa0002
      WITH KEY pernr = zfmfpgrouphdr-lider_turma BINARY SEARCH.
    IF sy-subrc EQ 0.
      v_lider = lwa_pa0002-cname.
    ENDIF.
  ENDIF.

  DO 5 TIMES.
    lv_index = sy-index.
    CONCATENATE 'FAZENDA' lv_index INTO DATA(lv_fieldname).
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE zfmfpgrouphdr
      TO FIELD-SYMBOL(<lv_fazenda>).
    IF sy-subrc EQ 0
    AND <lv_fazenda> IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lr_fazenda
        ASSIGNING FIELD-SYMBOL(<lwa_fazenda>).
      IF sy-subrc EQ 0.
        <lwa_fazenda> = 'IEQ'.
        <lwa_fazenda>-low = <lv_fazenda>.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lv_fazenda>
          IMPORTING
            output     = <lwa_fazenda>-low
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
      ENDIF.
    ENDIF.
  ENDDO.

  IF lr_fazenda[] IS NOT INITIAL.
    SELECT tplnr_fl, pltxt
      FROM /agri/glflot
      INTO TABLE @DATA(lt_fazendas)
     WHERE tplnr_fl IN @lr_fazenda[].

    IF sy-subrc EQ 0.
      SORT lt_fazendas BY tplnr_fl.
      IF zfmfpgrouphdr-fazenda1 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = zfmfpgrouphdr-fazenda1
          IMPORTING
            output     = zfmfpgrouphdr-fazenda1
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        READ TABLE lt_fazendas INTO DATA(lwa_fazenda)
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda1 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda1 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda2 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = zfmfpgrouphdr-fazenda2
          IMPORTING
            output     = zfmfpgrouphdr-fazenda2
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda2 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda2 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda3 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = zfmfpgrouphdr-fazenda3
          IMPORTING
            output     = zfmfpgrouphdr-fazenda3
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda3 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda3 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda4 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = zfmfpgrouphdr-fazenda4
          IMPORTING
            output     = zfmfpgrouphdr-fazenda4
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda4 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda4 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.

      IF zfmfpgrouphdr-fazenda5 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = zfmfpgrouphdr-fazenda5
          IMPORTING
            output     = zfmfpgrouphdr-fazenda5
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        READ TABLE lt_fazendas INTO lwa_fazenda
          WITH KEY tplnr_fl = zfmfpgrouphdr-fazenda5 BINARY SEARCH.
        IF sy-subrc EQ 0.
          v_fazenda5 = lwa_fazenda-pltxt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_TURMA_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_turma_id .

  TYPES: BEGIN OF type_srchp,
           turma_tipo TYPE zfmturma_tipo,
           turma_id   TYPE zfmturma_id,
           turma_text TYPE zfmturma_text,
         END OF type_srchp,

         type_srchp_t TYPE STANDARD TABLE OF type_srchp INITIAL SIZE 0.

  DATA: lt_dynpfields TYPE dynpread_tabtype,
        lwa_dynpfield TYPE dynpread,
        lt_srchp      TYPE type_srchp_t,
        lt_values     TYPE vrm_values,
        lv_vrm_id     TYPE vrm_id.

  SELECT turma_tipo,
         turma_id,
         turma_text
    INTO TABLE @lt_srchp
    FROM zfmfpgrouphdr
   WHERE turma_tipo = @p_turmay.

  LOOP AT lt_srchp INTO DATA(lwa_srchp).
    INSERT INITIAL LINE INTO TABLE lt_values
      ASSIGNING FIELD-SYMBOL(<lwa_value>).
    IF sy-subrc EQ 0.
      <lwa_value>-key = lwa_srchp-turma_id.
      <lwa_value>-text = lwa_srchp-turma_text.
    ENDIF.
  ENDLOOP.

  SORT lt_values BY key ASCENDING.

  lv_vrm_id = 'P_IDY'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_vrm_id
      values          = lt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CLEAR: p_textx, p_turmax.

  lwa_dynpfield-fieldname = 'P_IDY'.
  APPEND lwa_dynpfield TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module FORMAT_OUTPUT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE format_output OUTPUT.

  IF zfmfpgrouphdr-fazenda1 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = zfmfpgrouphdr-fazenda1
      IMPORTING
        output = zfmfpgrouphdr-fazenda1.
  ENDIF.

  IF zfmfpgrouphdr-fazenda2 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = zfmfpgrouphdr-fazenda2
      IMPORTING
        output = zfmfpgrouphdr-fazenda2.
  ENDIF.

  IF zfmfpgrouphdr-fazenda3 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = zfmfpgrouphdr-fazenda3
      IMPORTING
        output = zfmfpgrouphdr-fazenda3.
  ENDIF.

  IF zfmfpgrouphdr-fazenda4 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = zfmfpgrouphdr-fazenda4
      IMPORTING
        output = zfmfpgrouphdr-fazenda4.
  ENDIF.

  IF zfmfpgrouphdr-fazenda5 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = zfmfpgrouphdr-fazenda5
      IMPORTING
        output = zfmfpgrouphdr-fazenda5.
  ENDIF.

ENDMODULE.
