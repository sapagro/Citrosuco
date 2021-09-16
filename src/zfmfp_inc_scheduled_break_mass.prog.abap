*&---------------------------------------------------------------------*
*& Include ZFMFP_INC_TASK_UPLOAD
*&---------------------------------------------------------------------*

TABLES: /irm/gfsub.

INCLUDE /agri/global_badis.
INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
INCLUDE /agri/glpg_upload_process_sel.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0a.
INCLUDE /agri/glpg_upload_process_f0c.
INCLUDE /agri/glpg_upload_process_f0d.
INCLUDE /agri/glpg_upload_process_f0f.
INCLUDE /agri/glpg_upload_process_f0i.
INCLUDE /agri/glpg_upload_process_f0m.
INCLUDE /agri/glpg_upload_process_f0t.

DATA: gt_excel          TYPE /agri/t_excel_sheet,
      gt_bdc_messages   TYPE zt_fmfp_bdcmsgcoll,
      gv_file_extension TYPE toadd-doc_type,
      gv_subrc          TYPE sysubrc.

TYPES: BEGIN OF type_used_number,
         acppg TYPE zfmacppg,
         paunr TYPE pausenr,
       END OF type_used_number,

       type_t_used_number TYPE SORTED TABLE OF type_used_number
                          WITH UNIQUE KEY acppg paunr,

       type_t_max_number  TYPE STANDARD TABLE OF type_used_number.

***Rel 60E_SP1 - Authorization check for Customizing Transactions
DEFINE config_tcode_authority_check.
  AUTHORITY-CHECK OBJECT 'V_AB_CNFTX'
        ID '/AGRI/TCOD' FIELD &1
        ID 'ACTVT' FIELD &2.
  &3 = sy-subrc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Form FILE_PATH_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FNAME
*&---------------------------------------------------------------------*
FORM file_path_get CHANGING lv_filename.

  DATA: lv_rc            TYPE i,
        lv_title         TYPE string,
        ltd_table        TYPE filetable,
        lv_path_filename TYPE string.

*"Selecione o Arquivo/Select the File
  lv_title = TEXT-002.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_title
    CHANGING
      file_table              = ltd_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF lv_rc = 1.
    READ TABLE ltd_table INTO lv_path_filename INDEX 1.
    lv_filename = lv_path_filename.
  ELSE.
    IF NOT sy-msgid IS INITIAL
    AND NOT sy-msgty IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      message_simple space.
    ELSE.
*"Nome/caminho de arquivo incorreto
      MESSAGE i173(/agri/global).
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCEL_DATA_IMPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM excel_data_import USING lv_file_name lv_file_extension
                    CHANGING lt_excel TYPE /agri/t_excel_sheet
                             lt_msgs  TYPE zt_fmfp_bdcmsgcoll
                             lv_subrc TYPE sysubrc.

  DATA: lv_route TYPE char0256.

  REFRESH: lt_excel, lt_msgs.
  CLEAR: lv_subrc.

  lv_route = lv_file_name.

  DATA: lt_tab      TYPE STANDARD TABLE OF alsmex_tabline INITIAL SIZE 0,
        lv_filename TYPE localfile.

  lv_filename = lv_route.

*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*    EXPORTING
*      filename                = lv_filename
*      i_begin_col             = 1
*      i_begin_row             = 2
*      i_end_col               = 9
*      i_end_row               = 4000
*    TABLES
*      intern                  = lt_tab
*    EXCEPTIONS
*      inconsistent_parameters = 1
*      upload_ole              = 2
*      OTHERS                  = 3.

  CALL METHOD /agri/cl_glupload_master_data=>read_excel_file
    EXPORTING
      i_route  = lv_route
    IMPORTING
      et_sheet = lt_excel.

  IF lt_excel[] IS INITIAL.
*"Não existem dados para upload
    MESSAGE i002(zfmfp) INTO sy-msgli.
    message_simple space.
    lv_subrc = 4.
  ELSE.
    PERFORM create_sched_break USING lt_excel
                            CHANGING lt_msgs.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILE_EXTENSION_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_FILENAME
*&      --> LV_FILE_EXTENSION
*&---------------------------------------------------------------------*
FORM file_extension_get USING lv_file_name
                     CHANGING lv_file_extension lv_subrc.

  CLEAR lv_file_extension.
  lv_file_extension = segment( val = lv_file_name index = -1 sep = '.').

  IF lv_file_extension <> space.
    SET LOCALE LANGUAGE sy-langu.
    TRANSLATE lv_file_extension TO UPPER CASE.
    SET LOCALE LANGUAGE space.
  ENDIF.

  IF  lv_file_extension NE 'TXT'
  AND lv_file_extension NE 'XLS'
  AND lv_file_extension NE 'XLSX'
  AND lv_file_extension NE 'CSV'.
*"Não são suportados tipos de arquivo diferentes de TXT
*"File types other than TXT/XLS/XLSX/CSV are not supported
    MESSAGE ID 'ZFMFP' TYPE /agri/if_bcon=>mc_msg_type-error
      NUMBER '001' INTO sy-msgli.
    message_simple space.
    lv_subrc = 4.
    EXIT.
  ENDIF.

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
    IF screen-group1 = 'UPL'
    OR screen-group1 = 'UP1'.
      screen-active = '0'.
      MODIFY SCREEN.
    ELSEIF screen-name = '%B001000_BLOCK_1000'
        OR screen-name = '%_P_FILE_%_APP_%-TEXT'
        OR screen-name = 'P_FILE'
        OR screen-name = '%B001010_BLOCK_1000'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_SCHED_BREAK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXCEL
*&---------------------------------------------------------------------*
FORM create_sched_break USING lt_excel    TYPE /agri/t_excel_sheet
                     CHANGING lt_messages TYPE zt_fmfp_bdcmsgcoll.

  TYPES: BEGIN OF type_t001w,
           werks TYPE werks_d,
           name1 TYPE name1,
         END OF type_t001w,

         BEGIN OF type_tfacd,
           ident TYPE wfcid,
         END OF type_tfacd,

         BEGIN OF type_crhd,
           objty TYPE cr_objty,
           objid TYPE cr_objid,
           arbpl TYPE arbpl,
           werks TYPE werks_d,
         END OF type_crhd.

  DATA: lref_upload     TYPE REF TO zfmcl_excel_upload,
        lt_spreadsheet1 TYPE /agri/t_excel_sheet,
        lt_spreadsheet2 TYPE /agri/t_excel_sheet,
        lt_list         TYPE table_abaplist,
        lt_ascii        TYPE STANDARD TABLE OF soli INITIAL SIZE 0,
        lt_split        TYPE STANDARD TABLE OF so_text255 INITIAL SIZE 0,
        lt_sched_break1 TYPE STANDARD TABLE OF zsc_fmfp_scheduled_break1 INITIAL SIZE 0,
        lt_sched_break2 TYPE STANDARD TABLE OF zsc_fmfp_scheduled_break2 INITIAL SIZE 0,
        lv_row          TYPE /agri/s_excel_sheet-row.

  DATA: lt_centro_new     TYPE STANDARD TABLE OF zfmacwct INITIAL SIZE 0,
        lt_turno_new      TYPE STANDARD TABLE OF zfmacwork_shift INITIAL SIZE 0,
        lt_parada_new     TYPE STANDARD TABLE OF zfmacsched_break INITIAL SIZE 0,
        lt_t001w          TYPE STANDARD TABLE OF type_t001w INITIAL SIZE 0,
        lt_tfacd          TYPE STANDARD TABLE OF type_tfacd INITIAL SIZE 0,
        lt_crhd           TYPE STANDARD TABLE OF type_crhd INITIAL SIZE 0,
        lt_parada_sum     LIKE lt_parada_new,
        lt_parada_old     LIKE lt_parada_new,
        lt_zfmacwct       LIKE lt_centro_new,
        lwa_parada        LIKE LINE OF lt_parada_new,
        lv_parada         TYPE zfmacppg,
        lv_start_date     TYPE dats,
        lv_end_date       TYPE dats,
        lv_capacidade_tot TYPE zfmactut,
        lv_next_number    TYPE pausenr.

  CONSTANTS: BEGIN OF c_structures,
               spreadsheet1 TYPE tabname VALUE 'ZSC_FMFP_SCHEDULED_BREAK1',
               sheet1       TYPE numc2   VALUE 01,
               spreadsheet2 TYPE tabname VALUE 'ZSC_FMFP_SCHEDULED_BREAK2',
               sheet2       TYPE numc2   VALUE 02,
             END OF c_structures,

             BEGIN OF c_categoria,
               trabalho TYPE crhd-objty VALUE 'A ',
             END OF c_categoria.

  PERFORM structure_build USING c_structures-spreadsheet1
                                c_structures-sheet1
                                lt_excel
                       CHANGING lt_spreadsheet1
                                lt_sched_break1
                                lt_sched_break2.

  PERFORM structure_build USING c_structures-spreadsheet2
                                c_structures-sheet2
                                lt_excel
                       CHANGING lt_spreadsheet2
                                lt_sched_break1
                                lt_sched_break2.

*.ZFMACWCT (Centro de Trabalho)
  lt_centro_new[] = CORRESPONDING #( lt_sched_break2[] ).

*  SORT lt_centro_new BY werks arbpl fabkl.
  IF lt_centro_new[] IS NOT INITIAL.
    SELECT werks,
           name1
      FROM t001w
      INTO TABLE @lt_t001w
      FOR ALL ENTRIES IN @lt_centro_new
     WHERE werks = @lt_centro_new-werks.

    SELECT ident
      FROM tfacd
      INTO TABLE @lt_tfacd
      FOR ALL ENTRIES IN @lt_centro_new
     WHERE ident = @lt_centro_new-fabkl.

    SELECT objty,
           objid,
           arbpl,
           werks
      FROM crhd
      INTO TABLE @lt_crhd
      FOR ALL ENTRIES IN @lt_centro_new
     WHERE objty = @c_categoria-trabalho
       AND arbpl = @lt_centro_new-arbpl
       AND werks = @lt_centro_new-werks.

    SORT: lt_t001w BY werks,
          lt_tfacd BY ident,
          lt_crhd  BY werks arbpl.

    REFRESH gt_message.

*...ZFMACWCT (Centro de Trabalho)
    DATA(lv_aba) = 2.
    LOOP AT lt_centro_new INTO DATA(lwa_centro).
      DATA(lv_linha) = sy-tabix.
      DATA(lv_linha_excel) = sy-tabix + 1.
      READ TABLE lt_t001w TRANSPORTING NO FIELDS
        WITH KEY werks = lwa_centro-werks BINARY SEARCH.
      IF sy-subrc NE 0.
*       78:Excel linha &1 aba &2: Centro trabalho &3 inexistente.
        MESSAGE e078(zfmac) WITH lv_linha_excel lv_aba lwa_centro-werks INTO dummy.
        PERFORM add_message.
        READ TABLE lt_sched_break2 ASSIGNING FIELD-SYMBOL(<lwa_break2>) INDEX lv_linha.
        IF sy-subrc EQ 0.
          <lwa_break2>-invalido = abap_true.
        ENDIF.
        CONTINUE.
      ELSE.
        READ TABLE lt_tfacd TRANSPORTING NO FIELDS
          WITH KEY ident = lwa_centro-fabkl BINARY SEARCH.
        IF sy-subrc NE 0.
*         79:Excel linha &1 aba &2: Calendário &3 inexistente.
          MESSAGE e079(zfmac) WITH lv_linha_excel lv_aba lwa_centro-fabkl INTO dummy.
          PERFORM add_message.
          READ TABLE lt_sched_break2 ASSIGNING <lwa_break2> INDEX lv_linha.
          IF sy-subrc EQ 0.
            <lwa_break2>-invalido = abap_true.
          ENDIF.
          CONTINUE.
        ELSE.
          READ TABLE lt_crhd TRANSPORTING NO FIELDS
            WITH KEY werks = lwa_centro-werks
                     arbpl = lwa_centro-arbpl BINARY SEARCH.
          IF sy-subrc NE 0.
*           80:Excel linha &1 aba &2: Centro trabalho &3 inex. p/ centro &3.
            MESSAGE e080(zfmac) WITH lv_linha_excel lv_aba
              lwa_centro-arbpl lwa_centro-werks INTO dummy.
            PERFORM add_message.
            READ TABLE lt_sched_break2 ASSIGNING <lwa_break2> INDEX lv_linha.
            IF sy-subrc EQ 0.
              <lwa_break2>-invalido = abap_true.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      INSERT zfmacwct FROM lwa_centro.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
*       81:Excel linha &1 aba &2: Centro de trabalho &3 atualizado c/ sucesso.
        MESSAGE s081(zfmac) WITH lv_linha_excel lv_aba lwa_centro-arbpl INTO dummy.
        PERFORM add_message.
      ENDIF.
    ENDLOOP.
    SORT gt_message BY msgid msgno msgv1 msgv2 msgv3 msgv4.
    DELETE ADJACENT DUPLICATES FROM gt_message COMPARING ALL FIELDS.
  ENDIF.

*.ZFMACSCHED_BREAK (Paradas Programadas)
*-- BOC-T_T.KONNO
*  lt_parada_new[] = CORRESPONDING #( lt_sched_break1[] ).
  LOOP AT lt_sched_break1 INTO DATA(lwa_new_entry).
    INSERT INITIAL LINE INTO TABLE lt_parada_new
      ASSIGNING FIELD-SYMBOL(<lwa_new_entry>).
    IF sy-subrc EQ 0.
      <lwa_new_entry>-idactv = lwa_new_entry-idactv.
      <lwa_new_entry>-acppg = lwa_new_entry-acppg.
      <lwa_new_entry>-period = lwa_new_entry-period.
      <lwa_new_entry>-acdes = lwa_new_entry-acdes.
      <lwa_new_entry>-acprt = lwa_new_entry-acprt.
    ENDIF.
  ENDLOOP.
*-- EOC-T_T.KONNO

  IF lt_parada_new[] IS NOT INITIAL.
    SELECT *
      FROM zfmacsched_break
      INTO TABLE @lt_parada_old
      FOR ALL ENTRIES IN @lt_parada_new
     WHERE acppg = @lt_parada_new-acppg.

    SORT: lt_parada_old BY acppg acdes.

    lv_aba = 1.
    LOOP AT lt_parada_new INTO lwa_parada.
      lv_linha_excel = sy-tabix + 1.
      INSERT zfmacsched_break FROM lwa_parada.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
*       83:Excel linha &1 aba &2: Parada programada &3 atualizada c/ sucesso.
        MESSAGE s083(zfmac) WITH lv_linha_excel lv_aba lwa_parada-acppg INTO dummy.
        PERFORM add_message.
      ELSEIF sy-subrc EQ 4.
*       89:Excel linha &1 aba &2: Parada programada &3 existente.
        MESSAGE e089(zfmac) WITH lv_linha_excel lv_aba lwa_parada-acppg INTO dummy.
        PERFORM add_message.
      ENDIF.
    ENDLOOP.
  ENDIF.

*.ZFMACWORK_SHIFT (Turnos)
*  SORT lt_sched_break2 BY actrn acppg werks arbpl fabkl.
  lt_turno_new[] = CORRESPONDING #( lt_sched_break2[] ).

  lv_start_date = lv_end_date = sy-datum.

  IF lt_turno_new[] IS NOT INITIAL.
    SELECT *
      FROM zfmacsched_break
      INTO TABLE @DATA(lt_paradas)
      FOR ALL ENTRIES IN @lt_turno_new
     WHERE acppg = @lt_turno_new-acppg.

    SORT lt_paradas BY acppg period.

    IF sy-subrc EQ 0.
      LOOP AT lt_paradas INTO lwa_parada.
        CLEAR: lwa_parada-acdes, lwa_parada-idactv.
        COLLECT lwa_parada INTO lt_parada_sum.
      ENDLOOP.

      SORT lt_parada_sum BY acppg period.
    ENDIF.
  ENDIF.

  lv_aba = 2.
  LOOP AT lt_turno_new ASSIGNING FIELD-SYMBOL(<lwa_turno>).
    lv_linha_excel = sy-tabix + 1.
    READ TABLE lt_sched_break2 ASSIGNING <lwa_break2> INDEX sy-tabix.
    IF <lwa_break2>-invalido EQ abap_true.
      CONTINUE.
    ENDIF.
    READ TABLE lt_paradas TRANSPORTING NO FIELDS
      WITH KEY acppg  = <lwa_turno>-acppg
               period = <lwa_turno>-period BINARY SEARCH.
    IF sy-subrc NE 0.
*     87:Excel linha &1 aba &2: Parada programada &3 inexistente.
      MESSAGE e087(zfmac) WITH lv_linha_excel lv_aba <lwa_turno>-acppg INTO dummy.
      PERFORM add_message.
      CONTINUE.
    ENDIF.
    READ TABLE lt_tfacd TRANSPORTING NO FIELDS
      WITH KEY ident = <lwa_turno>-fabkl BINARY SEARCH.
    PERFORM calc_time_diff USING <lwa_turno>-achft
                                 <lwa_turno>-achit
                        CHANGING lv_capacidade_tot.
    READ TABLE lt_parada_sum INTO lwa_parada
      WITH KEY acppg = <lwa_turno>-acppg
               period = <lwa_turno>-period BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_turno>-actut = lv_capacidade_tot - ( lv_capacidade_tot * ( lwa_parada-acprt / 100 ) ).
      IF lv_capacidade_tot IS NOT INITIAL.
        <lwa_turno>-acocp = ( <lwa_turno>-actut / lv_capacidade_tot ) * 100.
      ENDIF.
      <lwa_turno>-accpd = <lwa_turno>-acnci * <lwa_turno>-actut.
    ENDIF.
    INSERT zfmacwork_shift FROM <lwa_turno>.
    IF sy-subrc EQ 0.
*     86:Excel linha &1 aba &2: Turno &3 atualizado c/ sucesso.
      MESSAGE s086(zfmac) WITH lv_linha_excel lv_aba <lwa_turno>-actrn INTO dummy.
      PERFORM add_message.
    ELSEIF sy-subrc EQ 4.
*     88:Excel linha &1 aba &2: Turno &3 existente.
      MESSAGE e088(zfmac) WITH lv_linha_excel lv_aba <lwa_turno>-actrn INTO dummy.
      PERFORM add_message.
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_message COMPARING ALL FIELDS.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form STRUCTURE_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> C_STRUCTURES_SPREADSHEET1
*&      <-- LT_SPREADSHEET1
*&---------------------------------------------------------------------*
FORM structure_build  USING lv_spreadsheet TYPE tabname
                            lv_sheet       TYPE numc2
                            lt_excel       TYPE /agri/t_excel_sheet
                   CHANGING lt_spreadsheet TYPE /agri/t_excel_sheet
                            lt_sched_break1 TYPE ztt_sched_break1
                            lt_sched_break2 TYPE ztt_sched_break2.

  DATA: lt_dd03l    TYPE STANDARD TABLE OF dd03l INITIAL SIZE 0,
        lref_upload TYPE REF TO zfmcl_excel_upload,
        lv_time     TYPE sy-uzeit,
        lv_row      TYPE /agri/s_excel_sheet-row.

  DATA: ld_date      LIKE sy-datum,
        ld_time      LIKE sy-uzeit,
        ld_field(30).

  CONSTANTS: BEGIN OF c_structures,
               spreadsheet1 TYPE tabname VALUE 'ZSC_FMFP_SCHEDULED_BREAK1',
               sheet1       TYPE numc2   VALUE 01,
               spreadsheet2 TYPE tabname VALUE 'ZSC_FMFP_SCHEDULED_BREAK2',
               sheet2       TYPE numc2   VALUE 02,
             END OF c_structures.

  CREATE OBJECT lref_upload.

  lt_spreadsheet[] = lt_excel[].
  CALL METHOD lref_upload->structure_build(
    EXPORTING
      i_structure = lv_spreadsheet
      i_sheet     = lv_sheet
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_spreadsheet ).

  SORT lt_dd03l BY position.

  IF lv_sheet EQ c_structures-sheet1.
    LOOP AT lt_spreadsheet INTO DATA(lwa_data) WHERE sheet = lv_sheet.
      IF lwa_data-row EQ 1.
        CONTINUE.
      ENDIF.
      IF lv_row NE lwa_data-row.
        INSERT INITIAL LINE INTO TABLE lt_sched_break1
          ASSIGNING FIELD-SYMBOL(<lwa_sched_break1>).
        IF sy-subrc NE 0.
          UNASSIGN <lwa_sched_break1>.
        ENDIF.
        lv_row = lwa_data-row.
      ENDIF.
      READ TABLE lt_dd03l INTO DATA(lwa_dd03l)
        WITH KEY position = lwa_data-column BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_sched_break1> TO FIELD-SYMBOL(<lv_value>).
        IF sy-subrc EQ 0.
*Convert date and time to internal format
          IF lwa_dd03l-inttype EQ 'D'.
            REPLACE ALL OCCURRENCES OF '/' IN lwa_data-value WITH '.'.
            REPLACE ALL OCCURRENCES OF '\' IN lwa_data-value WITH '.'.
            IF lwa_data-value IS NOT INITIAL
            AND lwa_data-value CO '0123456789. '.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
                EXPORTING
                  datum = lwa_data-value
                  dtype = 'DATS'
                IMPORTING
                  idate = <lv_value>.
            ENDIF.
          ELSE.
            <lv_value> = lwa_data-value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF lv_sheet EQ c_structures-sheet2.
    LOOP AT lt_spreadsheet INTO lwa_data WHERE sheet = lv_sheet.
      IF lwa_data-row EQ 1.
        CONTINUE.
      ENDIF.
      IF lv_row NE lwa_data-row.
        INSERT INITIAL LINE INTO TABLE lt_sched_break2
          ASSIGNING FIELD-SYMBOL(<lwa_sched_break2>).
        IF sy-subrc NE 0.
          UNASSIGN <lwa_sched_break2>.
        ENDIF.
        lv_row = lwa_data-row.
      ENDIF.
      READ TABLE lt_dd03l INTO lwa_dd03l
        WITH KEY position = lwa_data-column BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_sched_break2> TO <lv_value>.
        IF sy-subrc EQ 0.
*Convert date and time to internal format
          IF lwa_dd03l-inttype EQ 'D'.
            REPLACE ALL OCCURRENCES OF '/' IN lwa_data-value WITH '.'.
            REPLACE ALL OCCURRENCES OF '\' IN lwa_data-value WITH '.'.
            IF lwa_data-value IS NOT INITIAL
            AND lwa_data-value CO '0123456789. '.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
                EXPORTING
                  datum = lwa_data-value
                  dtype = 'DATS'
                IMPORTING
                  idate = <lv_value>.
            ENDIF.
          ELSEIF lwa_dd03l-inttype EQ 'T'
          AND lwa_data-value IS NOT INITIAL.
            PERFORM read_excel_time_field CHANGING lwa_data-value
                                                   lv_time.

            <lv_value> = lv_time.
          ELSE.
            <lv_value> = lwa_data-value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_EXCEL_TIME_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DATA_VALUE
*&      <-- V_TIME
*&---------------------------------------------------------------------*
FORM read_excel_time_field CHANGING lv_value
                                    lv_time.

  DATA : lv_h   TYPE n LENGTH 2,
         lv_m   TYPE n LENGTH 2,
         lv_s   TYPE n LENGTH 2,
         lv_num TYPE float.

  CLEAR lv_time.
  TRANSLATE lv_value USING ',.'.
  lv_num = lv_value.
  lv_num = lv_num * 24 .
  lv_h = floor( lv_num ).
  lv_num = lv_num - lv_h.
  lv_num = lv_num * 60.
  lv_m = floor( lv_num ).
  lv_num = lv_num - lv_m.
  lv_num = lv_num * 60.
  lv_s = lv_num.
  IF lv_s = 60.
    ADD 1 TO lv_m.
    CLEAR lv_s.
  ENDIF.
  IF lv_m = 60.
    ADD 1 TO lv_h.
    CLEAR lv_m.
  ENDIF.
  CONCATENATE lv_h lv_m lv_s INTO lv_time.
  lv_value = lv_time.

ENDFORM.

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
*& Form CALC_TIME_DIFF
*&---------------------------------------------------------------------*
*& Difference between two times
*&---------------------------------------------------------------------*
FORM calc_time_diff USING lv_achft TYPE tims
                          lv_achit TYPE tims
                 CHANGING lv_capacidade_tot.

  DATA: lv_hr TYPE i,
        lv_mn TYPE i,
        lv_se TYPE i.

  CLEAR lv_capacidade_tot.
  lv_se = ( lv_achft - lv_achit ) MOD 86400.
  lv_hr = lv_se DIV 3600.
  lv_capacidade_tot = lv_se MOD 3600.
  lv_capacidade_tot = lv_capacidade_tot / 3600.
  ADD lv_hr TO lv_capacidade_tot.
  lv_se = lv_se MOD 3600.
  lv_mn = lv_se DIV 60.
  lv_se = lv_se MOD 60.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGES_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIABLES_INITIATOR
*&---------------------------------------------------------------------*
FORM messages_table_display USING  lv_initiator TYPE /agri/gdescr.

  CONSTANTS: c_program TYPE sy-repid VALUE 'ZFMFP_SCHEDULED_BREAK_MASS'.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR lv_initiator.

ENDFORM.
