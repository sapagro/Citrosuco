*&---------------------------------------------------------------------*
*& Include ZFMFP_INC_TASK_UPLOAD
*&---------------------------------------------------------------------*

TABLES: /irm/gfsub, zfmfp_task_sheet, sscrfields.

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

TYPES: BEGIN OF ty_rows,
         xrow LIKE zfmfp_task_sheet-zrow,
         yrow LIKE zfmfp_task_sheet-zrow,
       END OF ty_rows.

DATA: gt_excel          TYPE /agri/t_excel_sheet,
      gt_bdc_messages   TYPE zt_fmfp_bdcmsgcoll,
      gt_task_sheet     TYPE STANDARD TABLE OF zfmfp_task_sheet,
      gv_file_extension TYPE toadd-doc_type,
      gv_subrc          TYPE sysubrc,
      gt_rows           TYPE STANDARD TABLE OF ty_rows.



DATA: BEGIN OF gt_verid OCCURS 0,
        verid TYPE verid,
      END OF gt_verid.

DATA: gt_taskorder      TYPE /agri/t_fmfp_uptask_mass,
      gt_taskorder_fcat TYPE /agri/t_fmfp_uptask_mass_fcat,
      gt_csdoc_infocus  TYPE /agri/t_glcs_doc,
      gt_flcma          TYPE /agri/t_glflcma,
      gt_msgs           TYPE zt_fmfp_bdcmsgcoll,
      gt_tplnr          TYPE /agri/t_gltplnr.

***Rel 60E_SP1 - Authorization check for Customizing Transactions
DEFINE config_tcode_authority_check.
  AUTHORITY-CHECK OBJECT 'V_AB_CNFTX'
        ID '/AGRI/TCOD' FIELD &1
        ID 'ACTVT' FIELD &2.
  &3 = sy-subrc.
END-OF-DEFINITION.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      CALL TRANSACTION 'ZFMFP_TSKUPL'.
    WHEN 'FC02'.
      CALL TRANSACTION 'ZFMFP_TSKUPL_PROC'.
    WHEN 'FC03'.
      CALL TRANSACTION 'ZFMFP_TSKUPL_MON'.
  ENDCASE.


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
                    CHANGING lt_excel    TYPE /agri/t_excel_sheet
                             lt_msgs     TYPE zt_fmfp_bdcmsgcoll
                             lv_subrc    TYPE sysubrc.

  DATA: lv_route TYPE char0256.

  REFRESH: lt_excel, lt_msgs.
  CLEAR: lv_subrc.

  lv_route = lv_file_name.

  CALL METHOD /agri/cl_glupload_master_data=>read_excel_file
    EXPORTING
      i_route  = lv_route
    IMPORTING
      et_sheet = lt_excel.

  DELETE lt_excel WHERE row EQ '1'.

  IF lt_excel[] IS INITIAL.
*"Não existem dados para upload
    MESSAGE i002(zfmfp) INTO sy-msgli.
    message_simple space.
    lv_subrc = 4.
  ELSE.
*    PERFORM tasks_create_via_submit USING lt_excel
*                                 CHANGING lt_msgs.

    PERFORM save_sheet_to_db USING lt_excel
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

  MOVE 'Upload de Tarefas' TO sscrfields-functxt_01.
  MOVE 'Processar Tarefas Pendentes' TO sscrfields-functxt_02.
  MOVE 'Monitor do processamento' TO sscrfields-functxt_03.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TASKS_CREATE_VIA_SUBMIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXCEL
*&---------------------------------------------------------------------*
FORM tasks_create_via_submit USING lt_excel    TYPE /agri/t_excel_sheet
                          CHANGING lt_messages TYPE zt_fmfp_bdcmsgcoll.

  DATA: lref_upload     TYPE REF TO zfmcl_task_upload,
        lt_bdcdata      TYPE bdcdata_tab,
        lt_table        TYPE /agri/t_excel_sheet,
        lt_list         TYPE table_abaplist,
        lt_ascii        TYPE STANDARD TABLE OF soli INITIAL SIZE 0,
        lt_split        TYPE STANDARD TABLE OF so_text255 INITIAL SIZE 0,
        lt_memory_msg   TYPE /agri/t_gprolog,
        lt_task_upl     TYPE STANDARD TABLE OF
                        zsc_fmfp_task_upload INITIAL SIZE 0,
        lt_dd03l        TYPE STANDARD TABLE OF dd03l INITIAL SIZE 0,
        lt_bdc_messages TYPE ettcd_msg_tabtype,
        lwa_bdc_options TYPE ctu_params,
        lr_strno        TYPE RANGE OF /agri/glstrno,
        lwa_task_sheet  LIKE LINE OF gt_task_sheet,
        lv_zrow         TYPE zfmfp_task_sheet-zrow,
        lv_zcolumn      TYPE zfmfp_task_sheet-zcolumn,
        lv_row          TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               task_upl TYPE tabname VALUE 'ZSC_FMFP_TASK_UPLOAD',
               ag_in    TYPE updkz_d VALUE 'I',
             END OF c_structures.

  CONSTANTS: BEGIN OF c_tcode,
               task_upl TYPE sy-tcode VALUE '/AGRI/FMUT23',
             END OF c_tcode.

  CREATE OBJECT lref_upload.

  lt_table[] = lt_excel[].
  CALL METHOD lref_upload->structure_build(
    EXPORTING
      i_structure = c_structures-task_upl
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO DATA(lwa_data) WHERE sheet = 1.
    IF lv_row NE lwa_data-row.
      INSERT INITIAL LINE INTO TABLE lt_task_upl
        ASSIGNING FIELD-SYMBOL(<lwa_task_upl>).
      IF sy-subrc NE 0.
        UNASSIGN <lwa_task_upl>.
      ENDIF.
      lv_row = lwa_data-row.
    ENDIF.
    READ TABLE lt_dd03l INTO DATA(lwa_dd03l)
      WITH KEY position = lwa_data-column BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT lwa_data-fieldname
        OF STRUCTURE <lwa_task_upl> TO FIELD-SYMBOL(<lv_value>).
      IF sy-subrc EQ 0.
        <lv_value> = lwa_data-value.
      ENDIF.
    ENDIF.
  ENDLOOP.

  lv_row = 1.
  LOOP AT lt_task_upl ASSIGNING <lwa_task_upl>.
    ADD 1 TO lv_row.
    REFRESH: lr_strno.
    INSERT INITIAL LINE INTO TABLE lr_strno
      ASSIGNING FIELD-SYMBOL(<lwa_stnro>).
    IF sy-subrc EQ 0.
      <lwa_stnro> = 'IEQ'.
      <lwa_stnro>-low = <lwa_task_upl>-strno.

      SUBMIT zfmfp_unplanned_taskorder
              WITH p_werks  EQ <lwa_task_upl>-werks
              WITH p_cmnum  EQ <lwa_task_upl>-cmnum
              WITH p_matnr  EQ <lwa_task_upl>-matnr
              WITH p_basoq  EQ <lwa_task_upl>-basoq
              WITH so_strno IN lr_strno[]
              WITH p_varia  EQ <lwa_task_upl>-varia
              WITH p_cpros  EQ <lwa_task_upl>-cpros
              WITH p_date   EQ sy-datum
              WITH p_verid  EQ <lwa_task_upl>-verid
              WITH p_res1   EQ <lwa_task_upl>-result1
              WITH p_res2   EQ <lwa_task_upl>-result2
              WITH p_res3   EQ <lwa_task_upl>-result3
              WITH p_res4   EQ <lwa_task_upl>-result4
              WITH p_res5   EQ <lwa_task_upl>-result5
              WITH p_res6   EQ <lwa_task_upl>-result6
              WITH p_res7   EQ <lwa_task_upl>-result7
              WITH p_res8   EQ <lwa_task_upl>-result8
              WITH p_res9   EQ <lwa_task_upl>-result9
              WITH p_res10  EQ <lwa_task_upl>-result10
              EXPORTING LIST TO MEMORY AND RETURN.

      WAIT UP TO '0.5' SECONDS.

      CALL FUNCTION 'ZFMFP_MEMORY_MESSAGES'
        IMPORTING
          et_messages = lt_memory_msg.

      LOOP AT lt_memory_msg INTO DATA(lwa_mem_msg).

        lv_zcolumn = sy-tabix.

        INSERT INITIAL LINE INTO TABLE lt_messages
           ASSIGNING FIELD-SYMBOL(<lwa_message>).
        IF sy-subrc EQ 0.
          <lwa_message>-row    = lv_row.
          <lwa_message>-msgid  = lwa_mem_msg-msgid.
          <lwa_message>-msgnr  = lwa_mem_msg-msgno.
          <lwa_message>-msgtyp = lwa_mem_msg-msgty.
          <lwa_message>-msgv1  = lwa_mem_msg-msgv1.
          <lwa_message>-msgv2  = lwa_mem_msg-msgv2.
          <lwa_message>-msgv3  = lwa_mem_msg-msgv3.
          <lwa_message>-msgv4  = lwa_mem_msg-msgv4.

          IF gt_task_sheet() IS NOT INITIAL.

            CLEAR lwa_task_sheet-message.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = lwa_mem_msg-msgid
                msgnr               = lwa_mem_msg-msgno
                msgv1               = lwa_mem_msg-msgv1
                msgv2               = lwa_mem_msg-msgv2
                msgv3               = lwa_mem_msg-msgv3
                msgv4               = lwa_mem_msg-msgv4
              IMPORTING
                message_text_output = lwa_task_sheet-message.

            lwa_task_sheet-processed = lwa_mem_msg-msgty.
            lwa_task_sheet-aenam     = sy-uname.
            lwa_task_sheet-aedat     = sy-datum.
            lwa_task_sheet-aezet     = sy-uzeit.


            READ TABLE gt_rows INTO DATA(lwa_rows) WITH KEY xrow = lv_row.
            IF sy-subrc = 0.
              lv_zrow = lwa_rows-yrow.
            ENDIF.

*            SHIFT lv_zrow LEFT DELETING LEADING '0'.
*            SHIFT lv_zrow LEFT DELETING LEADING ' '.
*           SHIFT lv_zcolumn LEFT DELETING LEADING '0'.
            SHIFT lv_zcolumn LEFT DELETING LEADING ' '.

            MODIFY gt_task_sheet FROM lwa_task_sheet
            TRANSPORTING message processed
             WHERE zrow = lv_zrow AND zcolumn = lv_zcolumn.

            MODIFY gt_task_sheet FROM lwa_task_sheet
            TRANSPORTING aenam aedat aezet
             WHERE zrow = lv_zrow.

          ENDIF.
        ENDIF.
      ENDLOOP.

*-- Get the output list from memory
      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = lt_list
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.

      CALL FUNCTION 'LIST_TO_ASCI'
        TABLES
          listasci   = lt_ascii
          listobject = lt_list
        EXCEPTIONS
          OTHERS     = 0.

*      IF sy-uname EQ 'T_H.KABABE'.
*        BREAK-POINT.
*      ENDIF.

      LOOP AT lt_ascii INTO DATA(lwa_ascii).
        IF lwa_ascii-line(5) NE 'DISP;'
        AND lwa_ascii-line(7) NE 'NODISP;'.
          DELETE lt_ascii INDEX sy-tabix.
        ELSE.
          REFRESH lt_split.
          IF lwa_ascii-line(7) EQ 'NODISP;'.
            INSERT INITIAL LINE INTO TABLE lt_messages
               ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
              SPLIT lwa_ascii-line AT ';' INTO TABLE lt_split.
              <lwa_message>-row = lv_row.
              DELETE lt_split INDEX 1.
              LOOP AT lt_split REFERENCE INTO DATA(lr_split).
                CASE sy-tabix.
                  WHEN 1.
                    <lwa_message>-msgid   = lr_split->*.
                  WHEN 2.
                    <lwa_message>-msgtyp  = lr_split->*.
                  WHEN 3.
                    <lwa_message>-msgnr   = lr_split->*.
                  WHEN 4.
                    <lwa_message>-msgv1   = lr_split->*.
                  WHEN 5.
                    <lwa_message>-msgv2   = lr_split->*.
                  WHEN 6.
                    <lwa_message>-msgv3   = lr_split->*.
                  WHEN 7.
                    <lwa_message>-msgv4   = lr_split->*.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.


  IF gt_task_sheet() IS NOT INITIAL.


    MODIFY zfmfp_task_sheet FROM TABLE gt_task_sheet[].

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_dynpro TABLES lt_bdcdata TYPE bdcdata_tab
                 USING lv_program lv_dynpro.

  DATA: lwa_bdcdata LIKE bdcdata.

  lwa_bdcdata-program  = lv_program.
  lwa_bdcdata-dynpro   = lv_dynpro.
  lwa_bdcdata-dynbegin = c_true.
  APPEND lwa_bdcdata TO lt_bdcdata.

ENDFORM.                    "bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_field TABLES lt_bdcdata TYPE bdcdata_tab
                USING lv_fnam lv_fval.

  DATA: lwa_bdcdata TYPE bdcdata.

  IF lv_fval IS  NOT INITIAL.
    lwa_bdcdata-fnam = lv_fnam.
    lwa_bdcdata-fval = lv_fval.
    SHIFT lwa_bdcdata-fval LEFT DELETING LEADING space.
    APPEND lwa_bdcdata TO lt_bdcdata.
  ENDIF.

ENDFORM.                    "bdc_field

*&---------------------------------------------------------------------*
*& Form FILL_BDC_FMUT23
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BDCDATA
*&      --> <LWA_TASK_UPL>
*&---------------------------------------------------------------------*
FORM fill_bdc_fmut23 TABLES lt_bdcdata   TYPE bdcdata_tab
                      USING lwa_task_upl TYPE zsc_fmfp_task_upload.

  DATA: lv_date TYPE char10.

  REFRESH: lt_bdcdata.
  PERFORM bdc_dynpro TABLES lt_bdcdata
    USING '/AGRI/FMFP_UNPLANNED_TASKORDER'        '1000'.

  WRITE sy-datum TO lv_date.
  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_CURSOR'       'P_MATNR'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_OKCODE'       '/00'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_WERKS'          lwa_task_upl-werks.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_CMNUM'          lwa_task_upl-cmnum.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_MATNR'          lwa_task_upl-matnr.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_BASOQ'          lwa_task_upl-basoq.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_DATE'           lv_date.

  PERFORM bdc_dynpro TABLES lt_bdcdata
    USING '/AGRI/FMFP_UNPLANNED_TASKORDER'        '1000'.

  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_CURSOR'       'P_VARIA'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_OKCODE'       '=VARI'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_VARIA'          lwa_task_upl-varia.

  PERFORM bdc_dynpro TABLES lt_bdcdata
    USING '/AGRI/FMFP_UNPLANNED_TASKORDER'        '1000'.

  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_CURSOR'       'P_CPROS'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_OKCODE'       '=PROS'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_CPROS'          lwa_task_upl-cpros.

  PERFORM bdc_dynpro TABLES lt_bdcdata
    USING '/AGRI/FMFP_UNPLANNED_TASKORDER'        '1000'.

  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_CURSOR'       'SO_STRNO-LOW'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_OKCODE'       'ONLI'.
  PERFORM bdc_field TABLES lt_bdcdata USING 'SO_STRNO-LOW'     lwa_task_upl-strno.
  PERFORM bdc_field TABLES lt_bdcdata USING 'P_VERID'          lwa_task_upl-verid.

  PERFORM bdc_dynpro TABLES lt_bdcdata
    USING '/AGRI/FMFP_UNPLANNED_TASKORDER'        '0100'.

  PERFORM bdc_field TABLES lt_bdcdata USING 'BDC_OKCODE'       '=CREA'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCEL_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LWA_TASK_UPL>
*&---------------------------------------------------------------------*
FORM excel_validations USING lwa_task_upl TYPE zsc_fmfp_task_upload.

  DATA: lv_cmnum  TYPE /agri/glcmnum,
        lwa_cmnum TYPE /agri/s_glcmnum,
        lt_cmnum  TYPE /agri/t_glcmnum.

  IF lwa_task_upl-werks IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '059' INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF lwa_task_upl-cmnum IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '060' INTO sy-msgli.
    message_simple space.
  ELSE.
    SELECT SINGLE cmnum
             FROM /agri/glcmhdr
             INTO lv_cmnum
            WHERE cmnum EQ lwa_task_upl-cmnum.

    IF lv_cmnum IS INITIAL.
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
                 NUMBER '066' WITH lwa_task_upl-cmnum INTO sy-msgli.
      message_simple space.
    ELSE.
      lwa_cmnum-cmnum = lv_cmnum.
      APPEND lwa_cmnum TO lt_cmnum.
    ENDIF.
  ENDIF.

  IF lwa_task_upl-matnr IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '042' INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF lwa_task_upl-verid IS NOT INITIAL.

    READ TABLE gt_verid WITH KEY verid = lwa_task_upl-verid.
    IF sy-subrc NE 0.
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
                 NUMBER '070' INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDIF.

  IF  lwa_task_upl-basoq IS INITIAL.
    MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error
               NUMBER '071' INTO sy-msgli.
    message_simple space.
  ENDIF.

  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE gt_tplnr
   WHERE strno EQ lwa_task_upl-strno.

  PERFORM valid_assignments_read USING lwa_task_upl
                                       sy-datum.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALID_ASSIGNMENTS_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_DATUM
*&---------------------------------------------------------------------*
FORM valid_assignments_read USING lwa_task_upl TYPE zsc_fmfp_task_upload
                                  lv_date      TYPE datab.

  DATA: lt_cskey  TYPE /agri/t_glcs_key.

  IF gt_tplnr[] IS NOT INITIAL.
    SELECT * FROM /agri/glflcma
             INTO CORRESPONDING FIELDS OF TABLE lt_cskey
              FOR ALL ENTRIES IN gt_tplnr
            WHERE tplnr_fl EQ gt_tplnr-tplnr_fl
              AND cmnum EQ lwa_task_upl-cmnum
              AND datab LE lv_date
              AND datbi GE lv_date
              AND class EQ c_application-farming
              AND astat EQ 'A'
              AND loevm EQ space.
  ENDIF.

  REFRESH: gt_csdoc_infocus.
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
    message_simple space.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ASSIGNMENTS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_assignments_data USING lwa_task_upl TYPE zsc_fmfp_task_upload.

  REFRESH: gt_flcma.
  IF NOT gt_tplnr[] IS INITIAL.
    SELECT * FROM /agri/glflcma
      INTO CORRESPONDING FIELDS OF TABLE gt_flcma
       FOR ALL ENTRIES IN gt_tplnr
      WHERE tplnr_fl EQ gt_tplnr-tplnr_fl
        AND cmnum    EQ lwa_task_upl-cmnum
        AND varia    EQ lwa_task_upl-varia
        AND datab    LE sy-datum
        AND datbi    GE sy-datum
        AND astat    EQ 'A'  "Active
        AND iwerk    EQ lwa_task_upl-werks
        AND loevm    EQ space.

    PERFORM assignments_data_prepare USING gt_flcma
                                           lwa_task_upl.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assignments_data_prepare  USING lt_flcma     TYPE /agri/t_glflcma
                                     lwa_task_upl TYPE zsc_fmfp_task_upload.

  DATA: lwa_taskorder      TYPE /agri/s_fmfp_uptask_mass,
        lwa_taskorder_fcat TYPE /agri/s_fmfp_uptask_mass_fcat,
        lwa_makt           TYPE makt,
        lwa_cmhdrt         TYPE /agri/glcmhdrt,
        lwa_flcma          TYPE /agri/glflcma.

  LOOP AT lt_flcma INTO lwa_flcma.
    lwa_taskorder-gstrp     = sy-datum.
    lwa_taskorder-tplnr_fl  = lwa_flcma-tplnr_fl.
    lwa_taskorder-cmnum     = lwa_flcma-cmnum.
    lwa_taskorder-contr     = lwa_flcma-contr.
    lwa_taskorder-matnr     = lwa_task_upl-matnr.
    lwa_taskorder-verid     = lwa_task_upl-verid.

    PERFORM special_application_change CHANGING lwa_taskorder.

    IF lwa_task_upl-basoq = 'A'.
      lwa_taskorder-aarea   = lwa_flcma-aarea.
      lwa_taskorder-meins   = lwa_flcma-msehi.
    ELSEIF lwa_task_upl-basoq = 'G'.
      lwa_taskorder-aarea   = lwa_flcma-garea.
      lwa_taskorder-meins   = lwa_flcma-msehi.
    ELSEIF lwa_task_upl-basoq = 'E'.
      lwa_taskorder-aarea   = lwa_flcma-eston.
      lwa_taskorder-meins   = lwa_flcma-esuom.
    ENDIF.
    APPEND lwa_taskorder TO gt_taskorder.
    MOVE-CORRESPONDING lwa_taskorder TO lwa_taskorder_fcat.
    key_text_get '/AGRI/GLCMHDRT' 'CMNUM' lwa_taskorder_fcat-cmnum
                 lwa_cmhdrt lwa_taskorder_fcat-descr.
    key_text_get 'MAKT' 'MATNR' lwa_taskorder_fcat-matnr
                 lwa_makt lwa_taskorder_fcat-maktx.
    APPEND lwa_taskorder_fcat TO gt_taskorder_fcat.
  ENDLOOP.

ENDFORM.                    " ASSIGNMENTS_DATA_PREPARE

*&---------------------------------------------------------------------*
*& Form SPECIAL_APPLICATION_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LWA_TASKORDER
*&---------------------------------------------------------------------*
FORM special_application_change CHANGING lwa_taskorder TYPE /agri/s_fmfp_uptask_mass.

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
           WHERE mtart EQ lv_mtart_temp.

    IF lv_mtart IS NOT INITIAL.
      lwa_taskorder-sappl = c_special_appl-chemical.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_SHEET_TO_DB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXCEL
*&---------------------------------------------------------------------*
FORM save_sheet_to_db USING lt_excel    TYPE /agri/t_excel_sheet
                          CHANGING lt_messages TYPE zt_fmfp_bdcmsgcoll.

  DATA: lt_task_sheet  TYPE STANDARD TABLE OF zfmfp_task_sheet,
        lwa_task_sheet LIKE LINE OF lt_task_sheet.


  SORT lt_excel BY sheet row column.
  DELETE ADJACENT DUPLICATES FROM lt_excel COMPARING sheet row column.

  SELECT MAX( zcount ) INTO @DATA(lv_count) FROM zfmfp_task_sheet
    WHERE ernam     = @sy-uname
      AND erdat     = @sy-datum.
  IF sy-subrc NE 0.
    lv_count = 0.
  ENDIF.

  LOOP AT lt_excel INTO DATA(lwa_data).

    CLEAR lwa_task_sheet.
    lwa_task_sheet-ernam     = sy-uname.
    lwa_task_sheet-erdat     = sy-datum.
    lwa_task_sheet-zcount    = sy-tabix + lv_count.
    lwa_task_sheet-erzet     = sy-uzeit.
*    lwa_task_sheet-aenam     = lwa_data-aenam.
*    lwa_task_sheet-aedat     = lwa_data-aedat.
*    lwa_task_sheet-aezet     = lwa_data-aezet.
*    lwa_task_sheet-processed = lwa_data-processed.
    lwa_task_sheet-sheet     = lwa_data-sheet.
    lwa_task_sheet-zrow      = lwa_data-row - 1.
    lwa_task_sheet-zcolumn   = lwa_data-column.
    lwa_task_sheet-fieldname = lwa_data-fieldname.
    lwa_task_sheet-value     = lwa_data-value.
    lwa_task_sheet-value_len = strlen( lwa_task_sheet-value ) .

    APPEND lwa_task_sheet TO lt_task_sheet.
  ENDLOOP.

  IF lt_task_sheet[] IS NOT INITIAL.

    MODIFY zfmfp_task_sheet FROM TABLE lt_task_sheet[].
    IF sy-subrc EQ 0.
      INSERT INITIAL LINE INTO TABLE lt_messages
         ASSIGNING FIELD-SYMBOL(<lwa_message>).
      IF sy-subrc EQ 0.
        <lwa_message>-msgid   = '/AGRI/GLOBAL'.
        <lwa_message>-msgtyp  = 'S'.
        <lwa_message>-msgnr   = 085.
        <lwa_message>-msgv1   = lines( lt_task_sheet ).
*        <lwa_message>-msgv2   = .
*        <lwa_message>-msgv3   = .
      ENDIF.
      MESSAGE s085(/agri/global).
    ELSE.
      INSERT INITIAL LINE INTO TABLE lt_messages
         ASSIGNING <lwa_message>.
      IF sy-subrc EQ 0.
        <lwa_message>-msgid   = '/AGRI/GLOBAL'.
        <lwa_message>-msgtyp  = 'E'.
        <lwa_message>-msgnr   = 288.
*        <lwa_message>-msgv1   = .
*        <lwa_message>-msgv2   = .
*        <lwa_message>-msgv3   = .
      ENDIF.
      MESSAGE i288(/agri/global).
    ENDIF.
  ENDIF.

ENDFORM.                                      "SAVE_SHEET_TO_DB
*&---------------------------------------------------------------------*
*& Form READ_SHEET_TO_DB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXCEL
*&---------------------------------------------------------------------*
FORM read_sheet_to_db CHANGING lt_excel    TYPE /agri/t_excel_sheet
                               lt_messages TYPE zt_fmfp_bdcmsgcoll
                               ls_ernam    TYPE ANY TABLE
                               ls_erdat    TYPE ANY TABLE
                               ls_zrow     TYPE ANY TABLE
                               ls_erzet    TYPE ANY TABLE
                               ls_aenam    TYPE ANY TABLE
                               ls_aedat    TYPE ANY TABLE
                               ls_aezet    TYPE ANY TABLE
                               lp_proc     TYPE ANY TABLE.


  DATA: lwa_excel  LIKE LINE OF lt_excel,
        lv_row     LIKE zfmfp_task_sheet-zrow,
        lv_row_old LIKE zfmfp_task_sheet-zrow.

  SELECT  *
    FROM zfmfp_task_sheet
    INTO TABLE gt_task_sheet[]
    WHERE ernam  IN ls_ernam  AND
          erdat  IN ls_erdat  AND
          zrow   IN ls_zrow   AND
          erzet  IN ls_erzet  AND
          aenam  IN ls_aenam  AND
          aedat  IN ls_aedat  AND
          aezet  IN ls_aezet  AND
          processed IN lp_proc.


  SORT gt_task_sheet BY ernam erdat zcount zrow zcolumn.

  lv_row = 1.
  lv_row_old = 0.

  IF sy-subrc = 0.
    REFRESH lt_excel.
    LOOP AT gt_task_sheet INTO DATA(lwa_data).

      IF lwa_data-zrow <> lv_row_old.
        ADD 1 TO lv_row.
      ENDIF.

      CLEAR lwa_excel.
      lwa_excel-sheet     = lwa_data-sheet.
      lwa_excel-row       = lv_row.
      lwa_excel-column    = lwa_data-zcolumn.
      lwa_excel-fieldname = lwa_data-fieldname.
      lwa_excel-value     = lwa_data-value.

      APPEND lwa_excel TO lt_excel.

      INSERT INITIAL LINE INTO TABLE gt_rows
         ASSIGNING FIELD-SYMBOL(<fwa_rows>).
      <fwa_rows>-xrow = lv_row.
      <fwa_rows>-yrow = lwa_data-zrow.
      lv_row_old      = lwa_data-zrow.

    ENDLOOP.
  ENDIF.

  SORT gt_rows.
  DELETE ADJACENT DUPLICATES FROM gt_rows.

  IF lt_excel[] IS NOT INITIAL.

    INSERT INITIAL LINE INTO TABLE lt_messages
       ASSIGNING FIELD-SYMBOL(<lwa_message>).
    IF sy-subrc EQ 0.
      <lwa_message>-msgid   = '/AGRI/GLOBAL'.
      <lwa_message>-msgtyp  = 'S'.
      <lwa_message>-msgnr   = 085.
      <lwa_message>-msgv1   = lines( lt_excel ).
*        <lwa_message>-msgv2   = .
*        <lwa_message>-msgv3   = .
    ENDIF.
    MESSAGE s085(/agri/global).
  ELSE.
    INSERT INITIAL LINE INTO TABLE lt_messages
       ASSIGNING <lwa_message>.
    IF sy-subrc EQ 0.
      <lwa_message>-msgid   = '/AGRI/GLOBAL'.
      <lwa_message>-msgtyp  = 'E'.
      <lwa_message>-msgnr   = 288.
*        <lwa_message>-msgv1   = .
*        <lwa_message>-msgv2   = .
*        <lwa_message>-msgv3   = .
    ENDIF.
    MESSAGE i288(/agri/global).
  ENDIF.


ENDFORM.                                      "READ_SHEET_TO_DB
