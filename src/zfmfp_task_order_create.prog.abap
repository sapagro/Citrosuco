*&---------------------------------------------------------------------*
*& Modulpool ZFMFP_TASK_ORDER_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zfmfp_task_order_create.

TYPE-POOLS: abap.

TYPES: ty_fmfphdr_tab TYPE STANDARD TABLE OF /agri/fmfphdr.

DATA: gt_fmfphdr      TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gv_logical_db   TYPE trdir-ldbname VALUE '/AGRI/FMFP',
      gt_callback     TYPE STANDARD TABLE OF ldbcb INITIAL SIZE 0,
      gt_seltab       TYPE TABLE OF rsparams,
      ref_process_log TYPE REF TO /agri/cl_process_log_manager,
      gs_environment  TYPE /agri/s_gprolog_environment,
      gs_context_data TYPE /agri/s_gprolog_context,
      BEGIN OF gs_message,
        dummy(80),
        level     TYPE i,
      END OF gs_message,
****Changed on 06/13/2005
      BEGIN OF gs_log_variables,
        document(10),
        item(10),
        subitem(10),
        docvariant(30),
****Added on 06/13/2005
        log_number     TYPE bal_s_lgnm-lognumber,
****Added on 07/30/2005
        amount(30),
**** Rel E SP1
        text           TYPE text255,
      END OF gs_log_variables.

CONSTANTS: BEGIN OF gc_object,
             bor              LIKE /agri/tgabo-object VALUE '/AGRI/FMFP',
             log              LIKE balobj-object      VALUE '/AGRI/FMFP',
             change_documents TYPE cdobjectcl         VALUE '/AGRI/FMFP',
             esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/FMFP',
             text_object      TYPE thead-tdobject     VALUE '/AGRI/FMFP',
           END OF gc_object,

           BEGIN OF c_application,
             nursery TYPE /agri/glab_class VALUE '0',
             farming TYPE /agri/glab_class VALUE '1',
             grower  TYPE /agri/glab_class VALUE '2',
           END OF c_application,

           BEGIN OF gc_process_material,
             implantacao TYPE matnr VALUE 'PIMP0001',
             formacao    TYPE matnr VALUE 'PFOR0001',
             manutencao  TYPE matnr VALUE 'PMAN0001',
             colheita    TYPE matnr VALUE 'PCOL0001',
           END OF gc_process_material,

           BEGIN OF gc_task_material,
             implantacao TYPE matnr VALUE 'TIMP',
             formacao    TYPE matnr VALUE 'TFOR',
             manutencao  TYPE matnr VALUE 'TMAN',
             colheita    TYPE matnr VALUE 'TCOL',
           END OF gc_task_material,

           BEGIN OF gc_doc_category,
             process_order TYPE /agri/gl_autyp VALUE 'AO',
             task_order    TYPE /agri/gl_autyp VALUE 'TO',
           END OF gc_doc_category,

           BEGIN OF c_order_level,
             process      TYPE /agri/glordlv VALUE 'A',
             process_task TYPE /agri/glordlv VALUE 'B',
           END OF c_order_level,

           BEGIN OF c_document_category,
             production_order TYPE /agri/gl_autyp VALUE 'AO',
             task_order       TYPE /agri/gl_autyp VALUE 'TO',
             produce_reciept  TYPE /agri/gl_autyp VALUE 'PR',
             work_order       TYPE /agri/gl_autyp VALUE 'WO',
             purchase_order   TYPE /agri/gl_autyp VALUE 'PO',
             confirmation     TYPE /agri/gl_autyp VALUE 'OC',
             reversals        TYPE /agri/gl_autyp VALUE 'CR',
             measurements     TYPE /agri/gl_autyp VALUE 'MD',
           END OF c_document_category,

****Message Types
           BEGIN OF c_msg_type,
             info    TYPE sy-msgty VALUE 'I',
             warning TYPE sy-msgty VALUE 'W',
             error   TYPE sy-msgty VALUE 'E',
             abend   TYPE sy-msgty VALUE 'A',
             success TYPE sy-msgty VALUE 'S',
             x       TYPE sy-msgty VALUE 'X',
           END OF c_msg_type,

****Update Indicators
           c_updkz_new(1)     TYPE c VALUE 'I',
           c_updkz_update(1)  TYPE c VALUE 'U',
           c_updkz_delete(1)  TYPE c VALUE 'D',
           c_updkz_old(1)     TYPE c VALUE ' ',
           c_updkz_newrow     TYPE c VALUE 'N',
           c_updkz_propose(1) TYPE c VALUE 'P'.

DEFINE object_refresh_all.

****ESP6 Task#36253 - Unreleaseed API Elimination
*  call function 'SWU_OBJECT_REFRESH'.
  CALL FUNCTION '/AGRI/G_SWU_OBJECT_REFRESH'.
****

END-OF-DEFINITION.

DEFINE messages_init.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ELSE.
    CALL METHOD ref_process_log->initialize.
  ENDIF.

END-OF-DEFINITION.

DEFINE messages_collect_all.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->environment_get
    IMPORTING
      es_environment = gs_environment.

  gs_environment-collect_all = 'X'.

  CALL METHOD ref_process_log->environment_set
    EXPORTING
      is_environment = gs_environment.

END-OF-DEFINITION.

****Set the initiator
DEFINE messages_initiator_set.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->initiator_set
    EXPORTING
      i_initiator = &1
      i_aplobj    = &2
      i_subobj    = &3.

END-OF-DEFINITION.

DEFINE message_simple.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  IF NOT &1 EQ space.
    gs_message-level = &1.
  ENDIF.

  CALL METHOD ref_process_log->message_add
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

****Messages Display
DEFINE messages_display.

  IF ref_process_log IS INITIAL.
    CREATE OBJECT ref_process_log.
  ENDIF.

  CALL METHOD ref_process_log->messages_display
    EXPORTING
      i_initiator   = &1
      i_display_all = &2
      i_full_screen = &3
      i_title       = &4
      is_disvariant = &5.

END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_data_initialize  USING lv_refresh_messages.

  object_refresh_all.

  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

ENDFORM.                    " DOCUMENT_DATA_INITIALIZE

*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_initialize  USING  lv_initiator TYPE /agri/gdescr
                                 lv_subobject TYPE balsubobj.

  messages_init.

  CHECK lv_initiator IS NOT INITIAL.
  messages_collect_all.
  messages_initiator_set lv_initiator gc_object-log lv_subobject.

ENDFORM.                    " MESSAGES_INITIALIZE

*&---------------------------------------------------------------------*
*& Form CHECK_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> MATKL
*&      --> EXTWG
*&      <-- GV_PROCESS_MATERIAL
*&      <-- IF
*&      <-- FOUND
*&---------------------------------------------------------------------*
FORM check_material USING lv_strnum TYPE /agri/glstrno
                          lv_acnum TYPE zfmacnum
                          lv_actyp TYPE ZFMACTYP
                          lv_ajahr TYPE ajahr
                          lv_data  TYPE begda
                          lv_matkl TYPE matkl
                          lv_extwg TYPE extwg
                 CHANGING lv_matnr TYPE matnr
                          lv_subrc TYPE sysubrc.


    SELECT *
      INTO TABLE @DATA(lt_zfmachdr)
      FROM zfmachdr
      WHERE acnum EQ @lv_acnum
        AND ajahr EQ @lv_ajahr
        AND actyp EQ @lv_actyp.

      DATA: lv_tplnr_fl TYPE /AGRI/GLTPLNR_FL.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input            = lv_strnum
      IMPORTING
        OUTPUT           = lv_tplnr_fl
*     EXCEPTIONS
*       NOT_FOUND        = 1
*       NOT_ACTIVE       = 2
*       OTHERS           = 3
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF sy-subrc EQ 0.
      SELECT *
        INTO TABLE @DATA(lt_zfmaitm)
        FROM zfmaitm
        FOR ALL ENTRIES IN @lt_zfmachdr
       WHERE acnum EQ @lt_zfmachdr-acnum AND
             tplnr_fl EQ @lv_tplnr_fl.

      IF sy-subrc EQ 0.

        SELECT *
          INTO TABLE @DATA(lt_glflcma)
          FROM /agri/glflcma
          FOR ALL ENTRIES IN @lt_zfmaitm
         WHERE tplnr_fl EQ @lt_zfmaitm-tplnr_fl
            AND astat EQ 'A'.
*           AND contr    EQ @lt_zfmaitm-contr.

        IF sy-subrc EQ 0.
*-- Fetch Farm Process Order Item Details for Tasks
          SELECT *
            INTO TABLE @DATA(lt_glcsprs)
            FROM /agri/glcsprs
            FOR ALL ENTRIES IN @lt_glflcma
           WHERE tplnr_fl EQ @lt_glflcma-tplnr_fl
             AND contr    EQ @lt_glflcma-contr
             AND strdt    LE @lv_data
             AND enddt    GE @lv_data
             AND prprs    EQ @abap_false.

  CLEAR lv_matnr.

  SELECT matnr, matkl, extwg
    INTO TABLE @DATA(gt_mara)
    FROM mara
   WHERE matkl EQ @lv_matkl
     AND extwg EQ @lv_extwg
    ORDER BY matnr, matkl, extwg.

  READ TABLE lt_glcsprs INTO DATA(ls_prs) INDEX 1.
  IF sy-subrc EQ 0.

    lv_matnr = 'T' && ls_prs-matnr+1(3) && '*'.
   LOOP AT gt_mara INTO DATA(lwa_mara) WHERE matnr CP lv_matnr.
     exit.
   ENDLOOP.
*  READ TABLE gt_mara INTO DATA(lwa_mara) INDEX 1.
  IF sy-subrc EQ 0.
    CASE lwa_mara-matnr(4).
      WHEN gc_task_material-implantacao.
        lv_matnr = gc_process_material-implantacao.
      WHEN gc_task_material-formacao.
        lv_matnr = gc_process_material-formacao.
      WHEN gc_task_material-manutencao.
        lv_matnr = gc_process_material-manutencao.
      WHEN gc_task_material-colheita.
        lv_matnr = gc_process_material-colheita.
    ENDCASE.
  ELSE.
    lv_subrc = 4.
  ENDIF.

  ENDIF.
ENDIF.

ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> STRNO
*&      --> GV_PROCESS_MATERIAL
*&      --> IF
*&      --> FOUND
*&---------------------------------------------------------------------*
FORM read_order USING lv_strno   TYPE /agri/glstrno
                      lv_matnr   TYPE matnr
             CHANGING lt_fmfphdr TYPE ty_fmfphdr_tab
                      lt_fpdoc   TYPE /agri/t_fmfp_doc
                      lv_subrc   TYPE sysubrc.

  DATA: lt_aufnr TYPE /agri/t_fmaufnr,
        lr_strno TYPE RANGE OF /agri/glstrno.

  REFRESH: gt_callback, gt_seltab, gt_fmfphdr.

*...IMPLANTAÇÃO&FORMAÇÃO(ZP00)
*...MANUTENÇÃO(ZG00)&COLHEITA(ZH00)
  INSERT INITIAL LINE INTO TABLE lr_strno
   ASSIGNING FIELD-SYMBOL(<lwa_strno>).
  IF sy-subrc EQ 0.
    <lwa_strno> = 'IEQ'.
    <lwa_strno>-low = lv_strno.
  ENDIF.

  IF lr_strno[] IS NOT INITIAL
  AND lv_matnr IS NOT INITIAL.
    SELECT tplnr_fl, strno FROM /agri/glflot
      INTO TABLE @DATA(lt_flot)
     WHERE strno IN @lr_strno[].

    IF sy-subrc NE 0.
      lv_subrc = 4.
    ELSE.
      SORT lt_flot BY tplnr_fl.
*...Prepare Callback Subroutine table
      INSERT INITIAL LINE INTO TABLE gt_callback
        ASSIGNING FIELD-SYMBOL(<lwa_callback>).
      IF sy-subrc EQ 0.
        <lwa_callback>-ldbnode = '/AGRI/FMFPHDR'.
        <lwa_callback>-get     = 'X'.
        <lwa_callback>-cb_prog = sy-repid.
        <lwa_callback>-cb_form = 'CALLBACK_FMFPHDR'.
      ENDIF.
*...Prepare Selections
****Tipo de ordem
      DO 3 TIMES.
        DATA(lv_index) = sy-index.
        INSERT INITIAL LINE INTO TABLE gt_seltab
          ASSIGNING FIELD-SYMBOL(<lwa_seltab>).
        IF sy-subrc EQ 0.
          <lwa_seltab>-selname = 'SO_AUART'.
          <lwa_seltab>-kind    = 'S'.
          <lwa_seltab>-sign    = 'I'.
          <lwa_seltab>-option  = 'EQ'.

          CASE lv_index.
            WHEN 1.
              <lwa_seltab>-low = 'ZP00'. "Planning
            WHEN 2.
              <lwa_seltab>-low = 'ZG00'. "Growing
            WHEN 3.
              <lwa_seltab>-low = 'ZH00'. "Harvest
          ENDCASE.
        ENDIF.
      ENDDO.
****Terreno
      LOOP AT lt_flot INTO DATA(lwa_flot).
        INSERT INITIAL LINE INTO TABLE gt_seltab
          ASSIGNING <lwa_seltab>.
        IF sy-subrc EQ 0.
          <lwa_seltab>-selname = 'SO_STRNO'.
          <lwa_seltab>-kind    = 'S'.
          <lwa_seltab>-sign    = 'I'.
          <lwa_seltab>-option  = 'EQ'.
          <lwa_seltab>-low     = lwa_flot-strno.
        ENDIF.
      ENDLOOP.
****Material
      INSERT INITIAL LINE INTO TABLE gt_seltab
        ASSIGNING <lwa_seltab>.
      IF sy-subrc EQ 0.
        <lwa_seltab>-selname = 'SO_MATNR'.
        <lwa_seltab>-kind    = 'S'.
        <lwa_seltab>-sign    = 'I'.
        <lwa_seltab>-option  = 'EQ'.
        <lwa_seltab>-low     = lv_matnr.
      ENDIF.
****Aplicação (1-Agrícola)
      INSERT INITIAL LINE INTO TABLE gt_seltab
        ASSIGNING <lwa_seltab>.
      IF sy-subrc EQ 0.
        <lwa_seltab>-selname = 'P_CLASS'.
        <lwa_seltab>-kind    = 'P'.
        <lwa_seltab>-sign    = 'I'.
        <lwa_seltab>-option  = 'EQ'.
        <lwa_seltab>-low     = '1'.
      ENDIF.

      CALL FUNCTION 'LDB_PROCESS'
        EXPORTING
          ldbname                     = gv_logical_db
        TABLES
          callback                    = gt_callback
          selections                  = gt_seltab[]
        EXCEPTIONS
          ldb_not_reentrant           = 1
          ldb_incorrect               = 2
          ldb_already_running         = 3
          ldb_error                   = 4
          ldb_selections_error        = 5
          ldb_selections_not_accepted = 6
          variant_not_existent        = 7
          variant_obsolete            = 8
          variant_error               = 9
          free_selections_error       = 10
          callback_no_event           = 11
          callback_node_duplicate     = 12
          callback_no_program         = 13
          callback_no_cbform          = 14
          dyn_node_no_type            = 15
          dyn_node_invalid_type       = 16
          OTHERS                      = 17.

      IF sy-subrc NE 0.
        lv_subrc = 4.
      ELSE.
        DELETE gt_fmfphdr WHERE autyp NE gc_doc_category-process_order.
        lt_fmfphdr[] = gt_fmfphdr[].
        IF lt_fmfphdr[] IS INITIAL.
          lv_subrc = 4.
        ENDIF.
      ENDIF.

      IF lv_subrc IS INITIAL.
        READ TABLE gt_fmfphdr INTO DATA(lwa_fmfphdr) INDEX 1.
        IF sy-subrc EQ 0.
          INSERT INITIAL LINE INTO TABLE lt_aufnr
            ASSIGNING FIELD-SYMBOL(<lwa_aufnr>).
          IF sy-subrc EQ 0.
            <lwa_aufnr>-aufnr = lwa_fmfphdr-aufnr.
            CALL FUNCTION '/AGRI/FMFP_VIEW'
              EXPORTING
                it_aufnr       = lt_aufnr
              IMPORTING
                et_fpdoc       = lt_fpdoc
              EXCEPTIONS ##FM_SUBRC_OK
                no_data_exists = 1
                OTHERS         = 2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALLBACK_FMFPHDR
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
FORM callback_fmfphdr USING lv_name  TYPE ldbn-ldbnode
                            lwa_data TYPE /agri/fmfphdr
                            lv_evt   TYPE c
                            lv_check TYPE c.

  DATA: lwa_fmfphdr LIKE LINE OF gt_fmfphdr.

  MOVE-CORRESPONDING lwa_data TO lwa_fmfphdr.
  APPEND lwa_fmfphdr TO gt_fmfphdr.

ENDFORM. "CALLBACK_FMFPHDR

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------**
FORM document_infocus_lock  USING lv_aufnr TYPE /agri/fmfpnum
                         CHANGING lv_subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  CALL FUNCTION 'ENQUEUE_ESORDER'
    EXPORTING
      aufnr          = lv_aufnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc IS NOT INITIAL.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    MESSAGE i037(/agri/fmfp) INTO sy-msgli
                             WITH lv_aufnr lv_msgv1.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock  USING lv_aufnr TYPE /agri/fmfpnum
                                    lv_rsnum TYPE rsnum.

  CALL FUNCTION 'DEQUEUE_ESORDER'
    EXPORTING
      aufnr     = lv_aufnr
      _synchron = abap_true.
  CALL FUNCTION 'DEQUEUE_EMRKPF'
    EXPORTING
      rsnum     = lv_rsnum
      _synchron = abap_true.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK

*&---------------------------------------------------------------------*
*& Form CREATE_TASK_ORDERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LWA_FPDOC>
*&      <-- IF
*&      <-- FOUND
*&---------------------------------------------------------------------*
FORM create_task_orders USING lv_matkl TYPE matkl
                              lt_fmfphdr TYPE ty_fmfphdr_tab
                     CHANGING lt_fpdoc TYPE /agri/t_fmfp_doc.

  DATA: lt_fpdoc_to    TYPE /agri/t_fmfp_doc,
        lt_messtab     TYPE tab_bdcmsgcoll,
        lt_messages    TYPE /agri/t_gprolog,
        lwa_fpitm      TYPE /agri/s_fmfpitm,
        lwa_fpcom      TYPE /agri/s_fmfpcom,
        lwa_fpcord     TYPE /agri/s_fmfpcord,
        lwa_csdoc      TYPE /agri/s_glcs_doc,
        lwa_order_comm TYPE /agri/s_glpocomm,
        lv_posnr       TYPE co_posnr,
        lv_subrc       TYPE sy-subrc,
        lv_commit_work TYPE abap_bool VALUE abap_true,
        lv_success.

  DATA: lv_string TYPE string.

  lv_string = lv_matkl && '*'.

  READ TABLE lt_fmfphdr INTO DATA(lwa_fmfphdr) INDEX 1.
  IF sy-subrc EQ 0.

    READ TABLE lt_fpdoc ASSIGNING FIELD-SYMBOL(<lwa_fpdoc>) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(lv_items) = lines( <lwa_fpdoc>-x-fpitm ).
      INSERT INITIAL LINE INTO TABLE <lwa_fpdoc>-x-fpitm
        ASSIGNING FIELD-SYMBOL(<lwa_fpitm>).
      IF sy-subrc EQ 0.
*************************************************************************
* Seleciona linha de acordo com tarefa
        LOOP AT <lwa_fpdoc>-x-fpitm INTO DATA(ls_items) WHERE ltxa1 CP lv_string.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_items TO <lwa_fpitm>.
        ENDIF.
* Seleciona linha de acordo com tarefa
        LOOP AT <lwa_fpdoc>-x-fpcom INTO DATA(ls_doc) WHERE posnr EQ ls_items-posnr.
          exit.
        ENDLOOP.
**************************************************************************
        <lwa_fpitm>-aufnr  = <lwa_fpdoc>-aufnr.
        <lwa_fpitm>-posnr  = lv_items + 1.
        <lwa_fpitm>-tomng = lwa_fmfphdr-gamng.

        SELECT *
          INTO TABLE @DATA(lt_mkal)
          FROM mkal
         WHERE matnr = @<lwa_fpitm>-matnr
           AND werks = @lwa_fmfphdr-iwerk.

        READ TABLE lt_mkal INTO DATA(lwa_mkal) INDEX 1.
        IF sy-subrc EQ 0.
          <lwa_fpitm>-verid = lwa_mkal-verid.
        ENDIF.

        SELECT SINGLE meins
          FROM mara
          INTO @<lwa_fpitm>-meinh
         WHERE matnr EQ @<lwa_fpitm>-matnr.

        <lwa_fpitm>-actdt  = sy-datum.
        <lwa_fpitm>-unpln  = abap_true.
        <lwa_fpitm>-confm = abap_true.
        <lwa_fpitm>-action = abap_true.

        INSERT INITIAL LINE INTO TABLE <lwa_fpdoc>-x-fpcom
          ASSIGNING FIELD-SYMBOL(<lwa_fpcom>).
        IF sy-subrc EQ 0.
          IF ls_doc IS NOT INITIAL.
            MOVE-CORRESPONDING ls_doc to <lwa_fpdoc>.
          ENDIF.
*          MOVE-CORRESPONDING <lwa_fpitm> TO <lwa_fpcom>.
          <lwa_fpcom>-contr = 1.
          <lwa_fpcom>-updkz = <lwa_fpitm>-updkz  = c_updkz_new.
        ENDIF.

        MOVE-CORRESPONDING <lwa_fpdoc>-x-fphdr TO lwa_fpcord.
        MOVE-CORRESPONDING <lwa_fpitm> TO lwa_fpcord.
        lwa_fpcord-matnr = <lwa_fpcom>-matnr.
        lwa_fpcord-gamng = <lwa_fpitm>-tomng.
        lwa_fpcord-gmein = <lwa_fpitm>-meinh.
        lwa_fpcord-gstrp = <lwa_fpitm>-actdt.
        lwa_fpcord-grcre = <lwa_fpdoc>-x-fphdr-grcre.
        lwa_fpcord-ordlv = c_order_level-process.

        CLEAR: lwa_fpcord-aufnr,
               lwa_fpcord-schta.
        IF <lwa_fpitm>-unpln IS NOT INITIAL.
          <lwa_fpitm>-gamng = lwa_fpcord-gamng.
        ENDIF.

        lwa_fpcord-autyp = c_document_category-task_order.

        MOVE-CORRESPONDING lwa_fpcord TO lwa_order_comm.
        CLEAR: lwa_order_comm-auart.

        CALL METHOD /agri/cl_glco_process=>order_create
          EXPORTING
            is_order_comm = lwa_order_comm
          IMPORTING
            e_aufnr       = lwa_fpcord-aufnr
            et_messages   = lt_messtab.

        IF lwa_fpcord-aufnr IS NOT INITIAL.
          PERFORM document_infocus_prepare IN PROGRAM /agri/saplfmfpm
                                           USING lwa_fpcord
                                                 lwa_csdoc
                                        CHANGING lt_fpdoc_to IF FOUND.

          READ TABLE lt_fpdoc_to ASSIGNING FIELD-SYMBOL(<lwa_fpdoc_to>) INDEX 1.
          IF sy-subrc EQ 0.
            <lwa_fpdoc_to>-x-fphdr-aufnr_v = <lwa_fpdoc>-x-fphdr-aufnr.

            READ TABLE <lwa_fpdoc>-x-fpitm ASSIGNING <lwa_fpitm>
              WITH KEY confm = abap_true.
            IF sy-subrc EQ 0.
              <lwa_fpdoc_to>-x-fphdr-posnr_v = <lwa_fpitm>-posnr.
              <lwa_fpdoc_to>-x-fphdr-equnr   = <lwa_fpitm>-equnr.
              <lwa_fpdoc_to>-x-fphdr-gangcd  = <lwa_fpitm>-gangcd.

              IF <lwa_fpitm>-unpln IS NOT INITIAL.
                <lwa_fpdoc_to>-x-fphdr-gamng = lwa_fpcord-gamng.
              ENDIF.

              CALL FUNCTION '/AGRI/FMFP_SAVE'
                EXPORTING
                  i_commit_work = lv_commit_work
                CHANGING
                  ct_fpdoc      = lt_fpdoc_to
                  ct_messages   = lt_messages
                EXCEPTIONS
                  no_change     = 1
                  OTHERS        = 2.

              lv_subrc = sy-subrc.

              LOOP AT lt_messages INTO DATA(lwa_message).
                MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
                   NUMBER lwa_message-msgno WITH lwa_message-msgv1
                   lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
                              INTO sy-msgli.
                message_simple space.
              ENDLOOP.

              IF lv_subrc NE 0.
                IF lv_subrc EQ 1.
                  MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                                    INTO sy-msgli.
                  message_simple space.
                ELSE.
***Extended Additional Syntax Check ATC  1709 PQ
                  MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '008'
                                   INTO sy-msgli. "#EC*
*****
                  message_simple space.
                ENDIF.
              ELSE.
                MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-success NUMBER '007'
                                   WITH <lwa_fpdoc_to>-aufnr INTO sy-msgli.
                message_simple space.
              ENDIF.

              IF lv_subrc EQ 0.
                lv_success = abap_true.
                PERFORM document_flow_update IN PROGRAM /agri/saplfmfpm
                  USING c_document_category-production_order
                        lwa_fpcord
                        lt_fpdoc_to IF FOUND.
                <lwa_fpitm>-aufnr_to = <lwa_fpdoc_to>-aufnr.
*****add new line for remaining quantity
                IF <lwa_fpitm>-gamng NE lwa_fpcord-gamng.
                  CLEAR: lwa_fpitm, lwa_fpcom.
                  lwa_fpitm = <lwa_fpitm>.
                  lwa_fpcom = <lwa_fpcom>.
                  CLEAR: lwa_fpitm-gamng,
                         lwa_fpitm-confm,
                         lwa_fpitm-gwemg,
                         lwa_fpitm-aufnr_to,
                         lwa_fpitm-tomng.
                  lwa_fpitm-gamng = <lwa_fpitm>-gamng - lwa_fpcord-gamng.
                  IF lv_posnr IS INITIAL.
                    LOOP AT <lwa_fpdoc>-x-fpitm INTO DATA(lwa_fpitm_tmp).
                      IF lv_posnr LT lwa_fpitm_tmp-posnr.
                        lv_posnr = lwa_fpitm_tmp-posnr.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                  lv_posnr = lv_posnr + 1.
                  lwa_fpitm-posnr = lv_posnr.
                  lwa_fpcom-posnr = lv_posnr.
                  lwa_fpitm-updkz = c_updkz_new.
                  lwa_fpcom-updkz = c_updkz_new.
                  APPEND lwa_fpitm TO <lwa_fpdoc>-x-fpitm.
                  APPEND lwa_fpcom TO <lwa_fpdoc>-x-fpcom.
                ENDIF.
                <lwa_fpitm>-gamng = <lwa_fpdoc_to>-x-fphdr-gamng.
                IF <lwa_fpitm>-updkz NE c_updkz_new.
                  <lwa_fpitm>-updkz = c_updkz_update.
                ENDIF.

                CLEAR: <lwa_fpitm>-confm.

                IF lv_success IS NOT INITIAL
                AND <lwa_fpdoc>-aufnr IS NOT INITIAL.
                  lv_commit_work = abap_true.
                  REFRESH lt_messages.
                  CLEAR lv_subrc.
                  CALL FUNCTION '/AGRI/FMFP_SAVE'
                    EXPORTING
                      i_commit_work = lv_commit_work
                    CHANGING
                      ct_fpdoc      = lt_fpdoc
                      ct_messages   = lt_messages
                    EXCEPTIONS
                      no_change     = 1
                      OTHERS        = 2.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_display USING lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = 'SAPLZFMFP_TASK_ORDER_CREATE'.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator abap_true space space ls_variant.
  messages_init.

ENDFORM.                    " MESSAGES_DISPLAY
