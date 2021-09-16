*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_ATTR_GROUP_FPSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CREATE_PROCESS_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_GLFLCMA_CMNUM
*&      --> LS_GLFLCMA_VARIA
*&      --> P_
*&---------------------------------------------------------------------*
FORM create_process_order USING lt_fmfphdr    TYPE ty_fmfphdr_tab
                                lt_cpros      TYPE ty_cpros_tab
                                ls_glflcma    TYPE /agri/glflcma
                                lv_cpros      TYPE /agri/glcpros
                                lv_tplnr_fl   TYPE /agri/gltplnr_fl
                                lv_start_date TYPE sydatum
                       CHANGING lt_fpdoc      TYPE /agri/t_fmfp_doc
                                lt_message    TYPE /agri/t_gprolog.

  DATA: lt_aufnr     TYPE /agri/t_fmaufnr,
        lt_cskey     TYPE STANDARD TABLE OF /agri/s_glcs_key INITIAL SIZE 0,
        lt_msg_fmfp  TYPE /agri/t_gprolog,
        lv_msgno     TYPE msgno,
        lv_null_date TYPE sydatum,
        lv_gstrp     TYPE co_gstrp,
        lv_msgv1     TYPE msgv1,
        lv_msgv2     TYPE msgv2.

*-- Create process orders for crop process
  READ TABLE lt_cpros INTO DATA(ls_cpros)
    WITH KEY cmnum = ls_glflcma-cmnum
             varia = ls_glflcma-varia
             cpros = lv_cpros.
  IF sy-subrc EQ 0.
*-- Check if any active process order exists for that cpros, variant
    READ TABLE lt_fmfphdr INTO DATA(ls_fmfphdr)
         WITH KEY autyp = 'AO'
                  contr = ls_glflcma-contr
                  cmnum = ls_glflcma-cmnum
                  varia = ls_glflcma-varia
                  cpros = lv_cpros.
    IF sy-subrc EQ 0
    AND ls_fmfphdr-tecom EQ abap_false.
      CLEAR: lv_msgv1, lv_msgv2.
      lv_msgv1 = lv_tplnr_fl.
      lv_msgv2 = ls_fmfphdr-aufnr.

      IF lv_cpros EQ c_crop_process-formacao.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura FORMAÇÃO!
        lv_msgno = '363'.
      ELSEIF lv_cpros EQ c_crop_process-manutencao.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura MANUTENÇÃO!
        lv_msgno = '372'.
      ELSEIF lv_cpros EQ c_crop_process-colheita.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura COLHEITA!
        lv_msgno = '367'.
      ELSEIF lv_cpros EQ c_crop_process-implantacao.
*-- Talhão &1: Ordem processo &2 existe p/ processo de cultura IMPLANTAÇÃO!
        lv_msgno = '383'.
      ENDIF.

*-- Talhão &1: Existe ordem p/ processo de cultura FORMAÇÃO/MANUTEÇÃO/COLHEITA!
      PERFORM build_msgs USING 'I' lv_msgno
                               lv_msgv1 lv_msgv2
                      CHANGING lt_message[].

      INSERT INITIAL LINE INTO TABLE lt_aufnr
        ASSIGNING FIELD-SYMBOL(<ls_aufnr>).
      IF sy-subrc EQ 0.
        <ls_aufnr>-aufnr = ls_fmfphdr-aufnr.

        CALL FUNCTION '/AGRI/FMFP_VIEW'
          EXPORTING
            it_aufnr       = lt_aufnr
          IMPORTING
            et_fpdoc       = lt_fpdoc
          EXCEPTIONS
            no_data_exists = 1
            OTHERS         = 2.
      ENDIF.
    ELSE.
      INSERT INITIAL LINE INTO TABLE lt_cskey
        ASSIGNING FIELD-SYMBOL(<ls_cskey>).
      IF sy-subrc EQ 0.
        <ls_cskey>-tplnr_fl = ls_glflcma-tplnr_fl.
        <ls_cskey>-contr    = ls_glflcma-contr.
      ENDIF.
*-- If not active process order found, then create process order
      IF lv_start_date NE lv_null_date.
        lv_gstrp = lv_start_date.
      ELSE.
        lv_gstrp = sy-datum.
      ENDIF.

      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = abap_true
          i_commit_work         = abap_true
          i_cpros               = ls_cpros-cpros
          i_gstrp               = lv_gstrp
*         i_release             = 'X'
          it_cskey              = lt_cskey[]
        IMPORTING
          et_fpdoc              = lt_fpdoc[]
          et_messages           = lt_msg_fmfp[]
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.

      IF sy-subrc EQ 0.
*-- Build messages
        LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
          CLEAR: lv_msgv1, lv_msgv2.
          lv_msgv1 = lv_tplnr_fl.
          lv_msgv2 = ls_fpdoc-aufnr.

          IF lv_cpros EQ c_crop_process-formacao.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura FORMAÇÃO!
            lv_msgno = '364'.
          ELSEIF lv_cpros EQ c_crop_process-manutencao.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura MANUTENÇÃO!
            lv_msgno = '373'.
          ELSEIF lv_cpros EQ c_crop_process-colheita.
            lv_msgno = '368'.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura COLHEITA!
          ELSEIF lv_cpros EQ c_crop_process-implantacao.
            lv_msgno = '384'.
*-- Talhão &1: Ordem processo &2 criada p/ processo de cultura IMPLANTAÇÃO!
          ENDIF.

*-- Talhão &1: Ordem &2 criada p/ processo de cultura FORMAÇÃO!
          PERFORM build_msgs USING 'S' lv_msgno
                                   lv_msgv1 lv_msgv2
                          CHANGING lt_message[].
          CLEAR: lv_msgv1.
        ENDLOOP.

        IF ls_fpdoc-aufnr IS NOT INITIAL.
          REFRESH: lt_aufnr, lt_fpdoc.
          INSERT INITIAL LINE INTO TABLE lt_aufnr
            ASSIGNING <ls_aufnr>.
          IF sy-subrc EQ 0.
            <ls_aufnr>-aufnr = ls_fpdoc-aufnr.

            CALL FUNCTION '/AGRI/FMFP_VIEW'
              EXPORTING
                it_aufnr       = lt_aufnr
              IMPORTING
                et_fpdoc       = lt_fpdoc
              EXCEPTIONS
                no_data_exists = 1
                OTHERS         = 2.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lt_fpdoc IS INITIAL.
        APPEND LINES OF lt_msg_fmfp TO lt_message.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_MSGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_CONSTANTS_MSGTY_SUCCESS
*&      --> P_
*&      --> LV_MSGV1
*&      --> SPACE
*&      <-- GT_MESSAGE[]
*&---------------------------------------------------------------------*
FORM build_msgs USING pv_msgty    TYPE msgty
                      pv_msgno    TYPE msgno
                      pv_msgv1    TYPE msgv1
                      pv_msgv2    TYPE msgv2
             CHANGING ct_messages TYPE /agri/t_gprolog.

*-- Local work areas
  DATA: ls_messages TYPE /agri/s_gprolog.

  ls_messages-msgid = 'ZFMFP'.
  ls_messages-msgty = pv_msgty.
  ls_messages-msgno = pv_msgno.
  ls_messages-msgv1 = pv_msgv1.
  ls_messages-msgv2 = pv_msgv2.
  APPEND ls_messages TO ct_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------**
FORM document_infocus_lock  USING lv_aufnr   TYPE /agri/fmfpnum
                         CHANGING lt_message TYPE /agri/t_gprolog
                                  lv_subrc   TYPE sysubrc.

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
    INSERT INITIAL LINE INTO TABLE lt_message
      ASSIGNING FIELD-SYMBOL(<ls_message>).
    IF sy-subrc EQ 0.
*-- A ordem &1 está bloqueada pelo usuário &2
      <ls_message>-msgid = '/AGRI/FMFP'.
      <ls_message>-msgno = '037'.
      <ls_message>-msgty = 'I'.
      <ls_message>-msgv1 = lv_aufnr.
      <ls_message>-msgv2 = lv_msgv1.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*& Form CREATE_TASK_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FPDOC
*&---------------------------------------------------------------------*
FORM create_task_order USING lv_tplnr_fl   TYPE /agri/gltplnr_fl
                             lv_matnr      TYPE matnr
                             lv_start_date TYPE sydatum
                    CHANGING lt_fpdoc      TYPE /agri/t_fmfp_doc
                             lt_message    TYPE /agri/t_gprolog.

  DATA: lt_fpitm       TYPE /agri/t_fmfpitm,
        lt_messages_to TYPE /agri/t_gprolog,
        lv_null_date   TYPE sydatum,
        lv_aufnr       TYPE /agri/fmfpnum.

  READ TABLE lt_fpdoc ASSIGNING FIELD-SYMBOL(<ls_fpdoc>) INDEX 1.
  IF sy-subrc EQ 0.
*-- LTXA1: TAREFA GENERICA PARA CUSTOS DE FORMAÇÃO ou MANUTENÇÃO
    READ TABLE <ls_fpdoc>-x-fpcom INTO DATA(ls_fpcom)
      WITH KEY matnr = lv_matnr.
    IF sy-subrc EQ 0.
      READ TABLE <ls_fpdoc>-x-fpitm ASSIGNING FIELD-SYMBOL(<ls_fpitm>)
        WITH KEY posnr = ls_fpcom-posnr
                 vornr = ls_fpcom-vornr.
      IF sy-subrc EQ 0.
        IF <ls_fpitm>-aufnr_to IS INITIAL.
          <ls_fpdoc>-x-fphdr-updkz = c_updkz_update.
          <ls_fpitm>-matnr = ls_fpcom-matnr.

          IF lv_start_date NE lv_null_date.
            <ls_fpitm>-schdt = lv_start_date.
          ELSE.
            <ls_fpitm>-schdt = sy-datum.
          ENDIF.

          <ls_fpitm>-updkz = c_updkz_update.

          CALL FUNCTION '/AGRI/FMFP_SAVE'
            EXPORTING
*             I_SET_UPDATE_TASK = 'X'
              i_commit_work = 'X'
            CHANGING
              ct_fpdoc      = lt_fpdoc
              ct_messages   = lt_messages_to
            EXCEPTIONS
              no_change     = 1
              OTHERS        = 2.

          APPEND LINES OF lt_messages_to TO lt_message.

          REFRESH lt_fpitm.
          INSERT INITIAL LINE INTO TABLE lt_fpitm
            ASSIGNING FIELD-SYMBOL(<ls_fpitm_new>).
          IF sy-subrc EQ 0.
            <ls_fpitm_new> = <ls_fpitm>.
            <ls_fpitm_new>-tomng = <ls_fpitm_new>-gamng.
          ENDIF.

          IF lt_fpitm[] IS NOT INITIAL.
            CALL FUNCTION '/AGRI/FMFP_TASK_ORDER_CREATE'
              EXPORTING
*               I_SAVE_MESSAGES   = ' '
                i_commit_work     = 'X'
                it_fpitm          = lt_fpitm
              IMPORTING
                et_todoc          = lt_fpdoc
                et_messages       = lt_messages_to
              EXCEPTIONS
                inconsistent_data = 1
                no_data           = 2
                OTHERS            = 3.

            APPEND LINES OF lt_messages_to TO lt_message.
          ENDIF.
        ELSE.
*-- Talhão &1: Ordem Processo &2 Ordem Tarefa &3 existentes para &4!
          INSERT INITIAL LINE INTO TABLE lt_message
            ASSIGNING FIELD-SYMBOL(<ls_message>).
          IF sy-subrc EQ 0.
            <ls_message>-msgid = 'ZFMFP'.
            <ls_message>-msgno = '365'.
            <ls_message>-msgty = 'I'.
            <ls_message>-msgv1 = lv_tplnr_fl.
            <ls_message>-msgv2 = <ls_fpitm>-aufnr.
            <ls_message>-msgv3 = <ls_fpitm>-aufnr_to.
            <ls_message>-msgv4 = lv_matnr.
          ENDIF.
        ENDIF.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <ls_fpdoc>-aufnr
          IMPORTING
            output = lv_aufnr.

*-- Talhão &1: Não existe material &2 em Ordem Processo &3!
        INSERT INITIAL LINE INTO TABLE lt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '366'.
          <ls_message>-msgty = 'I'.
          <ls_message>-msgv1 = lv_tplnr_fl.
          <ls_message>-msgv2 = lv_matnr.
          <ls_message>-msgv3 = lv_aufnr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock USING lv_aufnr TYPE /agri/fmfpnum
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
*& Form SAVE_CSDOC_CHANGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_CSDOC_CHANGE
*&      <-- LT_MSG_CHANGE
*&---------------------------------------------------------------------*
FORM save_csdoc_changes USING    lv_tplnr_fl     TYPE /agri/gltplnr_fl
                        CHANGING lt_csdoc_change TYPE /agri/t_glcs_doc
                                 lt_message      TYPE /agri/t_gprolog
                                 lv_subrc        TYPE sysubrc.

  DATA: lt_msg_save TYPE /agri/t_gprolog.

*-- Grava Modificações na Época de Cultura
  IF lt_csdoc_change[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLCS_SAVE'
      EXPORTING
        i_set_update_task  = abap_true
        i_commit_work      = 'X'
      CHANGING
        ct_csdoc           = lt_csdoc_change
        ct_messages        = lt_msg_save
      EXCEPTIONS
        no_change          = 1
        error_while_saving = 2
        OTHERS             = 3.

    IF sy-subrc NE 0.
      IF sy-msgid IS NOT INITIAL
      AND sy-msgty IS NOT INITIAL
      AND sy-msgno IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.

        INSERT INITIAL LINE INTO TABLE lt_message
          ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-subrc EQ 0.
          <ls_message>-msgid = sy-msgid.
          <ls_message>-msgno = sy-msgno.
          <ls_message>-msgty = sy-msgty.
          <ls_message>-msgv1 = sy-msgv1.
          <ls_message>-msgv2 = sy-msgv2.
          <ls_message>-msgv3 = sy-msgv3.
          <ls_message>-msgv4 = sy-msgv4.
        ENDIF.
      ENDIF.
      lv_subrc = 4.
    ELSE.
*-- Modif.Gravadas: Talhão &1/Cultura &2/Variante &3/Status &4
      READ TABLE lt_csdoc_change INTO DATA(ls_csdoc_change) INDEX 1.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE lt_message
          ASSIGNING <ls_message>.
        IF sy-subrc EQ 0.
          <ls_message>-msgid = 'ZFMFP'.
          <ls_message>-msgno = '358'.
          <ls_message>-msgty = 'S'.
          <ls_message>-msgv1 = lv_tplnr_fl.
          <ls_message>-msgv2 = ls_csdoc_change-x-cshdr-cmnum.
          <ls_message>-msgv3 = ls_csdoc_change-x-cshdr-varia.
          <ls_message>-msgv4 = ls_csdoc_change-x-cshdr-astat.
        ENDIF.
      ENDIF.
      lv_subrc = 0.
    ENDIF.

    LOOP AT lt_msg_save INTO DATA(ls_msg_change).
      INSERT INITIAL LINE INTO TABLE lt_message
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
        <ls_message>-msgid = ls_msg_change-msgid.
        <ls_message>-msgno = ls_msg_change-msgno.
        <ls_message>-msgty = ls_msg_change-msgty.
        <ls_message>-msgv1 = ls_msg_change-msgv1.
        <ls_message>-msgv2 = ls_msg_change-msgv2.
        <ls_message>-msgv3 = ls_msg_change-msgv3.
        <ls_message>-msgv4 = ls_msg_change-msgv4.
      ENDIF.
    ENDLOOP.

    REFRESH lt_csdoc_change.
  ENDIF.

ENDFORM.
