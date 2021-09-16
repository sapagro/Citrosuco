************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_WORKORDER_SUB                          *
* Tcode             :  ZABS_TRN_IORD_CRT                               *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  02.14.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Monitor Data Preperation and Display *
*                      Data.                                           *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form FETCH_IRRIGATION_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_irrigation_data .

  IF p_werks IS INITIAL.
*-- Informe a Fazenda!
    MESSAGE i407(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
*--Fetching Equipment header and Equipment master plants data
    SELECT SINGLE werks, name2
      FROM t001w
      INTO @DATA(ls_t001w)
     WHERE werks EQ @p_werks.
    IF sy-subrc NE 0.
*-- Fazenda &1 inválida!
      MESSAGE i410(zfmfp) WITH p_werks.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF so_equnr[] IS INITIAL.
*-- Informe o Projeto!
    MESSAGE i408(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT *
      FROM /agri/fmirhdr
      INTO TABLE @gt_fmirhdr
     WHERE equnr IN @so_equnr[].
    IF sy-subrc NE 0.
*-- Não foram encontrados projetos válidos para os parâmetros informados!
      MESSAGE i411(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT gt_fmirhdr BY equnr.

      SELECT *
        FROM zabst_planej
        INTO TABLE @gt_planej
        FOR ALL ENTRIES IN @gt_fmirhdr
       WHERE equnr = @gt_fmirhdr-equnr.

      SORT gt_planej BY equnr irrdate.
    ENDIF.
  ENDIF.

  IF p_date IS INITIAL.
*-- Informe a Data!
    MESSAGE i409(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT *
      FROM zabst_ordhdr
      INTO TABLE @gt_ordhdr
     WHERE werks EQ @p_werks
       AND equnr IN @so_equnr[]
       AND datab LE @p_date
       AND datbi GE @p_date.
    IF sy-subrc EQ 0.
      SORT gt_ordhdr BY equnr.

      LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
        DATA(lv_tabix) = sy-tabix.
        READ TABLE gt_ordhdr INTO DATA(ls_ordhdr)
          WITH KEY equnr = ls_fmirhdr-equnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_ordhdr-ordno
            IMPORTING
              output = ls_ordhdr-ordno.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_fmirhdr-equnr
            IMPORTING
              output = ls_fmirhdr-equnr.

          INSERT INITIAL LINE INTO TABLE gt_message
            ASSIGNING FIELD-SYMBOL(<ls_message>).
          IF sy-subrc EQ 0.
*-- Ordem &1 existe para projeto &2.
            <ls_message>-msgid = 'ZMFP'.
            <ls_message>-msgno = '412'.
            <ls_message>-msgty = 'I'.
            <ls_message>-msgv1 = ls_ordhdr-ordno.
            <ls_message>-msgv2 = ls_fmirhdr-equnr.
          ENDIF.
          DELETE gt_fmirhdr INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_fmirhdr, gt_planej, gt_ordhdr, gt_message.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIABLES_INITIATOR
*&---------------------------------------------------------------------*
FORM display_messages  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-cprog.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR lv_initiator.

ENDFORM.                    " MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*& Form CREATE_WORKORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_workorder .

*--Local declarations
  DATA: lt_messages TYPE /agri/t_gprolog,
        lv_ordno    TYPE /agri/fmfpnum.

  LOOP AT gt_fmirhdr INTO DATA(ls_fmirhdr).
    READ TABLE gt_planej INTO DATA(ls_planej)
      WITH KEY equnr   = ls_fmirhdr-equnr
               irrdate = p_date BINARY SEARCH.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_fmirhdr-equnr
          IMPORTING
            output = ls_fmirhdr-equnr.
*-- Não existe programação para projeto &1.
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '415'.
        <ls_message>-msgty = 'I'.
        <ls_message>-msgv1 = ls_fmirhdr-equnr.
      ENDIF.
      CONTINUE.
    ENDIF.

    REFRESH: lt_messages.
    CLEAR: lv_ordno.

*--FM to create WorkOrder
    CALL FUNCTION 'ZABS_FM_WO_CREATION'
      EXPORTING
        i_farm      = p_werks
        i_equnr     = ls_fmirhdr-equnr
        i_date      = p_date
      IMPORTING
        et_messages = lt_messages
        ev_ordno    = lv_ordno.

    INSERT INITIAL LINE INTO TABLE gt_message
      ASSIGNING <ls_message>.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_fmirhdr-equnr
        IMPORTING
          output = ls_fmirhdr-equnr.
*-- Projeto: &1
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = '413'.
      <ls_message>-msgty = 'I'.
      <ls_message>-msgv1 = ls_fmirhdr-equnr.
    ENDIF.

    APPEND LINES OF lt_messages TO gt_message.
  ENDLOOP.

ENDFORM.
