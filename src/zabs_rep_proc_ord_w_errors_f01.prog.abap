*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_MEASUREM_DOC_CREATF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_afko, gt_fmfphdr, gt_message.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_SHEET
*&---------------------------------------------------------------------*
FORM data_get.

  DATA: lv_qtd_vtx_zero TYPE co_gmein,
        lv_qtd_std_zero TYPE co_igmng.

  REFRESH: gt_fmfphdr, gt_afko.

  SELECT *
    FROM /agri/fmfphdr
    INTO TABLE @gt_fmfphdr
   WHERE aufnr IN @s_aufnr[]
     AND auart IN @s_auart[]
     AND gwemg NE @lv_qtd_vtx_zero
     AND iwerk IN @s_iwerk[]
     AND erdat IN @s_erdat[].
  IF sy-subrc EQ 0.
    SELECT aufnr, gamng, igmng
      FROM afko
      INTO TABLE @gt_afko
      FOR ALL ENTRIES IN @gt_fmfphdr
     WHERE aufnr = @gt_fmfphdr-aufnr.
*       AND igmng = @lv_qtd_std_zero.
  ENDIF.

  IF gt_fmfphdr[] IS INITIAL
  AND gt_afko[] IS INITIAL.
*-- Não existem dados para os parâmetros informados!
    MESSAGE i325(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_fmfphdr BY aufnr.
    SORT gt_afko BY aufnr.
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
FORM selection_validations.

  IF s_auart[] IS INITIAL.
*-- O parâmetro Tipo de Ordem é de preenchimento obrigatório!
    MESSAGE i322(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF s_iwerk[] IS INITIAL.
**-- O parâmetro Centro é de preenchimento obrigatório!
*    MESSAGE i323(zfmfp).
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  IF s_erdat[] IS INITIAL.
**-- O parâmetro Data de Criação é de preenchimento obrigatório!
*    MESSAGE i324(zfmfp).
*    LEAVE LIST-PROCESSING.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_PROCESS_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_process_order .

  DATA: lv_aufnr TYPE /agri/fmfpnum.

  LOOP AT gt_fmfphdr ASSIGNING FIELD-SYMBOL(<ls_fmfphdr>).
    DATA(lv_changed) = abap_false.
    DATA(lv_tabix) = sy-tabix.
    READ TABLE gt_afko INTO DATA(ls_afko)
      WITH KEY aufnr = <ls_fmfphdr>-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <ls_fmfphdr>-gwemg NE ls_afko-igmng.
        <ls_fmfphdr>-gwemg = ls_afko-igmng.
        lv_changed = abap_true.
      ENDIF.
    ENDIF.
    IF lv_changed EQ abap_false.
*      UNASSIGN <ls_fmfphdr>.
      DELETE gt_fmfphdr INDEX lv_tabix.
    ELSE.
*-- Ordem & foi atualizada com sucesso!
      INSERT INITIAL LINE INTO TABLE gt_message
        ASSIGNING FIELD-SYMBOL(<ls_message>).
      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <ls_fmfphdr>-aufnr
          IMPORTING
            output = lv_aufnr.

        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = '326'.
        <ls_message>-msgty = 'S'.
        <ls_message>-msgv1 = lv_aufnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF gt_fmfphdr[] IS NOT INITIAL.
    MODIFY /agri/fmfphdr FROM TABLE gt_fmfphdr.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
