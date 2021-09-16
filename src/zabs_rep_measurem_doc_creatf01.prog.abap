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

  REFRESH: gt_sheet, gt_message, gt_terrain_dtls,
           gt_farm_subtot, gt_notes_buffer.

  CLEAR: gv_ano_safra, gv_periodo, gv_tipo_laudo,
         gv_plantas, gv_centro, gv_norte, gv_sul,
         gv_exsul.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_SHEET
*&---------------------------------------------------------------------*
FORM data_get USING p_per    TYPE sy-datum
                    rb_lep   TYPE abap_bool
                    rb_lpp   TYPE abap_bool
           CHANGING lt_sheet TYPE /agri/t_excel_sheet
                    s_region TYPE ty_atwrt.

*-- Local Declarations
  DATA: lt_farm_subtot  LIKE gt_farm_subtot,
        lt_constants    TYPE zabs_tty_vkey_const,
        ls_farm_subtot  LIKE LINE OF lt_farm_subtot,
        lv_integer      TYPE i,
        lv_total_plants TYPE zabs_del_plant,
        lv_item         TYPE i,
        lv_per          TYPE char7.

  REFRESH: gt_terrain_dtls, gt_farm_subtot, s_region.

  CLEAR: gv_ano_safra, gv_periodo, gv_tipo_laudo,
         gv_plantas, gv_centro, gv_norte, gv_sul,
         gv_exsul, gv_print.

  INSERT INITIAL LINE INTO TABLE s_region
    ASSIGNING FIELD-SYMBOL(<ls_region>).
  IF sy-subrc EQ 0.
    <ls_region> = 'IBT'.
    <ls_region>-low = '1'.
    <ls_region>-high = '4'.
  ENDIF.

*-- 01/2020 x 20201125 [AAAAMMDD]
  lv_per = p_per+4(2) && '/' && p_per(4).
*-- Get form details
*  SUBMIT zabs_rep_detalhes_laudo
*    WITH p_per    EQ lv_per
*    WITH s_region IN s_region
*    WITH rb_lep   EQ rb_lep
*    WITH rb_lpp   EQ rb_lpp
*    WITH p_print  EQ abap_false
*    AND RETURN.
  SUBMIT zabs_rep_terrain_details
    WITH p_per    EQ lv_per
    WITH s_region IN s_region
    WITH rb_lep   EQ rb_lep
    WITH rb_lpp   EQ rb_lpp
    WITH p_print  EQ abap_false
    AND RETURN.

  IMPORT gt_farm_subtot FROM MEMORY ID 'ZVTX_FARM_SUBTOT'.
  IMPORT gt_terrain_dtls FROM MEMORY ID 'ZVTX_TERRAIN_DTLS'.
  IMPORT gv_print FROM MEMORY ID 'ZVTX_PRINT'.
  FREE MEMORY ID 'ZVTX_FARM_SUBTOT'.
  FREE MEMORY ID 'ZVTX_TERRAIN_DTLS'.
  FREE MEMORY ID 'ZVTX_PRINT'.

  IF gv_print EQ abap_false.
    MESSAGE i106(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ENDIF.

*-- Checks Accounting Season
  SELECT *
    FROM zfmfpsafras
    INTO TABLE @DATA(lt_safras)
   WHERE tipo_safra = 'C'.

  SORT lt_safras BY ano_safra.
  DATA(lv_season_found) = abap_false.
  LOOP AT lt_safras INTO DATA(ls_safra).
    DATA(lv_tabix) = sy-tabix.
*-- SOC INC0031179 7/7/2021
* Fill based on value in the period field on the selection screen
*    IF ls_safra-inicio_safra GT sy-datum
*    OR ls_safra-fim_safra LT sy-datum.
    IF ls_safra-inicio_safra GT p_per
    OR ls_safra-fim_safra LT p_per.
*-- EOC INC0031179 7/7/2021
      DELETE lt_safras INDEX lv_tabix.
    ELSE.
      lv_season_found = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = 'LAUD'
      iv_k1val     = 'GLMDHDR'
*     iv_k2val     = ls_header-mdtyp
    IMPORTING
      et_constants = lt_constants.

*-- Create Measurement Document
*-- SOC INC0031179 7/7/2021
*  DO 11 TIMES.
  DO 12 TIMES.
*-- EOC INC0031179 7/7/2021
    DATA(lv_column) = sy-index + 1.
    IF lv_column NE 6
*-- SOC INC0031179 7/7/2021
    AND lv_column NE 12.
*-- EOC INC0031179 7/7/2021
      INSERT INITIAL LINE INTO TABLE gt_sheet
        ASSIGNING FIELD-SYMBOL(<ls_sheet>).
      IF sy-subrc EQ 0.
        <ls_sheet>-sheet = 01.
        <ls_sheet>-row = '16'.
        <ls_sheet>-column = lv_column.
        CASE lv_column.
*-- Measurement Document
          WHEN 02.
            <ls_sheet>-value = '1'.
*-- Measurement Document Type
          WHEN 03.
*            <ls_sheet>-value = 'ZPTA'.
            READ TABLE lt_constants INTO DATA(ls_constant)
              WITH KEY cnval1 = 'MDTYP'.
            IF sy-subrc EQ 0.
              <ls_sheet>-value = ls_constant-cnval2.
            ENDIF.
*-- Terrain
          WHEN 04.
*            <ls_sheet>-value = '999999-99999'.
            READ TABLE lt_constants INTO ls_constant
              WITH KEY cnval1 = 'TPLNR_FL'.
            IF sy-subrc EQ 0.
              <ls_sheet>-value = ls_constant-cnval2.
            ENDIF.
*-- Crop Master
          WHEN 05.
*            <ls_sheet>-value = 'CITROS'.
            READ TABLE lt_constants INTO ls_constant
              WITH KEY cnval1 = 'CMNUM'.
            IF sy-subrc EQ 0.
              <ls_sheet>-value = ls_constant-cnval2.
            ENDIF.
*-- Equipment number
*      WHEN 06.
*    <ls_sheet>-value = ...
*-- Measurement Group
          WHEN 07.
*            <ls_sheet>-value = 'FAZ-LAUDO'.
            READ TABLE lt_constants INTO ls_constant
              WITH KEY cnval1 = 'MPGRP'.
            IF sy-subrc EQ 0.
              <ls_sheet>-value = ls_constant-cnval2.
            ENDIF.
*-- Valid To
          WHEN 08.
            <ls_sheet>-value = sy-datum.
*-- Date of Measurement
          WHEN 09.
            <ls_sheet>-value = sy-datum.
*-- Time of Measurement
          WHEN 10.
            <ls_sheet>-value = sy-uzeit.
*-- Measured By
          WHEN 11.
            <ls_sheet>-value = sy-uname.
*-- SOC INC0031179 7/7/2021
*          WHEN 12.
          WHEN 13.
*-- EOC INC0031179 7/7/2021
*-- Nível de medição
            <ls_sheet>-value = 'A'.
        ENDCASE.
        CONDENSE: <ls_sheet>-row,
                  <ls_sheet>-column,
                  <ls_sheet>-value.
      ENDIF.
    ENDIF.
  ENDDO.

  LOOP AT gt_farm_subtot INTO DATA(ls_farm_subtot_memo).
    ls_farm_subtot-region = ls_farm_subtot_memo-region.
    ls_farm_subtot-qtd_plants = ls_farm_subtot_memo-qtd_plants.
    COLLECT ls_farm_subtot INTO lt_farm_subtot.
    lv_total_plants = lv_total_plants + ls_farm_subtot_memo-qtd_plants.
  ENDLOOP.

  SORT lt_farm_subtot BY region.

*-- Measurement Document's Attributes
  DO 8 TIMES. "Quantidade de Atributos
    DATA(lv_index) = sy-index.
*-- Starting Line 16
    DATA(lv_line) = sy-index + 15.
*-- Measurement Document
*-- /AGRI/GLMDHDR-MDOCM [1,2,3,4,...]
    INSERT INITIAL LINE INTO TABLE gt_sheet
      ASSIGNING <ls_sheet>.
    IF sy-subrc EQ 0.
      <ls_sheet>-sheet = 02.
      <ls_sheet>-row = lv_line.
      <ls_sheet>-column = 2.
      <ls_sheet>-value = '1'.
      CONDENSE: <ls_sheet>-row,
                <ls_sheet>-column,
                <ls_sheet>-value.
    ENDIF.
*-- Measurement Document Item
*-- Attribute Position [1,2,3,4,...]
    INSERT INITIAL LINE INTO TABLE gt_sheet
      ASSIGNING <ls_sheet>.
    IF sy-subrc EQ 0.
      <ls_sheet>-sheet = 02.
      <ls_sheet>-row = lv_line.
      <ls_sheet>-column = 3.
      ADD 1 TO lv_item.
      <ls_sheet>-value = lv_item.
      CONDENSE: <ls_sheet>-row,
                <ls_sheet>-column,
                <ls_sheet>-value.
    ENDIF.
*-- Attribute Name [FAZ_LARGURA_COPA,FAZ_ALTURA_COPA,...]
    INSERT INITIAL LINE INTO TABLE gt_sheet
      ASSIGNING <ls_sheet>.
    IF sy-subrc EQ 0.
      <ls_sheet>-sheet = 02.
      <ls_sheet>-row = lv_line.
      <ls_sheet>-column = 4.
      CASE lv_index.
        WHEN 1.
          <ls_sheet>-value = 'FAZ_SAFRA'.
        WHEN 2.
          <ls_sheet>-value = 'FAZ_PERIODO'.
        WHEN 3.
          <ls_sheet>-value = 'FAZ_LAUDO'.
        WHEN 4.
          <ls_sheet>-value = 'FAZ_QUANTIDADE_PLANTAS'.
        WHEN 5.
          <ls_sheet>-value = 'FAZ_QTD_REG_CENTRO'.
        WHEN 6.
          <ls_sheet>-value = 'FAZ_QTD_REG_NORTE'.
        WHEN 7.
          <ls_sheet>-value = 'FAZ_QTD_REG_SUL'.
        WHEN 8.
          <ls_sheet>-value = 'FAZ_QTD_REG_EX_SUL'.
      ENDCASE.
      CONDENSE: <ls_sheet>-row,
                <ls_sheet>-column,
                <ls_sheet>-value.
    ENDIF.
*-- Attribute Value [4,71;3,43;...]
    INSERT INITIAL LINE INTO TABLE gt_sheet
      ASSIGNING <ls_sheet>.
    IF sy-subrc EQ 0.
      <ls_sheet>-sheet = 02.
      <ls_sheet>-row = lv_line.
      <ls_sheet>-column = 5.
      CASE lv_index.
        WHEN 1.
          IF lv_season_found EQ abap_true.
            gv_ano_safra = <ls_sheet>-value = ls_safra-ano_safra.
          ENDIF.
        WHEN 2.
          gv_periodo = <ls_sheet>-value = lv_per.
        WHEN 3.
          IF rb_lep EQ abap_true.
            gv_tipo_laudo = <ls_sheet>-value = 'LEP'.
          ELSEIF rb_lpp EQ abap_true.
            gv_tipo_laudo = <ls_sheet>-value = 'LPP'.
          ENDIF.
        WHEN 4. "'FAZ_QUANTIDADE_PLANTAS'.
          lv_integer = lv_total_plants.
          gv_plantas = <ls_sheet>-value = lv_integer.
        WHEN 5. "1:'FAZ_QTD_REG_CENTRO'.
          READ TABLE lt_farm_subtot INTO ls_farm_subtot
            WITH KEY region = '1' BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_integer = ls_farm_subtot-qtd_plants.
            gv_centro = <ls_sheet>-value = lv_integer.
          ENDIF.
        WHEN 6. "2:'FAZ_QTD_REG_NORTE'.
          READ TABLE lt_farm_subtot INTO ls_farm_subtot
            WITH KEY region = '2' BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_integer = ls_farm_subtot-qtd_plants.
            gv_norte = <ls_sheet>-value = lv_integer.
          ENDIF.
        WHEN 7. "3:'FAZ_QTD_REG_SUL'.
          READ TABLE lt_farm_subtot INTO ls_farm_subtot
            WITH KEY region = '3' BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_integer = ls_farm_subtot-qtd_plants.
            gv_sul = <ls_sheet>-value = lv_integer.
          ENDIF.
        WHEN 8. "4:'FAZ_QTD_REG_EX_SUL'.
          READ TABLE lt_farm_subtot INTO ls_farm_subtot
            WITH KEY region = '4' BINARY SEARCH.
          IF sy-subrc EQ 0.
            lv_integer = ls_farm_subtot-qtd_plants.
            gv_exsul = <ls_sheet>-value = lv_integer.
          ENDIF.
      ENDCASE.
      CONDENSE: <ls_sheet>-row,
                <ls_sheet>-column,
                <ls_sheet>-value.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MEASUREMENT_DOC_CREATE
*&---------------------------------------------------------------------*
FORM measurement_doc_create USING lt_sheet
                                  TYPE /agri/t_excel_sheet.

***** This is the Table where the Notes can be buffered.
  DATA: lt_notes_buffer    TYPE tty_notes_buffer,
        lw_notes_buffer    TYPE ty_notes_buffer,
        lt_notes           TYPE /agri/t_gnote,
        lw_notes           TYPE /agri/s_gnote,
        lt_notes_buffer_db LIKE TABLE OF zabs_notes_buff WITH HEADER LINE,
        lw_lines           TYPE LINE OF /agri/t_gline,
        ls_message         TYPE /agri/s_gprolog,
        lw_notes_line(80)  TYPE c.

***** Required for holding the text of the Note Type that is entered.
  DATA : BEGIN OF lt_notetype_text OCCURS 0,
           notyp LIKE /agri/tgntdt-notyp,
           descr LIKE /agri/tgntdt-descr,
         END OF lt_notetype_text.

  DATA: lt_active_note(80) OCCURS 0,
        ls_notes           TYPE /agri/s_gnote,
        ref_upload         TYPE REF TO zcl_abs_glupload_master_data,
        lv_region          TYPE zabs_del_reg,
        lv_objtyp          TYPE tojtb-name,
        lv_objkey          TYPE /agri/gnotes-object.

  PERFORM prepare_notes CHANGING lt_notes_buffer.
  gt_notes_buffer[] = lt_notes_buffer[].

  CREATE OBJECT ref_upload.
  ref_upload->measurement_document_create( EXPORTING it_table = lt_sheet
                                           IMPORTING et_messages = gt_message ).
  LOOP AT lt_notes_buffer INTO lw_notes_buffer.

    MOVE-CORRESPONDING lw_notes_buffer TO lt_notes_buffer_db.
    lt_notes_buffer_db-approval_step = 1.
    lt_notes[] = lw_notes_buffer-notes[].
    LOOP AT lt_notes INTO lw_notes.

      lt_notes_buffer_db-notyp = lw_notes-notyp.
      lt_notes_buffer_db-aenam = lw_notes-aenam.
      lt_notes_buffer_db-aedat = lw_notes-aedat.
      lt_notes_buffer_db-aezet = lw_notes-aezet.
      lt_notes_buffer_db-updkz = lw_notes-updkz.
      lt_notes_buffer_db-contr_total =  lw_notes-contr.
      LOOP AT lw_notes-line INTO lw_notes_line.
        ADD 1 TO lt_notes_buffer_db-contr.
        MOVE lw_notes_line TO lt_notes_buffer_db-line.
        APPEND lt_notes_buffer_db.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  IF gt_message[] IS NOT INITIAL.
    READ TABLE gt_message INTO ls_message
      WITH KEY msgid = '/AGRI/GLMD'
               msgno = '006'
               msgty = 'S'.
    IF sy-subrc EQ 0
    AND ls_message-msgv1 IS NOT INITIAL.
      lt_notes_buffer_db-batch =  ls_message-msgv1.
      MODIFY lt_notes_buffer_db TRANSPORTING batch WHERE batch IS INITIAL .
      INSERT zabs_notes_buff FROM TABLE lt_notes_buffer_db .
    ENDIF.
  ENDIF.

ENDFORM.                    " MEASUREMENT_DOC_CREATE

*&---------------------------------------------------------------------*
*& Form PREPARE_NOTES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_NOTES_BUFFER
*&---------------------------------------------------------------------*
FORM prepare_notes CHANGING lt_notes_buffer TYPE tty_notes_buffer.

***** Required for holding the text of the Note Type that is entered.
  DATA : BEGIN OF lt_notetype_text OCCURS 0,
           notyp LIKE /agri/tgntdt-notyp,
           descr LIKE /agri/tgntdt-descr,
         END OF lt_notetype_text.

  DATA: lt_notes           TYPE /agri/t_gnote,
        lt_collect         LIKE gt_farm_subtot,
        ls_collect         LIKE LINE OF lt_collect,
        lt_active_note(80) OCCURS 0,
        ls_notes           TYPE /agri/s_gnote,
        lv_qtde            TYPE zabs_del_plant,
        lv_nodec(20)       TYPE n,
        lv_sub             TYPE string,
        lv_first           TYPE c,
        lv_total           TYPE zabs_del_plant,
        lv_farm            TYPE zabs_del_farm,
        lv_region          TYPE zabs_del_reg,
        lv_objtyp          TYPE tojtb-name,
        lv_objkey          TYPE /agri/gnotes-object.

  CONSTANTS: lc_cell_ini(3) TYPE c VALUE '0#c',
             lc_cell_fin(3) TYPE c VALUE '1#c',
             lc_nl(3)       TYPE c VALUE '0#n',
             lc_p_ini(3)    TYPE c VALUE '0#p',
             lc_p_fin(3)    TYPE c VALUE '1#p',
             lc_uline(80)   TYPE c VALUE  '______________________________________'.

  SELECT SINGLE *
    FROM /agri/tgntd
    INTO @DATA(ls_tgntd)
   WHERE notyp = 'ZMDC'.

  IF sy-subrc EQ 0.
    ls_notes-notyp = ls_tgntd-notyp.
    ls_notes-aenam = sy-uname.
    ls_notes-aedat = sy-datum.
    ls_notes-aezet = sy-uzeit.
    ls_notes-updkz = c_updkz_new.

    SELECT SINGLE *
      INTO @DATA(ls_notetype_text)
      FROM /agri/tgntdt
     WHERE spras EQ @sy-langu
       AND notyp EQ @ls_tgntd-notyp.
    IF sy-subrc EQ 0.
      ls_notes-descr = ls_notetype_text-descr.
    ENDIF.

*********************************
    INSERT INITIAL LINE INTO TABLE lt_active_note
      ASSIGNING FIELD-SYMBOL(<ls_active_note_hdr>).
    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = '<style> #tab1 {border:blank;width:600;} #p1{border:2px;border-style:solid;'.
      <ls_active_note_hdr> = '<style> #tab1 {border:blank;width:600;} #thd{border: 2px solid;} : </style> '.
    ENDIF.

*    INSERT INITIAL LINE INTO TABLE lt_active_note ASSIGNING <ls_active_note_hdr>.
*    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = '#p1  { border:2px; border-style:solid; border-color:#000000;  } '.
*    ENDIF.

*    INSERT INITIAL LINE INTO TABLE lt_active_note ASSIGNING <ls_active_note_hdr>.
*    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = 'border-color:#000000;} #thd{border: 2px solid;} : </style>  '.
*    ENDIF.

    INSERT INITIAL LINE INTO TABLE lt_active_note ASSIGNING <ls_active_note_hdr>.
    IF sy-subrc EQ 0.
      <ls_active_note_hdr> = '<table id="tab1"> '.
    ENDIF.


*    INSERT INITIAL LINE INTO TABLE lt_active_note ASSIGNING <ls_active_note_hdr>.
*    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = '<tr><th colspan="3" width="100%">REGIONAL</th></tr> <tr id="thd">'.
*    ENDIF.
*    INSERT INITIAL LINE INTO TABLE lt_active_note  ASSIGNING <ls_active_note_hdr>.
*    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = '<th id="thd">Fazenda</th> <th id="thd">Qtd</th> <th id="thd">Motivo</th>'.
*    ENDIF.
*    INSERT INITIAL LINE INTO TABLE lt_active_note  ASSIGNING <ls_active_note_hdr>.
*    IF sy-subrc EQ 0.
*      <ls_active_note_hdr> = '<th id="thd">Qtd</th> </tr>'.
*    ENDIF.
**********************************
    CLEAR lv_first.
    LOOP AT gt_terrain_dtls INTO DATA(ls_terrain_dtls).
      IF ls_terrain_dtls-pltxt IS NOT INITIAL.
        READ TABLE gt_farm_subtot INTO DATA(ls_farm_subtot)
          WITH KEY region = ls_terrain_dtls-region.
        IF sy-subrc NE 0.
          CLEAR ls_farm_subtot.
        ELSE.
          IF lv_region NE ls_terrain_dtls-region.
            IF lt_active_note[] IS NOT INITIAL AND lv_first IS NOT INITIAL.
              INSERT INITIAL LINE INTO TABLE lt_active_note
                ASSIGNING FIELD-SYMBOL(<ls_blank_line>).
              <ls_blank_line> =  lc_uline.

              CLEAR lv_qtde. lv_sub = '<b>TOTAL MOTIVO</b>'. lv_first = 'X'.
              LOOP AT gt_farm_subtot INTO DATA(ls_farm_subtot_old) WHERE region = lv_region.

                INSERT INITIAL LINE INTO TABLE lt_active_note
                  ASSIGNING <ls_blank_line>.
                IF sy-subrc EQ 0.
                  WRITE ls_farm_subtot_old-qtd_plants TO lv_nodec DECIMALS 0.
                  SHIFT lv_nodec LEFT DELETING LEADING '0'.
                  <ls_blank_line> = lv_sub &&
                  lc_cell_ini && ls_farm_subtot_old-rsdesc && lc_cell_fin &&
                  lc_cell_ini &&  lv_nodec && lc_cell_fin.
                  lv_qtde = lv_qtde + ls_farm_subtot_old-qtd_plants.
                  lv_sub = '.'.
                ENDIF.

              ENDLOOP.
              IF lv_qtde IS NOT INITIAL.

                WRITE lv_qtde TO lv_nodec DECIMALS 0.
                SHIFT lv_nodec LEFT DELETING LEADING '0'.


                INSERT INITIAL LINE INTO TABLE lt_active_note
                  ASSIGNING <ls_blank_line>.
                IF sy-subrc EQ 0.
                  <ls_blank_line> = '<b>TOTAL</b>' &&
                  lc_cell_ini && 'Total' && lc_cell_fin &&
                  lc_cell_ini && lv_nodec && lc_cell_fin.
                ENDIF.
              ENDIF.

              INSERT INITIAL LINE INTO TABLE lt_active_note
                ASSIGNING <ls_blank_line>.

              INSERT INITIAL LINE INTO TABLE lt_active_note
                ASSIGNING <ls_blank_line>.

              INSERT INITIAL LINE INTO TABLE lt_active_note
                ASSIGNING <ls_blank_line>.


            ENDIF.
            INSERT INITIAL LINE INTO TABLE lt_active_note
              ASSIGNING FIELD-SYMBOL(<ls_active_note>).
            IF sy-subrc EQ 0.
*              TRANSLATE ls_farm_subtot-reg_txt TO UPPER CASE.
*              <ls_active_note> = 'REGIONAL:%' && ls_farm_subtot-reg_txt.
              <ls_active_note> = '<b>REGIONAL:' && ls_farm_subtot-reg_txt && '</b>'.
              TRANSLATE <ls_active_note> USING '% '.

              INSERT INITIAL LINE INTO TABLE lt_active_note
                ASSIGNING <ls_blank_line>.
              <ls_blank_line> =  lc_uline.

              INSERT INITIAL LINE INTO TABLE lt_active_note
               ASSIGNING <ls_active_note>.
              IF sy-subrc EQ 0.
                <ls_active_note> = '<b>FAZENDA</b>' &&
                                   lc_cell_ini && '<b>MOTIVO</b>' && lc_cell_fin &&
                                   lc_cell_ini && '<b>QTD.PLANTAS</b>' && lc_cell_fin.
                TRANSLATE <ls_active_note> USING '% '.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

*        READ TABLE gt_terrain_dtls INTO DATA(ls_subtotal)
*          WITH KEY region  = ls_terrain_dtls-region
*                   farm    = ls_terrain_dtls-farm
*                   sub_tot = 'Subtotal'.
*        IF sy-subrc NE 0.
*          CLEAR ls_subtotal.
*        ENDIF.
*
*        IF ls_terrain_dtls-farm NE lv_farm.
        INSERT INITIAL LINE INTO TABLE lt_active_note
          ASSIGNING <ls_active_note>.
        IF sy-subrc EQ 0.
*            lv_qtde = ls_subtotal-qtd_plants.
*            <ls_active_note> = ls_terrain_dtls-pltxt && '%-%Total:%'
*                               && ls_subtotal-qtd_plants.
*            <ls_active_note> = ls_terrain_dtls-pltxt && '%-%Total:%'
*                               && lv_qtde.

          WRITE ls_terrain_dtls-qtd_plants TO lv_nodec DECIMALS 0.
          SHIFT lv_nodec LEFT DELETING LEADING '0'.
          <ls_active_note> = ls_terrain_dtls-pltxt &&
                             lc_cell_ini && ls_terrain_dtls-rsdesc && lc_cell_fin &&
                             lc_cell_ini && lv_nodec  && lc_cell_fin.
          TRANSLATE <ls_active_note> USING '% '.
        ENDIF.
*        ENDIF.


*        INSERT INITIAL LINE INTO TABLE lt_active_note
*          ASSIGNING <ls_active_note>.
*        IF sy-subrc EQ 0.
*          <ls_active_note> = 'Subtotal%Motivo'.
*          TRANSLATE <ls_active_note> USING '% '.
*        ENDIF.




*        INSERT INITIAL LINE INTO TABLE lt_active_note
*          ASSIGNING <ls_active_note>.
*        IF sy-subrc EQ 0.
*          lv_qtde = ls_terrain_dtls-qtd_plants.
**          <ls_active_note> = '-%' && ls_terrain_dtls-reason && '%-%Subtotal:%'
**                             && ls_terrain_dtls-qtd_plants.
*          <ls_active_note> = '-%' && ls_terrain_dtls-reason && '%-%Subtotal:%'
*                             && lv_qtde.
*          TRANSLATE <ls_active_note> USING '% '.
*        ENDIF.
      ENDIF.
      lv_farm = ls_terrain_dtls-farm.
      lv_region = ls_terrain_dtls-region.
      lv_first = 'X'.
    ENDLOOP.

    IF lt_active_note[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lt_active_note
        ASSIGNING <ls_blank_line>.
      <ls_blank_line> =  lc_uline.

      CLEAR lv_qtde. lv_sub = '<b>TOTAL MOTIVO</b>'.
      LOOP AT gt_farm_subtot INTO ls_farm_subtot WHERE region = lv_region.

        INSERT INITIAL LINE INTO TABLE lt_active_note
          ASSIGNING <ls_blank_line>.
        IF sy-subrc EQ 0.

          WRITE ls_farm_subtot-qtd_plants TO lv_nodec DECIMALS 0.
          SHIFT lv_nodec LEFT DELETING LEADING '0'.
          <ls_blank_line> = lv_sub &&
          lc_cell_ini && ls_farm_subtot-rsdesc && lc_cell_fin &&
          lc_cell_ini && lv_nodec && lc_cell_fin.
          lv_qtde = lv_qtde + ls_farm_subtot-qtd_plants.
          lv_sub = '.'.
        ENDIF.

      ENDLOOP.
      IF lv_qtde IS NOT INITIAL.


        WRITE lv_qtde TO lv_nodec DECIMALS 0.
        SHIFT lv_nodec LEFT DELETING LEADING '0'.

        INSERT INITIAL LINE INTO TABLE lt_active_note
          ASSIGNING <ls_blank_line>.
        IF sy-subrc EQ 0.
          <ls_blank_line> = '<b>TOTAL</b>' &&
          lc_cell_ini && 'Total' && lc_cell_fin &&
          lc_cell_ini && lv_nodec && lc_cell_fin.
        ENDIF.

        INSERT INITIAL LINE INTO TABLE lt_active_note
         ASSIGNING <ls_blank_line>.

      ENDIF.
      LOOP AT gt_farm_subtot INTO ls_farm_subtot.
        ls_collect-reason = ls_farm_subtot-reason.
        ls_collect-rsdesc = ls_farm_subtot-rsdesc.
        ls_collect-qtd_plants = ls_farm_subtot-qtd_plants.
        ADD ls_farm_subtot-qtd_plants TO lv_total.
        COLLECT ls_collect INTO lt_collect.
      ENDLOOP.

      CLEAR lv_total.
      LOOP AT lt_collect INTO ls_collect.
*        IF sy-tabix EQ 1.
*          INSERT INITIAL LINE INTO TABLE lt_active_note
*            ASSIGNING <ls_active_note>.
*          INSERT INITIAL LINE INTO TABLE lt_active_note
*            ASSIGNING <ls_active_note>.
*          IF sy-subrc EQ 0.
*            lv_qtde = lv_total.
*            <ls_active_note> = 'Total:' && lv_qtde. "lv_total.
*          ENDIF.
*        ENDIF.

        IF sy-tabix EQ 1.

          INSERT INITIAL LINE INTO TABLE lt_active_note
            ASSIGNING <ls_active_note>.


          INSERT INITIAL LINE INTO TABLE lt_active_note
            ASSIGNING <ls_active_note>.
          IF sy-subrc EQ 0.
            <ls_active_note> = '<b>TOTAL MOTIVO – AGRICOLA</b>'.
          ENDIF.

          INSERT INITIAL LINE INTO TABLE lt_active_note
            ASSIGNING <ls_active_note>.
          IF sy-subrc EQ 0.
            <ls_active_note> = lc_uline.
          ENDIF.

        ENDIF.

        WRITE ls_collect-qtd_plants TO lv_nodec DECIMALS 0.
        SHIFT lv_nodec LEFT DELETING LEADING '0'.

        lv_total = lv_total + ls_collect-qtd_plants.



        INSERT INITIAL LINE INTO TABLE lt_active_note
          ASSIGNING <ls_active_note>.
        IF sy-subrc EQ 0.


          <ls_active_note> = ls_collect-reason &&
                             lc_cell_ini && 'Subtotal' && lc_cell_fin &&
                             lc_cell_ini && lv_nodec && lc_cell_fin.

*                             '-Subtotal:' &&
*                             lv_qtde. "ls_collect-qtd_plants.

        ENDIF.
      ENDLOOP.
      IF lv_total IS NOT INITIAL.


        WRITE lv_total TO lv_nodec DECIMALS 0.
        SHIFT lv_nodec LEFT DELETING LEADING '0'.

        INSERT INITIAL LINE INTO TABLE lt_active_note
          ASSIGNING <ls_active_note>.
        IF sy-subrc EQ 0.
          lv_qtde = lv_total.
          <ls_active_note> = '<b>' && 'TOTAL:' && '</b>' &&
                             lc_cell_ini && 'Total' && lc_cell_fin &&
                             lc_cell_ini && lv_nodec && lc_cell_fin.
        ENDIF.
      ENDIF.


      DATA(lv_count) = lines( lt_active_note ).
      ls_notes-contr = lv_count.
      ls_notes-line  = lt_active_note[].

      INSERT INITIAL LINE INTO TABLE lt_notes_buffer
        ASSIGNING FIELD-SYMBOL(<ls_notes_buffer>).
      IF sy-subrc EQ 0.
        <ls_notes_buffer>-objtyp = '/AGRI/GLMD'.
        <ls_notes_buffer>-objkey = 'Nv.'.
        <ls_notes_buffer>-subscreen = '0903'.
        ls_notes-posnr =  <ls_notes_buffer>-posnr + 1.
        <ls_notes_buffer>-posnr = ls_notes-posnr.
        INSERT ls_notes INTO <ls_notes_buffer>-notes INDEX 1.
        <ls_notes_buffer>-changed = c_true.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
