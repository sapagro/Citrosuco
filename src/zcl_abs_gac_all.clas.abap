class ZCL_ABS_GAC_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_GAC_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_GAC_ALL IMPLEMENTATION.


METHOD /agri/if_ex_badi_gac_all~after_save.
ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~attachments_data_modify.
ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~attachment_add.

  DATA: gt_att_content_hex  TYPE solix_tab,
        lt_attachment       TYPE /agri/t_gacatta,
        lt_binary_tab_solix TYPE solix_tab,
        lt_binary_tab       TYPE TABLE OF bapiconten,
        lrt_region          TYPE RANGE OF /agri/glflatv-atwrt,
        wt_attachment       TYPE LINE OF  /agri/t_gacatta,
        lt_dataref          TYPE /agri/t_gdataref,
        lr_region           TYPE RANGE OF /agri/glflatv-atwrt,
        wt_glmdhdr          TYPE /agri/glmdhdr,
        lwa_attachment      TYPE /agri/s_gacatta,
        lwa_glmdhdr         TYPE /agri/glmdhdr,
        lv_subrc            TYPE sy-subrc,
        lv_per              TYPE char7,
        lv_length           TYPE i.

  FIELD-SYMBOLS : <fs_dataref>       TYPE LINE OF  /agri/t_gdataref,
                  <fs_dataref_value> TYPE any.

  IF ( ( is_achdr-actmp EQ 'ZTPM_ZCONT' OR
         is_achdr-actmp EQ 'ZTPM_ZLAUD' )
  AND is_achdr-objtyp EQ '/AGRI/GLMD' ).
    READ TABLE lt_dataref ASSIGNING <fs_dataref> WITH KEY name = '/AGRI/GLMDHDR'.
    IF sy-subrc = 0.
      ASSIGN <fs_dataref>-value->* TO <fs_dataref_value>.
      IF sy-subrc EQ 0.
        wt_glmdhdr  = <fs_dataref_value>.
      ENDIF..
    ENDIF.

    lv_per = wt_glmdhdr-mdate+4(2) && '/' && wt_glmdhdr-mdate(4).

    SUBMIT zabs_rep_terrain_details
     WITH p_per    EQ lv_per
     WITH s_region IN lr_region
     WITH rb_lep   EQ 'X'
     WITH rb_lpp   EQ ''
     WITH p_print  EQ abap_false
     AND RETURN.

    IMPORT gt_att_content_hex TO gt_att_content_hex FROM MEMORY ID 'ZVTX_ATT_HEX'.
    FREE MEMORY ID 'ZVTX_ATT_HEX'.

    CHECK gt_att_content_hex IS NOT INITIAL.

    wt_attachment-data = gt_att_content_hex.
    wt_attachment-attyp = 'PDF'.

    APPEND wt_attachment TO ct_attachment.
    CLEAR wt_attachment.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~before_save.

  DATA: BEGIN OF ls_cabn,
          atnam1  TYPE atnam VALUE 'FAZ_SAFRA',
          atnam2  TYPE atnam VALUE 'FAZ_PERIODO',
          atnam3  TYPE atnam VALUE 'FAZ_LAUDO',
          atnam4  TYPE atnam VALUE 'FAZ_QUANTIDADE_PLANTAS',
          atnam5  TYPE atnam VALUE 'FAZ_QTD_REG_CENTRO',
          atnam6  TYPE atnam VALUE 'FAZ_QTD_REG_NORTE',
          atnam7  TYPE atnam VALUE 'FAZ_QTD_REG_SUL',
          atnam8  TYPE atnam VALUE 'FAZ_QTD_REG_EX_SUL',
          atnam9  TYPE atnam VALUE 'FAZ_VAR_MP_TECNICA',
          atnam10 TYPE atnam VALUE 'CIT-PORTA-ENXERTO',
          atnam11 TYPE atnam VALUE 'CIT-ESPACAMENTO-RUA',
          atnam12 TYPE atnam VALUE 'CIT-ESPACAMENTO-PES',
          atnam13 TYPE atnam VALUE 'FAZ_QUANTIDADE_PLANTAS',
          atnam14 TYPE atnam VALUE 'FAZ_AREA_TALHAO',
          atnam15 TYPE atnam VALUE 'FAZ_FIM_MP_PLANTIO',
          atnam16 TYPE atnam VALUE 'FAZ_PREV_ERRADICACAO',
*-- BOC-T_T.KONNO-07.12.21
          atnam17 TYPE atnam VALUE 'FAZ_CAL_PREV',
*-- EOC-T_T.KONNO-07.12.21
          atinn1  TYPE atinn,
          atinn2  TYPE atinn,
          atinn3  TYPE atinn,
          atinn4  TYPE atinn,
          atinn5  TYPE atinn,
          atinn6  TYPE atinn,
          atinn7  TYPE atinn,
          atinn8  TYPE atinn,
          atinn9  TYPE atinn,
          atinn10 TYPE atinn,
          atinn11 TYPE atinn,
          atinn12 TYPE atinn,
          atinn13 TYPE atinn,
          atinn14 TYPE atinn,
          atinn15 TYPE atinn,
          atinn16 TYPE atinn,
*-- BOC-T_T.KONNO-07.12.21
          atinn17 TYPE atinn,
*-- EOC-T_T.KONNO-07.12.21
        END OF ls_cabn.

  DATA: lv_off       TYPE i,
        lv_int4      TYPE int4,
        lv_char10    TYPE char10,
        lv_label     TYPE string,
        lv_index     TYPE char20,
        lv_search    TYPE string,
        lv_text      TYPE string,
        lv_link      TYPE string,
        lv_fieldname TYPE fieldname,
        lv_date      TYPE char10,
        lv_first_pos TYPE i,
        lv_last_pos  TYPE i,
        lv_length    TYPE i,
        lv_chars     TYPE i,
        lv_len       TYPE i.

  DATA: lt_servlist        TYPE TABLE OF icm_sinfo,
        lrt_atinn          TYPE RANGE OF atinn,
        lt_zabs_notes_buff TYPE TABLE OF zabs_notes_buff,
        ls_zabs_notes_buff LIKE LINE OF lt_zabs_notes_buff,
        lt_notes_line      TYPE /agri/t_gline,
        lw_notes_line      TYPE LINE OF /agri/t_gline,
        lw_notes_lines     LIKE TABLE OF lw_notes_line,
        lt_notes           TYPE /agri/t_gnote,
        lw_notes           TYPE /agri/s_gnote,
        lref_date          TYPE REF TO data,
        lv_glmdhdr_mdocm   TYPE /agri/glmdhdr-mdocm,
        lv_approval_step   LIKE ls_zabs_notes_buff-approval_step,
        lv_step_txt(60)    TYPE c,
*-- BOC-T_T.KONNO-07.12.21
*        lv_char            TYPE c,
        lv_char(2)         TYPE c,
*-- EOC-T_T.KONNO-07.12.21
        lv_menge           TYPE menge_d,
        lv_tabix           LIKE sy-subrc.

  TYPES: BEGIN OF ty_notes_buffer,
           objtyp    TYPE tojtb-name,
           objkey    TYPE /agri/gnotes-object,
           subobject TYPE /agri/gnotes-subobject,
           posnr     TYPE numc06,
           notes     TYPE /agri/t_gnote,
           subscreen TYPE sy-dynnr,
           changed   TYPE abap_bool,
         END OF ty_notes_buffer,

         BEGIN OF ty_estep_buffer,
           approval_step_ori TYPE zabs_notes_buff-batch,
           approval_step     TYPE zabs_notes_buff-approval_step,
         END OF ty_estep_buffer,

         tt_notes_buffer TYPE TABLE OF ty_notes_buffer.

  DATA: lt_cst_notes_buffer TYPE tt_notes_buffer,
        lw_approval_step    TYPE ty_estep_buffer.

  FIELD-SYMBOLS: <lt_cst_notes_buffer> TYPE tt_notes_buffer,
                 <ls_mddoc_infocus>    TYPE /agri/s_glmd_doc,
                 <ls_attachment>       TYPE /agri/t_gacatta,
                 <lv_date>             TYPE any.

  CONSTANTS: lc_cell_ini(3)  TYPE c VALUE '0#c',
             lc_cell_fin(3)  TYPE c VALUE '1#c',
             lc_p_ini(3)     TYPE c VALUE '0#p',
             lc_p_fin(3)     TYPE c VALUE '1#p',
             lc_nl(3)        TYPE c VALUE '0#n',
             lc_nl_htm       TYPE string VALUE '<tr></tr>>',
             lc_p_ini_htm    TYPE string VALUE '<p id="p1">',
             lc_p_fin_htm    TYPE string VALUE '</p>',
             lc_cell_ini_htm TYPE string VALUE '<td id="thd">',
             lc_cell_fin_htm TYPE string VALUE '</td>'.

  ASSIGN ('(/AGRI/SAPLGNOTES)GT_NOTES_BUFFER[]') TO <lt_cst_notes_buffer>.
  ASSIGN ('(/AGRI/SAPLGLMDM)GS_MDDOC_INFOCUS') TO <ls_mddoc_infocus>.

  SELECT SINGLE *
    FROM /agri/glmdhdr
    INTO @DATA(ls_glmdhdr)
   WHERE mdocm = @cs_xachdr-objkey.

  IF sy-subrc <> 0.
    lv_glmdhdr_mdocm = cs_xachdr-objkey.
    SHIFT lv_glmdhdr_mdocm LEFT DELETING LEADING '0'.
  ELSE.
    lv_glmdhdr_mdocm = ls_glmdhdr-mdocm.
    SHIFT lv_glmdhdr_mdocm LEFT DELETING LEADING '0'.
  ENDIF.

  IF <lt_cst_notes_buffer> IS NOT ASSIGNED
  OR <lt_cst_notes_buffer> IS INITIAL.
    SELECT *
      FROM zabs_notes_buff
      INTO TABLE lt_zabs_notes_buff
     WHERE batch = lv_glmdhdr_mdocm.

    SORT lt_zabs_notes_buff BY batch contr.

    LOOP AT lt_zabs_notes_buff INTO ls_zabs_notes_buff.
      lv_tabix = sy-tabix.

      IF lv_tabix = 1.
        INSERT INITIAL LINE INTO TABLE  lt_cst_notes_buffer
          ASSIGNING FIELD-SYMBOL(<ls_active_note>).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING  ls_zabs_notes_buff TO <ls_active_note>.
        ENDIF.
      ENDIF.

      lw_notes-contr = ls_zabs_notes_buff-contr_total.
      lw_notes_line = ls_zabs_notes_buff-line.
      APPEND lw_notes_line TO lt_notes_line.

      IF lv_tabix = lw_notes-contr.
        lw_notes-line = lt_notes_line.
        APPEND lw_notes TO lt_notes.
        <ls_active_note>-notes = lt_notes.
        REFRESH lt_notes.

        LOOP AT ct_xactxt ASSIGNING  FIELD-SYMBOL(<ls_xactxt>).
          FIND FIRST OCCURRENCE OF REGEX 'Status:'
        IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
          lv_step_txt =  <ls_xactxt>-text+lv_off(14).
          lv_len = strlen( lv_step_txt ).
          lv_tabix = 0.

          DO lv_len TIMES.
            lv_char = lv_step_txt+lv_tabix(1).
            IF lv_char NA '0123456789'.
              lv_step_txt+lv_tabix(1) = ' '.
            ENDIF.
            lv_tabix = lv_tabix + 1.
          ENDDO.

          SHIFT lv_step_txt LEFT DELETING LEADING ' '.
          SHIFT lv_step_txt LEFT DELETING LEADING '0'.
          lw_approval_step-approval_step = lv_step_txt.
          CLEAR: lv_len, lv_char, lv_len.
          EXIT.
        ENDLOOP.

        lv_approval_step = lw_approval_step-approval_step.
        ls_zabs_notes_buff-approval_step  = lw_approval_step-approval_step + 1.
        ls_zabs_notes_buff-approval_step_ori = ls_zabs_notes_buff-approval_step.
        MODIFY lt_zabs_notes_buff FROM ls_zabs_notes_buff TRANSPORTING approval_step approval_step_ori
         WHERE batch = lv_glmdhdr_mdocm  .
        IF lv_approval_step >= 5 AND ct_xactxt[] IS NOT INITIAL.
          DELETE zabs_notes_buff FROM TABLE lt_zabs_notes_buff.
        ELSE.
          MODIFY zabs_notes_buff FROM TABLE lt_zabs_notes_buff.
        ENDIF.
      ENDIF.
    ENDLOOP.
    ASSIGN lt_cst_notes_buffer TO <lt_cst_notes_buffer>.
  ENDIF.

  IF  lv_approval_step IS INITIAL.
    lv_approval_step  = 1.
  ENDIF.

  IF cs_xachdr-objtyp EQ '/AGRI/GLMD'.
*-- BOC-T_T.KONNO-07.12.21
*    DO 16 TIMES.
    DO 17 TIMES.
*-- EOC-T_T.KONNO-07.12.21
      lv_char = sy-index.

*-- BOC-T_T.KONNO-07.12.21
      SHIFT lv_char LEFT DELETING LEADING '0'.
      SHIFT lv_char LEFT DELETING LEADING ' '.
*-- EOC-T_T.KONNO-07.12.21

      lv_fieldname = 'ATINN' && lv_char.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_cabn
        TO FIELD-SYMBOL(<lv_atinn>).
      IF sy-subrc EQ 0.
        lv_fieldname = 'ATNAM' && lv_char.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_cabn
          TO FIELD-SYMBOL(<lv_atnam>).
        IF sy-subrc EQ 0
        AND <lv_atnam> IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = <lv_atnam>
            IMPORTING
              output = <lv_atinn>.

          INSERT INITIAL LINE INTO TABLE lrt_atinn
            ASSIGNING FIELD-SYMBOL(<lrs_atinn>).
          IF sy-subrc EQ 0.
            <lrs_atinn> = 'IEQ'.
            <lrs_atinn>-low = <lv_atinn>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    SELECT atnam, atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
     WHERE atinn IN @lrt_atinn[].
    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam.
    ENDIF.

    IF cs_xachdr-actmp EQ 'ZTPM_ZPRV'.
      IF <ls_mddoc_infocus> IS ASSIGNED
*-- BOC-T_T.KONNO-07.12.21
      "AND <ls_mddoc_infocus>-x-mdhdr-mdtyp EQ 'ZPTA'
*-- EOC-T_T.KONNO-07.12.21
      AND <ls_mddoc_infocus>-x-mdhdr-mpgrp EQ 'FAZ_PREVISAO'.

*-- BOC-T_T.KONNO-07.12.21
*        IF <ls_mddoc_infocus>-x-mdhdr-ustat IS NOT INITIAL.
*          lv_approval_step = <ls_mddoc_infocus>-x-mdhdr-ustat+4.
*        ENDIF.
        IF  <ls_mddoc_infocus>-y-mdhdr-ustat IS NOT INITIAL. "sy-tcode = 'ZABS_APROVA_DOC'
          lv_approval_step = <ls_mddoc_infocus>-y-mdhdr-ustat+4.
          ADD 1 TO lv_approval_step.
        ENDIF.
        CALL FUNCTION 'ICM_GET_INFO'
          TABLES
            servlist = lt_servlist.
        IF sy-subrc = 0.
          READ TABLE lt_servlist INTO DATA(ls_serv_list) INDEX 1.
          IF sy-subrc = 0 AND lv_approval_step IS NOT INITIAL .
            lv_link = '</br><a href="http://' && ls_serv_list-hostname && ':' && ls_serv_list-service &&
            '/sap/bc/gui/sap/its/webgui?~transaction=*zabs_aprova_doc P_MDTYP=' && <ls_mddoc_infocus>-x-mdhdr-mdtyp && ';P_MPGRP=FAZ_PREVISAO;P_ETAPA=' && lv_approval_step
            && ';SO_MDOCM-LOW=' && cs_xachdr-objkey
            && '">Clique aqui para Revisar e Aprovar o Documento</a></br></br>'.
          ENDIF.
        ENDIF.
*-- EOC-T_T.KONNO-07.12.21

        LOOP AT ct_xactxt ASSIGNING <ls_xactxt>.
          lv_tabix = sy-tabix.
**-- Approval Link
          FIND FIRST OCCURRENCE OF REGEX 'LABEL_LINK'
            IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
          IF sy-subrc EQ 0.
            REPLACE 'LABEL_LINK'
              WITH lv_link INTO <ls_xactxt>-text.
          ENDIF.

*-- Variedade Técnica
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO DATA(ls_mdatv)
            WITH KEY atinn = ls_cabn-atinn9."'FAZ_VAR_MP_TECNICA'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atwrt IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_VARIEDADE'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_VARIEDADE'
                WITH ls_mdatv-atwrt INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_VARIEDADE'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_VARIEDADE'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Porta-Enxerto
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn10."'CIT-PORTA-ENXERTO'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atwrt IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_PORTAENXERTO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_PORTAENXERTO'
                WITH ls_mdatv-atwrt INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_PORTAENXERTO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_PORTAENXERTO'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Espaçamento entre Rua
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn11."'CIT-ESPACAMENTO-RUA'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_ESPRUA'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              REPLACE 'LABEL_ESPRUA'
                WITH lv_char10 INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_ESPRUA'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_ESPRUA'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Espaçamento entre Pés
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn12." 'CIT-ESPACAMENTO-PES'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_ESPPE'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              REPLACE 'LABEL_ESPPE'
                WITH lv_char10 INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_ESPPE'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_ESPPE'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Quantidade de Plantas
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn13."'FAZ_QUANTIDADE_PLANTAS'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_QTDPLANTAS'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              REPLACE 'LABEL_QTDPLANTAS'
                WITH lv_char10 INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_QTDPLANTAS'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_QTDPLANTAS'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Área do talhão
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn14."'FAZ_AREA_TALHAO'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_AREATALHAO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              REPLACE 'LABEL_AREATALHAO'
                WITH lv_char10 INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_AREATALHAO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_AREATALHAO'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Data de Plantio
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn15."'FAZ_FIM_MP_PLANTIO'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_DTAPLANTIO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              lv_date = lv_char10+6(2) && '.' && lv_char10+4(2) && '.' && lv_char10(4).
              REPLACE 'LABEL_DTAPLANTIO'
                WITH lv_date INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_DTAPLANTIO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_DTAPLANTIO'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Previsão de Erradicação
          CLEAR ls_mdatv.
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn16."'FAZ_PREV_ERRADICACAO'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atflv IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_DTAERRADICACAO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              MOVE ls_mdatv-atflv TO lv_int4.
              MOVE lv_int4 TO lv_char10.
              CONDENSE lv_char10.
              lv_date = lv_char10+6(2) && '.' && lv_char10+4(2) && '.' && lv_char10(4).
              REPLACE 'LABEL_DTAERRADICACAO'
                WITH lv_date INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_DTAERRADICACAO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_DTAERRADICACAO'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.
*-- Cancelamento de Erradicação
          READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
            WITH KEY atinn = ls_cabn-atinn17."'FAZ_CAL_PREV'.
          IF sy-subrc EQ 0
          AND ls_mdatv-atwrt IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_CANCELAMENTO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_CANCELAMENTO'
                WITH ls_mdatv-atwrt INTO <ls_xactxt>-text.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX 'LABEL_CANCELAMENTO'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'LABEL_CANCELAMENTO'
                WITH space INTO <ls_xactxt>-text.
            ENDIF.
          ENDIF.


        ENDLOOP.
      ENDIF.
    ELSEIF cs_xachdr-actmp EQ 'ZTPM_ZCONT'
        OR cs_xachdr-actmp EQ 'ZTPM_ZLAUD'.
      CALL FUNCTION 'ICM_GET_INFO'
        TABLES
          servlist = lt_servlist.
      IF sy-subrc = 0.
        READ TABLE lt_servlist INTO ls_serv_list INDEX 1.
        IF sy-subrc = 0 AND cs_xachdr-actmp EQ 'ZTPM_ZLAUD' AND lv_approval_step IS NOT INITIAL .
          lv_link = 'Plantio.</br></br><a href="http://' && ls_serv_list-hostname && ':' && ls_serv_list-service &&
          '/sap/bc/gui/sap/its/webgui?~transaction=*zabs_aprova_massa P_MDTYP=' && <ls_mddoc_infocus>-x-mdhdr-mdtyp && ';P_MPGRP=FAZ-LAUDO;P_ETAPA=' && lv_approval_step
          && ';SO_MDOCM-LOW=' && cs_xachdr-objkey
          && '">Clique aqui para Revisar e Aprovar o Documento</a></br></br>'.
        ENDIF.
      ENDIF.

      IF "( ( ls_glmdhdr-mdtyp EQ 'ZPTA' AND
             ls_glmdhdr-mpgrp EQ 'FAZ-LAUDO' " )
      OR ( <ls_mddoc_infocus> IS ASSIGNED AND
           "<ls_mddoc_infocus>-x-mdhdr-mdtyp EQ 'ZPTA' AND"
           <ls_mddoc_infocus>-x-mdhdr-mpgrp EQ 'FAZ-LAUDO' ). ").
        SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
               h~contr, h~cmnum, h~equnr, h~mpgrp,
               v~atzhl, v~atwrt, v~atflv,
               c~atinn, c~atnam
          INTO TABLE @DATA(lt_glmdhdr_join)
          FROM /agri/glmdhdr AS h
          INNER JOIN /agri/glmdatv AS v
          ON v~mdocm EQ h~mdocm
          INNER JOIN cabn AS c
          ON c~atinn EQ v~atinn
         WHERE h~mdocm = @ls_glmdhdr-mdocm.

        IF sy-subrc EQ 0.
          DATA(lv_memory) = abap_false.
        ELSE.
          lv_memory = abap_true.
        ENDIF.

        IF lv_memory EQ abap_false.
          LOOP AT ct_xactxt ASSIGNING <ls_xactxt>.
            lv_tabix = sy-tabix.

*-- Approval Link
            IF lv_link IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'Plantio'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'Plantio'
                  WITH lv_link INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Safra
            READ TABLE lt_glmdhdr_join INTO DATA(ls_atributo)
              WITH KEY atnam = 'FAZ_SAFRA'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SAFRA'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                MOVE ls_atributo-atflv TO lv_int4.
                MOVE lv_int4 TO lv_char10.
                CONDENSE lv_char10.
                REPLACE 'LABEL_SAFRA'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SAFRA'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_SAFRA'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Período
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_PERIODO'.
            IF sy-subrc EQ 0
            AND ls_atributo-atwrt IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PERIODO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PERIODO'
                  WITH ls_atributo-atwrt INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PERIODO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PERIODO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Tipo de Laudo
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_LAUDO'.
            IF sy-subrc EQ 0
            AND ls_atributo-atwrt IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_LAUDO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_LAUDO'
                  WITH ls_atributo-atwrt INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_LAUDO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_LAUDO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_QUANTIDADE_PLANTAS'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PLANTAS'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_atributo-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_atributo-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_PLANTAS'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PLANTAS'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PLANTAS'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Centro
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_QTD_REG_CENTRO'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_CENTRO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_atributo-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_atributo-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_CENTRO'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_CENTRO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_CENTRO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Norte
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_QTD_REG_NORTE'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_NORTE'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_atributo-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_atributo-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_NORTE'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_NORTE'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_NORTE'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Sul
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_QTD_REG_SUL'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_atributo-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_atributo-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_SUL'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_SUL'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Extremo Sul
            CLEAR ls_atributo.
            READ TABLE lt_glmdhdr_join INTO ls_atributo
              WITH KEY atnam = 'FAZ_QTD_REG_EX_SUL'.
            IF sy-subrc EQ 0
            AND ls_atributo-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_EXSUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_atributo-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_atributo-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_EXSUL'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_EXSUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_EXSUL'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.

            IF <lt_cst_notes_buffer> IS ASSIGNED
            AND <lt_cst_notes_buffer>[] IS NOT INITIAL.
              READ TABLE <lt_cst_notes_buffer>
                ASSIGNING FIELD-SYMBOL(<ls_custom_text>) INDEX 1.
              IF sy-subrc EQ 0.
                READ TABLE <ls_custom_text>-notes
                  ASSIGNING FIELD-SYMBOL(<ls_notes_text>) INDEX 1.
                IF sy-subrc EQ 0.
                  DATA(lv_lines) = <ls_notes_text>-contr.
                  DO 100 TIMES.
                    lv_index = sy-index.
                    lv_label = 'LABEL' && lv_index.
                    CONDENSE lv_label.
                    FIND FIRST OCCURRENCE OF REGEX lv_label
                      IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                    IF sy-subrc EQ 0.
                      READ TABLE <ls_notes_text>-line
                        ASSIGNING FIELD-SYMBOL(<ls_text_line>) INDEX lv_index.
                      IF sy-subrc EQ 0.
                        REPLACE lv_label
                          WITH <ls_text_line> INTO <ls_xactxt>-text.
                      ELSE.
                        REPLACE lv_label
                           WITH '' INTO <ls_xactxt>-text.
                      ENDIF.
                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDDO.

                  FIND FIRST OCCURRENCE OF REGEX lc_cell_ini
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_cell_ini IN <ls_xactxt>-text
                      WITH lc_cell_ini_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_cell_fin
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_cell_fin IN <ls_xactxt>-text
                      WITH lc_cell_fin_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_p_ini
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_p_ini IN <ls_xactxt>-text
                      WITH lc_p_ini_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_p_fin
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_p_fin IN <ls_xactxt>-text
                      WITH lc_p_fin_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_nl
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_nl IN <ls_xactxt>-text
                      WITH lc_nl_htm.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT ct_xactxt ASSIGNING <ls_xactxt>.
            lv_tabix = sy-tabix.
*-- Approval Link
            FIND FIRST OCCURRENCE OF REGEX 'Plantio'
              IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
            IF sy-subrc EQ 0.
              REPLACE 'Plantio'
                WITH lv_link INTO <ls_xactxt>-text.
            ENDIF.
*-- Safra
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn1. "'FAZ_SAFRA'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SAFRA'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                MOVE ls_mdatv-atflv TO lv_int4.
                MOVE lv_int4 TO lv_char10.
                CONDENSE lv_char10.
                REPLACE 'LABEL_SAFRA'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SAFRA'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_SAFRA'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Período
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn2. "'FAZ_PERIODO'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atwrt IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PERIODO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PERIODO'
                  WITH ls_mdatv-atwrt INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PERIODO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PERIODO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Tipo de Laudo
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn3. "'FAZ_LAUDO'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atwrt IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_LAUDO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_LAUDO'
                  WITH ls_mdatv-atwrt INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_LAUDO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_LAUDO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn4. "'FAZ_QUANTIDADE_PLANTAS'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PLANTAS'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_mdatv-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_mdatv-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_PLANTAS'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_PLANTAS'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_PLANTAS'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Centro
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn5. "'FAZ_QTD_REG_CENTRO'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_CENTRO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_mdatv-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_mdatv-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_CENTRO'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_CENTRO'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_CENTRO'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Norte
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn6. "'FAZ_QTD_REG_NORTE'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_NORTE'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_mdatv-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_mdatv-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_NORTE'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_NORTE'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_NORTE'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Sul
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn7. "'FAZ_QTD_REG_SUL'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_mdatv-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_mdatv-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_SUL'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_SUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_SUL'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.
*-- Qtde.Plantas Extremo Sul
            CLEAR ls_mdatv.
            READ TABLE <ls_mddoc_infocus>-x-mdatv INTO ls_mdatv
              WITH KEY atinn = ls_cabn-atinn8. "'FAZ_QTD_REG_EX_SUL'.
            IF sy-subrc EQ 0
            AND ls_mdatv-atflv IS NOT INITIAL.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_EXSUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
*-- BOC- T_T.KONNO-05.27.21
*                MOVE ls_mdatv-atflv TO lv_int4.
*                MOVE lv_int4 TO lv_char10.
*                CONDENSE lv_char10.
                CLEAR lv_char10.
                CALL FUNCTION 'ZABS_FM_CHANGE_FORMAT'
                  EXPORTING
                    iv_atflv     = ls_mdatv-atflv
                  IMPORTING
                    ev_formatted = lv_char10.
*-- EOC- T_T.KONNO-05.27.21
                REPLACE 'LABEL_EXSUL'
                  WITH lv_char10 INTO <ls_xactxt>-text.
              ENDIF.
            ELSE.
              FIND FIRST OCCURRENCE OF REGEX 'LABEL_EXSUL'
                IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
              IF sy-subrc EQ 0.
                REPLACE 'LABEL_EXSUL'
                  WITH space INTO <ls_xactxt>-text.
              ENDIF.
            ENDIF.

            IF <lt_cst_notes_buffer> IS ASSIGNED
            AND <lt_cst_notes_buffer>[] IS NOT INITIAL.
              READ TABLE <lt_cst_notes_buffer> ASSIGNING <ls_custom_text> INDEX 1.
              IF sy-subrc EQ 0.
                READ TABLE <ls_custom_text>-notes ASSIGNING <ls_notes_text> INDEX 1.
                IF sy-subrc EQ 0.
                  lv_lines = <ls_notes_text>-contr.
                  DO 100 TIMES.
                    lv_index = sy-index.
                    lv_label = 'LABEL' && lv_index.
                    CONDENSE lv_label. "NO-GAPS.
                    FIND FIRST OCCURRENCE OF REGEX lv_label
                      IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                    IF sy-subrc EQ 0.
                      READ TABLE <ls_notes_text>-line ASSIGNING <ls_text_line> INDEX lv_index.
                      IF sy-subrc EQ 0.
                        REPLACE lv_label
                          WITH <ls_text_line> INTO <ls_xactxt>-text.
                      ELSE.
                        REPLACE lv_label
                           WITH '' INTO <ls_xactxt>-text.
                      ENDIF.
                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDDO.

                  FIND FIRST OCCURRENCE OF REGEX lc_cell_ini
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_cell_ini IN <ls_xactxt>-text
                      WITH lc_cell_ini_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_cell_fin
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_cell_fin IN <ls_xactxt>-text
                      WITH lc_cell_fin_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_p_ini
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_p_ini IN <ls_xactxt>-text
                      WITH lc_p_ini_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_p_fin
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_p_fin IN <ls_xactxt>-text
                      WITH lc_p_fin_htm.
                  ENDIF.

                  FIND FIRST OCCURRENCE OF REGEX lc_nl
                    IN <ls_xactxt>-text MATCH OFFSET lv_off MATCH LENGTH lv_len.
                  IF sy-subrc EQ 0.
                    REPLACE ALL OCCURRENCES OF lc_nl IN <ls_xactxt>-text
                      WITH lc_nl_htm.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~custom_data_get.
ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~header_defaults_fill.
ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~processors_modify.
ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~processor_check.

  DATA: lv_stop TYPE xfeld.

  PERFORM f_get_susbst IN PROGRAM saplzabs_fg_md
                                   CHANGING lv_stop.
  IF lv_stop IS INITIAL.
    c_subrc = 0.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_gac_all~smartform_parameters_modify.
ENDMETHOD.
ENDCLASS.
