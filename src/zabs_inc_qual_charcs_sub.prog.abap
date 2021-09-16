*&---------------------------------------------------------------------*
*& Include          ZABS_REP_QUAL_CHARCS_SUB
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS cl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click.
  ENDMETHOD.

  "handle_hotspot_click
ENDCLASS.                    "cl_event_receiver IMPLEMENTATION

CLASS lcl_qual IMPLEMENTATION.

  METHOD : get_qual_charcs_data.

    DATA : lt_char_req        TYPE TABLE OF bapi2045d1,
           lt_char_res        TYPE TABLE OF bapi2045d2,
           lt_sample_res      TYPE TABLE OF bapi2045d3,
           lt_plpo_qm01       TYPE TABLE OF ty_plpo,
           lt_plpo_qm03       TYPE TABLE OF ty_plpo,
           lt_oper_list       TYPE TABLE OF bapi2045l2,
           lt_status          TYPE STANDARD TABLE OF bapi2045ss,
           lt_user_status     TYPE STANDARD TABLE OF bapi2045us,
           lrt_auart          TYPE RANGE OF aufart,
           ls_mic             TYPE ty_mic,
           ls_char_req        TYPE bapi2045d1,
           ls_char_res        TYPE bapi2045d2,
           ls_sample_res      TYPE bapi2045d3,
           ls_language        TYPE bapi2045la,
           ls_qual_char       TYPE zabs_str_qual_char,
           ls_operation       TYPE bapi2045l2,
           ls_insppoint       TYPE bapi2045d5,
           ls_return          TYPE bapiret2,
           lo_tqual_char_qm01 TYPE REF TO data,
           lo_tqual_char_qm03 TYPE REF TO data,
           lo_squal_char_qm01 TYPE REF TO data,
           lo_squal_char_qm03 TYPE REF TO data,
           lv_tabix           TYPE sy-tabix.

    REFRESH gt_oper.
*-----------------------------------------------------------------------
*-- Deleting task order where the status is equal to "TECO".
*    SELECT aufnr, tplnr_fl, erdat
*      FROM /agri/fmfphdr
*      INTO TABLE @DATA(lt_fmfphdr)
*     WHERE tplnr_fl IN @so_tplnr
*       AND erdat    IN @so_datum
*       AND tecom    EQ @space.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      INSERT INITIAL LINE INTO TABLE lrt_auart
        ASSIGNING FIELD-SYMBOL(<lrs_auart>).
      IF sy-subrc EQ 0.
        <lrs_auart> = 'IEQ'.
        CASE lv_index.
          WHEN 1.
            <lrs_auart>-low = 'ZG10'.
          WHEN 2.
            <lrs_auart>-low = 'ZP20'.
        ENDCASE.
      ENDIF.
    ENDDO.

    DATA lr_steus TYPE RANGE OF steus.
    APPEND VALUE #( low = 'QM01' sign = 'I' option = 'EQ' )
      TO lr_steus.
    APPEND VALUE #( low = 'QM02' sign = 'I' option = 'EQ' )
      TO lr_steus.
    APPEND VALUE #( low = 'QM03' sign = 'I' option = 'EQ' )
      TO lr_steus.

    SELECT aufnr, tplnr_fl, erdat
      FROM /agri/fmfphdr
      INTO TABLE @DATA(lt_fmfphdr)
     WHERE auart    IN @lrt_auart[]
       AND tplnr_fl IN @so_tplnr[]
       AND iwerk    IN @so_werks[]
       AND erdat    IN @so_datum[].

    IF lt_fmfphdr[] IS NOT INITIAL.
      SORT lt_fmfphdr BY aufnr.

      SELECT tplnr_fl,
             bukrs,
             iwerk
        FROM /agri/glflot
        INTO TABLE @gt_glflot
        FOR ALL ENTRIES IN @lt_fmfphdr[]
       WHERE tplnr_fl EQ @lt_fmfphdr-tplnr_fl.

      IF sy-subrc EQ 0.
        SORT gt_glflot BY tplnr_fl.
      ENDIF.

*-- Select For Plant and Material Details.
      SELECT prueflos, werk, selmatnr, aufnr
        FROM qals
        INTO TABLE @DATA(lt_qals)
        FOR ALL ENTRIES IN @lt_fmfphdr
       WHERE prueflos IN @so_ilot[]
         AND werk     IN @so_werks[]
         AND art      EQ @p_art
         AND selmatnr IN @so_matnr[]
         AND aufnr    EQ @lt_fmfphdr-aufnr.

      IF sy-subrc NE 0.
        MESSAGE TEXT-015 TYPE zcl_abs_abap_maintain=>c_msgty_info.
        LEAVE LIST-PROCESSING.
      ELSE.
        SORT lt_qals BY prueflos.
      ENDIF.

      ls_language-langu = sy-langu.

*-- Deleting records with inspection lot doesnt have status as "RREC"
      LOOP AT lt_qals INTO DATA(ls_qals).
        lv_index = sy-tabix.
        CALL FUNCTION 'BAPI_INSPLOT_GETSTATUS'
          EXPORTING
            number        = ls_qals-prueflos
            language      = ls_language
          TABLES
            system_status = lt_status
            user_status   = lt_user_status.

*-- Checking the lt_status has RREC or not.
        READ TABLE lt_status INTO DATA(ls_status)
          WITH KEY sy_st_text = TEXT-005."RESU
        IF sy-subrc NE 0.
          DELETE lt_qals INDEX lv_index.
        ENDIF.
      ENDLOOP.

      REFRESH gt_qapp_all.
      IF lt_qals[] IS NOT INITIAL.
*-- Select For Inspection Lots.
        SELECT prueflos, vorglfnr, ppsortkey,
               probenr, userc1, userc2, usern1,
               usern2, userd1, usert1, ersteldat,
               erstelzeit, aenderdat, aenderzeit
          FROM qapp
          INTO CORRESPONDING FIELDS OF TABLE @gt_qapp_all
          FOR ALL ENTRIES IN @lt_qals[]
         WHERE prueflos   EQ @lt_qals-prueflos
*-- Avaliação dos codes do catálogo de controle
*-- '': Não avaliado/A:Aceito(OK)/R:Rejeitado(não está OK)
*           AND vbewertung EQ 'A'
           AND vbewertung NE 'R'
           AND ersteldat  IN @so_erdat[].

        IF sy-subrc NE 0.
*-- Lotes de controle não encontrados
          MESSAGE TEXT-019 TYPE zcl_abs_abap_maintain=>c_msgty_info.
          LEAVE LIST-PROCESSING.
        ELSE.
          DATA(lt_prueflos) = gt_qapp_all[].
          SORT lt_prueflos BY prueflos.
          DELETE ADJACENT DUPLICATES FROM lt_prueflos COMPARING prueflos.
          DATA lrt_prueflos TYPE RANGE OF qplos.
          LOOP AT lt_prueflos INTO DATA(ls_prueflos).
            APPEND VALUE #( low = ls_prueflos-prueflos sign = 'I' option = 'EQ' )
              TO lrt_prueflos.
          ENDLOOP.
          DELETE lt_qals WHERE prueflos NOT IN lrt_prueflos[].

          IF lt_qals[] IS INITIAL.
*-- Lotes de controle não encontrados
            MESSAGE TEXT-019 TYPE zcl_abs_abap_maintain=>c_msgty_info.
            LEAVE LIST-PROCESSING.
          ENDIF.

          gt_last_created = CORRESPONDING #( gt_qapp_all ).
          gt_last_changed = CORRESPONDING #( gt_qapp_all ).
**Modify Start of commented below lines #INC0020624 On 19/09/2020 T_C.KARANAM
**              SORT gt_last_created BY prueflos   ASCENDING
**                                  ersteldat  DESCENDING
**                                  erstelzeit DESCENDING.
**Modify End of #INC0020624 On 19/09/2020 T_C.KARANAM

*-- BOC T_T.KONNO-17.02.21
***Modify Start of Added below lines #INC0020624 On 19/09/2020 T_C.KARANAM
*          SORT gt_last_created BY prueflos ASCENDING
*                                  probenr DESCENDING.
***Modify End of #INC0020624 On 19/09/2020 T_C.KARANAM
*
*          DELETE ADJACENT DUPLICATES FROM gt_last_created
*            COMPARING prueflos.
          SORT gt_last_created BY prueflos ASCENDING
                                  vorglfnr ASCENDING
                                  probenr  DESCENDING.

          DELETE ADJACENT DUPLICATES FROM gt_last_created
            COMPARING prueflos vorglfnr.
*-- EOC T_T.KONNO-17.02.21

**Modify Start of commented below lines #INC0020624 On 19/09/2020 T_C.KARANAM
**          SORT gt_last_changed BY prueflos   ASCENDING
**                                  aenderdat  DESCENDING
**                                  aenderzeit DESCENDING.
**
**          DELETE ADJACENT DUPLICATES FROM gt_last_changed
**            COMPARING prueflos.
**Modify End of commented lines #INC0020624 On 19/09/2020 T_C.KARANAM

          REFRESH gt_exception.
          LOOP AT gt_last_created ASSIGNING FIELD-SYMBOL(<ls_last_created>).
            READ TABLE gt_last_changed ASSIGNING FIELD-SYMBOL(<ls_last_changed>)
              WITH KEY prueflos = <ls_last_created>-prueflos.
            IF sy-subrc EQ 0.
              IF <ls_last_changed>-aenderdat IS NOT INITIAL.
                IF <ls_last_changed>-aenderdat GT <ls_last_created>-ersteldat.
                  <ls_last_created> = <ls_last_changed>.
                ELSEIF <ls_last_changed>-aenderdat EQ <ls_last_created>-ersteldat.
                  IF <ls_last_changed>-ersteldat EQ <ls_last_created>-ersteldat
                  AND <ls_last_changed>-aenderdat EQ <ls_last_created>-aenderdat
                  AND <ls_last_changed>-erstelzeit EQ <ls_last_created>-erstelzeit
                  AND <ls_last_changed>-aenderzeit EQ <ls_last_created>-aenderzeit
                  AND <ls_last_changed>-vorglfnr NE <ls_last_created>-vorglfnr.
                    <ls_last_created>-exception = abap_true.
                    INSERT INITIAL LINE INTO TABLE gt_exception
                      ASSIGNING FIELD-SYMBOL(<ls_exception>).
                    IF sy-subrc EQ 0.
                      <ls_exception> = <ls_last_changed>.
                      <ls_exception>-exception = abap_true.
                    ENDIF.
                  ELSE.
                    IF <ls_last_changed>-aenderzeit GT <ls_last_created>-erstelzeit.
                      <ls_last_created> = <ls_last_changed>.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          APPEND LINES OF gt_exception TO gt_last_created.

          SORT gt_last_created BY prueflos   ASCENDING
                                  ersteldat  DESCENDING
                                  erstelzeit DESCENDING.

          SORT gt_qapp_all BY prueflos vorglfnr probenr.
        ENDIF.

        SELECT matnr, werks, plnty,
               plnnr, plnal, zaehl
          FROM mapl
          INTO TABLE @DATA(lt_mapl)
          FOR ALL ENTRIES IN @lt_qals
        WHERE matnr EQ @lt_qals-selmatnr
          AND werks EQ @lt_qals-werk
          AND plnty EQ 'N'
          AND loekz EQ @space.

*-- BOC T_T.KONNO-02.04.21
        SELECT q~prueflos, m~matnr, m~werks, m~plnty,
               m~plnnr, m~plnal, m~zaehl, k~verwe,
               p~plnkn, p~vornr, p~steus
          FROM mapl AS m
          INNER JOIN plko AS k
          ON  m~plnty EQ k~plnty
          AND m~plnnr EQ k~plnnr
          AND m~plnal EQ k~plnal
          AND m~werks EQ k~werks
          INNER JOIN plpo AS p
          ON  p~plnty EQ k~plnty
          AND p~plnnr EQ k~plnnr
          AND p~werks EQ k~werks
          INNER JOIN qals AS q
          ON  q~plnnr EQ p~plnnr
          INTO TABLE @gt_oper
          FOR ALL ENTRIES IN @lt_qals
        WHERE q~prueflos EQ @lt_qals-prueflos
          AND m~matnr    EQ @lt_qals-selmatnr
          AND m~werks    EQ @lt_qals-werk
          AND m~plnty    EQ 'N'
          AND m~loekz    EQ @space
          AND k~datuv    LE @sy-datum
          AND k~loekz    EQ @space
          AND k~verwe    EQ '1'
          AND k~statu    EQ '4'
          AND p~steus    IN @lr_steus[]
          AND p~datuv    LE @sy-datum
          AND p~loekz    EQ @space.

*        SORT gt_oper BY matnr steus.
        DELETE ADJACENT DUPLICATES FROM gt_oper COMPARING ALL FIELDS.
        SORT gt_oper BY prueflos matnr vornr DESCENDING.
*-- EOC T_T.KONNO-02.04.21
      ENDIF.
    ENDIF.

*-- Check if the plan has deletion indicator to skip
    IF lt_mapl[] IS NOT INITIAL.
      SORT lt_mapl BY matnr.
      SELECT plnty, plnnr, plnal, zaehl,
             loekz, verwe, werks
        FROM plko
        INTO TABLE @DATA(lt_plko)
        FOR ALL ENTRIES IN @lt_mapl
       WHERE plnty EQ @lt_mapl-plnty
         AND plnnr EQ @lt_mapl-plnnr
         AND plnal EQ @lt_mapl-plnal
         AND datuv LE @sy-datum
         AND verwe EQ '1'
         AND werks EQ @lt_mapl-werks
         AND statu EQ '4'. "Released Status for General

      DELETE lt_plko WHERE loekz = abap_true.
    ENDIF.

    IF lt_plko[] IS NOT INITIAL.
*      DATA lr_steus TYPE RANGE OF steus.
*      APPEND VALUE #( low = 'QM01' sign = 'I' option = 'EQ' )
*        TO lr_steus.
*      APPEND VALUE #( low = 'QM02' sign = 'I' option = 'EQ' )
*        TO lr_steus.
*      APPEND VALUE #( low = 'QM03' sign = 'I' option = 'EQ' )
*        TO lr_steus.

*-- Task list - operation/activity
      SELECT plnty, plnnr, plnkn, zaehl,
             vornr, steus, werks
        FROM plpo
        INTO TABLE @DATA(lt_plpo)
        FOR ALL ENTRIES IN @lt_plko
       WHERE plnty EQ @lt_plko-plnty
         AND plnnr EQ @lt_plko-plnnr
         AND steus IN @lr_steus[]
         AND datuv LE @sy-datum
         AND loekz EQ @space
         AND werks EQ @lt_plko-werks.

      SORT lt_plpo BY plnnr vornr.

      LOOP AT lt_plpo INTO DATA(ls_plpo).
        CASE ls_plpo-steus.
          WHEN 'QM01'.
            gv_oper_qm01 = ls_plpo-vornr.
            APPEND ls_plpo TO lt_plpo_qm01.
          WHEN 'QM03'.
            gv_oper_qm03 = ls_plpo-vornr.
            APPEND ls_plpo TO lt_plpo_qm03.
        ENDCASE.
      ENDLOOP.

      SORT: lt_plpo_qm01 BY vornr,
            lt_plpo_qm03 BY vornr.
    ENDIF.

*-----------------------------------------------------------------------
*--Fetching The Inspection characteristics for MIC'S.
*--Fetching the operations for inspection records.
    LOOP AT lt_qals INTO ls_qals.
      REFRESH lt_oper_list.
*--Fetching operations for inspection lot.
      CALL FUNCTION 'BAPI_INSPLOT_GETOPERATIONS'
        EXPORTING
          number        = ls_qals-prueflos
        TABLES
          inspoper_list = lt_oper_list.

**--Allowing only PLPOD-STEUS = QM01.
      SORT lt_oper_list BY inspoper.
      APPEND INITIAL LINE TO gt_pruef ASSIGNING FIELD-SYMBOL(<fs_pruef>).
      IF sy-subrc EQ 0.
        <fs_pruef>-prueflos = ls_qals-prueflos.
      ENDIF.

*--Fetching the inspection characteristics for mic's for every operatn.
      LOOP AT lt_oper_list INTO DATA(ls_oper_list).
        READ TABLE lt_mapl ASSIGNING FIELD-SYMBOL(<fs_mapl>)
          WITH KEY matnr = ls_qals-selmatnr BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_plpo ASSIGNING FIELD-SYMBOL(<fs_plpo>)
            WITH KEY plnnr = <fs_mapl>-plnnr
                     vornr = ls_oper_list-inspoper BINARY SEARCH.
          IF sy-subrc = 0.
            IF <fs_plpo>-steus = 'QM01'.
              CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
                EXPORTING
                  insplot                = ls_qals-prueflos
                  inspoper               = ls_oper_list-inspoper
                  read_char_requirements = abap_true
                  read_char_results      = abap_true
                  read_sample_results    = abap_true
                  max_insppoints         = 1000
                TABLES
                  char_requirements      = lt_char_req
                  char_results           = lt_char_res
                  sample_results         = lt_sample_res.

              IF lt_char_res IS NOT INITIAL.
                SORT lt_char_res BY insplot.
                SORT lt_sample_res BY insplot inspoper
                                      inspchar inspsample.
              ENDIF.

              LOOP AT lt_char_req INTO ls_char_req.
                ls_mic-prueflos = ls_qals-prueflos.
                ls_mic-operatn  = ls_oper_list-inspoper.
                ls_mic-mstr_char  = ls_char_req-mstr_char.
                ls_mic-inspchar   = ls_char_req-inspchar.
                ls_mic-char_descr = ls_char_req-char_descr.
                READ TABLE lt_sample_res TRANSPORTING NO FIELDS
                  WITH KEY insplot  = ls_char_req-insplot
                           inspoper = ls_char_req-inspoper
                           inspchar = ls_char_req-inspchar BINARY SEARCH.
                IF sy-subrc EQ 0.
                  lv_tabix = sy-tabix.
                  LOOP AT lt_sample_res INTO ls_sample_res FROM lv_tabix.
                    IF ls_sample_res-insplot <> ls_char_req-insplot
                     OR ls_sample_res-inspoper <> ls_char_req-inspoper
                     OR ls_sample_res-inspchar <> ls_char_req-inspchar.
                      EXIT.
                    ENDIF.
                    ls_mic-qibpprobe  = ls_sample_res-inspsample.
                    IF ls_sample_res-mean_value IS INITIAL.
                      ls_mic-mean_value = ls_sample_res-code1.
                    ELSE.
                      ls_mic-mean_value = ls_sample_res-mean_value.
                    ENDIF.
                    APPEND ls_mic TO gt_mic_qm01.
                  ENDLOOP.
                ENDIF.
                APPEND ls_mic TO gt_mic_n_qm01.
                CLEAR ls_mic.
              ENDLOOP.

              REFRESH: lt_char_req, lt_char_res, lt_sample_res.
              CLEAR: ls_sample_res, ls_char_req, ls_mic,
                     ls_operation, ls_insppoint, ls_return.
            ELSEIF <fs_plpo>-steus = 'QM03'.
              CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
                EXPORTING
                  insplot                = ls_qals-prueflos
                  inspoper               = ls_oper_list-inspoper
                  read_char_requirements = abap_true
                  read_char_results      = abap_true
                  read_sample_results    = abap_true
                  max_insppoints         = 1000
                IMPORTING
                  operation              = ls_operation
                  insppoint_requirements = ls_insppoint
                  return                 = ls_return
                TABLES
                  char_requirements      = lt_char_req
                  char_results           = lt_char_res
                  sample_results         = lt_sample_res.

              IF lt_char_res IS NOT INITIAL.
                SORT lt_char_res BY insplot.
                SORT lt_sample_res BY insplot inspoper inspchar inspsample.
              ENDIF.

              LOOP AT lt_char_req INTO ls_char_req.
                ls_mic-prueflos = ls_qals-prueflos.
                ls_mic-operatn  = ls_oper_list-inspoper.
                ls_mic-mstr_char  = ls_char_req-mstr_char.
                ls_mic-inspchar   = ls_char_req-inspchar.
                ls_mic-char_descr = ls_char_req-char_descr.
                READ TABLE lt_sample_res INTO ls_sample_res
                  WITH KEY insplot  = ls_char_req-insplot
                           inspoper = ls_char_req-inspoper
                           inspchar = ls_char_req-inspchar BINARY SEARCH.
                IF sy-subrc EQ 0.
                  lv_tabix = sy-tabix.
                  LOOP AT lt_sample_res INTO ls_sample_res FROM lv_tabix.
                    IF ls_sample_res-insplot <> ls_char_req-insplot
                     OR ls_sample_res-inspoper <> ls_char_req-inspoper
                     OR ls_sample_res-inspchar <> ls_char_req-inspchar.
                      EXIT.
                    ENDIF.
                    ls_mic-qibpprobe  = ls_sample_res-inspsample.
                    IF ls_sample_res-mean_value IS INITIAL.
                      ls_mic-mean_value = ls_sample_res-code1.
                    ELSE.
                      ls_mic-mean_value = ls_sample_res-mean_value.
                    ENDIF.
                    ls_mic-char_descr = ls_char_req-char_descr.
                    APPEND ls_mic TO gt_mic_qm03.
                  ENDLOOP.
                ENDIF.
                <fs_pruef>-flag = abap_true.
                APPEND ls_mic TO gt_mic_n_qm03.
                CLEAR ls_mic.
              ENDLOOP.

              REFRESH: lt_char_req, lt_char_res, lt_sample_res.
              CLEAR: ls_sample_res, ls_char_req, ls_mic,
                     ls_operation, ls_insppoint, ls_return.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT: gt_mic_qm01   BY prueflos,
          gt_mic_qm03   BY prueflos,
          gt_mic_n_qm01 BY mstr_char inspchar,
          gt_mic_n_qm03 BY mstr_char inspchar,
          gt_pruef      BY prueflos flag.

    DELETE ADJACENT DUPLICATES FROM gt_mic_n_qm01 COMPARING mstr_char inspchar.
    DELETE ADJACENT DUPLICATES FROM gt_mic_n_qm03 COMPARING mstr_char inspchar.

*--Perform Fieldcatalog.(For QM01)
    PERFORM prepare_fcat USING gt_mic_n_qm01
                               'GT_MIC_QM01'
                      CHANGING gt_fcat_qm01.

*--Perform Fieldcatalog.(For QM03)
    PERFORM prepare_fcat USING gt_mic_n_qm03
                               'GT_MIC_QM03'
                      CHANGING gt_fcat_qm03.

*---Preparing dynamic internal table.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = gt_fcat_qm01
      IMPORTING
        ep_table                  = lo_tqual_char_qm01
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

*---Preparing dynamic internal table.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = gt_fcat_qm03
      IMPORTING
        ep_table                  = lo_tqual_char_qm03
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

    ASSIGN lo_tqual_char_qm01->* TO <fs_tqual_char_qm01>.
    CREATE DATA lo_squal_char_qm01 LIKE LINE OF <fs_tqual_char_qm01>.
    ASSIGN lo_squal_char_qm01->* TO <fs_squal_char_qm01>.

    ASSIGN lo_tqual_char_qm03->* TO <fs_tqual_char_qm03>.
    CREATE DATA lo_squal_char_qm03 LIKE LINE OF <fs_tqual_char_qm03>.
    ASSIGN lo_squal_char_qm03->* TO <fs_squal_char_qm03>.

    gt_mapl = lt_mapl.
    gt_plpo = lt_plpo.
*-----------------------------------------------------------------------
*-- Build Final Table FOR QM01
    PERFORM built_qual_char_qm01 USING gt_mic_qm01
                                       lt_qals
                                       gt_qapp_all
                                       lt_fmfphdr
                                       'QM01'
                              CHANGING lo_tqual_char_qm01
                                       lo_squal_char_qm01.

*-- BOC T_T.KONNO-17.02.21
**-- Build Final Table FOR QM03
*    PERFORM built_qual_char_qm03 USING gt_mic_qm03
*                                       lt_qals
*                                       gt_last_created
*                                       lt_fmfphdr
*                                       'QM03'
*                              CHANGING lo_tqual_char_qm03
*                                       lo_squal_char_qm03.
    DATA(lt_last_created) = gt_last_created[].
    SORT lt_last_created BY prueflos ASCENDING
                            vorglfnr ASCENDING
                            probenr  DESCENDING.

*-- Build Final Table FOR QM03
    PERFORM built_qual_char_qm03 USING gt_mic_qm03
                                       lt_qals
                                       lt_last_created
                                       lt_fmfphdr
                                       'QM03'
                              CHANGING lo_tqual_char_qm03
                                       lo_squal_char_qm03.
*-- EOC T_T.KONNO-17.02.21

  ENDMETHOD.

  METHOD : display_qual_charcs_data.
    IF <fs_tqual_char_qm01> IS ASSIGNED.
      IF sy-batch EQ abap_false.
        CALL SCREEN 0100.
      ELSE.
        PERFORM controls_display.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization.
  CREATE OBJECT : go_qual.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM title_set.
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Back Button
*&---------------------------------------------------------------------*
FORM fcode_back.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CREATE
*&---------------------------------------------------------------------*
*& For CREATE Button
*&---------------------------------------------------------------------*
FORM fcode_create.

  TYPES: BEGIN OF lty_inspdata,
           fieldname TYPE fieldname,
           pos       TYPE c LENGTH 1,
         END OF lty_inspdata,

         BEGIN OF lty_inspoint,
           userc1 TYPE qusrchar18,
           userc2 TYPE qusrchar10,
           usern1 TYPE qusrnumc10,
           usern2 TYPE qusrnumc3,
           userd1 TYPE qeifusrdat,
           usert1 TYPE qeifusrtim,
         END OF lty_inspoint.

  DATA: lt_char_requirements TYPE TABLE OF bapi2045d1,
        lt_char_results      TYPE TABLE OF bapi2045d2,
        lt_samp_results      TYPE TABLE OF bapi2045d3,
        lt_return_temp       TYPE TABLE OF bapiret2,
        lt_return            TYPE TABLE OF bapiret2,
        lt_fields            TYPE TABLE OF sval,
        lt_insppoints        TYPE TABLE OF bapi2045l4,
        lt_inspdata          TYPE TABLE OF lty_inspdata,
        lt_inspoint_new      TYPE TABLE OF lty_inspoint,
        lrt_steus            TYPE RANGE OF steus,
        ls_inspoint_new      TYPE lty_inspoint,
        ls_fields            TYPE sval,
        ls_char_results      TYPE bapi2045d2,
        ls_samp_results      TYPE bapi2045d3,
        ls_inspoint          TYPE bapi2045l4,
        ls_return            TYPE bapiret2,
        ls_layout            TYPE lvc_s_layo,
        lv_message           TYPE string,
        lv_new               TYPE flag,
        lv_qprobenrpp        TYPE qprobenrpp,
        lv_answer.

  FIELD-SYMBOLS: <value> TYPE any.

  DATA: lo_tqual_char_qm03 TYPE REF TO data,
        lo_tqm03           TYPE REF TO data.

  CHECK <fs_tqm03> IS ASSIGNED.

  APPEND VALUE #( low = 'QM03' sign = 'I' option = 'EQ' )
    TO lrt_steus.
  DATA(lt_oper_ne_qm03) = gt_oper[].
  DELETE lt_oper_ne_qm03 WHERE steus IN lrt_steus[].

  DATA(lt_oper_qm03) = gt_oper[].
  DELETE lt_oper_qm03 WHERE steus NOT IN lrt_steus[].

  SORT: lt_oper_ne_qm03 BY prueflos matnr steus,
        lt_oper_qm03 BY prueflos matnr steus.

  LOOP AT <fs_tqm03> ASSIGNING FIELD-SYMBOL(<fs_sqm03>).
*-- BOC-T_T.KONNO-05.12.21
    REFRESH: lt_char_results, lt_samp_results, lt_return_temp.
*-- EOC-T_T.KONNO-05.12.21
    CLEAR: ls_char_results, ls_inspoint_new, ls_inspoint.
    CLEAR: gv_oper_qm01, gv_oper_qm02, gv_oper_qm03.

    ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_sqm03>
      TO FIELD-SYMBOL(<lv_prueflos>).
    IF <fs_vqual_char> IS ASSIGNED.
      ls_char_results-insplot = <lv_prueflos>.
    ENDIF.

*--BOC T_T.KONNO-02.04.21
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_sqm03>
      TO FIELD-SYMBOL(<lv_matnr>).
    IF sy-subrc NE 0.
      UNASSIGN <lv_matnr>.
    ELSE.
*      READ TABLE gt_oper INTO DATA(ls_oper)
*        WITH KEY matnr = <lv_matnr>
*                 steus = 'QM03' BINARY SEARCH.
      READ TABLE lt_oper_ne_qm03 INTO DATA(ls_oper_ne_qm03)
        WITH KEY prueflos = ls_char_results-insplot
                 matnr    = <lv_matnr>
                 steus    = 'QM01' BINARY SEARCH.
      IF sy-subrc EQ 0.
        gv_oper_qm01 = ls_oper_ne_qm03-vornr.
      ENDIF.
      READ TABLE lt_oper_ne_qm03 INTO ls_oper_ne_qm03
        WITH KEY prueflos = ls_char_results-insplot
                 matnr    = <lv_matnr>
                 steus    = 'QM02' BINARY SEARCH.
      IF sy-subrc EQ 0.
        gv_oper_qm02 = ls_oper_ne_qm03-vornr.
      ENDIF.

      READ TABLE lt_oper_qm03 INTO DATA(ls_oper_qm03)
        WITH KEY prueflos = ls_char_results-insplot
                 matnr    = <lv_matnr>
                 steus    = 'QM03'.
      IF sy-subrc EQ 0.
        gv_oper_qm03 = ls_oper_qm03-vornr.
      ENDIF.
    ENDIF.
*--EOC T_T.KONNO-02.04.21

    ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_sqm03>
      TO FIELD-SYMBOL(<lv_ppsortkey>).
    IF <lv_ppsortkey> IS ASSIGNED.
      SPLIT <lv_ppsortkey> AT '|'
        INTO ls_inspoint_new-userc1 ls_inspoint_new-userc2
             ls_inspoint_new-usern1 ls_inspoint_new-usern2
             ls_inspoint_new-userd1 ls_inspoint_new-usert1.
    ENDIF.

    ls_inspoint-insplot = ls_char_results-insplot.
    ls_inspoint-inspoper = gv_oper_qm03.

    READ TABLE gt_pruef TRANSPORTING NO FIELDS
      WITH KEY prueflos = ls_char_results-insplot
               flag     = abap_true BINARY SEARCH.
    IF sy-subrc <> 0.
*-- Não há uma operação de resultados associada ao ponto de inspeção selecionado.
      CONTINUE.
    ENDIF.

    READ TABLE gt_mic_qm03 INTO DATA(ls_mic_qm03)
      WITH KEY prueflos = ls_inspoint-insplot BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_new = abap_false.
    ENDIF.

    SELECT SINGLE slwbez
      FROM qals
      INTO @DATA(lv_slwbez)
     WHERE prueflos = @ls_inspoint-insplot.

    IF sy-subrc EQ 0.
      SELECT SINGLE userc1akt, userc2akt, usern1akt,
                    usern2akt, userd1akt, usert1akt
        FROM tq79
        INTO @DATA(ls_tq79)
       WHERE slwbez EQ @lv_slwbez.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM tq79t
          INTO @DATA(ls_tq79t)
         WHERE slwbez  EQ @lv_slwbez
           AND sprache EQ @sy-langu.

        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING FIELD-SYMBOL(<fs_inspdata>).
          <fs_inspdata>-fieldname = 'USERC1AKT'.
          <fs_inspdata>-pos       = ls_tq79-userc1akt.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING <fs_inspdata>.
          <fs_inspdata>-fieldname = 'USERC2AKT'.
          <fs_inspdata>-pos       = ls_tq79-userc2akt.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING <fs_inspdata>.
          <fs_inspdata>-fieldname = 'USERN1AKT'.
          <fs_inspdata>-pos       = ls_tq79-usern1akt.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING <fs_inspdata>.
          <fs_inspdata>-fieldname = 'USERN2AKT'.
          <fs_inspdata>-pos       = ls_tq79-usern2akt.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING <fs_inspdata>.
          <fs_inspdata>-fieldname = 'USERD1AKT'.
          <fs_inspdata>-pos       = ls_tq79-userd1akt.
          APPEND INITIAL LINE TO lt_inspdata ASSIGNING <fs_inspdata>.
          <fs_inspdata>-fieldname = 'USERT1AKT'.
          <fs_inspdata>-pos       = ls_tq79-usert1akt.
          DELETE lt_inspdata WHERE pos IS INITIAL.
          SORT lt_inspdata BY pos.
        ENDIF.
      ENDIF.
    ENDIF.

    ls_inspoint-userc1    = ls_inspoint_new-userc1.
    ls_inspoint-userc2    = ls_inspoint_new-userc2.
    ls_inspoint-usern1    = ls_inspoint_new-usern1.
    ls_inspoint-usern2    = ls_inspoint_new-usern2.
    ls_inspoint-usert1    = ls_inspoint_new-usert1.
    ls_inspoint-userd1    = ls_inspoint_new-userd1.
    ls_inspoint-insppoint = 1.
    ls_inspoint-insp_date = sy-datum.
    ls_inspoint-insp_time = sy-uzeit.

    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
      EXPORTING
        insplot                = ls_inspoint-insplot
        inspoper               = ls_inspoint-inspoper
        read_insppoints        = abap_true
        read_char_requirements = abap_true
        read_char_results      = abap_true
        read_sample_results    = abap_true
        read_single_results    = abap_true
        max_insppoints         = 1000
      IMPORTING
        return                 = ls_return
      TABLES
        insppoints             = lt_insppoints
        char_requirements      = lt_char_requirements.

*-- Checks if there is an inspection point for the same key fields
    READ TABLE lt_insppoints INTO DATA(ls_insppoints)
      WITH KEY userc1 = ls_inspoint_new-userc1
               userc2 = ls_inspoint_new-userc2
               usern1 = ls_inspoint_new-usern1
               usern2 = ls_inspoint_new-usern2
               usert1 = ls_inspoint_new-usert1
               userd1 = ls_inspoint_new-userd1.
    IF sy-subrc EQ 0.
      ls_inspoint-insppoint = ls_insppoints-insppoint.
    ENDIF.

    LOOP AT lt_char_requirements INTO DATA(ls_char_requirements).
      ls_char_results-insplot     = ls_char_requirements-insplot.
      ls_char_results-inspoper    = ls_char_requirements-inspoper.
      ls_char_results-inspchar    = ls_char_requirements-inspchar.
      ls_char_results-inspector   = sy-uname.
      ls_char_results-start_date  = sy-datum.
      ls_char_results-start_time  = sy-uzeit.
      ls_char_results-end_date    = sy-datum.
      ls_char_results-end_time    = sy-uzeit.
      ls_char_results-closed      = abap_true.
      ls_char_results-evaluated   = abap_true.
      ls_char_results-evaluation  = 'A'.

      ASSIGN COMPONENT ls_char_requirements-mstr_char
        OF STRUCTURE <fs_sqm03> TO <fs_vqual_char>.
      IF <fs_vqual_char> IS ASSIGNED.
        CONDENSE <fs_vqual_char>.
        IF ls_char_requirements-char_type = '01'.
          ls_char_results-mean_value = <fs_vqual_char>.
        ELSEIF ls_char_requirements-char_type = '02'.
          ls_char_results-code1     = <fs_vqual_char>.
          ls_char_results-code_grp1 = ls_char_requirements-sel_set1.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING ls_char_results TO ls_samp_results.

      IF ls_char_requirements-sample_res = abap_true
        AND lv_new = abap_true.
        ls_samp_results-inspsample = ls_inspoint-insppoint.
        CLEAR ls_char_results-evaluation.
      ENDIF.

      APPEND ls_samp_results TO lt_samp_results.
      APPEND ls_char_results TO lt_char_results.
      CLEAR: ls_char_results-mean_value, ls_char_results-code1,
             ls_char_results-code_grp1, ls_samp_results.
    ENDLOOP.

    REFRESH lt_return_temp.

*-- Creating the resultant characteristics point.
    CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
      EXPORTING
        insplot        = ls_char_results-insplot
        inspoper       = gv_oper_qm03
        insppointdata  = ls_inspoint
      IMPORTING
        return         = ls_return
      TABLES
        char_results   = lt_char_results
        sample_results = lt_samp_results
        returntable    = lt_return_temp.

    IF lt_return_temp IS NOT INITIAL.
      APPEND LINES OF lt_return_temp TO lt_return.
    ENDIF.

*-- BOC-T_T.KONNO-05.12.21
*   READ TABLE lt_return TRANSPORTING NO FIELDS
    READ TABLE lt_return_temp TRANSPORTING NO FIELDS
*-- EOC-T_T.KONNO-05.12.21
      WITH KEY type = 'E'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      IF lv_new IS NOT INITIAL.
*-- Os resultados do cálculo do lote de controle &1 foram criados corretamente.
*        MESSAGE TEXT-020 type 'S'.
*-- Os resultados do lote de controle &1 foram criados corretamente.
        MESSAGE s340(zfmfp) WITH ls_char_results-insplot.
      ELSE.
*-- Os resultados do cálculo do lote de controle &1 foram atualizados corretamente.
*        MESSAGE TEXT-021 type 'S'.
*-- Os resultados do lote de controle &1 foram atualizados corretamente.
        MESSAGE s341(zfmfp) WITH ls_char_results-insplot.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*-- BOC-T_T.KONNO-05.12.21
*     READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
      READ TABLE lt_return_temp ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
*-- EOC-T_T.KONNO-05.12.21
      IF sy-subrc = 0.
        MESSAGE <fs_return>-message TYPE 'I' DISPLAY LIKE 'I'.
      ELSE.
*-- Alguns problemas ocorreram durante o processamento dos resultados.
*-- Verifique o lote de inspeção
*        MESSAGE TEXT-022 TYPE 'I' DISPLAY LIKE 'I'.
*-- Ocorreram problemas durante o processamento dos resultados do lote &1.
        MESSAGE i342(zfmfp) WITH ls_char_results-insplot.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

*--Local Data Declarations.
  DATA: ls_layout         TYPE lvc_s_layo,
        ls_environment    TYPE /agri/s_glvc_environment,
        lo_event_receiver TYPE REF TO lcl_event_receiver.

  IF sy-batch EQ abap_false.
    IF gobj_ref_cust_cont IS INITIAL.
*--Creation of Container.
      CREATE OBJECT gobj_ref_cust_cont
        EXPORTING
          container_name              = c_qual_char_0100_cc
          repid                       = sy-repid
          dynnr                       = sy-dynnr
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

*--Splitting the custom container.
      CREATE OBJECT gobj_ref_split_cont
        EXPORTING
          parent  = cl_gui_container=>default_screen "gobj_ref_cust_cont
          rows    = 2
          columns = 1.

*--Building two Container.
      CALL METHOD gobj_ref_split_cont->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = gobj_container_qm01.

      gobj_ref_split_cont->set_row_height( id = 1 height = 80 ).

      CALL METHOD gobj_ref_split_cont->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = gobj_container_qm03.

*--Creation of Grid's.
      CREATE OBJECT gobj_ref_grid_qm01
        EXPORTING
          i_parent           = gobj_container_qm01
          is_lvc_environment = ls_environment
        EXCEPTIONS
          error_cntl_create  = 1
          error_cntl_init    = 2
          error_cntl_link    = 3
          error_dp_create    = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      CREATE OBJECT gobj_ref_grid_qm03
        EXPORTING
          i_parent           = gobj_container_qm03
          is_lvc_environment = ls_environment
        EXCEPTIONS
          error_cntl_create  = 1
          error_cntl_init    = 2
          error_cntl_link    = 3
          error_dp_create    = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.
    ENDIF.

    CREATE OBJECT lo_event_receiver. " Creating event object.

    CLEAR ls_layout.
    ls_layout-zebra      = abap_true.
    ls_layout-sel_mode   = 'A'.
    ls_layout-grid_title = TEXT-018.

    CALL METHOD gobj_ref_grid_qm01->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = <fs_tqual_char_qm01>
        it_fieldcatalog               = gt_fcat_qm01
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER lo_event_receiver->handle_hotspot_click FOR gobj_ref_grid_qm01.
  ENDIF.

  PERFORM handle_hotspot_click.

  PERFORM fcode_create.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PREPARE_FCAT
*&---------------------------------------------------------------------*
*& Preparing Fieldcatalog.
*&---------------------------------------------------------------------*
FORM prepare_fcat USING lt_mic  TYPE tty_mic
                        lv_fcat_name TYPE char20
               CHANGING lt_fcat TYPE lvc_t_fcat.

*--Local Workarea and Variables.
  DATA: lv_col_pos  TYPE i,
        lv_mstrchar TYPE qmstr_char,
        lv_chars    TYPE string,
        ls_fcat     TYPE lvc_s_fcat.

*--Local Internal Tables.
  DATA: lt_mic_temp TYPE TABLE OF ty_mic.

*--Preparing field catalog
*-- ZABS_STR_QUAL_CHAR
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_str_qual_char
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  DESCRIBE TABLE lt_fcat LINES lv_col_pos.
*--counte varible.
  ls_fcat-fieldname = 'COUNT'.
  ls_fcat-col_pos   = lv_col_pos + 1.
  ls_fcat-intlen    = '10'.
  ls_fcat-coltext   = TEXT-013.
  APPEND ls_fcat TO lt_fcat.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-emphasize = 'C300' .
*    IF <fs_fcat>-fieldname EQ 'ENTSTEHDAT'.
**-- Data Lote Inspeção
*      <fs_fcat>-reptext   = TEXT-025. "40
*      <fs_fcat>-scrtext_l = TEXT-025. "40
*      <fs_fcat>-scrtext_m = TEXT-025. "20
*      CLEAR <fs_fcat>-scrtext_s.
*    ELSEIF <fs_fcat>-fieldname EQ 'ERSTELDAT'.
**-- Dta.Criação Pt.Insp.
*      <fs_fcat>-reptext   = TEXT-026. "40
*      <fs_fcat>-scrtext_l = TEXT-026. "40
*      <fs_fcat>-scrtext_m = TEXT-026. "20
*      CLEAR <fs_fcat>-scrtext_s.
*    ENDIF.
  ENDLOOP.

*  IF r_disp EQ abap_true.
  IF lv_fcat_name <> 'GT_MIC_QM03'.
    READ TABLE lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
      WITH KEY fieldname = 'PRUEFLOS'.
    IF sy-subrc EQ 0.
      <ls_fcat>-hotspot = abap_true.
    ENDIF.
  ENDIF.
*  ENDIF.

  lt_mic_temp[] = lt_mic[].
  SORT lt_mic_temp BY operatn mstr_char inspchar.
  DELETE ADJACENT DUPLICATES FROM lt_mic_temp COMPARING operatn mstr_char inspchar.
  DELETE lt_mic_temp WHERE mstr_char IS INITIAL.
*-- Building Dynamic Fileds.
  CLEAR ls_fcat.
  LOOP AT lt_mic_temp INTO DATA(ls_mic_temp).
    IF lv_mstrchar IS INITIAL.
      lv_mstrchar = ls_mic_temp-mstr_char.
    ELSE.
      IF lv_mstrchar <> ls_mic_temp-mstr_char.
        lv_mstrchar = ls_mic_temp-mstr_char.
      ENDIF.
    ENDIF.

*--Append Dynamic Columns to Field Catalog
    IF lv_fcat_name = 'GT_MIC_QM01'.
      lv_chars = ls_mic_temp-mstr_char.
      REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
      ls_fcat-fieldname = lv_chars && ls_mic_temp-inspchar.
    ELSE.
      ls_fcat-fieldname = ls_mic_temp-mstr_char.
    ENDIF.

    ls_fcat-col_pos    = lv_col_pos + 1.
    ls_fcat-intlen     = '30'.
    ls_fcat-coltext    = ls_mic_temp-mstr_char.
    ls_fcat-coltext    = ls_mic_temp-char_descr.
    ls_fcat-scrtext_l  = ls_mic_temp-char_descr.
    ls_fcat-scrtext_m  = ls_mic_temp-char_descr.
    ls_fcat-scrtext_s  = ls_mic_temp-char_descr.
    APPEND ls_fcat TO lt_fcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALCULATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ROWS
*&---------------------------------------------------------------------*
FORM calculations  USING lt_rows_aux   TYPE tty_row   "new table
                         lt_rows       TYPE lvc_t_row "old table
                         lv_tplnr_fl   TYPE /agri/gltplnr_fl.

  TYPES: BEGIN OF lty_charres,
           char  TYPE qmerknr,
           chars TYPE qmerknr,
           res   TYPE c LENGTH 50,
           count TYPE i,
         END OF lty_charres.

  TYPES: BEGIN OF lty_fieldname,
           fieldname TYPE fieldname,
         END OF lty_fieldname.

  DATA : lt_charres         TYPE TABLE OF lty_charres,
         lt_fieldname       TYPE TABLE OF lty_fieldname,
         lt_fieldnamet      TYPE TABLE OF lty_fieldname,
         lt_mic_n_qm01      TYPE TABLE OF ty_mic,
         lv_numplants       TYPE zabs_del_gen_value VALUE IS INITIAL,
         lv_pointpres       TYPE zabs_del_gen_value VALUE IS INITIAL,
         lv_totalvalue      TYPE zabs_del_gen_value VALUE IS INITIAL,
         lv_count_a         TYPE i VALUE IS INITIAL,
         lv_count_l         TYPE i VALUE IS INITIAL,
         lv_count_o         TYPE i VALUE IS INITIAL,
         lv_col_pos         TYPE i VALUE 7,
         lv_i               TYPE i,
         lv_it              TYPE i,
         lv_sum             TYPE p LENGTH 15 DECIMALS 4,
         lv_num             TYPE p LENGTH 15 DECIMALS 4,
         lv_total           TYPE zabs_del_total_value,
         lv_string          TYPE string,
         lv_chars           TYPE string,
         ls_fcat            TYPE lvc_s_fcat,
         ls_rows            TYPE lvc_s_row,
         lo_tqual_char_qm03 TYPE REF TO data,
         lo_squal_char_qm03 TYPE REF TO data.

  FIELD-SYMBOLS : <fs_vqm01_res> TYPE any,
                  <inspchar>     TYPE any,
                  <fs_micqm01>   TYPE ty_mic,
                  <value>        TYPE any.

  CLEAR <fs_squal_char_qm03>.

  DESCRIBE TABLE lt_rows LINES lv_numplants.

*-- Fetching country code and plant for respective terrain.
  READ TABLE gt_glflot INTO DATA(ls_glflot)
    WITH KEY tplnr_fl = lv_tplnr_fl BINARY SEARCH.

  IF sy-subrc EQ 0.
    SELECT *
      FROM zabs_qual_char
      INTO TABLE @DATA(lt_formula)
     WHERE bukrs EQ @ls_glflot-bukrs
       AND art   EQ @p_art.
    IF sy-subrc EQ 0.
      SORT lt_formula BY r_mkmnr formula_typ.
      DATA(lt_formula_tmp) = lt_formula[].
      DELETE ADJACENT DUPLICATES FROM lt_formula_tmp[] COMPARING r_mkmnr.
    ENDIF.
  ENDIF.

  LOOP AT lt_formula_tmp INTO DATA(ls_formula_tmp).
    ASSIGN COMPONENT ls_formula_tmp-r_mkmnr
      OF STRUCTURE <fs_squal_char_qm03> TO <fs_vqual_char>.

    IF sy-subrc EQ 0.
*--BOC-T_T.KONNO-10.19.20
      DATA(lt_numplants) = lt_rows_aux[].
      DELETE lt_numplants WHERE qm03_field NE ls_formula_tmp-r_mkmnr.
      lv_numplants = lines( lt_numplants ).
*--EOC-T_T.KONNO-10.19.20
      DATA(ls_formula) = ls_formula_tmp.

      CASE ls_formula_tmp-formula_typ.

        WHEN 'P'."'PERCENTAGE'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO DATA(ls_numplant).
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.
              LOOP AT lt_formula INTO ls_formula WHERE r_mkmnr EQ ls_formula_tmp-r_mkmnr.
                lt_mic_n_qm01 = gt_mic_n_qm01.
                DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                  lv_chars = <fs_micqm01>-mstr_char.
                  REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                  ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                   OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                  IF <fs_vqm01_res> <> 0.
                    lv_pointpres = lv_pointpres + 1.
                  ENDIF.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'ZABS_FM_FORMULA_CALCULATE'
            EXPORTING
              iv_formula     = ls_formula_tmp-formula
              iv_pointpres   = lv_pointpres
              iv_numplants   = lv_numplants
              iv_totalvalue  = lv_totalvalue
            IMPORTING
              iv_total_value = lv_total.

          <fs_vqual_char> = lv_total.

          CLEAR lv_pointpres.

        WHEN 'D'."'DENSITY'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.
              LOOP AT lt_formula INTO ls_formula WHERE r_mkmnr EQ ls_formula_tmp-r_mkmnr.
                lt_mic_n_qm01 = gt_mic_n_qm01.
                DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                  lv_chars = <fs_micqm01>-mstr_char.
                  REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                  ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                   OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                  IF <fs_vqm01_res> <> 0.
                    lv_pointpres = lv_pointpres + 1.
                    lv_totalvalue = lv_totalvalue + <fs_vqm01_res>.
                  ENDIF.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'ZABS_FM_FORMULA_CALCULATE'
            EXPORTING
              iv_formula     = ls_formula_tmp-formula
              iv_pointpres   = lv_pointpres
              iv_numplants   = lv_numplants
              iv_totalvalue  = lv_totalvalue
            IMPORTING
              iv_total_value = lv_total.

          <fs_vqual_char> = lv_total.

          CLEAR: lv_pointpres, lv_totalvalue.

        WHEN 'T'."'TOTAL'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.
              LOOP AT lt_formula INTO ls_formula WHERE r_mkmnr EQ ls_formula_tmp-r_mkmnr.
                lt_mic_n_qm01 = gt_mic_n_qm01.
                DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                  lv_chars = <fs_micqm01>-mstr_char.
                  REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                  ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                   OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                  IF <fs_vqm01_res> NE space
                    AND <fs_vqm01_res> NE '0'.
                    lv_pointpres = lv_pointpres + 1.
                  ENDIF.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'ZABS_FM_FORMULA_CALCULATE'
            EXPORTING
              iv_formula     = ls_formula_tmp-formula
              iv_pointpres   = lv_pointpres
              iv_numplants   = lv_numplants
              iv_totalvalue  = lv_totalvalue
            IMPORTING
              iv_total_value = lv_total.

          <fs_vqual_char> = lv_total.

          CLEAR lv_pointpres.

        WHEN 'T(X)'."'TOTAL OF OVO'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.

              READ TABLE lt_formula TRANSPORTING NO FIELDS
                                    WITH KEY r_mkmnr = ls_formula_tmp-r_mkmnr
                                    BINARY SEARCH.

              IF sy-subrc = 0.

                LOOP AT lt_formula INTO ls_formula FROM sy-tabix.

                  IF ls_formula-r_mkmnr <> ls_formula_tmp-r_mkmnr.
                    EXIT.
                  ENDIF.
                  lt_mic_n_qm01 = gt_mic_n_qm01.
                  DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                  LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                    lv_chars = <fs_micqm01>-mstr_char.
                    REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                    ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                     OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                    IF <fs_vqm01_res> NE space
                      AND <fs_vqm01_res> NE '0'.
                      lv_pointpres = lv_pointpres + 1.
                      CASE <fs_vqm01_res>.
                        WHEN ls_formula-vars.
                          lv_count_o = lv_count_o + 1.
                        WHEN OTHERS.
                      ENDCASE.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDLOOP.

          "based on the field name we should cal the value.
          IF lv_pointpres <> 0.
            lv_total = ( lv_count_o / lv_pointpres ) * 100 .
            <fs_vqual_char> = lv_total.
          ENDIF.
          CLEAR lv_pointpres.
          CLEAR lv_count_o.

        WHEN 'S'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.

              READ TABLE lt_formula TRANSPORTING NO FIELDS
                WITH KEY r_mkmnr = ls_formula_tmp-r_mkmnr BINARY SEARCH.

              IF sy-subrc = 0.
                LOOP AT lt_formula INTO ls_formula FROM sy-tabix.
                  IF ls_formula-r_mkmnr <> ls_formula_tmp-r_mkmnr.
                    EXIT.
                  ENDIF.

                  lt_mic_n_qm01 = gt_mic_n_qm01.
                  DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                  LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                    lv_chars = <fs_micqm01>-mstr_char.
                    REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                    ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                     OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                    IF <fs_vqm01_res> NE space
                      AND <fs_vqm01_res> NE '0'
                      AND  <fs_vqm01_res> CA '*/ + - ? ! % ( ) = 0123456789'.
                      lv_string = <fs_vqm01_res>.
                      CONDENSE lv_string.

                      CALL FUNCTION 'HRCM_STRING_TO_AMOUNT_CONVERT'
                        EXPORTING
                          string            = lv_string
                          decimal_separator = ','
                        IMPORTING
                          betrg             = lv_num
                        EXCEPTIONS
                          convert_error     = 1
                          OTHERS            = 2.

                      lv_sum = lv_sum + lv_num.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.

              ENDIF.

            ENDIF.
          ENDLOOP.

          IF lv_sum <> 0.
            <fs_vqual_char> = lv_sum.
          ENDIF.

        WHEN 'F'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.

              READ TABLE lt_formula TRANSPORTING NO FIELDS
                                    WITH KEY r_mkmnr = ls_formula_tmp-r_mkmnr
                                    BINARY SEARCH.

              IF sy-subrc = 0.

                LOOP AT lt_formula INTO ls_formula FROM sy-tabix.

                  IF ls_formula-r_mkmnr <> ls_formula_tmp-r_mkmnr.
                    EXIT.
                  ENDIF.

                  lt_mic_n_qm01 = gt_mic_n_qm01.
                  DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                  LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                    lv_chars = <fs_micqm01>-mstr_char.
                    REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                    ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                                     OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.
                    IF <fs_vqm01_res> = ls_formula-vars.
                      lv_count_a = lv_count_a + 1.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.

              ENDIF.

            ENDIF.
          ENDLOOP.

          IF lv_count_a IS NOT INITIAL.
            <fs_vqual_char> = lv_count_a.
          ENDIF.

        WHEN '%P'.
*--BOC-T_T.KONNO-10.19.20
*          LOOP AT lt_rows INTO ls_rows.
*            READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rows-index.
          LOOP AT lt_numplants INTO ls_numplant.
            READ TABLE <fs_tqual_char_qm01>
              ASSIGNING <fs_squal_char_qm01> INDEX ls_numplant-qm01_row.
*--EOC-T_T.KONNO-10.19.20
            IF <fs_squal_char_qm01> IS ASSIGNED.

              READ TABLE lt_formula TRANSPORTING NO FIELDS
                WITH KEY r_mkmnr = ls_formula_tmp-r_mkmnr BINARY SEARCH.

              IF sy-subrc = 0.

                LOOP AT lt_formula INTO ls_formula FROM sy-tabix.

                  IF ls_formula-r_mkmnr <> ls_formula_tmp-r_mkmnr.
                    EXIT.
                  ENDIF.

                  lt_mic_n_qm01 = gt_mic_n_qm01.
                  DELETE lt_mic_n_qm01 WHERE mstr_char <> ls_formula-d_mkmnr.
                  LOOP AT lt_mic_n_qm01 ASSIGNING <fs_micqm01>.
                    lv_chars = <fs_micqm01>-mstr_char.
                    REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                    ASSIGN COMPONENT lv_chars && <fs_micqm01>-inspchar
                      OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqm01_res>.

                    IF sy-subrc = 0.
                      SPLIT ls_formula-vars AT ';' INTO TABLE lt_fieldnamet.

                      LOOP AT lt_fieldnamet ASSIGNING FIELD-SYMBOL(<fs_fieldnamet>).
                        READ TABLE lt_fieldname TRANSPORTING NO FIELDS
                          WITH KEY fieldname = <fs_fieldnamet>-fieldname.
                        IF sy-subrc <> 0.
                          APPEND <fs_fieldnamet> TO lt_fieldname.
                        ENDIF.
                      ENDLOOP.

                      READ TABLE lt_charres ASSIGNING FIELD-SYMBOL(<fs_charrest>)
                        WITH KEY char  = ls_formula-r_mkmnr
                                 chars = ls_formula-d_mkmnr
                                 res   = <fs_vqm01_res>.
                      IF sy-subrc = 0.
                        <fs_charrest>-count = <fs_charrest>-count + 1.
                      ELSE.
                        APPEND INITIAL LINE TO lt_charres ASSIGNING FIELD-SYMBOL(<fs_charres>).
                        <fs_charres>-char  =  ls_formula-r_mkmnr.
                        <fs_charres>-chars =  ls_formula-d_mkmnr.
                        <fs_charres>-res   =  <fs_vqm01_res>.
                        <fs_charres>-count =  1.
                      ENDIF.

                    ENDIF.
                  ENDLOOP.
                ENDLOOP.

              ENDIF.

            ENDIF.
          ENDLOOP.

          DELETE lt_charres WHERE res IS INITIAL.

          LOOP AT lt_charres ASSIGNING FIELD-SYMBOL(<fs_charress>).
            lv_i = lv_i + <fs_charress>-count.
          ENDLOOP.

          LOOP AT lt_fieldname ASSIGNING FIELD-SYMBOL(<fs_fieldname>).
            READ TABLE lt_charres ASSIGNING FIELD-SYMBOL(<fs_charreso>)
              WITH KEY res = <fs_fieldname>.
            IF sy-subrc = 0.
              lv_it = lv_it + <fs_charreso>-count.
            ENDIF.
          ENDLOOP.

          IF lt_charres IS NOT INITIAL.
            lv_total = ( lv_it / lv_i ) * 100.
            <fs_vqual_char> = lv_total.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    CLEAR: lv_total, lv_count_a, lv_it, lv_i,
           lv_sum, lv_num, lv_pointpres." lv_numplants.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_QUAL_CHAR_QM01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MIC_QM01
*&---------------------------------------------------------------------*
FORM built_qual_char_qm01  USING lt_mic        TYPE tty_mic
                                 lt_qals       TYPE tty_qals
                                 lt_qapp       TYPE tty_qapp
                                 lt_fmfphdr    TYPE tty_fmfphdr
                                 lv_steus      TYPE steus
                        CHANGING lo_tqual_char TYPE any
                                 lo_squal_char TYPE any.

  DATA: lo_squal_char_qm01 TYPE REF TO data,
        ls_qm01            LIKE LINE OF gt_qm01,
        lv_chars           TYPE string,
        lt_plpo            TYPE gtt_plpo.

  FIELD-SYMBOLS : <fs_tqual_char>      TYPE STANDARD TABLE,
                  <fs_squal_char_qm01> TYPE any,
                  <fs_squal_char>      TYPE any,
                  <fs_squal_chat>      TYPE any,
                  <fs_vqual_char>      TYPE any.

  ASSIGN lo_tqual_char->* TO <fs_tqual_char>.
  CREATE DATA lo_squal_char LIKE LINE OF <fs_tqual_char>.
  ASSIGN lo_squal_char->* TO <fs_squal_char>.
  ASSIGN lo_squal_char->* TO <fs_squal_chat>.
  CREATE DATA lo_squal_char_qm01 LIKE LINE OF <fs_tqual_char_qm01>.
  ASSIGN lo_squal_char_qm01->* TO <fs_squal_char_qm01>.

  DATA : lv_count TYPE i VALUE IS INITIAL.

  LOOP AT lt_fmfphdr INTO DATA(ls_fmfphdr).

    ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-tplnr_fl.
    ENDIF.

    ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-aufnr.
    ENDIF.

    ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-erdat.
    ENDIF.

    READ TABLE lt_qals TRANSPORTING NO FIELDS
      WITH KEY aufnr = ls_fmfphdr-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_qals INTO DATA(ls_qals) FROM sy-tabix.
        IF ls_qals-aufnr <> ls_fmfphdr-aufnr.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
        IF <fs_vqual_char> IS ASSIGNED.
          <fs_vqual_char> = ls_qals-werk.
        ENDIF.

        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
        IF <fs_vqual_char> IS ASSIGNED.
          <fs_vqual_char> = ls_qals-selmatnr.
        ENDIF.

        READ TABLE gt_mapl ASSIGNING FIELD-SYMBOL(<fs_mapl>)
          WITH KEY matnr = ls_qals-selmatnr BINARY SEARCH.

        IF sy-subrc = 0.
          lt_plpo = gt_plpo.
          DELETE lt_plpo WHERE plnnr <> <fs_mapl>-plnnr.
          DELETE lt_plpo WHERE steus <> lv_steus.
        ENDIF.

        LOOP AT lt_plpo ASSIGNING FIELD-SYMBOL(<fs_plpo>).
          READ TABLE lt_qapp TRANSPORTING NO FIELDS
            WITH KEY prueflos = ls_qals-prueflos
                     vorglfnr  = <fs_plpo>-plnkn BINARY SEARCH.
          IF sy-subrc EQ 0.
            LOOP AT lt_qapp INTO DATA(ls_qapp) FROM sy-tabix.
              IF ls_qapp-prueflos <> ls_qals-prueflos
              OR ls_qapp-vorglfnr <> <fs_plpo>-plnkn.
                lv_count = 0.
                EXIT.
              ENDIF.

              ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-prueflos.
              ENDIF.

              ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-ersteldat.
              ENDIF.

              ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-ppsortkey.
              ENDIF.

              ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                lv_count = lv_count + 1.
                <fs_vqual_char> = lv_count.
              ENDIF.

              <fs_squal_chat> = <fs_squal_char>.

              LOOP AT lt_mic INTO DATA(ls_mic)
                WHERE prueflos  = ls_qapp-prueflos
                  AND qibpprobe = ls_qapp-probenr.
                IF lv_steus <> 'QM03'.
                  lv_chars = ls_mic-mstr_char.
                  REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                  ASSIGN COMPONENT lv_chars && ls_mic-inspchar
                          OF STRUCTURE <fs_squal_chat> TO <fs_vqual_char>.
                ELSE.
                  ASSIGN COMPONENT ls_mic-mstr_char
                        OF STRUCTURE <fs_squal_chat> TO <fs_vqual_char>.
                ENDIF.
                IF sy-subrc = 0.
                  <fs_vqual_char> = ls_mic-mean_value.
                ENDIF.
              ENDLOOP.

              ls_qm01-prueflos = ls_qapp-prueflos.
              ls_qm01-ppsortkey = ls_qapp-ppsortkey.
              APPEND ls_qm01 TO gt_qm01.

              APPEND <fs_squal_chat> TO <fs_tqual_char>.
              CLEAR <fs_squal_chat>.
              ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_fmfphdr-tplnr_fl.
              ENDIF.

              ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_fmfphdr-aufnr.
              ENDIF.

              ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_fmfphdr-erdat.
              ENDIF.

              ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_qapp-ersteldat.
              ENDIF.

              ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_qals-werk.
              ENDIF.

              ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF <fs_vqual_char> IS ASSIGNED.
                <fs_vqual_char> = ls_qals-selmatnr.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILT_QUAL_CHAR_QM03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MIC_QM03
*&---------------------------------------------------------------------*
FORM built_qual_char_qm03 USING lt_mic        TYPE tty_mic
                                lt_qals       TYPE tty_qals
                                lt_qapp       TYPE tty_qapp
                                lt_fmfphdr    TYPE tty_fmfphdr
                                lv_steus      TYPE steus
                       CHANGING lo_tqual_char TYPE any
                                lo_squal_char TYPE any.

  DATA: lo_squal_char_qm01 TYPE REF TO data,
        lv_chars           TYPE string,
        lt_plpo            TYPE gtt_plpo.

  FIELD-SYMBOLS : <fs_tqual_char>      TYPE STANDARD TABLE,
                  <fs_squal_char_qm01> TYPE any,
                  <fs_squal_char>      TYPE any,
                  <fs_squal_chat>      TYPE any,
                  <fs_vqual_char>      TYPE any.

  ASSIGN lo_tqual_char->* TO <fs_tqual_char>.
  CREATE DATA lo_squal_char LIKE LINE OF <fs_tqual_char>.
  ASSIGN lo_squal_char->* TO <fs_squal_char>.
  ASSIGN lo_squal_char->* TO <fs_squal_chat>.
  CREATE DATA lo_squal_char_qm01 LIKE LINE OF <fs_tqual_char_qm01>.
  ASSIGN lo_squal_char_qm01->* TO <fs_squal_char_qm01>.

  DATA : lv_count TYPE i VALUE IS INITIAL.

  LOOP AT lt_fmfphdr INTO DATA(ls_fmfphdr).

    ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-tplnr_fl.
    ENDIF.

    ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-aufnr.
    ENDIF.

    ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
    IF <fs_vqual_char> IS ASSIGNED.
      <fs_vqual_char> = ls_fmfphdr-erdat.
    ENDIF.

    READ TABLE lt_qals TRANSPORTING NO FIELDS
      WITH KEY aufnr = ls_fmfphdr-aufnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_qals INTO DATA(ls_qals) FROM sy-tabix.
        IF ls_qals-aufnr <> ls_fmfphdr-aufnr.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
        IF <fs_vqual_char> IS ASSIGNED.
          <fs_vqual_char> = ls_qals-werk.
        ENDIF.

        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
        IF <fs_vqual_char> IS ASSIGNED.
          <fs_vqual_char> = ls_qals-selmatnr.
        ENDIF.

        READ TABLE gt_mapl ASSIGNING FIELD-SYMBOL(<fs_mapl>)
          WITH KEY matnr = ls_qals-selmatnr BINARY SEARCH.

        IF sy-subrc = 0.
          lt_plpo = gt_plpo.
          DELETE lt_plpo WHERE plnnr <> <fs_mapl>-plnnr.
          DELETE lt_plpo WHERE steus <> lv_steus.
        ENDIF.

        LOOP AT lt_plpo ASSIGNING FIELD-SYMBOL(<fs_plpo>).
*-- BOC T_T.KONNO-17.02.21
*          READ TABLE lt_qapp TRANSPORTING NO FIELDS
*            WITH KEY prueflos = ls_qals-prueflos BINARY SEARCH.
          READ TABLE lt_qapp TRANSPORTING NO FIELDS
            WITH KEY prueflos = ls_qals-prueflos
                     vorglfnr = <fs_plpo>-plnkn BINARY SEARCH.
*-- EOC T_T.KONNO-17.02.21
          IF sy-subrc EQ 0.
            LOOP AT lt_qapp INTO DATA(ls_qapp) FROM sy-tabix.
*-- BOC T_T.KONNO-17.02.21
*              IF ls_qapp-prueflos <> ls_qals-prueflos.
              IF ls_qapp-prueflos <> ls_qals-prueflos
              OR ls_qapp-vorglfnr <> <fs_plpo>-plnkn.
*-- EOC T_T.KONNO-17.02.21
                lv_count = 0.
                EXIT.
              ENDIF.

              ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-prueflos.
              ENDIF.

              ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-ppsortkey.
              ENDIF.

              ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                lv_count = lv_count + 1.
                <fs_vqual_char> = lv_count.
              ENDIF.

              ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qapp-ersteldat.
              ENDIF.

              <fs_squal_chat> = <fs_squal_char>.

*-- Check if same operation
              IF <fs_plpo>-plnkn EQ ls_qapp-vorglfnr.
                LOOP AT lt_mic INTO DATA(ls_mic)
                  WHERE prueflos  = ls_qapp-prueflos
                    AND qibpprobe = ls_qapp-probenr
                    AND operatn   = <fs_plpo>-vornr.
                  IF lv_steus <> 'QM03'.
                    lv_chars = ls_mic-mstr_char.
                    REPLACE ALL OCCURRENCES OF REGEX '[.,;:]' IN lv_chars WITH space.
                    ASSIGN COMPONENT lv_chars && ls_mic-inspchar
                      OF STRUCTURE <fs_squal_chat> TO <fs_vqual_char>.
                  ELSE.
                    ASSIGN COMPONENT ls_mic-mstr_char
                      OF STRUCTURE <fs_squal_chat> TO <fs_vqual_char>.
                  ENDIF.
                  IF sy-subrc = 0.
                    <fs_vqual_char> = ls_mic-mean_value.
                  ENDIF.
                ENDLOOP.
              ENDIF.

              APPEND <fs_squal_chat> TO <fs_tqual_char>.
              CLEAR <fs_squal_chat>.
              ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_fmfphdr-tplnr_fl.
              ENDIF.

              ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_fmfphdr-aufnr.
              ENDIF.

              ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_fmfphdr-erdat.
              ENDIF.

              ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qals-werk.
              ENDIF.

              ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
              IF sy-subrc EQ 0.
                <fs_vqual_char> = ls_qals-selmatnr.
              ENDIF.
            ENDLOOP.
          ELSE.
            READ TABLE <fs_tqual_char_qm01> TRANSPORTING NO FIELDS
              WITH KEY ('PRUEFLOS') = ls_qals-prueflos BINARY SEARCH.
            IF sy-subrc EQ 0.
              LOOP AT <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> FROM sy-tabix.
                ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01>
                 TO FIELD-SYMBOL(<fs_prueflos_qm01>).
                IF sy-subrc EQ 0.
                  IF <fs_prueflos_qm01> <> ls_qals-prueflos.
                    lv_count = 0.
                    EXIT.
                  ENDIF.
                ELSE.
                  EXIT.
                ENDIF.

                ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_qals-prueflos.
                ENDIF.

                ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char_qm01>
                    TO FIELD-SYMBOL(<fs_ppsortkey>).
                  IF sy-subrc EQ 0.
                    <fs_vqual_char> = <fs_ppsortkey>.
                  ENDIF.
                ENDIF.

                ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  lv_count = lv_count + 1.
                  <fs_vqual_char> = lv_count.
                ENDIF.

                <fs_squal_chat> = <fs_squal_char>.

                APPEND <fs_squal_chat> TO <fs_tqual_char>.
                CLEAR <fs_squal_chat>.
                ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_fmfphdr-tplnr_fl.
                ENDIF.

                ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_fmfphdr-aufnr.
                ENDIF.

                ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_fmfphdr-erdat.
                ENDIF.

                ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char_qm01>
                    TO FIELD-SYMBOL(<fs_ersteldat>).
                  IF sy-subrc EQ 0.
                    <fs_vqual_char> = <fs_ersteldat>.
                  ENDIF.
                ENDIF.

                ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_qals-werk.
                ENDIF.

                ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char> TO <fs_vqual_char>.
                IF sy-subrc EQ 0.
                  <fs_vqual_char> = ls_qals-selmatnr.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

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

  IF p_art IS INITIAL.
    MESSAGE TEXT-007 TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM handle_hotspot_click .

  DATA : ls_selected_row   TYPE lvc_s_row,
         ls_rowid          TYPE lvc_s_row,
         ls_layout         TYPE lvc_s_layo,
         lv_ilot_qm01      TYPE qplos,
         lv_ilot_qm03      TYPE qplos,
         lv_count          TYPE int2,
         lv_tplnr_fl_qm01  TYPE /agri/gltplnr_fl,
         lv_original_field TYPE zabs_e_sqmerknr,
         lv_mess           TYPE string,
         lt_rows           TYPE lvc_t_row,
         ls_rows           TYPE lvc_s_row,
         lt_rows_total     TYPE STANDARD TABLE OF ty_row INITIAL SIZE 0,
         lt_rows_aux       TYPE STANDARD TABLE OF ty_row INITIAL SIZE 0,
         ls_row_aux        LIKE LINE OF lt_rows_aux,
         lo_tqm03          TYPE REF TO data.

  FIELD-SYMBOLS : <fs_ilot_qm01>     TYPE any,
                  <fs_ilot_qm03>     TYPE any,
                  <fs_tplnr_fl_qm01> TYPE any,
                  <fs_count>         TYPE any,
                  <value>            TYPE any,
                  <value2>           TYPE any,
                  <ls_row_aux>       LIKE LINE OF lt_rows_aux,
                  <fs_f_ins0230030>  TYPE any,
                  <fs_f_ins0240040>  TYPE any,
                  <fs_f_ins0250050>  TYPE any.

*--BOC-T_T.KONNO-10.19.20
  SELECT *
    FROM zabs_qual_char
    INTO TABLE @DATA(lt_formula)
   WHERE art EQ @p_art.
*--EOC-T_T.KONNO-10.19.20

*---Preparing dynamic internal table.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fcat_qm03
    IMPORTING
      ep_table                  = lo_tqm03
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  ASSIGN lo_tqm03->* TO <fs_tqm03>.

  IF <fs_tqual_char_qm01> IS ASSIGNED
  AND <fs_tqual_char_qm03> IS ASSIGNED.
    DATA(lv_qm01) = lines( <fs_tqual_char_qm01> ).
    CLEAR ls_rowid.
    SORT gt_qm01 BY prueflos ppsortkey.
    SORT gt_last_created BY prueflos ppsortkey.
    DO lv_qm01 TIMES.
      ADD 1 TO ls_rowid-index.
      ls_selected_row = ls_rowid.
      READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> INDEX ls_rowid-index.
      IF <fs_squal_char_qm01> IS ASSIGNED.
        ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01> TO <fs_ilot_qm01>.
        ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char_qm01>
          TO FIELD-SYMBOL(<fs_key_qm01>).
        IF <fs_ilot_qm01> IS NOT ASSIGNED
        OR <fs_key_qm01> IS NOT ASSIGNED.
          EXIT.
        ENDIF.
*-- Consider higher counter value
        READ TABLE gt_last_created TRANSPORTING NO FIELDS
          WITH KEY prueflos  = <fs_ilot_qm01>
                   ppsortkey = <fs_key_qm01> BINARY SEARCH.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <fs_ilot_qm01> IS ASSIGNED.
          lv_ilot_qm01 = <fs_ilot_qm01>.
*-- Checking whether create button initial or not.
          ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char_qm01> TO <fs_tplnr_fl_qm01>.
          IF <fs_tplnr_fl_qm01> IS ASSIGNED.
            lv_tplnr_fl_qm01 = <fs_tplnr_fl_qm01>.
          ENDIF.

          ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_squal_char_qm01> TO <fs_count>.
          IF <fs_count> IS ASSIGNED.
            lv_count = <fs_count>.
            lv_count = lv_count - 1.
          ENDIF.

          DATA(ls_rowid_aux) = ls_rowid.
          ls_rowid_aux-index = ls_rowid-index - lv_count.

*--BOC-T_T.KONNO-10.19.20
*          REFRESH lt_rows.
*          LOOP AT <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> FROM ls_rowid_aux-index.
*            ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqual_char>.
*            IF <fs_vqual_char> IS ASSIGNED.
*              IF <fs_vqual_char> EQ lv_ilot_qm01.
***Modify Start Added below lines #INC0020624 On 19/09/2020 T_C.KARANAM
*                ASSIGN COMPONENT 'F_INS0230030' OF STRUCTURE <fs_squal_char_qm01> TO <fs_f_ins0230030>.
*                ASSIGN COMPONENT 'F_INS0240040' OF STRUCTURE <fs_squal_char_qm01> TO <fs_f_ins0240040>.
*                ASSIGN COMPONENT 'F_INS0250050' OF STRUCTURE <fs_squal_char_qm01> TO <fs_f_ins0250050>.
*                IF <fs_f_ins0230030> IS NOT INITIAL OR
*                   <fs_f_ins0240040> IS NOT INITIAL OR
*                   <fs_f_ins0250050> IS NOT INITIAL.
*                  ls_rows-index = sy-tabix.
*                  APPEND ls_rows TO lt_rows.
*                ENDIF.
***Modify End of Added lines for #INC0020624 On 24/09/2020 T_C.KARANAM
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.

          DATA(lt_formula_aux) = lt_formula[].
          SORT lt_formula_aux BY d_mkmnr.
          DATA(lt_fcat_qm01) = gt_fcat_qm01[].
          DELETE lt_fcat_qm01 WHERE fieldname(5) NE 'F_INS'.
          SORT lt_fcat_qm01 BY fieldname.
*-- Fetching country code and plant for respective terrain.
          READ TABLE gt_glflot INTO DATA(ls_glflot)
            WITH KEY tplnr_fl = lv_tplnr_fl_qm01 BINARY SEARCH.
          IF sy-subrc EQ 0.
            DELETE lt_formula_aux WHERE bukrs NE ls_glflot-bukrs.
          ENDIF.

          REFRESH lt_rows.
          LOOP AT <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01> FROM ls_rowid_aux-index.
            DATA(lv_tabix) = sy-tabix.
            ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01> TO <fs_vqual_char>.
            IF <fs_vqual_char> IS ASSIGNED.
              IF <fs_vqual_char> EQ lv_ilot_qm01.
                LOOP AT lt_fcat_qm01 INTO DATA(ls_fcat_qm01).
                  lv_original_field = ls_fcat_qm01-fieldname(8).
                  READ TABLE lt_formula_aux INTO DATA(ls_formula_aux)
                    WITH KEY d_mkmnr = lv_original_field BINARY SEARCH.
                  WHILE sy-subrc EQ 0.
                    DATA(lv_tabix_aux) = sy-tabix + 1.
                    UNASSIGN <ls_row_aux>.
                    READ TABLE lt_rows_aux ASSIGNING <ls_row_aux>
                      WITH KEY qm01_row   = lv_tabix
                               qm01_field = ls_fcat_qm01-fieldname
                               qm03_field = ls_formula_aux-r_mkmnr.
                    IF sy-subrc NE 0.
                      INSERT INITIAL LINE INTO TABLE lt_rows_aux
                        ASSIGNING <ls_row_aux>.
                    ENDIF.
                    IF <ls_row_aux> IS ASSIGNED.
                      <ls_row_aux>-prueflos   = <fs_ilot_qm01>.
                      <ls_row_aux>-qm01_row   = lv_tabix.
                      <ls_row_aux>-qm01_field = ls_fcat_qm01-fieldname.
                      <ls_row_aux>-qm03_field = ls_formula_aux-r_mkmnr.
                      ASSIGN COMPONENT ls_fcat_qm01-fieldname
                        OF STRUCTURE <fs_squal_char_qm01> TO FIELD-SYMBOL(<lv_value>).
                      IF sy-subrc EQ 0.
                        IF <lv_value> IS NOT INITIAL.
                          <ls_row_aux>-not_null = abap_true.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                    READ TABLE lt_formula_aux INTO ls_formula_aux
                      INDEX lv_tabix_aux COMPARING d_mkmnr.
                  ENDWHILE.
                ENDLOOP.
              ELSE.
                EXIT.
              ENDIF.
            ENDIF.
          ENDLOOP.

          lt_rows_total[] = lt_rows_aux[].
          DELETE lt_rows_aux WHERE not_null IS INITIAL.
          SORT lt_rows_aux BY qm01_row qm03_field not_null.
          DELETE ADJACENT DUPLICATES FROM lt_rows_aux
            COMPARING qm01_row qm03_field not_null.
          IF <fs_ilot_qm01> IS NOT INITIAL.
            DELETE lt_rows_aux WHERE prueflos NE <fs_ilot_qm01>.
          ENDIF.
*--EOC-T_T.KONNO-10.19.20

*--BOC-T_T.KONNO-10.19.20
*          PERFORM calculations USING lt_rows
*                                     lv_tplnr_fl_qm01.
          PERFORM calculations USING lt_rows_aux   "new table
                                     lt_rows       "old table
                                     lv_tplnr_fl_qm01.
*--EOC-T_T.KONNO-10.19.20

          READ TABLE <fs_tqual_char_qm01> ASSIGNING <fs_squal_char_qm01>
            INDEX ls_selected_row-index.
          IF <fs_tqual_char_qm03> IS NOT INITIAL.
            DATA(lv_found) = abap_false.
            LOOP AT <fs_tqual_char_qm03> ASSIGNING FIELD-SYMBOL(<fs_qm03>).
              ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01> TO <value>.
              ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_qm03> TO <value2>.
              CHECK sy-subrc = 0
                AND <value> = <value2>.

              ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_squal_char_qm01>
                TO FIELD-SYMBOL(<lv_qm01_key>).
              IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'PPSORTKEY' OF STRUCTURE <fs_qm03>
                  TO FIELD-SYMBOL(<lv_qm03_key>).
                IF sy-subrc EQ 0.
                  IF <lv_qm01_key> NE <lv_qm03_key>.
*                    CLEAR <fs_squal_char_qm03>.
                    CONTINUE.
                  ELSE.
                    lv_found = abap_true.
                  ENDIF.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'WERK' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'ENTSTEHDAT' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'ERSTELDAT' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'TPLNR_FL' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.

              ENDIF.

              ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <value>.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_qm03> TO <value>.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'COUNT' OF STRUCTURE <fs_squal_char_qm03> TO <value2>.
                IF sy-subrc = 0.
                  <value2> = <fs_count>.
                ENDIF.
              ENDIF.

              IF lv_found EQ abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF <fs_squal_char_qm03> IS INITIAL.
            lv_mess = TEXT-023.
            ASSIGN COMPONENT 'PRUEFLOS' OF STRUCTURE <fs_squal_char_qm01> TO <value>.
            REPLACE '&1' IN lv_mess WITH <value>.
            MESSAGE lv_mess TYPE 'I' DISPLAY LIKE 'W'.
            RETURN.
          ENDIF.

          IF <fs_squal_char_qm03> IS ASSIGNED
          AND lv_found EQ abap_true
          AND <fs_tqm03> IS ASSIGNED.
            APPEND <fs_squal_char_qm03> TO <fs_tqm03>.
            REFRESH lt_rows_aux.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.

*-- Display for qm03 records.
  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-grid_title = TEXT-017.

  IF sy-batch EQ abap_false.
    IF <fs_tqm03> IS ASSIGNED
    AND <fs_tqm03> IS NOT INITIAL.
      CALL METHOD gobj_ref_grid_qm03->set_table_for_first_display
        EXPORTING
          is_layout                     = ls_layout
        CHANGING
          it_outtab                     = <fs_tqm03>
          it_fieldcatalog               = gt_fcat_qm03
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  IF so_werks[] IS INITIAL.
*--O parâmetro Centro é de preenchimento obrigatório!
    MESSAGE i250(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_matnr[] IS INITIAL.
*--O parâmetro Material é de preenchimento obrigatório!
    MESSAGE i251(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_erdat[] IS INITIAL.
*--Informar a Data de Criação do Ponto de Inspeção!
    MESSAGE i252(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
