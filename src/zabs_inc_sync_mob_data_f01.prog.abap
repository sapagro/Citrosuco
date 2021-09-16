*----------------------------------------------------------------------*
***INCLUDE ZABS_INC_SYNC_MOB_DATA_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form READ_ACCOMPLISHMENT_SHEET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM read_accomplishment_sheet .

  REFRESH: gt_aufk, gt_fmacitm, gt_activity.

*--Fetching accomplishment data
  SELECT h~accom, h~actyp, h~gjahr, h~bukrs,
         h~werks, h~status, i~posnr, i~tplnr,
         i~tmatnr, i~aufnr, i~idresource,
         i~arbpl, i~equnr, i~idactvl, i~idactve
    FROM /agri/fmachdr AS h
    INNER JOIN /agri/fmacitm AS i
    ON h~accom EQ i~accom
    INTO TABLE @gt_fmacitm
   WHERE h~actyp   IN @s_actyp[]
     AND h~werks   IN @s_werks[]
     AND h~status  NE @zcl_abs_abap_maintain=>c_ac_status_deleted
     AND i~tplnr   IN @s_tplnr[]
     AND i~tmatnr  IN @s_tmatnr[]
     AND i~aufnr   IN @s_aufnr[]
     AND i~arbpl   IN @s_arbpl[]
     AND h~strtdat IN @s_begda[]
     AND h~findat  IN @s_endda[].

  IF sy-subrc EQ 0.
    SELECT k~aufnr, k~auart, k~autyp, k~bukrs,
           k~werks, k~kokrs, k~waers, k~loekz,
           o~gasmg, o~gamng, o~gmein, o~plnbez,
           o~plnty, o~plnnr, o~plnal, o~stlty,
           o~stlbez, o~stlst, o~stlnr
      FROM aufk AS k
      INNER JOIN afko AS o
      ON k~aufnr = o~aufnr
      INTO TABLE @gt_aufk
      FOR ALL ENTRIES IN @gt_fmacitm
     WHERE k~aufnr = @gt_fmacitm-aufnr.

    IF s_aufnr[] IS NOT INITIAL.
      DELETE: gt_fmacitm WHERE aufnr NOT IN s_aufnr[],
              gt_aufk    WHERE aufnr NOT IN s_aufnr[].
    ENDIF.

    SELECT *
      FROM /agri/fmacact
      INTO TABLE @gt_activity
      FOR ALL ENTRIES IN @gt_fmacitm
     WHERE idactv = @gt_fmacitm-idactve.
  ENDIF.

  SORT: gt_aufk     BY aufnr,
        gt_fmacitm  BY accom posnr,
        gt_activity BY idactv.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_ACCOMPLISHMENT_SHEET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_accomplishment_sheet .

  TYPES: BEGIN OF ly_confirmed,
           aufnr TYPE aufnr,
         END OF ly_confirmed.

  DATA: lt_conf_msg  TYPE /agri/t_gprolog,
        lt_acitm_new TYPE /agri/t_fmfmacitm,
        lt_acitm_all TYPE /agri/t_fmfmacitm,
        lt_confirmed TYPE TABLE OF ly_confirmed,
        ls_confirmed LIKE LINE OF lt_confirmed,
        ls_acitm_new LIKE LINE OF lt_acitm_new,
        ls_message   TYPE /agri/s_gprolog,
        lv_confirmed TYPE abap_bool,
        lv_accom     TYPE /agri/fmaccom,
        lv_msgv1     TYPE sy-msgv1,
        lv_msgli     TYPE sy-msgli,
        lv_subrc     TYPE sysubrc.

  REFRESH: gt_messages.

  IF gt_fmacitm[] IS NOT INITIAL.
    SELECT a~idactv, a~rstype, a~bill, a~actype,
           a~zzactcg, i~accom, i~posnr, i~aufnr,
           i~idactvl, i~idactve, i~zzconftyp
      FROM /agri/fmacact AS a
      INNER JOIN /agri/fmacitm AS i
      ON i~idactve = a~idactv
      INTO TABLE @DATA(lt_billable)
      FOR ALL ENTRIES IN @gt_fmacitm
     WHERE i~accom  = @gt_fmacitm-accom
       AND a~idactv = @gt_fmacitm-idactvl
       AND a~bill   = 'YES'.

    SELECT a~idactv, a~rstype, a~bill, a~actype,
           a~zzactcg, i~accom, i~posnr, i~aufnr,
           i~idactvl, i~idactve, i~zzconftyp
      FROM /agri/fmacact AS a
      INNER JOIN /agri/fmacitm AS i
      ON i~idactve = a~idactv
      APPENDING TABLE @lt_billable
      FOR ALL ENTRIES IN @gt_fmacitm
     WHERE i~accom  = @gt_fmacitm-accom
       AND a~idactv = @gt_fmacitm-idactve
       AND a~bill   = 'YES'.

    SORT lt_billable BY accom posnr aufnr.

    LOOP AT gt_fmacitm INTO DATA(ls_fmacitm).
      READ TABLE lt_acitm_all TRANSPORTING NO FIELDS
        WITH KEY accom = ls_fmacitm-accom.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.

      CLEAR: lv_subrc, lv_msgv1, lv_msgli.
      CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
        EXPORTING
          accom          = ls_fmacitm-accom
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        lv_subrc = sy-subrc.
        lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
        MESSAGE i017(/agri/fmac) WITH ls_fmacitm-accom lv_msgv1 INTO lv_msgli.
        sy-msgli = lv_msgli.
        CLEAR ls_message.
        ls_message-msgty   = 'E'.
        ls_message-msgid   = '/AGRI/FMAC'.
        ls_message-msgno   = 017.
        ls_message-msgv1   = ls_fmacitm-accom.
        ls_message-msgv2   = lv_msgv1.
        APPEND ls_message TO gt_messages.
      ENDIF.

      REFRESH: gt_accom, gt_acdoc.
      gs_accom = ls_fmacitm-accom.
      APPEND gs_accom TO gt_accom.

*-- Fetch the Accomplishment data
      CALL FUNCTION '/AGRI/FMAC_VIEW'
        EXPORTING
          it_accom       = gt_accom
        IMPORTING
          et_acdoc       = gt_acdoc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.

      IF gt_acdoc[] IS NOT INITIAL.
        READ TABLE gt_acdoc ASSIGNING FIELD-SYMBOL(<ls_acdoc>) INDEX 1.
        IF sy-subrc EQ 0.
          gt_acitm_new[] = <ls_acdoc>-x-acitm[].

          REFRESH: lt_acitm_new, lt_confirmed.
          DATA(lv_update) = abap_false.
          LOOP AT gt_acitm_new ASSIGNING FIELD-SYMBOL(<ls_acitm_new>).
*-- Entradas desordenadas
            READ TABLE lt_confirmed TRANSPORTING NO FIELDS
              WITH KEY aufnr = <ls_acitm_new>-aufnr.
            IF sy-subrc NE 0.
              lv_confirmed = abap_false.
            ELSE.
              lv_confirmed = abap_true.
            ENDIF.

            READ TABLE gt_aufk INTO DATA(ls_aufk)
              WITH KEY aufnr = <ls_acitm_new>-aufnr BINARY SEARCH.
            IF sy-subrc NE 0.
              CONTINUE.
            ELSE.
              READ TABLE lt_billable INTO DATA(ls_billable)
                WITH KEY accom = <ls_acitm_new>-accom
                         posnr = <ls_acitm_new>-posnr
                         aufnr = <ls_acitm_new>-aufnr BINARY SEARCH.
*-- Faturável
              IF sy-subrc EQ 0.
                IF <ls_acitm_new>-zzconftyp EQ c_conftype-total
                OR <ls_acitm_new>-zzconftyp EQ c_conftype-complement.
                  IF lv_confirmed EQ abap_false.
                    <ls_acitm_new>-zzlmnga = ls_aufk-gamng.
                  ENDIF.
                  <ls_acitm_new>-zzmeinh = ls_aufk-gmein.
                  ls_confirmed-aufnr = <ls_acitm_new>-aufnr.
                  APPEND ls_confirmed TO lt_confirmed.
                ELSEIF <ls_acitm_new>-zzconftyp EQ c_conftype-partial.
                  CLEAR <ls_acitm_new>-zzlmnga.
                  <ls_acitm_new>-zzmeinh = ls_aufk-gmein.
                ENDIF.
                lv_update = abap_true.
                <ls_acitm_new>-updkz = c_updkz_update.
              ENDIF.
            ENDIF.

            ls_acitm_new = <ls_acitm_new>.
            APPEND ls_acitm_new TO lt_acitm_new.
            APPEND ls_acitm_new TO lt_acitm_all.
          ENDLOOP.

          IF lv_update EQ abap_true.
            <ls_acdoc>-updkz = c_updkz_update.
            <ls_acdoc>-x-achdr-updkz = c_updkz_update.
            <ls_acdoc>-x-acitm[] = gt_acitm_new[].

            CLEAR gs_acdoc_new.
            REFRESH gt_fmac_messages.
*-- Create the new Accomplishment sheet
            CALL FUNCTION 'ZABS_FMAC_CREATE'
              EXPORTING
                is_fmacdoc  = <ls_acdoc>
                iv_mcnf     = abap_true
              IMPORTING
                es_fmac_doc = gs_acdoc_new
                et_messages = gt_fmac_messages.

            READ TABLE gt_fmac_messages INTO DATA(ls_fmac_msg)
              WITH KEY msgty = 'E'.
            IF sy-subrc EQ 0.
              IF gt_fmac_messages[] IS NOT INITIAL
              AND lv_accom NE ls_fmacitm-accom.
*-- Folha de Realização &1
                ls_message-msgty = 'E'.
                ls_message-msgid = 'ZFMFP'.
                ls_message-msgno = 175.
                ls_message-msgv1 = ls_fmacitm-accom.
                APPEND ls_message TO gt_messages.
              ENDIF.

              LOOP AT gt_fmac_messages INTO ls_fmac_msg.
                CLEAR ls_message.
                ls_message-msgty   = ls_fmac_msg-msgty.
                ls_message-msgid   = ls_fmac_msg-msgid.
                ls_message-msgno   = ls_fmac_msg-msgno.
                ls_message-msgv1   = ls_fmac_msg-msgv1.
                ls_message-msgv2   = ls_fmac_msg-msgv2.
                ls_message-msgv3   = ls_fmac_msg-msgv3.
                ls_message-msgv4   = ls_fmac_msg-msgv4.
                APPEND ls_message TO gt_messages.
              ENDLOOP.
            ELSE.
              REFRESH gt_fmac_messages.
              <ls_acdoc> = gs_acdoc_new.
              CLEAR gs_acdoc_new.
              CALL FUNCTION 'ZABS_FMAC_CONFIRMATION'
                EXPORTING
                  is_fmacdoc  = <ls_acdoc>
                  iv_mcnf     = abap_true
                IMPORTING
                  es_fmac_doc = gs_acdoc_new
                  et_messages = gt_fmac_messages.

              <ls_acdoc> = gs_acdoc_new.
              lt_conf_msg[] = gt_fmac_messages[].

              IF gt_fmac_messages[] IS NOT INITIAL
              AND lv_accom NE ls_fmacitm-accom.
*-- Folha de Realização &1
                READ TABLE gt_fmac_messages TRANSPORTING NO FIELDS
                  WITH KEY msgty = 'E'.
                IF sy-subrc EQ 0.
                  ls_message-msgty = 'E'.
                ELSE.
                  ls_message-msgty = 'S'.
                ENDIF.
                ls_message-msgid = 'ZFMFP'.
                ls_message-msgno = 175.
                ls_message-msgv1 = ls_fmacitm-accom.
                APPEND ls_message TO gt_messages.
              ENDIF.

              IF ( lv_accom NE ls_fmacitm-accom )
              OR ( ( lv_accom EQ ls_fmacitm-accom ) AND
                   ( lt_conf_msg[] NE gt_fmac_messages[] ) ).
                LOOP AT gt_fmac_messages INTO ls_fmac_msg.
                  CLEAR ls_message.
                  ls_message-msgty = ls_fmac_msg-msgty.
                  ls_message-msgid = ls_fmac_msg-msgid.
                  ls_message-msgno = ls_fmac_msg-msgno.
                  ls_message-msgv1 = ls_fmac_msg-msgv1.
                  ls_message-msgv2 = ls_fmac_msg-msgv2.
                  ls_message-msgv3 = ls_fmac_msg-msgv3.
                  ls_message-msgv4 = ls_fmac_msg-msgv4.
                  APPEND ls_message TO gt_messages.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMAC'
        EXPORTING
          accom = ls_fmacitm-accom.
      lv_accom = ls_fmacitm-accom.
    ENDLOOP.

    IF gt_fmacitm[] IS NOT INITIAL.
      SELECT *
        FROM /agri/fmfphdr
        INTO TABLE @DATA(lt_fmfphdr)
        FOR ALL ENTRIES IN @gt_fmacitm
       WHERE aufnr = @gt_fmacitm-aufnr.

      IF sy-subrc EQ 0.
        SORT lt_fmfphdr BY aufnr.
        DELETE ADJACENT DUPLICATES FROM lt_fmfphdr COMPARING ALL FIELDS.

        SELECT aufnr, gamng, igmng
          FROM afko
          INTO TABLE @DATA(lt_afko)
          FOR ALL ENTRIES IN @lt_fmfphdr
         WHERE aufnr = @lt_fmfphdr-aufnr.

        IF sy-subrc EQ 0.
          SORT lt_fmfphdr BY aufnr.
          SORT lt_afko BY aufnr.

          LOOP AT lt_fmfphdr ASSIGNING FIELD-SYMBOL(<ls_fmfphdr>).
            DATA(lv_changed) = abap_false.
            DATA(lv_tabix) = sy-tabix.
            READ TABLE lt_afko INTO DATA(ls_afko)
              WITH KEY aufnr = <ls_fmfphdr>-aufnr BINARY SEARCH.
            IF sy-subrc EQ 0.
              IF <ls_fmfphdr>-gwemg NE ls_afko-igmng.
                <ls_fmfphdr>-gwemg = ls_afko-igmng.
                lv_changed = abap_true.
              ENDIF.
            ENDIF.
            IF lv_changed EQ abap_false.
              DELETE lt_fmfphdr INDEX lv_tabix.
            ENDIF.
          ENDLOOP.

          IF lt_fmfphdr[] IS NOT INITIAL.
            MODIFY /agri/fmfphdr FROM TABLE lt_fmfphdr.
            IF sy-subrc EQ 0.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*-- Não existem dados para os parâmetros informados!
    CLEAR ls_message.
    ls_message-msgty   = 'S'.
    ls_message-msgid   = 'ZFMFP'.
    ls_message-msgno   = 171.
    APPEND ls_message TO gt_messages.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_parameters .

  IF s_actyp[] IS INITIAL.
*-- O parâmetro Tipo de Realização deve ser preenchido!
    MESSAGE i172(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_werks[] IS INITIAL.
*-- O parâmetro Centro deve ser preenchido!
    MESSAGE i173(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_arbpl[] IS INITIAL.
*-- O parâmetro Centro de Trabalho deve ser preenchido!
    MESSAGE i174(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
