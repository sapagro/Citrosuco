FUNCTION zabs_fm_checkin_fruta.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CENTRO) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_CAVALO) TYPE  ZABS_DEL_PLACA OPTIONAL
*"     REFERENCE(I_REMESSA) TYPE  ZREMS OPTIONAL
*"     REFERENCE(I_IMOVEL_ARRENDADO) TYPE  ZFMPRIMOVEL OPTIONAL
*"     REFERENCE(I_FABRICA_DESTINO) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_SEMIREBOQUE1) TYPE  ZFMPRSEMIREB1 OPTIONAL
*"     REFERENCE(I_SEMIREBOQUE2) TYPE  ZFMPRSEMIREB2 OPTIONAL
*"     REFERENCE(I_PESO_TARA) TYPE  ZFMPRFTW OPTIONAL
*"     REFERENCE(I_TIPO_VIAGEM) TYPE  ZABS_DEL_TIPO_VIAGEM OPTIONAL
*"     REFERENCE(I_VIAGEM_ORIGINAL) TYPE  ZABS_DEL_VIAGEM_ORIG OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      CENTRO_INVALIDO
*"      FABRICA_DESTINO_INVALIDA
*"----------------------------------------------------------------------

  DATA: lt_prdoc     TYPE /agri/t_fmpr_doc,
        ls_prdoc     TYPE /agri/s_fmpr_doc,
        ls_message   TYPE /agri/s_gprolog,
        ls_t001w     TYPE t001w,
        lv_prnum     TYPE /agri/fmprnum,
        lv_numki     TYPE numki VALUE '01',
        lv_subrc     TYPE int4,
        lv_failed    TYPE abap_bool,
        lv_stop_save TYPE xfeld.

  PERFORM plant_check USING i_centro
                   CHANGING lv_subrc.

  IF lv_subrc NE 0.
    INSERT INITIAL LINE INTO TABLE ct_messages
      ASSIGNING FIELD-SYMBOL(<ls_message>).
    IF sy-subrc EQ 0.
*-- Centro &1 inválido!
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = 225.
      <ls_message>-msgty = c_msg_type-error.
      <ls_message>-msgv1 = i_centro.
    ENDIF.
    RAISE centro_invalido.
  ENDIF.

  MOVE: c_updkz_new        TO ls_prdoc-updkz,
        sy-datum+0(4)      TO ls_prdoc-gjahr,
        i_centro           TO ls_prdoc-x-prhdr-werks,
        i_cavalo           TO ls_prdoc-x-prhdr-lic_plate,
        i_remessa          TO ls_prdoc-x-prhdr-zrems,
        i_imovel_arrendado TO ls_prdoc-x-prhdr-zarrend,
        i_peso_tara        TO ls_prdoc-x-prhdr-obgew,
        i_peso_tara        TO ls_prdoc-x-prhdr-zfmprftw,
        sy-datum+0(4)      TO ls_prdoc-x-prhdr-gjahr,
        i_semireboque1     TO ls_prdoc-x-prhdr-semireb1,
        i_semireboque2     TO ls_prdoc-x-prhdr-semireb2,
        cgl_const-fldty    TO ls_prdoc-x-prhdr-fldty,
        cgl_const-cmnum    TO ls_prdoc-x-prhdr-cmnum,
        cgl_const-bukrs    TO ls_prdoc-x-prhdr-bukrs,
        cgl_const-gtart    TO ls_prdoc-x-prhdr-gtart,
        cgl_const-status   TO ls_prdoc-x-prhdr-status,
        sy-datum           TO ls_prdoc-x-prhdr-datum,
        c_updkz_new        TO ls_prdoc-x-prhdr-updkz,
        sy-uzeit           TO ls_prdoc-x-prhdr-uzeit,
        sy-datum           TO ls_prdoc-x-prhdr-budat,
        abap_true          TO ls_prdoc-x-prhdr-fruta_mercado,
        i_tipo_viagem      TO ls_prdoc-x-prhdr-tipo_viagem,
        i_viagem_original  TO ls_prdoc-x-prhdr-viagem_original.

  IF i_fabrica_destino IS NOT INITIAL.
    CALL FUNCTION 'T001W_SINGLE_READ'
      EXPORTING
        t001w_werks = i_fabrica_destino
      IMPORTING
        wt001w      = ls_t001w
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      ls_prdoc-x-prhdr-zzwerks_dest = ls_t001w-werks.
    ENDIF.
  ENDIF.

  PERFORM wscale_get USING i_centro
                  CHANGING ls_prdoc-x-prhdr-wscale
                           ls_prdoc-x-prhdr-gewei.

  PERFORM ticket_number_generate USING ls_prdoc-x-prhdr
                                       lv_numki
                              CHANGING ls_prdoc-x-prhdr-prnum
                                       lv_failed.

  IF lv_failed EQ abap_false.
    ls_prdoc-prnum = ls_prdoc-x-prhdr-prnum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_prdoc-x-prhdr-prnum
      IMPORTING
        output = ls_prdoc-x-prhdr-prnum.

    APPEND ls_prdoc TO lt_prdoc.

    CALL FUNCTION '/AGRI/FMPR_SAVE'
      CHANGING
        ct_prdoc    = lt_prdoc
        ct_messages = ct_messages
      EXCEPTIONS
        no_change   = 1
        OTHERS      = 2.

    READ TABLE ct_messages TRANSPORTING NO FIELDS
      WITH KEY msgty = 'E'.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE ct_messages
        ASSIGNING <ls_message>.
      IF sy-subrc EQ 0.
*-- Viagem &1 criado com sucesso!
        <ls_message>-msgid = 'ZFMFP'.
        <ls_message>-msgno = 224.
        <ls_message>-msgty = c_msg_type-success.
        <ls_message>-msgv1 = ls_prdoc-x-prhdr-prnum.
      ENDIF.
    ENDIF.
  ELSE.
    INSERT INITIAL LINE INTO TABLE ct_messages
      ASSIGNING <ls_message>.
    IF sy-subrc EQ 0.
*-- Verificar subintervalo 01 do intervalo de numeração ZABS_FMPR!
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = 223.
      <ls_message>-msgty = 'E'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
