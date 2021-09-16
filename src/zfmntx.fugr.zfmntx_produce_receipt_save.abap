FUNCTION zfmntx_produce_receipt_save.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_PRNUM) TYPE  /AGRI/FMPRNUM
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_EQUNR) TYPE  /AGRI/FMEQUNR
*"     VALUE(I_ZREMS) TYPE  ZREMS
*"     VALUE(I_ARREND) TYPE  ZFMPRIMOVEL OPTIONAL
*"     VALUE(I_WERKS_DEST) TYPE  WERKS_D
*"     REFERENCE(I_SEMIREB1) TYPE  ZFMPRSEMIREB1 OPTIONAL
*"     REFERENCE(I_SEMIREB2) TYPE  ZFMPRSEMIREB2 OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      ERROR_PLANT
*"      TICKET_EXISTENTE
*"      TRANSPORTE_NAO_EXISTE
*"----------------------------------------------------------------------

  DATA: lt_prdoc            TYPE /agri/t_fmpr_doc,
        lv_conuter_internal TYPE int4,
        ls_prdoc            TYPE /agri/s_fmpr_doc,
        ls_message          TYPE /agri/s_gprolog,
        ls_t001w            TYPE t001w,
        lv_prnum            TYPE /agri/fmprnum,
        lv_subrc            TYPE int4,
        lv_funcname         TYPE rs38l-name,
        lv_rfcdest          TYPE rfcdest,
        lv_rfc_message      TYPE c LENGTH 200,
        lv_dlv_number       TYPE /scmtms/base_btd_id,
        lv_ordem_frete      TYPE ys4_st_read_data-external_doc_id,
        lv_transp_id        TYPE ys4_st_read_data-carrier,
        lv_transp_name      TYPE ys4_st_read_data-carr_name,
        lv_placa_cavalo     TYPE ys4_st_read_data-lic_plate,
        lv_placa_smr_1      TYPE ys4_st_read_data-lic_plate,
        lv_placa_smr_2      TYPE ys4_st_read_data-lic_plate,
        lv_stop_save        TYPE xfeld.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_prnum
    IMPORTING
      output = lv_prnum.

  IF lv_prnum NOT BETWEEN '6000000000'
                      AND '6999999999'.
*-- Número de Ticket & inválido!
    INSERT INITIAL LINE INTO TABLE ct_messages
      ASSIGNING FIELD-SYMBOL(<ls_message>).
    IF sy-subrc EQ 0.
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = 146.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgv1 = i_prnum.
    ENDIF.
    RETURN.
  ENDIF.

  PERFORM plant_check USING i_werks
                   CHANGING lv_subrc.
  IF lv_subrc NE 0.
    RAISE error_plant.
  ENDIF.

  PERFORM prnum_check USING i_prnum
                      CHANGING lv_subrc.

  IF lv_subrc  EQ 0.
*-- Ticket &1 já existe!
    INSERT INITIAL LINE INTO TABLE ct_messages
      ASSIGNING <ls_message>.
    IF sy-subrc EQ 0.
      <ls_message>-msgid = 'ZFMFP'.
      <ls_message>-msgno = 147.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgv1 = i_prnum.
    ENDIF.
    RAISE ticket_existente.
  ENDIF.

  MOVE: c_updkz_new      TO ls_prdoc-updkz,
        i_prnum          TO ls_prdoc-prnum,
        sy-datum+0(4)    TO ls_prdoc-gjahr,
        i_werks          TO ls_prdoc-x-prhdr-werks,
        i_prnum          TO ls_prdoc-x-prhdr-prnum,
        i_zrems          TO ls_prdoc-x-prhdr-zrems,
        i_arrend         TO ls_prdoc-x-prhdr-zarrend,
        sy-datum+0(4)    TO ls_prdoc-x-prhdr-gjahr,
        i_semireb1       TO ls_prdoc-x-prhdr-semireb1,
        i_semireb2       TO ls_prdoc-x-prhdr-semireb2,
        cgl_const-fldty  TO ls_prdoc-x-prhdr-fldty,
        cgl_const-cmnum  TO ls_prdoc-x-prhdr-cmnum,
        cgl_const-bukrs  TO ls_prdoc-x-prhdr-bukrs,
        cgl_const-gtart  TO ls_prdoc-x-prhdr-gtart,
        cgl_const-status TO ls_prdoc-x-prhdr-status,
        sy-datum         TO ls_prdoc-x-prhdr-datum,
        c_updkz_new      TO ls_prdoc-x-prhdr-updkz,
        sy-uzeit         TO ls_prdoc-x-prhdr-uzeit,
        sy-datum         TO ls_prdoc-x-prhdr-budat.

  IF i_werks_dest IS NOT INITIAL.
    CALL FUNCTION 'T001W_SINGLE_READ'
      EXPORTING
        t001w_werks = i_werks_dest
      IMPORTING
        wt001w      = ls_t001w
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      ls_prdoc-x-prhdr-zzwerks_dest = ls_t001w-werks.
    ENDIF.
  ENDIF.

  PERFORM wscale_get USING i_werks
                     CHANGING ls_prdoc-x-prhdr-wscale
                              ls_prdoc-x-prhdr-gewei.

  PERFORM weight_get USING i_prnum
                     CHANGING ls_prdoc-x-prhdr.

  lv_funcname = 'Y_S4TM_YL_BUSCARFC'.
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = lv_funcname
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'Y_S4TM_YL_BUSCARFC'
      IMPORTING
        rfc_name = lv_rfcdest.

    CALL FUNCTION 'RFC_PING'
      DESTINATION lv_rfcdest
      EXCEPTIONS
        communication_failure = 1 MESSAGE lv_rfc_message
        system_failure        = 2 MESSAGE lv_rfc_message
        OTHERS                = 3.

    IF sy-subrc EQ 0.
      lv_funcname = 'Y_S4TMF_BUSCA_DADOS_OF'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        DESTINATION lv_rfcdest
        EXPORTING
          funcname           = lv_funcname
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.

      IF sy-subrc EQ 0.
        lv_dlv_number = ls_prdoc-x-prhdr-zrems.
        CALL FUNCTION 'Y_S4TMF_BUSCA_DADOS_OF'
          DESTINATION lv_rfcdest
          EXPORTING
            i_dlv_number   = lv_dlv_number
          IMPORTING
            e_ordem_frete  = lv_ordem_frete
            e_transp_id    = lv_transp_id
            e_transp_name  = lv_transp_name
            e_placa_cavalo = lv_placa_cavalo
            e_placa_smr_1  = lv_placa_smr_1
            e_placa_smr_2  = lv_placa_smr_2.

        ls_prdoc-x-prhdr-semireb1 = lv_placa_smr_1.
        ls_prdoc-x-prhdr-semireb2 = lv_placa_smr_2.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ls_prdoc-x-prhdr-caval(5) EQ 'TRUCK'.
    ls_prdoc-x-prhdr-semireb1 = ls_prdoc-x-prhdr-lic_plate.
  ENDIF.

  APPEND ls_prdoc TO lt_prdoc.

  CALL FUNCTION '/AGRI/FMPR_SAVE'
    CHANGING
      ct_prdoc    = lt_prdoc
      ct_messages = ct_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

ENDFUNCTION.
