METHOD zabs_order_create .

  DATA: lwa_params      TYPE ctu_params,
        lwa_prodord     TYPE /agri/s_glprodord,
        lwa_messages    TYPE bdcmsgcoll,
        lwa_afko        TYPE afko,
        lv_mess         TYPE string,
        lt_msglog       TYPE tab_bdcmsgcoll,
        ls_mast         TYPE mast,
        lt_positions    TYPE /agri/t_glprodord_mod,
        ls_positions    TYPE /agri/s_glprodord_mod,
        lcl_settlrule   TYPE REF TO /agri/cl_gl_settl_rule,
        lcx_settl_rule  TYPE REF TO /agri/cx_gl_settl_rule,
        lv_schsetr      TYPE /agri/glschsetr,
        lv_clssfld      TYPE char10,
        lv_labor        TYPE labor,
        lv_dispo        TYPE dispo,
        lv_matnr        TYPE matnr,
        lv_werks        TYPE iwerk,
        lv_sdate        TYPE datum.

  CLEAR: e_aufnr.
  REFRESH: et_messages.

  MOVE-CORRESPONDING is_order_comm TO lwa_prodord.
  lwa_prodord-tplnr = is_order_comm-tplnr_fl.
  lwa_prodord-werks = is_order_comm-iwerk.

  CALL METHOD /agri/cl_abgl_utils=>unit_value_convert_external
    EXPORTING
      i_input        = lwa_prodord-gmein
    IMPORTING
      e_ouput        = lwa_prodord-gmein
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

**-- Get material settings
  SELECT SINGLE a~labor c~dispo
    INTO (lv_labor, lv_dispo)
    FROM mara AS a JOIN marc AS c
      ON a~matnr = c~matnr
   WHERE a~matnr = lwa_prodord-matnr "#EC CI_NOORDER
     AND c~werks = lwa_prodord-werks.

*-- Building classification field
  CONCATENATE lv_labor lv_dispo INTO lv_clssfld.

* Get Settlement rule. Header
  SELECT SINGLE schsetr
    INTO lv_schsetr
    FROM /agri/glsrh   "#EC CI_GENBUFF
   WHERE werks = lwa_prodord-werks "#EC CI_NOORDER
     AND clssfld = lv_clssfld.
  IF sy-subrc = 0.

* Object to determine settlement rule
    CREATE OBJECT lcl_settlrule.

*get settlement rule information
    TRY.
        CALL METHOD lcl_settlrule->settlement_rule_get
          EXPORTING
            i_schsetr = lv_schsetr
            i_tplnr   = lwa_prodord-tplnr
            i_matnr   = lwa_prodord-matnr
            i_werks   = lwa_prodord-werks
            i_cmnum   = is_order_comm-cmnum
            i_datum   = lwa_prodord-gstrp
          IMPORTING
            e_konty   = lwa_prodord-konty
            e_empge   = lwa_prodord-empge.

      CATCH /agri/cx_gl_settl_rule INTO lcx_settl_rule.
        lv_mess = lcx_settl_rule->if_message~get_text( ).
    ENDTRY.
  ENDIF.

  IF lv_mess IS NOT INITIAL.
    MESSAGE ID    '/AGRI/GLSR'
          TYPE    'E'
          NUMBER  '033'
          WITH    lv_mess
          INTO    sy-msgli.

    lwa_messages-msgid   = sy-msgid.
    lwa_messages-msgtyp  = sy-msgty.
    lwa_messages-msgnr   = sy-msgno.
    lwa_messages-msgv1   = sy-msgv1.
    lwa_messages-msgv2   = sy-msgv2.
    lwa_messages-msgv3   = sy-msgv3.
    lwa_messages-msgv4   = sy-msgv4.
    APPEND lwa_messages TO et_messages.
    RETURN.
  ENDIF.
  CLEAR lv_mess.
  lwa_prodord-prozs  = '100'.
  IF is_order_comm-proid IS NOT INITIAL.
    conversion_exit_abpsp_output is_order_comm-proid lwa_prodord-posid.
  ENDIF.

  lwa_params-dismode  = mc_dismode.
  lwa_params-nobiend  = mc_nobiend.
  lwa_params-racommit = mc_racommit.
  lwa_params-nobinpt  = mc_nobinpt.

  et_messages =
  /agri/cl_glco_process=>zabs_production_order_create( is_prodord = lwa_prodord
                                                       is_params  = lwa_params ).

  READ TABLE et_messages INTO lwa_messages
  WITH KEY msgid = 'CO'
           msgnr = '100'.
  IF sy-subrc = 0.
    e_aufnr =  lwa_messages-msgv1.
    conversion_exit_alpha_input e_aufnr e_aufnr.
    REFRESH: et_messages.
    IF i_commit_work IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.

  DO 10 TIMES.
     CALL FUNCTION '/AGRI/G_AFKO_PRODNET_READ'
       EXPORTING
        i_aufnr         = e_aufnr
       IMPORTING
        E_PRODNET       = lwa_afko-prodnet
       EXCEPTIONS
        NOT_FOUND       = 1
        OTHERS          = 2.
*---
    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF lwa_afko-prodnet IS NOT INITIAL.

* Get Material to BOM Link information
    SELECT SINGLE * "#EC CI_ALL_FIELDS_NEEDED
      INTO ls_mast
      FROM mast
     WHERE matnr = is_order_comm-matnr. "#EC CI_NOORDER
    IF sy-subrc = 0.
      SELECT idnrk
        INTO CORRESPONDING FIELDS OF TABLE lt_positions
        FROM stpo
       WHERE stlty = 'M'
         AND stlnr = ls_mast-stlnr
         AND lkenz = space.
      IF sy-subrc = 0.
        LOOP AT lt_positions INTO ls_positions.
          ls_positions-projn = lwa_prodord-posid.
          MODIFY lt_positions FROM ls_positions.
        ENDLOOP.
      ENDIF.
    ENDIF.

* Update WBS-Element for inferior orders
    CALL METHOD production_order_modify
      EXPORTING
        i_aufnr     = e_aufnr
        i_matnr     = is_order_comm-matnr
        it_bomitems = lt_positions
      IMPORTING
        et_messtab  = lt_msglog.

    DELETE lt_msglog WHERE msgid = 'CO' AND msgnr = '100'.
    IF lt_msglog[] IS NOT INITIAL.
      APPEND LINES OF lt_msglog TO et_messages.
    ENDIF.

  ENDIF.

ENDMETHOD.
