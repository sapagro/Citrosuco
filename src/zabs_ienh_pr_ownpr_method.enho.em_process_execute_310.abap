METHOD PROCESS_EXECUTE_310 .

   DATA: lv_bukrs    TYPE bukrs,
        lv_aufnr     TYPE aufnr,
        lv_werks     TYPE werks_d,
        lv_ntgew     TYPE ntgew,
         lv_tabix    TYPE sytabix,
        lv_gewei     TYPE gewei,
        lt_afvc_ord  TYPE /agri/t_fmconmat,
        lt_afvc_oper TYPE /agri/t_fmconmat,
        lv_initiator TYPE /agri/gdescr.
  DATA: lwa_return   TYPE bapiret2.
  DATA: lt_conmat    TYPE /agri/t_fmconmat,
        lt_return    TYPE ty_t_bapi_coru_return,
        lt_prvarec   TYPE /agri/t_fmprvarec.

  DATA: lref_document_process TYPE REF TO /agri/cl_fm_document_process.

  FIELD-SYMBOLS:
        <lwa_return> TYPE bapi_coru_return,
        <lwa_afvc_ord>  TYPE /agri/s_fmconmat,
        <lwa_afvc_oper> TYPE /agri/s_fmconmat.

* Company code
  lv_bukrs = get_company_code( ms_master-werks ).

 CALL METHOD get_yard_order
    EXPORTING
      i_bukrs               = lv_bukrs
      i_matnr               = i_matnr
      i_yaufnr              = i_yaufnr
    IMPORTING
      e_aufnr               = lv_aufnr
      e_werks               = lv_werks
    EXCEPTIONS
      order_not_found       = 1
      order_types_not_found = 2
      status_not_found      = 3
      OTHERS                = 4.
  IF sy-subrc = 0.

* <210-1> / <210-2>: Get inferior harvest orders
    SELECT DISTINCT k~aufnr k~plnbez
      INTO CORRESPONDING FIELDS OF TABLE lt_afvc_ord
      FROM afko AS k
     INNER JOIN aufk AS a
        ON k~aufnr = a~aufnr
     INNER JOIN jest AS j
        ON a~objnr = j~objnr
      WHERE k~aufnr EQ lv_aufnr
        AND j~stat   IN ('I0001', 'I0002')     " Allowed status for task orders
        AND j~inact = space.

* Get material to inferior orders
  LOOP AT lt_afvc_ord ASSIGNING <lwa_afvc_ord>.
* Clear
    CLEAR: lt_afvc_oper.
* Get operations
    lt_afvc_oper  = get_receipt_operation_list( <lwa_afvc_ord>-aufnr ).
* Assigning material
    LOOP AT lt_afvc_oper ASSIGNING <lwa_afvc_oper>.
      lv_tabix = sy-tabix.
      <lwa_afvc_oper>-aufnr  = <lwa_afvc_ord>-aufnr.
      <lwa_afvc_oper>-plnbez = <lwa_afvc_ord>-plnbez.
      MODIFY lt_afvc_oper FROM <lwa_afvc_oper> INDEX lv_tabix.
    ENDLOOP.
  ENDLOOP.

    IF lt_afvc_oper[] IS NOT INITIAL.

* <210-2>: Get quantity to confirm from document class settings
      CALL METHOD get_quantity_to_post
        EXPORTING
          i_confirm = 'X'
        IMPORTING
          e_ntgew   = lv_ntgew
          e_gewei   = lv_gewei.

* Instance
      CREATE OBJECT lref_document_process.

* <210-3>: Order confirmation
      CALL METHOD lref_document_process->confirmation_quant_material
        EXPORTING
          it_conmat      = lt_afvc_oper
          i_ntgew        = lv_ntgew
          i_gewei        = lv_gewei
          i_mode         = space
          i_flg_activity = space
          i_budat        = i_budat
          i_bldat        = i_bldat
          i_testrun      = i_testrun
          i_setproc      = i_setproc
          i_idproc       = i_idproc
          is_prhdr       = is_prhdr
        CHANGING
          ct_return      = lt_return
          c_subrc        = c_subrc.
    ELSE.
      c_subrc = 4.
      MESSAGE e140(/agri/fmpr) INTO sy-msgli.
      lwa_return-id         = sy-msgid.
      lwa_return-type       = sy-msgty.
      lwa_return-number     = sy-msgno.
      lwa_return-message_v1 = sy-msgv1.
      lwa_return-message_v2 = sy-msgv2.
      lwa_return-message_v3 = sy-msgv3.
      lwa_return-message_v4 = sy-msgv4.
      APPEND lwa_return TO lt_return.
    ENDIF.

  ELSE.

    c_subrc = 4.
    MESSAGE e139(/agri/fmpr) INTO sy-msgli.
    lwa_return-id         = sy-msgid.
    lwa_return-type       = sy-msgty.
    lwa_return-number     = sy-msgno.
    lwa_return-message_v1 = sy-msgv1.
    lwa_return-message_v2 = sy-msgv2.
    lwa_return-message_v3 = sy-msgv3.
    lwa_return-message_v4 = sy-msgv4.
    APPEND lwa_return TO lt_return.

  ENDIF.

* IfIf real execution
  CHECK i_testrun IS INITIAL.

* Consider only error messages
  DELETE lt_return WHERE type = 'S' OR type = 'W' OR type = 'I'.

* There are some messages?
  IF NOT lt_return[] IS INITIAL.

* Description
    lv_initiator = get_initiator( ).

* Initilize log messages
    CALL METHOD initilize_messages
      EXPORTING
        i_object    = i_object
        i_subobject = i_subobject
        i_initiator = lv_initiator.

* Add messages to application log
    LOOP AT lt_return ASSIGNING <lwa_return>.
      MESSAGE ID <lwa_return>-id
            TYPE <lwa_return>-type
          NUMBER <lwa_return>-number
            WITH <lwa_return>-message_v1
                 <lwa_return>-message_v2
                 <lwa_return>-message_v3
                 <lwa_return>-message_v4
            INTO sy-msgli.
      message_simple space.
    ENDLOOP.

* Save Messges
    messages_save lv_initiator space.

  ENDIF.

* Write process variables
  CALL METHOD write_variables_230
    EXPORTING
      i_budat    = i_budat
      i_bldat    = i_bldat
      i_ntgew    = lv_ntgew
      i_gewei    = lv_gewei
      it_afko    = lt_conmat
    CHANGING
      ct_prvarec = lt_prvarec.

* Execution RCP
  CALL METHOD execute_rcp
    EXPORTING
      it_prvarec = lt_prvarec
      i_bukrs    = lv_bukrs
      i_prnum    = is_prhdr-prnum
      i_gjahr    = is_prhdr-gjahr
      i_setproc  = i_setproc
      i_idproc   = i_idproc
      i_lgnumber = gs_log_variables-log_number
      i_subrc    = c_subrc.


ENDMETHOD.
