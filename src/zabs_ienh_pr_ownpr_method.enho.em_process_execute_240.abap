METHOD process_execute_240 .

  DATA: lv_bukrs     TYPE bukrs,
        lv_aufnr     TYPE aufnr,
        lv_ntgew     TYPE ntgew,
        lv_gewei     TYPE gewei,
        lv_initiator TYPE /agri/gdescr,
        ls_header	   TYPE bapi2017_gm_head_01,
        ls_item	     TYPE bapi2017_gm_item_create.

  DATA: lwa_return   TYPE bapiret2.
  DATA: lt_conmat  TYPE /agri/t_fmconmat,
        lt_return  TYPE  bapirettab,
        lt_prvarec TYPE /agri/t_fmprvarec.

  DATA: lref_document_process TYPE REF TO /agri/cl_fm_document_process.

  FIELD-SYMBOLS:
        <lwa_return> TYPE bapiret2.

* Company code
  lv_bukrs = get_company_code( ms_master-werks ).

* <200-1>: Get superior harvest order / material
  CALL METHOD get_produce_receipt_order
    EXPORTING
      i_bukrs               = lv_bukrs
      i_matnr               = i_matnr
    RECEIVING
      r_aufnr               = lv_aufnr
    EXCEPTIONS
      order_types_not_found = 1
      status_not_found      = 2
      order_not_found       = 3
      OTHERS                = 4.
  IF sy-subrc = 0.

* <210-1> / <210-2>: Get inferior harvest orders
    CALL METHOD get_task_receipt_order
      EXPORTING
        i_aufnr                  = lv_aufnr
      RECEIVING
        rt_afvc                  = lt_conmat
      EXCEPTIONS
        status_not_found         = 1
        task_materials_not_found = 2
        OTHERS                   = 3.
    IF lt_conmat[] IS NOT INITIAL.

* <210-2>: Get quantity to confirm from document class settings
      CALL METHOD get_quantity_to_post
        EXPORTING
          i_confirm = 'X'
        IMPORTING
          e_ntgew   = lv_ntgew
          e_gewei   = lv_gewei.

      READ TABLE lt_conmat INTO DATA(lwa_conmat) INDEX 1.

* Instance
      CREATE OBJECT lref_document_process.

*<200-3>: Goods movements to harvest order
      CALL METHOD lref_document_process->make_movement_material
        EXPORTING
          i_aufnr   = lwa_conmat-aufnr
          i_matnr   = lwa_conmat-plnbez
          i_ntgew   = lv_ntgew
          i_gewei   = lv_gewei
          i_budat   = i_budat
          i_bldat   = i_bldat
          i_testrun = i_testrun
          i_setproc = i_setproc
          i_idproc  = i_idproc
          i_prnum   = is_prhdr-prnum
          i_gjahr   = is_prhdr-gjahr
          i_bukrs   = lv_bukrs
        CHANGING
          ct_return = lt_return
          cs_header = ls_header
          cs_item   = ls_item
          c_subrc   = c_subrc.
    ELSE.
      c_subrc = 4.
      MESSAGE e138(/agri/fmpr) INTO sy-msgli.
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
    CALL METHOD write_variables_200
      EXPORTING
        is_item    = ls_item
        is_header  = ls_header
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

  ENDIF.

ENDMETHOD.
