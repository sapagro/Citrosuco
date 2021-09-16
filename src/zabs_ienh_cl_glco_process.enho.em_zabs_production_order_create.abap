METHOD zabs_production_order_create .

  DATA: t_abap_stack TYPE  abap_callstack,
        t_sys_stack  TYPE  sys_callst,
        lv_matnr     TYPE matnr,
        orderdata    TYPE bapi_pp_order_create,
        return       TYPE bapiret2,
        ordernumber  TYPE bapi_order_key-order_number,
        ordertype    TYPE bapi_order_copy-order_type,
        lwa_messages TYPE bdcmsgcoll.

  EXPORT is_prodord FROM is_prodord TO MEMORY ID 'IS_PRODORD'.
  EXPORT it_items_bom FROM it_items_bom TO MEMORY ID 'IT_ITEMS_BOM'.
  EXPORT i_chemical FROM i_chemical TO MEMORY ID 'I_CHEMICAL'.
  EXPORT it_items_bom_temp FROM it_items_bom_temp TO MEMORY ID 'IT_ITEMS_BOM_TEMP'.

  orderdata-plant = is_prodord-werks.

  SELECT SINGLE t~auart
    INTO orderdata-order_type
    FROM marc AS c
    INNER JOIN tco43 AS t
    ON c~werks = t~werks AND
       c~sfcpf = t~co_prodprf
    WHERE c~matnr = is_prodord-matnr
      AND c~werks = is_prodord-werks.

  orderdata-basic_start_date = is_prodord-gstrp.
  orderdata-basic_start_time = is_prodord-gsuzp.
  orderdata-basic_end_date = is_prodord-gltrp.
  orderdata-basic_end_time = is_prodord-gluzp.
  orderdata-quantity = is_prodord-gamng.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input          = is_prodord-gmein
      language       = sy-langu
    IMPORTING
      output         = orderdata-quantity_uom
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  orderdata-prod_version = is_prodord-verid.

  SELECT SINGLE lgpro
    INTO orderdata-storage_location
    FROM marc
    WHERE matnr = is_prodord-matnr
      AND werks = is_prodord-werks.

  orderdata-material_long = is_prodord-matnr.

  CALL FUNCTION 'BAPI_PRODORD_CREATE'
    EXPORTING
      orderdata    = orderdata
    IMPORTING
      return       = return
      order_number = ordernumber
      order_type   = ordertype.

  IF ordernumber IS NOT INITIAL.
    lwa_messages-msgid = 'CO'.
    lwa_messages-msgnr = 100.
    lwa_messages-msgv1 = ordernumber.
    INSERT lwa_messages INTO TABLE et_messtab.
  ELSE.
    lwa_messages-msgid  = return-id.
    lwa_messages-msgtyp = return-type.
    lwa_messages-msgnr  = return-number.
    lwa_messages-msgv1  = return-message_v1.
    lwa_messages-msgv2  = return-message_v2.
    lwa_messages-msgv3  = return-message_v3.
    lwa_messages-msgv4  = return-message_v4.
    APPEND lwa_messages TO et_messtab.
  ENDIF.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack    = t_abap_stack
      et_callstack = t_sys_stack.

  READ TABLE t_abap_stack TRANSPORTING NO FIELDS
    WITH KEY mainprogram = 'ZFMFP_UNPLANNED_TASKORDER'.

  IF sy-subrc EQ 0.
    INCLUDE zabs_inc_inspoper IF FOUND.
  ENDIF.

ENDMETHOD.
