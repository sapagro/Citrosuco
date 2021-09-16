class ZCL_ABS_GET_VARIANTS definition
  public
  final
  create public .

public section.

  types:
    tt_vtitm_itm TYPE STANDARD TABLE OF zabst_vtitm .
  types:
    BEGIN OF ty_vthdr.
        INCLUDE TYPE zabst_vthdr.
    TYPES: mod TYPE zabs_del_mod,
           END OF ty_vthdr .
  types:
    BEGIN OF ty_vtitm.
        INCLUDE TYPE zabst_vtitm.
    TYPES: mod TYPE zabs_del_mod,
           END OF ty_vtitm .

  class-methods GET_CONSTANT_SINGLE
    importing
      !IV_MOD type ZABS_DEL_MOD default SPACE
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !EV_CNVAL1 type ANY
      !EV_CNVAL2 type ANY
      !EV_CNVAL3 type ANY .
  class-methods CHECK_KEY_EXISTS
    importing
      !IV_MOD type ZABS_DEL_MOD default SPACE
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !EV_KEY_EXIST type XFELD .
  class-methods GET_CONSTANT_MULTIPLE
    importing
      !IV_MOD type ZABS_DEL_MOD default SPACE
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !ET_CONSTANTS type ZABS_TTY_VKEY_CONST .
  class-methods GET_VRECORDS_WITH_KEY
    importing
      !IV_MOD type ZABS_DEL_MOD default SPACE
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !ET_VARIANT_DATA type TT_VTITM_ITM .
  class-methods GET_RANGE_CONSTANTS
    importing
      !IV_MOD type ZABS_DEL_MOD optional
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !EO_RANGE type ref to DATA .
protected section.
private section.

  class-data:
    t_variant_hdr TYPE SORTED TABLE OF ty_vthdr "zabst_vthdr
                         WITH UNIQUE KEY objid mod .
  class-data:
    t_variant_itm TYPE SORTED TABLE OF ty_vtitm "zabst_vtitm
             WITH UNIQUE KEY objid mod k1val k2val k3val k4val k5val seqno .
  constants C_CONSTANT type ZABS_DEL_OBJTP value 'C' ##NO_TEXT.

  class-methods PREPARE_KEY_CONDITION
    importing
      !IV_OBJID type ZABS_DEL_OBJID
      !IV_K1VAL type ANY optional
      !IV_K2VAL type ANY optional
      !IV_K3VAL type ANY optional
      !IV_K4VAL type ANY optional
      !IV_K5VAL type ANY optional
    exporting
      !EV_COND type STRING .
  class-methods GET_VARIANT_DATA
    importing
      !IV_MOD type ZABS_DEL_MOD default SPACE
      !IV_OBJID type ZABS_DEL_OBJID
    exporting
      !ES_VARIANT_HDR type ZABST_VTHDR
      !ET_VARIANT_ITM type TT_VTITM_ITM .
ENDCLASS.



CLASS ZCL_ABS_GET_VARIANTS IMPLEMENTATION.


  METHOD check_key_exists.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : CHECK_KEY_EXISTS                                    *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Check whether key exists or not                     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Data Declarations
    DATA: ls_variant_hdr TYPE zabst_vthdr,
          ls_variant_itm TYPE zabst_vtitm,
          lt_variant_itm TYPE tt_vtitm_itm,
          lv_cond        TYPE string.

    CLEAR ev_key_exist.

*-- Get Variant data
    CALL METHOD zcl_abs_get_variants=>get_variant_data
      EXPORTING
        iv_mod         = iv_mod
        iv_objid       = iv_objid
      IMPORTING
        es_variant_hdr = ls_variant_hdr
        et_variant_itm = lt_variant_itm.

*-- Prepare conditions with provided key values
    CALL METHOD zcl_abs_get_variants=>prepare_key_condition
      EXPORTING
        iv_objid = iv_objid
        iv_k1val = iv_k1val
        iv_k2val = iv_k2val
        iv_k3val = iv_k3val
        iv_k4val = iv_k4val
        iv_k5val = iv_k5val
      IMPORTING
        ev_cond  = lv_cond.

*-- Get the constant value maintained in variant table with provided keys
    LOOP AT lt_variant_itm INTO ls_variant_itm WHERE (lv_cond).
      ev_key_exist = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_constant_multiple.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : GET_CONSTANT_MULTIPLE                               *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Get multiple constants                              *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Data Declarations
    DATA: ls_variant_hdr TYPE zabst_vthdr,
          ls_variant_itm TYPE zabst_vtitm,
          lt_variant_itm TYPE tt_vtitm_itm,
          lv_cond        TYPE string,
          ls_constants   TYPE zabs_str_vkey_const.

    REFRESH et_constants.

*-- Get Variant data
    CALL METHOD zcl_abs_get_variants=>get_variant_data
      EXPORTING
        iv_mod         = iv_mod
        iv_objid       = iv_objid
      IMPORTING
        es_variant_hdr = ls_variant_hdr
        et_variant_itm = lt_variant_itm.

*-- If data is not maintained as constant in variant table
    IF ls_variant_hdr-objtp NE c_constant.
      RETURN.
    ENDIF.

*-- Prepare conditions with provided key values
    CALL METHOD zcl_abs_get_variants=>prepare_key_condition
      EXPORTING
        iv_objid = iv_objid
        iv_k1val = iv_k1val
        iv_k2val = iv_k2val
        iv_k3val = iv_k3val
        iv_k4val = iv_k4val
        iv_k5val = iv_k5val
      IMPORTING
        ev_cond  = lv_cond.

*-- Get the constant value maintained in variant table with provided keys
    LOOP AT lt_variant_itm INTO ls_variant_itm WHERE (lv_cond).
      CLEAR ls_constants.
      ls_constants-cnval1 = ls_variant_itm-cnval1.
      ls_constants-cnval2 = ls_variant_itm-cnval2.
      ls_constants-cnval3 = ls_variant_itm-cnval3.
      APPEND ls_constants TO et_constants.
    ENDLOOP.

  ENDMETHOD.


METHOD get_constant_single.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : GET_CONSTANT_SINGLE                                 *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Get single constant from variant table              *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Data Declarations
  DATA: ls_variant_hdr TYPE zabst_vthdr,
        ls_variant_itm TYPE zabst_vtitm,
        lt_variant_itm TYPE tt_vtitm_itm,
        lv_cond        TYPE string.

  CLEAR: ev_cnval1, ev_cnval2, ev_cnval3.

*-- Get Variant data
  CALL METHOD zcl_abs_get_variants=>get_variant_data
    EXPORTING
      iv_mod         = iv_mod
      iv_objid       = iv_objid
    IMPORTING
      es_variant_hdr = ls_variant_hdr
      et_variant_itm = lt_variant_itm.

*-- If data is not maintained as constant in variant table
  IF ls_variant_hdr-objtp NE c_constant.
    RETURN.
  ENDIF.

*-- Prepare conditions with provided key values
  CALL METHOD zcl_abs_get_variants=>prepare_key_condition
    EXPORTING
      iv_objid = iv_objid
      iv_k1val = iv_k1val
      iv_k2val = iv_k2val
      iv_k3val = iv_k3val
      iv_k4val = iv_k4val
      iv_k5val = iv_k5val
    IMPORTING
      ev_cond  = lv_cond.

*-- Get the constant value maintained in variant table with provided keys
  LOOP AT lt_variant_itm INTO ls_variant_itm WHERE (lv_cond).
    ev_cnval1 = ls_variant_itm-cnval1.
    ev_cnval2 = ls_variant_itm-cnval2.
    ev_cnval3 = ls_variant_itm-cnval3.
    EXIT.
  ENDLOOP.

ENDMETHOD.


 METHOD get_range_constants.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : GET_RANGE_CONSTANTS                                 *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Get range constants                                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Workareas
   DATA:
     ls_variant_hdr TYPE zabst_vthdr,
     ls_variant_itm TYPE zabst_vtitm,
     lt_variant_itm TYPE tt_vtitm_itm,
     lv_cond        TYPE string.

*-- Objects
   DATA:
     lo_s_range     TYPE REF TO data.

*-- Field Symbols
   FIELD-SYMBOLS:
     <fs_t_range> TYPE STANDARD TABLE,
     <fs_s_range> TYPE any,
     <fs_value>   TYPE any.

   CHECK eo_range IS BOUND.

   ASSIGN eo_range->* TO <fs_t_range>.
   IF <fs_t_range> IS ASSIGNED.
     CREATE DATA lo_s_range LIKE LINE OF <fs_t_range>.
     IF lo_s_range IS BOUND.
       ASSIGN lo_s_range->* TO <fs_s_range>.
     ENDIF.
   ENDIF.

   CHECK <fs_t_range> IS ASSIGNED AND <fs_s_range> IS ASSIGNED.

*-- Get Variant data
   CALL METHOD zcl_abs_get_variants=>get_variant_data
     EXPORTING
       iv_mod         = iv_mod
       iv_objid       = iv_objid
     IMPORTING
       es_variant_hdr = ls_variant_hdr
       et_variant_itm = lt_variant_itm.

*-- If data is not maintained as constant in variant table
   IF ls_variant_hdr-objtp EQ c_constant.
     RETURN.
   ENDIF.

*-- Prepare conditions with provided key values
   CALL METHOD zcl_abs_get_variants=>prepare_key_condition
     EXPORTING
       iv_objid = iv_objid
       iv_k1val = iv_k1val
       iv_k2val = iv_k2val
       iv_k3val = iv_k3val
       iv_k4val = iv_k4val
       iv_k5val = iv_k5val
     IMPORTING
       ev_cond  = lv_cond.

*-- Get the constant value maintained in variant table with provided keys
   LOOP AT lt_variant_itm INTO ls_variant_itm WHERE (lv_cond).
     CLEAR <fs_s_range>.
     ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_s_range> TO <fs_value>.
     IF <fs_value> IS ASSIGNED.
       <fs_value> = ls_variant_itm-sign.
       UNASSIGN <fs_value>.
     ENDIF.
     ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_s_range> TO <fs_value>.
     IF <fs_value> IS ASSIGNED.
       <fs_value> = ls_variant_itm-optin.
       UNASSIGN <fs_value>.
     ENDIF.
     ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_s_range> TO <fs_value>.
     IF <fs_value> IS ASSIGNED.
       <fs_value> = ls_variant_itm-low.
       UNASSIGN <fs_value>.
     ENDIF.
     ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_s_range> TO <fs_value>.
     IF <fs_value> IS ASSIGNED.
       <fs_value> = ls_variant_itm-high.
       UNASSIGN <fs_value>.
     ENDIF.

     IF <fs_s_range> IS NOT INITIAL.
       APPEND <fs_s_range> TO <fs_t_range>.
     ENDIF.
   ENDLOOP.

 ENDMETHOD.


  METHOD get_variant_data.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : GET_VARIANT_DATA                                    *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Get Variant data                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Data Declarations
    DATA: ls_variant_itm     TYPE ty_vtitm,
*-- BOC-T_T.KONNO-05.13.21
          lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
          lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
*-- EOC-T_T.KONNO-05.13.21
          lv_hdr_tabname     TYPE dd02l-tabname,
          lv_itm_tabname     TYPE dd02l-tabname,
          ls_variant_hdr     TYPE ty_vthdr.

    CLEAR es_variant_hdr.
    REFRESH et_variant_itm.

*-- BOC-T_T.KONNO-05.13.21
    lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
    lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).
*-- EOC-T_T.KONNO-05.13.21

*-- Dynamic Selection of Tables
    IF iv_mod = space.
      lv_hdr_tabname = zcl_abs_abap_maintain=>c_varhdr_tab.     "'ZABST_VTHDR'.
      lv_itm_tabname = zcl_abs_abap_maintain=>c_varitm_tab.     "'ZABST_VTITM'.
    ELSE.
      lv_hdr_tabname = zcl_abs_abap_maintain=>c_cus_varhdr_tab. "'ZABST_VTCHDR'.
      lv_itm_tabname = zcl_abs_abap_maintain=>c_cus_varitm_tab. "'ZABST_VTCITM'.
    ENDIF.

*-- Check variant data is available in global data with OBJID
    READ TABLE t_variant_hdr INTO es_variant_hdr
          WITH KEY objid = iv_objid
                   mod   = iv_mod
        BINARY SEARCH.
    IF sy-subrc NE 0.       "If not Available, get from data base tables
      SELECT SINGLE *
        FROM (lv_hdr_tabname)                      "zabst_vthdr
        INTO es_variant_hdr
       WHERE objid EQ iv_objid.
      IF sy-subrc EQ 0.
        CLEAR ls_variant_hdr.
        ls_variant_hdr     = es_variant_hdr.
        ls_variant_hdr-mod = iv_mod.
        INSERT ls_variant_hdr INTO TABLE t_variant_hdr.
      ENDIF.

      SELECT *
        FROM (lv_itm_tabname)      "zabst_vtitm
        INTO TABLE et_variant_itm
       WHERE objid EQ iv_objid.
      IF sy-subrc EQ 0.
        LOOP AT et_variant_itm INTO ls_variant_itm.
          ls_variant_itm-mod = iv_mod.
          INSERT ls_variant_itm INTO TABLE t_variant_itm.
        ENDLOOP.
      ENDIF.
    ELSE.                         "If available, copy from global tables
      READ TABLE t_variant_itm TRANSPORTING NO FIELDS
            WITH KEY objid = iv_objid
                     mod   = iv_mod
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        LOOP AT t_variant_itm INTO ls_variant_itm FROM sy-tabix.
          IF ls_variant_itm-objid NE iv_objid OR
             ls_variant_itm-mod   NE iv_mod.
            EXIT.
          ENDIF.
          APPEND ls_variant_itm TO et_variant_itm.
        ENDLOOP.
      ENDIF.
    ENDIF.

*-- BOC-T_T.KONNO-05.13.21
    IF iv_objid EQ 'STAT'
    AND ( sy-cprog = '/AGRI/SAPLGLMDM' OR sy-cprog EQ '/AGRI/GLMD_MASS_PROCESS' ).
      READ TABLE lt_formatted_stack INTO DATA(ls_formatted_stack)
        WITH KEY kind     = 'FORM'
                 progname = '/AGRI/SAPLGLMDM'
                 event    = 'MD_HEADER_UPDATE'.
      IF sy-subrc EQ 0.
        SORT et_variant_itm BY objid k1val k2val cnval1 seqno.
      ENDIF.
    ENDIF.
*-- EOC-T_T.KONNO-05.13.21

  ENDMETHOD.


  METHOD get_vrecords_with_key.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : GET_VRECORDS_WITH_KEY                               *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Get variant records with key                        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Data Declarations
    DATA: ls_variant_hdr TYPE zabst_vthdr,
          ls_variant_itm TYPE zabst_vtitm,
          lt_variant_itm TYPE tt_vtitm_itm,
          lv_cond        TYPE string.

    REFRESH et_variant_data.

*-- Get Variant data
    CALL METHOD zcl_abs_get_variants=>get_variant_data
      EXPORTING
        iv_mod         = iv_mod
        iv_objid       = iv_objid
      IMPORTING
        es_variant_hdr = ls_variant_hdr
        et_variant_itm = lt_variant_itm.

*-- If data is not maintained as constant in variant table
    IF ls_variant_hdr-objtp NE c_constant.
      RETURN.
    ENDIF.

*-- Prepare conditions with provided key values
    CALL METHOD zcl_abs_get_variants=>prepare_key_condition
      EXPORTING
        iv_objid = iv_objid
        iv_k1val = iv_k1val
        iv_k2val = iv_k2val
        iv_k3val = iv_k3val
        iv_k4val = iv_k4val
        iv_k5val = iv_k5val
      IMPORTING
        ev_cond  = lv_cond.

*-- Get the constant value maintained in variant table with provided keys
    LOOP AT lt_variant_itm INTO ls_variant_itm WHERE (lv_cond).
      APPEND ls_variant_itm TO et_variant_data.
    ENDLOOP.

  ENDMETHOD.


 METHOD prepare_key_condition.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Class Name     : ZCL_ABS_GET_VARIANTS                                *
* Method Name    : PREPARE_KEY_CONDITION                               *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 6.24.2019                                           *
* TR             : C4DK901782                                          *
* Version        : 001                                                 *
* Description    : Prepare Key condition                               *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*-- Prepare condition based on the keys provided in the callig method
*-- Note: Field names which are hardcoded below should be available in
*-- workarea in calling method and variable names should be sames as
*-- variable names in calling method.

   ev_cond = 'OBJID EQ IV_OBJID'.

   IF iv_k1val IS NOT INITIAL.
     CONCATENATE ev_cond 'AND K1VAL EQ IV_K1VAL'
            INTO ev_cond SEPARATED BY space.
   ENDIF.

   IF iv_k2val IS NOT INITIAL.
     CONCATENATE ev_cond 'AND K2VAL EQ IV_K2VAL'
            INTO ev_cond SEPARATED BY space.
   ENDIF.

   IF iv_k3val IS NOT INITIAL.
     CONCATENATE ev_cond 'AND K3VAL EQ IV_K3VAL'
            INTO ev_cond SEPARATED BY space.
   ENDIF.

   IF iv_k4val IS NOT INITIAL.
     CONCATENATE ev_cond 'AND K4VAL EQ IV_K4VAL'
            INTO ev_cond SEPARATED BY space.
   ENDIF.

   IF iv_k5val IS NOT INITIAL.
     CONCATENATE ev_cond 'AND K5VAL EQ IV_K5VAL'
            INTO ev_cond SEPARATED BY space.
   ENDIF.

 ENDMETHOD.
ENDCLASS.
