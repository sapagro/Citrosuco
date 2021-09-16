*----------------------------------------------------------------------*
***INCLUDE LZABST_MD_ATR_MAPF01.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
***INCLUDE LZABST_MD_ATR_MAPF01.
*----------------------------------------------------------------------*
FORM before_save.

  FIELD-SYMBOLS : <fs_field> TYPE any.

  LOOP AT total.
    CASE <action>.
      WHEN neuer_eintrag.

        ASSIGN COMPONENT 'ERNAM'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uname .
        ENDIF.

        ASSIGN COMPONENT 'ERDAT'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-datum .
        ENDIF.

        ASSIGN COMPONENT 'ERZET'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uzeit .
        ENDIF.

      WHEN aendern.

        ASSIGN COMPONENT 'AENAM'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uname .
        ENDIF.

        ASSIGN COMPONENT 'AEDAT'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-datum .
        ENDIF.

        ASSIGN COMPONENT 'AEZET'
            OF STRUCTURE <vim_total_struc> TO <fs_field> .
        IF sy-subrc = 0 .
          <fs_field> = sy-uzeit .
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc = 0.
      extract = total .
      MODIFY extract INDEX sy-tabix.
    ENDIF.

    MODIFY total.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  TRATNAM_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& Validating Measurement Document Attribute Group
*----------------------------------------------------------------------*
MODULE mdclass_validation INPUT.
  PERFORM class_validation USING zcl_abs_abap_maintain=>c_agcat_mp "'MP'
                                 zabst_md_atr_map-mdclass.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form MDCLASS_VALIDATION
*&---------------------------------------------------------------------*
*& Validating Attribute Group
*&---------------------------------------------------------------------*
FORM class_validation USING pv_agact TYPE /agri/glagcat
                            pv_class TYPE klasse_d.

  IF pv_class IS NOT INITIAL.
    SELECT SINGLE agcat
      FROM /agri/glagha
      INTO @DATA(lv_agcat)
      WHERE class = @pv_class
        AND agcat = @pv_agact.
    IF sy-subrc <> 0.
      MESSAGE e040(zabs_msgcls) WITH pv_class
                                     pv_agact.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  MDATNAM_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& Validating Measurement Document Attribute
*----------------------------------------------------------------------*
MODULE mdatnam_validation INPUT.
  PERFORM mdatnam_validation.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form MDATNAM_VALIDATION
*&---------------------------------------------------------------------*
*& Validating Measurement Document Attribute
*&---------------------------------------------------------------------*
FORM mdatnam_validation.

  IF zabst_md_atr_map-mdclass IS INITIAL.
    MESSAGE e036(zabs_msgcls).
    RETURN.
  ENDIF.
  IF zabst_md_atr_map-mdatnam IS INITIAL.
    MESSAGE e037(zabs_msgcls).
    RETURN.
  ENDIF.

  PERFORM atnam_validation USING zabst_md_atr_map-mdatnam
                                 zabst_md_atr_map-mdclass.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FIELDNAME_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*& Validating fieldname
*----------------------------------------------------------------------*
MODULE fieldname_validation INPUT.
  PERFORM fieldname_validation.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FIELDNAME_VALIDATION
*&---------------------------------------------------------------------*
*& Validating fieldname
*&---------------------------------------------------------------------*
FORM fieldname_validation .

  DATA: r_descr      TYPE REF TO cl_abap_structdescr,
        lwa_glcsview TYPE zabs_str_glflcmaca,
        wa_comp      TYPE abap_compdescr,
        lt_cs_afn    TYPE /plmb/t_fieldname,
        lwa_cs_afn   TYPE /plmb/s_fieldname.

  IF zabst_md_atr_map-fieldname IS INITIAL.
    RETURN.
  ENDIF.

*--Retrieving field names dynamically into internal table
  r_descr ?= cl_abap_structdescr=>describe_by_data( lwa_glcsview ).

  LOOP AT r_descr->components INTO wa_comp.
    lwa_cs_afn-fieldname = wa_comp-name.
    APPEND lwa_cs_afn TO lt_cs_afn.
  ENDLOOP.

*--Reading fieldname to validate
  READ TABLE lt_cs_afn TRANSPORTING NO FIELDS
  WITH KEY fieldname = zabst_md_atr_map-fieldname.
  IF sy-subrc <> 0.
    MESSAGE e041(zabs_msgcls) WITH zabst_md_atr_map-fieldname.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ATNAM_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM atnam_validation  USING pv_atnam TYPE atnam
                             pv_class TYPE klasse_d.

  SELECT SINGLE atinn,
                atnam
           FROM cabn
     INTO @DATA(ls_cabn)
          WHERE atnam = @pv_atnam.
  IF sy-subrc = 0.

    SELECT SINGLE b~imerk
             FROM klah AS a
             JOIN ksml AS b
               ON b~clint = a~clint
       INTO @DATA(lv_imerk)
            WHERE b~imerk = @ls_cabn-atinn.
    IF sy-subrc <> 0.
      MESSAGE e038(zabs_msgcls) WITH pv_atnam
                                     pv_class.
    ENDIF.
  ENDIF.

ENDFORM.
