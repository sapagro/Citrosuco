class ZCL_ABS_GLMD_ATTRIBUTE definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_GLMD_ATTRIBUTE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_GLMD_ATTRIBUTE IMPLEMENTATION.


  method /AGRI/IF_GLMD_ATTRIBUTE~DYNAMIC_VALUE_SET.
  endmethod.


  method /AGRI/IF_GLMD_ATTRIBUTE~INITIAL_VALUE_SET.
  endmethod.


  METHOD /agri/if_glmd_attribute~value_check.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Method Name       :  VALUE_CHECK                                     *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.04.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Validations for attributes at Measurement       *
*                      Document level.                                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local Data Declaration
    DATA: lv_numdata TYPE REF TO data.

*--Field Symbol Declaration
    FIELD-SYMBOLS: <lv_any> TYPE any.

*-Fetch Additional Values and Attribute details
* by joining Attribute Header Additional data
* and Characteristic tables
    SELECT a~atinn,
           a~zzvalres,
           a~zztgtval,
           a~zzrep_sts,
           b~atnam,
           b~atfor,
           b~anzst,
           b~anzdz
      FROM /agri/gatha AS a
      JOIN cabn        AS b
      ON   a~atinn = b~atinn
      INTO TABLE @DATA(lt_gatha)
      FOR ALL ENTRIES IN @it_mdatv
      WHERE a~atinn = @it_mdatv-atinn.
    IF sy-subrc NE 0.
      RETURN.
    ELSE.
      SORT lt_gatha BY atinn.
    ENDIF.

*--Fetch Attribute data from Measurement Document
    SELECT b~atinn,
           b~atwrt,
           b~atflv
      FROM /agri/glmdhdr AS a
      JOIN /agri/glmdatv AS b
      ON   a~mdocm = b~mdocm
      INTO TABLE @DATA(lt_glmd)
      WHERE a~mdtyp = @is_mdhdr-mdtyp
        AND a~mpgrp = @is_mdhdr-mpgrp
        AND a~mdate = @is_mdhdr-mdate.
    IF sy-subrc = 0.
      SORT lt_glmd BY atinn.
    ENDIF.

*--Process data to validate attribute values.
    LOOP AT it_mdatv INTO DATA(ls_mdatv).

      IF ls_mdatv-atwrt IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_gatha INTO DATA(ls_gatha)
        WITH KEY atinn = ls_mdatv-atinn
        BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF ls_gatha-zzvalres IS INITIAL OR ls_gatha-atfor NE 'NUM'.
        CREATE DATA lv_numdata TYPE p LENGTH ls_gatha-anzst DECIMALS ls_gatha-anzdz.
        ASSIGN lv_numdata->* TO <lv_any>.
        IF <lv_any> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        <lv_any> = ls_mdatv-atflv.
        CASE ls_gatha-zzvalres.
          WHEN 'EQ'.
            IF <lv_any> EQ ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN 'NE'.
            IF <lv_any> NE ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN 'GT'.
            IF <lv_any> GT ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN 'GE'.
            IF <lv_any> GE ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN 'LT'.
            IF <lv_any> LT ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN 'LE'.
            IF <lv_any> LE ls_gatha-zztgtval.
            ELSE.
              MESSAGE e016(zabs_msgcls) WITH ls_gatha-atnam ls_gatha-zzvalres ls_gatha-zztgtval.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

      IF ls_gatha-zzrep_sts IS NOT INITIAL.
        READ TABLE lt_glmd INTO DATA(ls_glmd)
          WITH KEY atinn = ls_mdatv-atinn
          BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_glmd-atwrt IS NOT INITIAL OR ls_glmd-atflv IS NOT INITIAL.
            MESSAGE e017(zabs_msgcls) WITH ls_gatha-atnam.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
