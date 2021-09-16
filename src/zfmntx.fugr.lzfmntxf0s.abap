*----------------------------------------------------------------------*
***INCLUDE LZFMNTXF0S.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SEMIREBOQUE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_SEMR1
*&      --> I_SEMR2
*&      --> I_SEMR3
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM semireboque_check  USING lv_semireboque1 TYPE zfmprsemireb1
                              lv_semireboque2 TYPE zfmprsemireb2
                              lv_semireboque3 TYPE zfmprsemireb3
                  CHANGING lv_subrc TYPE int4
                           lv_counter TYPE int4.


  FIELD-SYMBOLS: <target>   TYPE ANY TABLE,
                 <semirebo> TYPE any.

  DATA: lv_semireboque TYPE string,
        lv_count_str   TYPE string.

  DO 3 TIMES.
    ADD 1 TO lv_counter.
    MOVE lv_counter to lv_count_str.
    CONCATENATE 'LV_SEMIREBOQUE' lv_count_str INTO lv_semireboque.
    CONDENSE lv_semireboque NO-GAPS.
    ASSIGN (lv_semireboque) TO <semirebo>.

    SELECT *
      FROM /sapyl/yo_item
      INTO CORRESPONDING FIELDS OF TABLE <target>
                     BYPASSING BUFFER
                     WHERE ( ldap_code  EQ 'PVR' )
                     AND ( means_of_transp EQ  <semirebo> ).
    MOVE sy-subrc TO lv_subrc.
    if lv_subrc NE 0.
      EXIT.
      ENDIF.
  ENDDO.

ENDFORM.
