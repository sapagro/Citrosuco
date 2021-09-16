*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_CHECKIN_FRUTAF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form PLANT_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_WERKS
*&---------------------------------------------------------------------*
FORM plant_check USING    lv_werks TYPE werks_d
                 CHANGING lv_subrc TYPE int4.

  SELECT SINGLE bwkey
    FROM t001k
    INTO @DATA(lv_plant)
   WHERE bukrs = @cgl_const-bukrs
     AND bwkey = @lv_werks.

  lv_subrc = sy-subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PRNUM_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_PRNUM
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM prnum_check USING    lv_prnum TYPE /agri/fmprnum
                 CHANGING lv_subrc TYPE int4.

  DATA: lv_prnum_local TYPE /agri/fmprnum.

  SELECT SINGLE prnum FROM /agri/fmprhdr
                      INTO (lv_prnum_local)
                WHERE prnum = lv_prnum
                   AND gjahr = sy-datum+0(4).
  MOVE sy-subrc TO lv_subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form WSCALE_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_PRDOC_X_PRHDR_WSCALE
*&---------------------------------------------------------------------*
FORM wscale_get USING lv_werks  TYPE werks_d
             CHANGING lv_wscale TYPE /agri/glscale
                      lv_meins  TYPE meins.

  SELECT bukrs, wscale, werks, meins
    FROM /agri/fmmaster
    INTO TABLE @DATA(lt_fmmaster)
   WHERE bukrs EQ @cgl_const-bukrs
     AND werks EQ @lv_werks.

  IF sy-subrc EQ 0.
    SORT lt_fmmaster BY wscale DESCENDING.
    READ TABLE lt_fmmaster INTO DATA(ls_fmmaster) INDEX 1.
    IF sy-subrc EQ 0.
      lv_wscale = ls_fmmaster-wscale.
      lv_meins  = ls_fmmaster-meins.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form WEIGHT_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_EQUNR
*&      <-- LS_PRDOC_X_PRHDR_NTGEW
*&      <-- LS_PRDOC_X_PRHDR_BRGEW
*&      <-- LS_PRDOC_X_PRHDR_OBGEW
*&      <-- LS_PRDOC_X_PRHDR_DIRWGTH
*&      <-- LS_PRDOC_X_PRHDR_LICNUM
*&      <-- LS_PRDOC_X_PRHDR_GEWEI
*&---------------------------------------------------------------------*
FORM weight_get USING    lv_prnum    TYPE /agri/fmprnum
                CHANGING ls_prhdr    TYPE /agri/s_fmprhdr.

  DATA: lt_tmtransport TYPE TABLE OF /sapyl/yo_item,
        lv_counter     TYPE int4,
        lv_do_counter  TYPE int4,
        lv_counter_str TYPE string,
        lv_field       TYPE string.

  FIELD-SYMBOLS: <lv_dinamyc_field> TYPE any.

  SELECT *
    FROM /sapyl/yo_hdr AS r
    INNER JOIN /sapyl/yo_item AS m
    ON r~db_key = m~parent_key
    INTO CORRESPONDING FIELDS OF TABLE lt_tmtransport
   WHERE r~external_doc_id = lv_prnum.

  IF sy-subrc EQ 0.
    LOOP AT  lt_tmtransport INTO DATA(lwa_transports).
      IF lwa_transports-ldap_code  = 'AVR'.
        MOVE-CORRESPONDING lwa_transports TO ls_prhdr-caval_meca.
        MOVE lwa_transports-means_of_transp TO ls_prhdr-caval_meca-caval.
      ENDIF.
      IF lwa_transports-ldap_code  = 'PVR'.
        ADD 1 TO lv_counter. MOVE lv_counter TO lv_counter_str.
        DO 8 TIMES.
          ADD 1 TO lv_do_counter.
          CASE lv_do_counter.
              IF <lv_dinamyc_field> IS ASSIGNED.
                UNASSIGN: <lv_dinamyc_field>.
              ENDIF.
              CLEAR:lv_field.
            WHEN 1.
              CONCATENATE 'LS_PRHDR-SEMIREB' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-means_of_transp TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 2.
              CONCATENATE 'LS_PRHDR-LIC_PLATE' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-lic_plate TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 3.
              CONCATENATE 'LS_PRHDR-TARE_WEIGHT' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-tare_weight TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 4.
              CONCATENATE 'LS_PRHDR-TARE_WEIGHT_UOM' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-tare_weight_uom TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 5.
              CONCATENATE 'LS_PRHDR-VOLUME' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-volume TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 6.
              CONCATENATE 'LS_PRHDR-VOLUME_UOM' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-volume_uom TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 7.
              CONCATENATE 'LS_PRHDR-WEIGHT' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-weight TO <lv_dinamyc_field>.
              ENDIF.
            WHEN 8.
              CONCATENATE 'LS_PRHDR-WEIGHT_UOM' lv_counter_str INTO lv_field.
              CONDENSE lv_field NO-GAPS.
              ASSIGN (lv_field) TO <lv_dinamyc_field>.
              IF <lv_dinamyc_field> IS ASSIGNED.
                MOVE lwa_transports-weight_uom TO <lv_dinamyc_field>.
              ENDIF.
          ENDCASE.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ELSE.
    RAISE transporte_nao_existe.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CAVAL_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_CAVAL
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM caval_check  USING    i_caval TYPE zfmprcavame
                  CHANGING lv_subrc TYPE int4.

  FIELD-SYMBOLS: <target> TYPE ANY TABLE.
  SELECT *
  FROM /sapyl/yo_item
  INTO CORRESPONDING FIELDS OF TABLE <target>
                 BYPASSING BUFFER
                 WHERE ( ldap_code  EQ 'AVR' )
                 AND ( means_of_transp EQ  i_caval ).
  MOVE sy-subrc TO lv_subrc.

ENDFORM.

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
    MOVE lv_counter TO lv_count_str.
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
    IF lv_subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TICKET_NUMBER_GENERATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ticket_number_generate USING ls_fmprhdr TYPE /agri/s_fmprhdr
                                  lv_numki
                         CHANGING lv_prnum
                                  lv_failed.

  TYPES: BEGIN OF ly_fmprhdr,
           prnum TYPE /agri/fmprnum,
           gjahr TYPE gjahr,
         END OF ly_fmprhdr.

  DATA: lt_fmprhdr_db TYPE STANDARD TABLE OF ly_fmprhdr INITIAL SIZE 0.

  CONSTANTS: BEGIN OF c_number_range,
               pr LIKE inri-object VALUE 'ZABS_FMPR',
             END OF c_number_range.

  DATA(lv_select) = abap_true.

  DO.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lv_numki
        object                  = c_number_range-pr
      IMPORTING
        number                  = lv_prnum
      EXCEPTIONS
        interval_not_found      = 01
        number_range_not_intern = 02
        object_not_found        = 03.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      lv_failed = c_true.
      EXIT.
    ELSE.
      IF lv_select EQ abap_true.
        SELECT prnum, gjahr
          FROM /agri/fmprhdr
          INTO TABLE @lt_fmprhdr_db
         WHERE prnum GE @lv_prnum
           AND gjahr EQ @ls_fmprhdr-gjahr
          ORDER BY prnum, gjahr.

        CLEAR lv_select.
      ENDIF.

      READ TABLE lt_fmprhdr_db INTO DATA(ls_fmprhdr_db)
        WITH KEY prnum = lv_prnum
                 gjahr = ls_fmprhdr-gjahr BINARY SEARCH.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    " TICKET_NUMBER_GENERATE
