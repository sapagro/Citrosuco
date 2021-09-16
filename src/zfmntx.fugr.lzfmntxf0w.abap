*----------------------------------------------------------------------*
***INCLUDE LZFMNTXF0W.
*----------------------------------------------------------------------*
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

*  SELECT SINGLE wscale meins
*    FROM /agri/fmmaster INTO (lv_wscale, lv_meins )
*   WHERE werks EQ lv_werks
*     AND bukrs EQ cgl_const-bukrs.
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
FORM weight_get  USING    lv_prnum    TYPE /agri/fmprnum
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
