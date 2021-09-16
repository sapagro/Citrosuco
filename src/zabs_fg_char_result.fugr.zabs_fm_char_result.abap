FUNCTION zabs_fm_char_result.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_QALS) TYPE  QALS OPTIONAL
*"     REFERENCE(IS_QAQEE) TYPE  QAQEE OPTIONAL
*"     REFERENCE(IS_QAPO) TYPE  QAPO OPTIONAL
*"  CHANGING
*"     REFERENCE(CS_QAPPD) TYPE  QAPPD OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_insppoints    TYPE /agri/t_fmbapi2045l4,
        lt_char_req      TYPE co_mes_bapi2045d1_t,
        lt_char_res      TYPE rplm_tt_bapi2045d2,
        lt_samples       TYPE rplm_tt_bapi2045d3,
        lt_single        TYPE rplm_tt_bapi2045d4,
        ls_operation     TYPE bapi2045l2,
        ls_insppoint_req TYPE bapi2045d5,
        ls_insppoint_ret TYPE bapiret2,
        lv_terreno       TYPE /agri/gltplnr_fl.

  IF is_qals IS SUPPLIED
  AND is_qaqee IS SUPPLIED
  AND is_qals-prueflos IS NOT INITIAL
  AND is_qals-aufnr IS NOT INITIAL
  AND is_qaqee-vornr IS NOT INITIAL.
    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
      EXPORTING
        insplot                = is_qals-prueflos
        inspoper               = is_qaqee-vornr
        read_insppoints        = 'X'
        read_char_requirements = 'X'
        read_char_results      = 'X'
        read_sample_results    = 'X'
        read_single_results    = 'X'
      IMPORTING
        operation              = ls_operation
        insppoint_requirements = ls_insppoint_req
        return                 = ls_insppoint_ret
      TABLES
        insppoints             = lt_insppoints
        char_requirements      = lt_char_req
        char_results           = lt_char_res
        sample_results         = lt_samples
        single_results         = lt_single.

    SELECT SINGLE aufnr, auart, autyp, tplnr_fl, contr,
                  tplma, cmnum, varia, cpros
      FROM /agri/fmfphdr
      INTO @DATA(ls_fmfphdr)
     WHERE aufnr = @is_qals-aufnr.

    IF sy-subrc EQ 0.
      SELECT tplnr_fl, contr, cmnum, eston, ymatnr, zzfazplantio
        INTO TABLE @DATA(lt_glflcma)
        FROM /agri/glflcma
       WHERE tplnr_fl EQ @ls_fmfphdr-tplnr_fl
         AND loevm    EQ @abap_false
         AND astat  EQ 'A'
       ORDER BY contr DESCENDING.
      IF sy-subrc EQ 0.
        READ TABLE lt_glflcma INTO DATA(ls_glflcma) INDEX 1.
        IF sy-subrc EQ 0
        AND ls_glflcma-ymatnr IS NOT INITIAL.
          SELECT SINGLE maktx
            INTO @DATA(lv_maktx)
            FROM makt
           WHERE matnr = @ls_glflcma-ymatnr
             AND spras = @sy-langu.
          IF sy-subrc EQ 0.
            TRANSLATE lv_maktx USING '@ £ § ! # / ~ } ] { [ ´ ` = + - _ & * '.
            TRANSLATE lv_maktx USING '( ) & ¨ % $ # @ ! " , . ; : \ | '.
          ENDIF.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_fmfphdr-tplnr_fl
        IMPORTING
          output = lv_terreno.
*-- Z28: Terreno:/Variedade:/Plantio:
      cs_qappd-userc1 = lv_terreno.
      cs_qappd-userc2 = lv_maktx+8(10).
      cs_qappd-usern1 = ls_glflcma-zzfazplantio+0(4).
      UNPACK cs_qappd-usern1 TO cs_qappd-usern1.
    ENDIF.
  ENDIF.

ENDFUNCTION.
