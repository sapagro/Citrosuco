class ZCL_ZFARM_CREATE_PROG_DPC_EXT definition
  public
  inheriting from ZCL_ZFARM_CREATE_PROG_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
protected section.

  methods SETGETDATAPROGSE_CREATE_ENTITY
    redefinition .
  methods SETGETDATAPROGSE_GET_ENTITYSET
    redefinition .
  methods GETPROGSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZFARM_CREATE_PROG_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
*  EXPORTING
*    IT_OPERATION_INFO =
**  CHANGING
**    cv_defer_mode     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  METHOD getprogset_get_entityset.

    DATA: lt_prog TYPE TABLE OF zfmplprsemanal,
          lt_dates TYPE /scwm/tt_lm_dates,
          ls_prog LIKE LINE OF lt_prog,
          ls_prog_aux LIKE LINE OF lt_prog.

    DATA: lv_begda TYPE begda,
          lv_endda TYPE endda.

    DATA ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_tabix TYPE sy-tabix,
          lv_oper TYPE string,
          lv_matkl TYPE matkl,
          lv_acnum TYPE zfmacnum.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Data'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Processo'.
          lv_oper = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    lv_begda = sy-datum(6) && '01'.

    CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in                  = lv_begda
      IMPORTING
        last_day_of_month       = lv_endda.

     CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
       EXPORTING
         iv_begda       = lv_begda
         iv_endda       = lv_endda
       IMPORTING
         et_dates       = lt_dates.

     SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_prog
      FROM zfmplprsemanal
      WHERE acnum EQ lv_acnum AND
            operacao EQ lv_oper AND
            matkl    EQ lv_matkl AND
            plday BETWEEN lv_begda AND lv_endda.
       IF sy-subrc EQ 0.
         SORT lt_prog BY actrn.
       ELSE.

       ENDIF.
     CLEAR: lt_prog[].
     LOOP AT lt_dates INTO DATA(ls_dates).
       IF lt_prog IS NOT INITIAL.

       LOOP AT lt_prog INTO ls_prog WHERE plday EQ ls_dates.
         ls_prog_aux = ls_prog.

         at END OF actrn.
           ls_entityset-data = ls_dates. "ls_dates+6(2) && '.' && ls_dates+4(2) && '.' && ls_dates(4).
           APPEND ls_entityset TO et_entityset.
         ENDAT.


       ENDLOOP.
       ELSE.
           ls_entityset-data = ls_dates. "ls_dates+6(2) && '.' && ls_dates+4(2) && '.' && ls_dates(4).
           APPEND ls_entityset TO et_entityset.
       ENDIF.
     ENDLOOP.

  ENDMETHOD.


  METHOD setgetdataprogse_create_entity.

    DATA: lt_dates    TYPE /scwm/tt_lm_dates,
          ls_prog     LIKE er_entity,
          ls_prog_sem TYPE zfmplprsemanal,
          lv_begda    TYPE begda,
          lv_endda    TYPE endda,
          lv_tabix    TYPE sy-tabix,
          lv_betrg    TYPE betrg.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_prog ).

    lv_begda = ls_prog-begda.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    DATA(lv_changed) = abap_false.
    LOOP AT lt_dates INTO DATA(ls_dates).
      CASE sy-tabix.
        WHEN '1'.
          lv_changed = abap_true.
          ls_prog_sem-plday = ls_dates.
          ls_prog_sem-plapr = ls_prog-d1h.
          ls_prog_sem-qtyrs = ls_prog-d1r.
        WHEN '2'.
          lv_changed = abap_true.
          ls_prog_sem-plday = ls_dates.
          ls_prog_sem-plapr = ls_prog-d2h.
          ls_prog_sem-qtyrs = ls_prog-d2r.
        WHEN '3'.
          lv_changed = abap_true.
          ls_prog_sem-plday = ls_dates.
          ls_prog_sem-plapr = ls_prog-d3h.
          ls_prog_sem-qtyrs = ls_prog-d3r.
        WHEN '04'.
          lv_changed = abap_true.
          ls_prog_sem-plday = ls_dates.
          ls_prog_sem-plapr = ls_prog-d4h.
          ls_prog_sem-qtyrs = ls_prog-d4r.
        WHEN '05'.
          lv_changed = abap_true.
          ls_prog_sem-plday = ls_dates.
          ls_prog_sem-plapr = ls_prog-d5h.
          ls_prog_sem-qtyrs = ls_prog-d5r.
      ENDCASE.

*-- Processo
      ls_prog_sem-acnum = ls_prog-acnum.
      ls_prog_sem-matkl = ls_prog-matkl.
      ls_prog_sem-operacao = ls_prog-processo.

*-- Turno
      IF ls_prog-turno EQ 'Turno 1'.
        ls_prog_sem-actrn = 'T1'.
      ELSEIF ls_prog-turno EQ 'Turno 2'.
        ls_prog_sem-actrn = 'T2'.
      ELSEIF ls_prog-turno EQ 'Turno 3'.
        ls_prog_sem-actrn = 'T3'.
      ENDIF.

      IF lv_changed = abap_true.
        MODIFY zfmplprsemanal FROM ls_prog_sem.
      ENDIF.
      lv_changed = abap_false.
    ENDLOOP.

    er_entity = ls_prog.

  ENDMETHOD.


  METHOD setgetdataprogse_get_entityset.

    DATA: lt_dates     TYPE /scwm/tt_lm_dates,
          ls_entityset LIKE LINE OF et_entityset,
          lv_begda     TYPE begda,
          lv_endda     TYPE endda,
          lv_index     TYPE sy-index,
          lv_oper      TYPE string,
          lv_matkl     TYPE matkl,
          lv_acnum     TYPE zfmacnum,
          lv_h_field   TYPE fieldname,
          lv_r_field   TYPE fieldname.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_oper = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    DO 3 TIMES.
      ADD 1 TO lv_index.
      CASE lv_index.
        WHEN '1'.
          ls_entityset-turno = 'Turno 1'.
        WHEN '2'.
          ls_entityset-turno = 'Turno 2'.
        WHEN '3'.
          ls_entityset-turno = 'Turno 3'.
        WHEN OTHERS.
      ENDCASE.
      APPEND ls_entityset TO et_entityset.
    ENDDO.

    SELECT *
     INTO TABLE @DATA(lt_prog)
     FROM zfmplprsemanal
    WHERE acnum    EQ @lv_acnum
      AND operacao EQ @lv_oper
      AND matkl    EQ @lv_matkl
      AND plday    BETWEEN @lv_begda AND @lv_endda.

    IF sy-subrc EQ 0.
      SORT lt_prog BY actrn.
    ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    SORT lt_prog BY plday actrn.

    LOOP AT lt_dates INTO DATA(ls_date).
      DATA(lv_day) = sy-tabix.
      READ TABLE lt_prog TRANSPORTING NO FIELDS
        WITH KEY plday = ls_date BINARY SEARCH.
      IF sy-subrc EQ 0.
        LOOP AT lt_prog INTO DATA(ls_prog) FROM sy-tabix.
          IF ls_prog-plday NE ls_date.
            EXIT.
          ELSE.
            IF ls_prog-actrn EQ 'T1'.
              DATA(lv_turno) = 'Turno 1'.
            ELSEIF ls_prog-actrn EQ 'T2'.
              lv_turno = 'Turno 2'.
            ELSEIF ls_prog-actrn EQ 'T3'.
              lv_turno = 'Turno 3'.
            ELSE.
              CLEAR lv_turno.
            ENDIF.
          ENDIF.

          IF lv_turno IS NOT INITIAL.
            READ TABLE et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>)
              WITH KEY turno = lv_turno.
            IF sy-subrc NE 0.
              INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
            ENDIF.

            IF <ls_entityset> IS ASSIGNED.
              lv_h_field = 'D' && lv_day && 'H'.
              lv_r_field = 'D' && lv_day && 'R'.
              ASSIGN COMPONENT lv_h_field OF STRUCTURE <ls_entityset>
                TO FIELD-SYMBOL(<lv_h_field>).
              IF sy-subrc EQ 0.
                <lv_h_field> = ls_prog-plapr.
              ENDIF.

              ASSIGN COMPONENT lv_r_field OF STRUCTURE <ls_entityset>
                TO FIELD-SYMBOL(<lv_r_field>).
              IF sy-subrc EQ 0.
                <lv_r_field> = ls_prog-qtyrs.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    ls_entityset-turno = 'Total'.
    APPEND ls_entityset TO et_entityset.

  ENDMETHOD.
ENDCLASS.
