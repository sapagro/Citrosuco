FUNCTION zabs_fm_shp_exit_proc_muda.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_SHP_EXIT_PORTA_ENXERTO                  *
* Created By        :  Helio Kababe                                    *
* Requested by      :  Daniele Janes                                   *
* Created on        :  12.14.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Search help exit for Porta Enxerto              *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

  DATA: lt_value_tab TYPE api_value OCCURS 0 WITH HEADER LINE,
        lt_cabn      TYPE cabn OCCURS 1  WITH HEADER LINE,
        lt_cawn      TYPE cawn OCCURS 10 WITH HEADER LINE,
        lt_cawnt     TYPE cawnt OCCURS 0 WITH HEADER LINE,
        lt_fcat      TYPE slis_t_fieldcat_alv,
        lv_atnam     TYPE /agri/gatnam VALUE 'PROC_MUDA',
        lv_value     TYPE atwrt,
        lt_values    TYPE TABLE OF cawn.

  RANGES: lrt_atinn FOR cabn-atinn.

  IF callcontrol-step EQ 'DISP'.
*    BREAK-POINT.
    REFRESH lt_cabn.
*-- CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
    CALL FUNCTION 'CLSE_SELECT_CABN_VIA_NAME'
      EXPORTING
        characteristic = lv_atnam
      TABLES
        t_cabn         = lt_cabn
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.

    IF lt_cabn-atidn IS INITIAL.
      CLEAR lt_cabn.
      READ TABLE lt_cabn INDEX 1.
      IF sy-subrc EQ 0.
        REFRESH lrt_atinn.
        lrt_atinn = 'IEQ'.
        lrt_atinn-low = lt_cabn-atinn.
        APPEND lrt_atinn.

****Get values of characteristic
        IF NOT lt_cabn-atfor EQ 'ADT '
        AND NOT lt_cabn-atfor EQ 'UDEF'
        AND lt_cabn-auswahlmge IS INITIAL.
          REFRESH lt_cawn.
          CALL FUNCTION 'CLSE_SELECT_CAWN'
            TABLES
              in_cabn        = lrt_atinn
              t_cawn         = lt_cawn
            EXCEPTIONS ##FM_SUBRC_OK
              no_entry_found = 1
              OTHERS         = 2.

          IF lt_cabn-atfor EQ 'CHAR'
          OR lt_cabn-atfor EQ 'BOOL'.
            CALL FUNCTION 'CLSE_SELECT_CAWNT'
              EXPORTING
                language       = sy-langu
              TABLES
                in_cabn        = lrt_atinn
                t_cawnt        = lt_cawnt
              EXCEPTIONS
                no_entry_found = 1
                OTHERS         = 2.

****Sort values
            SORT lt_cawn BY atzhl.

            CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
              EXPORTING
                i_structure_name       = 'API_VALUE'
              CHANGING
                ct_fieldcat            = lt_fcat[]
              EXCEPTIONS
                inconsistent_interface = 1
                program_error          = 2
                OTHERS                 = 3.

            LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<lwa_fcat>).
              CASE <lwa_fcat>-fieldname.
                WHEN 'ATWRT'.
                WHEN 'ATWTB'.
                  <lwa_fcat>-tech = abap_true.
                WHEN 'ATAUT'.
                  <lwa_fcat>-tech = abap_true.
                WHEN OTHERS.
                  <lwa_fcat>-tech = abap_true.
              ENDCASE.
            ENDLOOP.

            LOOP AT lt_cawn.
              lt_value_tab-atinn = lt_cawn-atinn.
              lt_value_tab-atwrt = lt_cawn-atwrt.

              READ TABLE lt_cawnt WITH KEY atinn = lt_cawn-atinn
                                           atzhl = lt_cawn-atzhl.
              IF sy-subrc EQ 0.
                lt_value_tab-atwtb = lt_cawnt-atwtb.
              ELSE.
                CLEAR lt_value_tab-atwtb.
              ENDIF.

              APPEND lt_value_tab.
              CLEAR: lt_value_tab.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*--Filling record_tab from internal table
    LOOP AT lt_value_tab.
      record_tab-string+0(70) = lt_value_tab-atwrt.
      record_tab-string+71(70) = lt_value_tab-atwtb.
      APPEND record_tab.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
