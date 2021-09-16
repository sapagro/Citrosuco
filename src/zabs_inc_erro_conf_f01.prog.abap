*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_STORDENS_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_data .

  DATA: lt_acdoc TYPE /agri/t_fmacs_doc,
        lt_accom TYPE /agri/t_fmacom,
        ls_accom TYPE /agri/s_fmacom,
        lv_subrc TYPE sysubrc,
        lv_msgli TYPE bapi_msg, "syst_msgli,
        lv_accom TYPE /agri/fmaccom.

  LOOP AT gt_fmacitm INTO DATA(ls_fmacitm).
    IF ls_fmacitm-idresource IS INITIAL.
      INSERT INITIAL LINE INTO TABLE gt_outtab
        ASSIGNING FIELD-SYMBOL(<ls_outtab>).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
        <ls_outtab>-id = '@5C@'. "ICON_RED_LIGHT
*-- ID do Empregado Vazio!
        <ls_outtab>-message = TEXT-m01.
      ENDIF.
    ENDIF.

    IF ls_fmacitm-equnr IS INITIAL.
      INSERT INITIAL LINE INTO TABLE gt_outtab
        ASSIGNING <ls_outtab>.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
        <ls_outtab>-id = '@5C@'. "ICON_RED_LIGHT
*-- ID do Equipamento Vazio!
        <ls_outtab>-message = TEXT-m02.
      ENDIF.
    ENDIF.

*    IF ls_fmacitm-idactvl IS INITIAL.
*      INSERT INITIAL LINE INTO TABLE gt_outtab
*        ASSIGNING <ls_outtab>.
*      IF sy-subrc EQ 0.
*        MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
**-- Atividade do Empregado não informada!
*        <ls_outtab>-message = TEXT-m03.
*      ENDIF.
*    ENDIF.

    IF ls_fmacitm-idactve IS INITIAL.
      INSERT INITIAL LINE INTO TABLE gt_outtab
        ASSIGNING <ls_outtab>.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
        <ls_outtab>-id = '@5C@'. "ICON_RED_LIGHT
*-- Atividade do Equipamento não informada!
        <ls_outtab>-message = TEXT-m04.
      ENDIF.
    ENDIF.

    IF ls_fmacitm-duran GT 24.
      INSERT INITIAL LINE INTO TABLE gt_outtab
        ASSIGNING <ls_outtab>.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
        <ls_outtab>-id = '@5C@'. "ICON_RED_LIGHT
*-- Duração maior que 24 horas!
        <ls_outtab>-message = TEXT-m05.
      ENDIF.
    ENDIF.

    REFRESH: lt_accom, lt_acdoc.
    IF lv_accom NE ls_fmacitm-accom.
      ls_accom = ls_fmacitm-accom.
      APPEND ls_accom TO lt_accom.

*-- Fetch the Accomplishment data
      CALL FUNCTION '/AGRI/FMAC_VIEW'
        EXPORTING
          it_accom       = lt_accom
        IMPORTING
          et_acdoc       = lt_acdoc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        READ TABLE lt_acdoc INTO DATA(ls_acdoc_infocus) INDEX 1.
        IF sy-subrc EQ 0.
          CLEAR: lv_subrc, lv_msgli.
          PERFORM conf_time_validate USING ls_acdoc_infocus
                                  CHANGING lv_subrc
                                           lv_msgli.
          IF lv_subrc EQ 4.
            INSERT INITIAL LINE INTO TABLE gt_outtab
              ASSIGNING <ls_outtab>.
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING ls_fmacitm TO <ls_outtab>.
              <ls_outtab>-id = '@5C@'. "ICON_RED_LIGHT
*-- Data lançamento &1 já tem limite de 24 horas reportado por &2 &3.
              <ls_outtab>-message = lv_msgli.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    lv_accom = ls_fmacitm-accom.
  ENDLOOP.

  IF gt_outtab[] IS INITIAL.
*--Não existem registros os parâmetros informados!
    MESSAGE i209(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  SET PF-STATUS 'MAIN100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  SET TITLEBAR 'MAIN100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.

  DATA: lt_fcat    TYPE slis_t_fieldcat_alv,
        lt_buttons TYPE ui_functions,
        ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant.

  PERFORM build_fcat CHANGING lt_fcat.

  PERFORM define_buttons CHANGING lt_buttons.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = g_custom_container.

*--Set ALV attributes FOR LAYOUT
    ls_layout-cwidth_opt = abap_true.
    ls_layout-zebra      = abap_true.

*--Displaying ALV Data
    ls_variant-report = sy-repid.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_buttons
        i_structure_name              = 'ZABS_STR_ERRO_CONF'
      CHANGING
        it_outtab                     = gt_outtab
        it_fieldcatalog               = gt_fieldcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat CHANGING lt_fcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_ERRO_CONF'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CASE <ls_fcat>-fieldname.
        WHEN '.INCLUDE'.
          <ls_fcat>-no_out = c_true.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fcat
    IMPORTING
      et_fieldcat_lvc = gt_fieldcat[]
    TABLES
      it_data         = gt_outtab[]
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DEFINE_BUTTONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_BUTTONS
*&---------------------------------------------------------------------*
FORM define_buttons CHANGING lt_buttons TYPE ui_functions.

  DATA: ls_button TYPE ui_func.

*--Toolbar Button CHECK
  ls_button = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button REFRESH
  ls_button = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button UNDO
  ls_button = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button PRINT
  ls_button = cl_gui_alv_grid=>mc_fc_print_back.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button Print Preview
  ls_button = cl_gui_alv_grid=>mc_fc_print_prev.
  APPEND ls_button TO lt_buttons.
*--Menu Button PASTE add Menu Item PASTE NEW ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button INSERT ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button DELETE ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button COPY ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button APPEND ROW
  ls_button = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button CUT
  ls_button = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_button TO lt_buttons.
*--Toolbar Button INFORMATION
  ls_button = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_button TO lt_buttons.
*--Menu Button SUBTOTAL
  ls_button = cl_gui_alv_grid=>mc_mb_subtot.
  APPEND ls_button TO lt_buttons.
*--Menu Button SUM add Menu Item SUM
  ls_button = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_mb_view.
  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_mb_sum.
  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_button TO lt_buttons.
  ls_button = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_button TO lt_buttons.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_CONFIRMATION_ERRORS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_confirmation_errors .
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      SET SCREEN '0'.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.



ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONF_TIME_VALIDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM conf_time_validate USING ls_acdoc_infocus TYPE /agri/s_fmacs_doc
                     CHANGING lv_subrc         TYPE sysubrc
                              lv_msgli         TYPE bapi_msg. "syst_msgli.

  DATA: lt_fmacitm    TYPE /agri/t_fmacitm,
        lt_fmacitm2   TYPE /agri/t_fmacitm,
        lt_fmacitmtmp TYPE /agri/t_fmacitm,
        lt_daytim     TYPE /agri/t_fmac_dur_daytime,
        lv_tim        TYPE t,
        lv_tim2       TYPE t,
        lv_dattmp     TYPE d,
        lv_day        TYPE d,
        lv_timadd     TYPE c LENGTH 11,
        lv_timstr     TYPE c LENGTH 11,
        lv_timadd2    TYPE t,
        lv_timstr2    TYPE t,
        lv_target     TYPE c,
        lv_resource   TYPE /agri/fmidrsc,
        lv_name       TYPE c LENGTH 9,
        lv_length     TYPE i,
        lv_subrctmp   TYPE sysubrc,
        lr_budat      TYPE RANGE OF budat,
        lwa_budat     LIKE LINE OF lr_budat.

  FIELD-SYMBOLS: <lwa_fmacitml>   TYPE /agri/s_fmacitm_layout,
                 <lwa_fmacact>    TYPE /agri/fmacact,
                 <lwa_fmacitm>    TYPE /agri/s_fmacitm,
                 <lwa_fmacitmtmp> TYPE /agri/s_fmacitm,
                 <lwa_daytim>     TYPE /agri/s_fmac_dur_daytime,
                 <lwa_items_tmp>  TYPE /agri/s_fmacitm_layout.

  lt_fmacitmtmp = ls_acdoc_infocus-x-acitm.

  LOOP AT lt_fmacitmtmp ASSIGNING <lwa_fmacitmtmp> WHERE zzconfm IS INITIAL.
    DATA(lv_tabix1) = sy-tabix.
    lt_fmacitm = lt_fmacitmtmp.
    lwa_budat-option = c_operator_word-between.
    lwa_budat-sign   = c_sign-include.
    lwa_budat-low    = <lwa_fmacitmtmp>-strtdat.
    lwa_budat-high   = <lwa_fmacitmtmp>-findat.
    APPEND lwa_budat TO lr_budat.

    DELETE lt_fmacitm WHERE strtdat NOT IN lr_budat
                        AND findat  NOT IN lr_budat.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      IF lv_target IS INITIAL.
        lt_fmacitm2 = lt_fmacitm.
        DELETE lt_fmacitm2 WHERE idresource IS INITIAL
                              OR idresource
                              NE <lwa_fmacitmtmp>-idresource.
        lv_target = c_true.
        lv_name   = 'Employee'(016).
        CLEAR: lv_tim.
        REFRESH lt_daytim.
      ELSE.
        lt_fmacitm2 = lt_fmacitm.
        DELETE lt_fmacitm2 WHERE equnr IS INITIAL
                              OR equnr
                              NE <lwa_fmacitmtmp>-equnr.
        lv_name = 'Equipment'(017).
        CLEAR: lv_target, lv_tim, lv_subrctmp.
        REFRESH lt_daytim.
      ENDIF.
      LOOP AT lt_fmacitm2 ASSIGNING <lwa_fmacitm>.
        DATA(lv_tabix2) = sy-tabix.
        CLEAR: lv_timadd, lv_tim2.
        IF lv_target IS NOT INITIAL.
          lv_resource = <lwa_fmacitm>-idresource.
        ELSE.
          lv_resource = <lwa_fmacitm>-equnr.
        ENDIF.
        CHECK <lwa_fmacitm> IS ASSIGNED.
        CALL FUNCTION '/AGRI/FMAC_CALC_HR_PR_DAY'
          EXPORTING
            i_date1          = <lwa_fmacitm>-strtdat
            i_time1          = <lwa_fmacitm>-strttim
            i_date2          = <lwa_fmacitm>-findat
            i_time2          = <lwa_fmacitm>-fintim
          IMPORTING
            e_tim_tab        = lt_daytim
          EXCEPTIONS
            invalid_datetime = 1
            days_over_limit  = 2
            OTHERS           = 3.
        IF sy-subrc EQ 0.
          READ TABLE lt_daytim ASSIGNING <lwa_daytim>
            WITH KEY day = <lwa_fmacitmtmp>-strtdat.
          IF sy-subrc EQ 0.
            lv_day = <lwa_daytim>-day.
          ELSE.
            READ TABLE lt_daytim ASSIGNING <lwa_daytim>
              WITH KEY day = <lwa_fmacitmtmp>-findat.
            IF sy-subrc EQ 0.
              lv_day = <lwa_daytim>-day.
            ENDIF.
          ENDIF.
        ENDIF.
        IF <lwa_daytim>-hours IS ASSIGNED.
          WRITE <lwa_daytim>-hours TO lv_timadd.
        ENDIF.
        SHIFT lv_timadd LEFT DELETING LEADING space.
        IF lv_timadd EQ '24:00'.
          lv_timadd = '23:59'.
        ENDIF.
        lv_length = strlen( lv_timadd ).
        IF lv_length EQ 4.
          CONCATENATE '0' lv_timadd(1) lv_timadd+2(2) '00' INTO lv_timadd2.
        ELSEIF lv_length EQ 3.
          CONCATENATE '00' lv_timadd+1(2) '00' INTO lv_timadd2.
        ELSE.
          CONCATENATE lv_timadd(2) lv_timadd+3(2) '00' INTO lv_timadd2.
        ENDIF.
        CLEAR lv_dattmp.
        CALL FUNCTION '/AGRI/G_DIMP_ADD_TIME'
          EXPORTING
            i_starttime = lv_tim
            i_startdate = lv_day
            i_addtime   = lv_timadd2
          IMPORTING
            e_endtime   = lv_tim2
            e_enddate   = lv_dattmp.
*---
        lv_tim = lv_tim2.
        IF lv_tim2 GT '235900'
        OR lv_dattmp NE lv_day.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '003'
            WITH lv_day lv_name lv_resource INTO lv_msgli.
          lv_subrctmp = lv_subrc = 4.
          EXIT.
        ENDIF.
        REFRESH lt_daytim.
      ENDLOOP.
    ENDDO.
    DELETE lt_fmacitmtmp WHERE idresource = <lwa_fmacitmtmp>-idresource
                                OR equnr   = <lwa_fmacitmtmp>-equnr
                               AND strtdat NOT IN lr_budat
                               AND findat  NOT IN lr_budat.
  ENDLOOP.

ENDFORM.
