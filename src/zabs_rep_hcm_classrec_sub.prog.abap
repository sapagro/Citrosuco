************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_CLASSREC_SUB                           *
* Tcode          : ZABS_TRN_HCMCLASS                                   *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Daniele Janes                                       *
* Created on     : 30.03.2020                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Create job with daily execution to read class       *
*                  records in HCM and update FARM class records        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH gt_final.
  CLEAR: gobj_cont, gobj_grid.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_DATA
*&---------------------------------------------------------------------*
*& FETCH_DATA
*&---------------------------------------------------------------------*
FORM fetch_data.

  TYPES: BEGIN OF ly_turma,
           turma             TYPE yocod_turma,
           cod_encarreg_turm TYPE zhrst_cod_encarr,
           lider_turma       TYPE persno,
           usr_aprov         TYPE uname,
           usr_aprov2        TYPE uname,
           orgeh             TYPE orgeh,
           usr_aprov_aux     TYPE sysid,
           usr_aprov2_aux    TYPE sysid,
         END OF ly_turma.

*-- Local Declarations
  DATA: lt_insert_hdr     TYPE STANDARD TABLE OF zfmfpgrouphdr INITIAL SIZE 0,
        lt_inactivate_hdr TYPE STANDARD TABLE OF zfmfpgrouphdr INITIAL SIZE 0,
        lt_insert_itm     TYPE STANDARD TABLE OF zfmfpgroupitm INITIAL SIZE 0,
        lt_delete_itm     TYPE STANDARD TABLE OF zfmfpgroupitm INITIAL SIZE 0,
        lt_turma          TYPE STANDARD TABLE OF ly_turma INITIAL SIZE 0,
        lrt_user          TYPE RANGE OF sysid,
        ls_final          TYPE zabs_str_classrec,
        lv_endda          TYPE endda VALUE '99991231',
        lv_turma          TYPE yocod_turma.

*-- Constants
  CONSTANTS: BEGIN OF lc_turma,
               colheita TYPE zfmturma_tipo VALUE 'ZCLH',
             END OF lc_turma,

             BEGIN OF lc_status_turma,
               ativo   TYPE zfm_status VALUE '01',
               inativo TYPE zfm_status VALUE '02',
             END OF lc_status_turma,

             lc_pernr_from_usrid TYPE subty VALUE '0001',

             BEGIN OF lc_grupo_empreg,
               eletricista TYPE persg VALUE 'A',
               cozinheiro  TYPE persg VALUE 'F',
               horista     TYPE persg VALUE 'H',
               mensalista  TYPE persg VALUE 'M',
               colheita    TYPE persg VALUE 'S',
             END OF lc_grupo_empreg,

             lc_autonomos TYPE persk VALUE 'SI'.

*-- Fetch the Turmas from HCM
  SELECT a~turma, a~cod_encarreg_turm, a~lider_turma,
         a~usr_aprov, a~usr_aprov2,
         b~orgeh
    FROM zhrst_tb_turma AS a
    LEFT OUTER JOIN yoturmas AS b
               ON b~turma = a~turma
    INTO TABLE @lt_turma
   WHERE a~endda GE @p_date
     AND a~begda LE @p_date.

  IF sy-subrc NE 0.
*-- Sem turmas disponíveis com a seleção dada
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT lt_turma ASSIGNING FIELD-SYMBOL(<ls_turma>).
*-- Supervisor
      <ls_turma>-usr_aprov_aux  = <ls_turma>-usr_aprov.
      IF <ls_turma>-usr_aprov_aux IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE lrt_user
          ASSIGNING FIELD-SYMBOL(<lrs_user>).
        IF sy-subrc EQ 0.
          <lrs_user> = 'IEQ'.
          <lrs_user>-low = <ls_turma>-usr_aprov_aux .
        ENDIF.
      ENDIF.
*-- Coordenador
      <ls_turma>-usr_aprov2_aux = <ls_turma>-usr_aprov2.
      IF <ls_turma>-usr_aprov2_aux IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE lrt_user
          ASSIGNING <lrs_user>.
        IF sy-subrc EQ 0.
          <lrs_user> = 'IEQ'.
          <lrs_user>-low = <ls_turma>-usr_aprov2_aux .
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lrt_user[] IS NOT INITIAL.
      SELECT pernr, subty, endda, begda, usrid
        INTO TABLE @DATA(lt_pa0105)
        FROM pa0105
       WHERE subty EQ @lc_pernr_from_usrid
         AND endda GE @p_date
         AND begda LE @p_date
         AND usrid IN @lrt_user[].

      SORT lt_pa0105 BY usrid.
    ENDIF.
  ENDIF.

  DATA(lt_orgeh) = lt_turma[].
  DATA lv_colheita TYPE persg VALUE 'S'.

  IF lt_orgeh[] IS NOT INITIAL.
*-- Fetch the employess under HCM Turmas
    SELECT a~pernr, a~endda, a~begda, a~persg,
           a~persk, a~orgeh, b~stat2
      FROM pa0001 AS a
     INNER JOIN pa0000 AS b
        ON b~pernr EQ a~pernr
      INTO TABLE @DATA(lt_employee)
       FOR ALL ENTRIES IN @lt_orgeh
     WHERE a~orgeh EQ @lt_orgeh-orgeh
       AND a~persg EQ @lc_grupo_empreg-colheita "S
       AND a~persk NE @lc_autonomos. "SI

    DELETE lt_employee WHERE endda NE lv_endda. "31/12/9999

    IF lt_employee[] IS INITIAL.
*-- Nenhum funcionário disponível na turma
      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*-- Fetch the Turmas in FARM class
  SELECT *
    FROM zfmfpgrouphdr
    INTO TABLE @DATA(lt_grphdr).

  SORT lt_grphdr BY turma_id.

*-- Fetch the employees under FARM class Turmas
  SELECT *
    FROM zfmfpgroupitm
    INTO TABLE @DATA(lt_grpitm)
    FOR ALL ENTRIES IN @lt_grphdr
   WHERE turma_id EQ @lt_grphdr-turma_id.

  IF sy-subrc EQ 0.
    DELETE zfmfpgroupitm FROM TABLE lt_grpitm[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  REFRESH lt_grpitm.

  SORT: lt_turma    BY turma,
        lt_grphdr   BY turma_id,
        lt_employee BY orgeh pernr stat2.

*-- Read the ABS Turma
  LOOP AT lt_grphdr INTO DATA(ls_grphdr).
*-- Read the HCM Turma
    READ TABLE lt_turma INTO DATA(ls_turma)
      WITH KEY turma = ls_grphdr-turma_id BINARY SEARCH.
*-- Inactivate
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE lt_inactivate_hdr
        ASSIGNING FIELD-SYMBOL(<ls_inactivate_hdr>).
      IF sy-subrc EQ 0.
        <ls_inactivate_hdr> = ls_grphdr.
        <ls_inactivate_hdr>-mandt = sy-mandt.
        <ls_inactivate_hdr>-status = lc_status_turma-inativo.
        <ls_inactivate_hdr>-erdat = sy-datum.
        <ls_inactivate_hdr>-erzet = sy-uzeit.
        <ls_inactivate_hdr>-ernam = sy-uname.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-- Read the HCM Turma
  LOOP AT lt_turma INTO ls_turma.
    IF ls_turma-turma EQ lv_turma.
      CONTINUE.
    ENDIF.
*-- Read the ABS Turma
    READ TABLE lt_grphdr INTO ls_grphdr
      WITH KEY turma_id = ls_turma-turma BINARY SEARCH.
*-- Create ABS Turma
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE lt_insert_hdr
        ASSIGNING FIELD-SYMBOL(<ls_insert_hdr>).
      IF sy-subrc EQ 0.
        <ls_insert_hdr>-mandt = sy-mandt.
        <ls_insert_hdr>-turma_id = ls_turma-turma.
        <ls_insert_hdr>-turma_tipo = lc_turma-colheita.
        CONCATENATE  'Turma' space ls_turma-turma
          INTO <ls_insert_hdr>-turma_text RESPECTING BLANKS.
        <ls_insert_hdr>-status = lc_status_turma-ativo.
        <ls_insert_hdr>-erdat = sy-datum.
        <ls_insert_hdr>-erzet = sy-uzeit.
        <ls_insert_hdr>-ernam = sy-uname.

        READ TABLE lt_pa0105 INTO DATA(ls_pa0105)
          WITH KEY usrid = ls_turma-usr_aprov2_aux.
        IF sy-subrc EQ 0.
          <ls_insert_hdr>-coordenador = ls_pa0105-pernr.
        ENDIF.

        READ TABLE lt_pa0105 INTO ls_pa0105
          WITH KEY usrid = ls_turma-usr_aprov_aux.
        IF sy-subrc EQ 0.
          <ls_insert_hdr>-supervisor = ls_pa0105-pernr.
        ENDIF.

        <ls_insert_hdr>-lider_turma = ls_turma-lider_turma.
        <ls_insert_hdr>-supervisor  = ls_turma-usr_aprov.
        <ls_insert_hdr>-coordenador = ls_turma-usr_aprov2.
      ENDIF.
    ENDIF.

    lv_turma = ls_turma-turma.
  ENDLOOP.

  SORT lt_turma BY orgeh.

  DATA(lt_employee_aux) = lt_employee[].
  SORT lt_employee_aux BY pernr.

*-- Read the ABS Employee
  LOOP AT lt_employee INTO DATA(ls_employee).
    READ TABLE lt_turma INTO ls_turma
      WITH KEY orgeh = ls_employee-orgeh BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
      EXPORTING
        pernr            = ls_employee-pernr
        begda            = p_date
        endda            = '99991231'
      EXCEPTIONS
        pernr_not_found  = 1
        pernr_not_active = 2
        OTHERS           = 3.

*-- Active employee in HCM
    IF sy-subrc EQ 0.
*-- Add employee in FARM if employee is active in HCM
      INSERT INITIAL LINE INTO TABLE lt_insert_itm
        ASSIGNING FIELD-SYMBOL(<ls_grpitm>).
      IF sy-subrc EQ 0.
        <ls_grpitm>-mandt      = sy-mandt.
        <ls_grpitm>-turma_id   = ls_turma-turma.
        <ls_grpitm>-pernr      = ls_employee-pernr.
        <ls_grpitm>-idresource = ls_employee-pernr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_insert_hdr[] IS NOT INITIAL.
    SORT lt_insert_hdr BY turma_id.
    DELETE ADJACENT DUPLICATES FROM lt_insert_hdr COMPARING turma_id.
*-- Create ABS Turma
    MODIFY zfmfpgrouphdr FROM TABLE lt_insert_hdr.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_inactivate_hdr[] IS NOT INITIAL.
*-- Inactivate ABS Turma
    MODIFY zfmfpgrouphdr FROM TABLE lt_inactivate_hdr.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_delete_itm[] IS NOT INITIAL.
    SORT lt_delete_itm BY turma_id pernr.
    DELETE ADJACENT DUPLICATES FROM lt_delete_itm COMPARING turma_id pernr.
*-- Delete the in active HCM employees from FARM Turmas
    DELETE zfmfpgroupitm FROM TABLE lt_delete_itm.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_insert_itm[] IS NOT INITIAL.
    SORT lt_insert_itm BY turma_id pernr.
    DELETE ADJACENT DUPLICATES FROM lt_insert_itm COMPARING turma_id pernr.
*-- Insert the active employees to FARM Turmas
    MODIFY zfmfpgroupitm FROM TABLE lt_insert_itm.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  LOOP AT lt_insert_itm INTO DATA(ls_grpitm).
    CLEAR ls_final.
    ls_final-icon = zcl_abs_abap_maintain=>c_icon_go. "'@5B@'
    ls_final-turma_id = ls_grpitm-turma_id.
    ls_final-pernr = ls_grpitm-pernr.
    ls_final-idresource = ls_grpitm-idresource.
    APPEND ls_final TO gt_final.
  ENDLOOP.

  LOOP AT lt_delete_itm INTO ls_grpitm.
    CLEAR ls_final.
    ls_final-icon = zcl_abs_abap_maintain=>c_icon_stop. "'@5C@'
    ls_final-turma_id = ls_grpitm-turma_id.
    ls_final-pernr = ls_grpitm-pernr.
    ls_final-idresource = ls_grpitm-idresource.
    APPEND ls_final TO gt_final.
  ENDLOOP.

  SORT gt_final BY turma_id icon.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*& DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM display_output.
  IF gt_final IS NOT INITIAL.
    CALL SCREEN 100.
  ELSE.
    MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set.
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*& USER_COMMAND_0100
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM user_command.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
*& USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.

  IF sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_back. "'BACK'
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
FORM controls_display.

*-- Local Declarations
  DATA: lt_fcat   TYPE lvc_t_fcat,
        ls_layout TYPE lvc_s_layo.

  IF cl_gui_alv_grid=>offline( ) IS INITIAL. "Online - ALV Display

*--Create Object For Custom Container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'ZHCM_CLASS_100CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ELSE.                                     ""Offline - ALV in Spool

    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ENDIF.

*-- Field Catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_str_hcm_classrec ""'ZABS_STR_CLASSREC'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--Set ALV attributes FOR LAYOUT
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.

*--Displaying ALV Data
  IF gobj_grid IS NOT INITIAL.
    CALL METHOD gobj_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_final
        it_fieldcatalog               = lt_fcat
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

ENDFORM.
