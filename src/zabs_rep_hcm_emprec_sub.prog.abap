************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_EMPREC_SUB                             *
* Tcode          : ZABS_TRN_HCMEMP                                     *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Daniele Janes                                       *
* Created on     : 30.03.2020                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : Create job with daily execution to read employee    *
*                  records in HCM and update FARM employee records     *
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
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_DATA
*&---------------------------------------------------------------------*
*& FETCH_DATA
*&---------------------------------------------------------------------*
FORM fetch_data.

  DATA: lwa_final          TYPE zabs_str_emprec,
        lv_cnval1          TYPE zabs_del_cnval,
        lwa_fmacres_insert TYPE /agri/fmacres,
        lt_fmacres_insert  TYPE SORTED TABLE OF /agri/fmacres
                           WITH UNIQUE KEY idresource arbpl,
        lt_fmacres_delete  TYPE SORTED TABLE OF /agri/fmacres
                           WITH UNIQUE KEY idresource arbpl,
        lt_fmacres_update  TYPE SORTED TABLE OF /agri/fmacres
                           WITH UNIQUE KEY idresource arbpl,
        ltr_subty          LIKE RANGE OF p2001-subty,
        lo_range_subty     TYPE REF TO data,
        lv_tabix           LIKE sy-tabix.

*--Field-Symbols declaration
  FIELD-SYMBOLS: <fs_subty> TYPE any.

  CONSTANTS: BEGIN OF lc_grupo_empreg,
               eletricista TYPE persg VALUE 'A',
               cozinheiro  TYPE persg VALUE 'F',
               horista     TYPE persg VALUE 'H',
               mensalista  TYPE persg VALUE 'M',
               colheita    TYPE persg VALUE 'S',
             END OF lc_grupo_empreg,

             lc_autonomos TYPE persk VALUE 'SI'.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_hcm_wrkc "'HCM'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_work_center "'WRKC'
    IMPORTING
      ev_cnval1 = lv_cnval1.

*--Fetching Employee records fron PA0001 and PA0000 tables
  SELECT a~pernr, b~stat2
    INTO TABLE @DATA(lt_pa0000)
    FROM pa0001 AS a
   INNER JOIN pa0000 AS b
      ON a~pernr EQ b~pernr
   WHERE a~endda GE @p_date
     AND a~begda LE @p_date
     AND a~persg EQ @lc_grupo_empreg-colheita "S
     AND a~persk NE @lc_autonomos. "SI

  IF sy-subrc NE 0.
*-- Nenhum funcionário disponível na turma
    "    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
*    LEAVE LIST-PROCESSING.
  ELSE.


*--Fetching data from (/AGRI/FMACRES)
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @DATA(lt_fmacres)
       FOR ALL ENTRIES IN @lt_pa0000
      WHERE arbpl      EQ @lv_cnval1
        AND pernr EQ @lt_pa0000-pernr.
    IF sy-subrc = 0.
      SORT lt_fmacres BY pernr.
    ENDIF.

    DATA(lt_pa0000_03) = lt_pa0000.
    DELETE lt_pa0000_03 WHERE stat2 NE 3.

*--Fetching Employee records fron PA0002 tables
    SELECT pernr, cname
      FROM pa0002
      INTO TABLE @DATA(lt_pa0002)
       FOR ALL ENTRIES IN @lt_pa0000_03
     WHERE pernr = @lt_pa0000_03-pernr.
    IF sy-subrc = 0.
      SORT lt_pa0002 BY pernr.
    ENDIF.

    LOOP AT lt_pa0000 INTO DATA(lwa_pa0000).
      CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
        EXPORTING
          pernr            = lwa_pa0000-pernr
          begda            = p_date
          endda            = '99991231'
        EXCEPTIONS
          pernr_not_found  = 1
          pernr_not_active = 2
          OTHERS           = 3.

      IF sy-subrc = 0.
        READ TABLE lt_fmacres INTO DATA(lwa_fmacres)
          WITH KEY pernr = lwa_pa0000-pernr
          BINARY SEARCH.
        IF sy-subrc NE 0.

          lwa_final-arbpl = lv_cnval1. "'COLHEITA'

          READ TABLE lt_pa0002 INTO DATA(lwa_pa0002)
          WITH KEY pernr = lwa_pa0000-pernr.
          IF sy-subrc = 0.
            lwa_final-description = lwa_pa0002-cname.
            lwa_final-idresource = lwa_pa0002-pernr.
          ENDIF.

          lwa_final-rstype = zcl_abs_abap_maintain=>c_rstyp_labour. "'A'
          lwa_final-hr = abap_true.
          lwa_final-pernr = lwa_pa0002-pernr.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_go. "'@5B@'

          MOVE-CORRESPONDING lwa_final TO lwa_fmacres_insert.

          INSERT lwa_fmacres_insert INTO TABLE lt_fmacres_insert.
          APPEND lwa_final TO gt_final.

          CLEAR : lwa_final,
                  lwa_fmacres_insert.
        ENDIF. "lt_fmacres
      ELSE.
*--Checking Employees to delete from table
        READ TABLE lt_fmacres INTO lwa_fmacres
         WITH KEY pernr = lwa_pa0000-pernr
         BINARY SEARCH.
        IF sy-subrc = 0.

          MOVE-CORRESPONDING lwa_fmacres TO lwa_final.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_stop. "'@5C@'

          INSERT lwa_fmacres INTO TABLE lt_fmacres_delete.
          APPEND lwa_final TO gt_final.

          CLEAR : lwa_fmacres,
                  lwa_final.

        ENDIF. "lt_fmacres
      ENDIF. "lwa_pa0000-stat2
    ENDLOOP. "lt_pa0000


*Begin - Adonis - 05.25.2021

    IF lt_fmacres_delete IS NOT INITIAL.
      DELETE /agri/fmacres FROM TABLE lt_fmacres_delete.
    ENDIF.

    IF lt_fmacres_insert IS NOT INITIAL.
      INSERT /agri/fmacres FROM TABLE lt_fmacres_insert.
    ENDIF.

    COMMIT WORK AND WAIT.


  ENDIF.

*--Fetching Employee records fron PA0001 and PA0000 tables
  SELECT a~pernr, b~arbpl
    INTO TABLE @DATA(lt_pa0001)
    FROM pa0001 AS a
   INNER JOIN zabs_emp_hcm_upd AS b
      ON a~kostl = b~kostl AND
         a~orgeh = b~orgeh AND
         a~stell = b~stell
   WHERE a~endda GE @p_date
     AND a~begda LE @p_date
     AND a~persg EQ @lc_grupo_empreg-mensalista. "M

  IF sy-subrc = 0.
*-- Nenhum funcionário disponível na turma
"    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'

*--Fetching data from (/AGRI/FMACRES)
    REFRESH lt_fmacres.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @lt_fmacres
       FOR ALL ENTRIES IN @lt_pa0001
      WHERE pernr EQ @lt_pa0001-pernr.
    IF sy-subrc = 0.
      SORT lt_fmacres BY pernr.
    ENDIF.

*--Fetching Employee records fron PA0002 tables
    REFRESH lt_pa0002.
    SELECT pernr, cname
      FROM pa0002
      INTO TABLE @lt_pa0002
       FOR ALL ENTRIES IN @lt_pa0001
     WHERE pernr = @lt_pa0001-pernr.
    IF sy-subrc = 0.
      SORT lt_pa0002 BY pernr.
    ENDIF.


    LOOP AT lt_pa0001 INTO DATA(lwa_pa0001).
      CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
        EXPORTING
          pernr            = lwa_pa0001-pernr
          begda            = p_date
          endda            = '99991231'
        EXCEPTIONS
          pernr_not_found  = 1
          pernr_not_active = 2
          OTHERS           = 3.

      IF sy-subrc = 0.
        READ TABLE lt_fmacres INTO lwa_fmacres
          WITH KEY pernr = lwa_pa0001-pernr
          BINARY SEARCH.
        IF sy-subrc NE 0.

          lwa_final-arbpl = lwa_pa0001-arbpl.

          READ TABLE lt_pa0002 INTO lwa_pa0002
          WITH KEY pernr = lwa_pa0001-pernr.
          IF sy-subrc = 0.
            lwa_final-description = lwa_pa0002-cname.
            lwa_final-idresource = lwa_pa0002-pernr.
          ENDIF.

          lwa_final-rstype = zcl_abs_abap_maintain=>c_rstyp_labour. "'A'
          lwa_final-hr = abap_true.
          lwa_final-pernr = lwa_pa0002-pernr.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_go. "'@5B@'

          MOVE-CORRESPONDING lwa_final TO lwa_fmacres_insert.

          INSERT lwa_fmacres_insert INTO TABLE lt_fmacres_insert.
          APPEND lwa_final TO gt_final.

          CLEAR : lwa_final,
                  lwa_fmacres_insert.
        ENDIF. "lt_fmacres
      ELSE.
*--Checking Employees to delete from table
        READ TABLE lt_fmacres INTO lwa_fmacres
         WITH KEY pernr = lwa_pa0001-pernr
         BINARY SEARCH.
        IF sy-subrc = 0.

          MOVE-CORRESPONDING lwa_fmacres TO lwa_final.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_stop. "'@5C@'

          INSERT lwa_fmacres INTO TABLE lt_fmacres_delete.
          APPEND lwa_final TO gt_final.

          CLEAR : lwa_fmacres,
                  lwa_final.

        ENDIF. "lt_fmacres
      ENDIF.
    ENDLOOP. "lt_pa0000


    IF lt_fmacres_delete IS NOT INITIAL.
      DELETE /agri/fmacres FROM TABLE lt_fmacres_delete.
    ENDIF.

    IF lt_fmacres_insert IS NOT INITIAL.
      INSERT /agri/fmacres FROM TABLE lt_fmacres_insert.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDIF.

*Absenteismo

  CREATE DATA lo_range_subty LIKE ltr_subty.

  IF lo_range_subty IS BOUND.
    CALL METHOD zcl_abs_get_variants=>get_range_constants
      EXPORTING
      iv_objid  = 'HCMA'
      iv_k1val  = 'WRKA'
      IMPORTING
        eo_range = lo_range_subty.

    ASSIGN lo_range_subty->* TO <fs_subty>.
    IF <fs_subty> IS ASSIGNED.
      ltr_subty = <fs_subty>.
    ENDIF.
  ENDIF.


  IF ltr_subty[] IS NOT INITIAL.
*--Fetching data from (/AGRI/FMACRES)
    REFRESH lt_fmacres.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @lt_fmacres
      WHERE rstype = 'A'.

    IF sy-subrc = 0.

      SORT lt_fmacres BY pernr.
*--Fetching Employee records fron PA0001 and PA0000 tables
      SELECT pernr
        INTO TABLE @DATA(lt_pa2001)
        FROM pa2001
        FOR ALL ENTRIES IN @lt_fmacres
         "WHERE pernr = @lt_fmacres-idresource
         WHERE pernr = @lt_fmacres-pernr
         AND endda GE @p_date
         AND begda LE @p_date
         AND subty IN @ltr_subty.
      IF sy-subrc = 0.

        LOOP AT lt_pa2001 INTO DATA(lwa_pa2001).
          lv_tabix = sy-tabix.
          CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
            EXPORTING
              pernr            = lwa_pa2001-pernr
              begda            = p_date
              endda            = '99991231'
            EXCEPTIONS
              pernr_not_found  = 1
              pernr_not_active = 2
              OTHERS           = 3.

          IF sy-subrc <> 0.
            DELETE lt_pa2001 INDEX lv_tabix.
          ENDIF.

        ENDLOOP.

        SORT lt_pa2001 BY pernr.
      ENDIF.

**--Fetching Employee records fron PA0002 tables
*    REFRESH lt_pa0002.
*    SELECT pernr, cname
*      FROM pa0002
*      INTO TABLE @lt_pa0002
*      FOR ALL ENTRIES IN @lt_fmacres
*       WHERE pernr = @lt_fmacres-idresource.
*    IF sy-subrc = 0.
*      SORT lt_pa0002 BY pernr.
*    ENDIF.

      LOOP AT lt_fmacres INTO lwa_fmacres.

        READ TABLE lt_pa2001 INTO lwa_pa2001
          WITH KEY pernr = lwa_fmacres-idresource
          BINARY SEARCH.
        IF sy-subrc NE 0.

*        lwa_final-arbpl = lwa_pa0001-arbpl.
*
*        READ TABLE lt_pa0002 INTO lwa_pa0002
*        WITH KEY pernr = lwa_fmacres-idresource.
*        IF sy-subrc = 0.
*          lwa_final-description = lwa_pa0002-cname.
*          lwa_final-idresource = lwa_pa0002-pernr.
*        ENDIF.
*
*        lwa_final-rstype = zcl_abs_abap_maintain=>c_rstyp_labour. "'A'
*        lwa_final-hr = abap_true.
*        lwa_final-pernr = lwa_pa0002-pernr.
          lwa_fmacres-zabsausencia = ' '.
          MOVE-CORRESPONDING lwa_fmacres TO lwa_final.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_go. "'@5B@'

          MOVE-CORRESPONDING lwa_fmacres TO lwa_fmacres_insert.

          INSERT lwa_fmacres_insert INTO TABLE lt_fmacres_update.
          "APPEND lwa_final TO gt_final.

          CLEAR : lwa_final,
                  lwa_fmacres_insert.
        ELSE.

          lwa_fmacres-zabsausencia = 'X'.
          MOVE-CORRESPONDING lwa_fmacres TO lwa_final.
          lwa_final-icon = zcl_abs_abap_maintain=>c_icon_go. "'@5B@'

          MOVE-CORRESPONDING lwa_fmacres TO lwa_fmacres_insert.

          INSERT lwa_fmacres_insert INTO TABLE lt_fmacres_update.
          APPEND lwa_final TO gt_final.

          CLEAR : lwa_final,
                  lwa_fmacres_insert.

        ENDIF.
      ENDLOOP. "lt_pa0000

      IF lt_fmacres_update IS NOT INITIAL.
        MODIFY /agri/fmacres FROM TABLE lt_fmacres_update.
      ENDIF.

      COMMIT WORK AND WAIT.

    ENDIF.
  ENDIF.

  IF gt_final[] IS INITIAL.
*-- Nenhum funcionário disponível na turma
    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.
*End - Adonis - 05.25.2021

  SORT gt_final BY icon idresource arbpl.
  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING icon idresource arbpl.

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
        container_name = 'ZHCM_EMP_100CC'.

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
      i_structure_name       = zcl_abs_abap_maintain=>c_str_hcm_emprec "'ZABS_STR_EMPREC'
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
