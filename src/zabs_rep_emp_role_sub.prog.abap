************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_EMP_ROLE_SUB                               *
* Tcode          : ZABS_TRN_EMPROLE                                    *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 05.26.2020                                          *
* TR             : C4DK914465                                          *
* Version        : 001                                                 *
* Description    : Employees Role                                      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form F4_FILEPATH
*&---------------------------------------------------------------------*
*& F4_FILEPATH
*&---------------------------------------------------------------------*
FORM f4_filepath.

*--Local variable declaration
  DATA: lv_rcount TYPE i,

*--internal table declaration
        lt_path   TYPE filetable.

*--Calling method to get File path
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = lt_path
      rc         = lv_rcount.

  IF lt_path IS NOT INITIAL.
    READ TABLE lt_path INTO DATA(ls_path) INDEX 1.
    IF sy-subrc EQ 0.
      p_path = ls_path.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_EXCEL_DATA
*&---------------------------------------------------------------------*
*& GET_EXCEL_DATA
*&---------------------------------------------------------------------*
FORM get_excel_data.

*--Local data Declaration
  DATA : lwa_emp_role_temp TYPE zabs_emp_role,

         lt_tab_raw        TYPE truxs_t_text_data,
         lt_insert         TYPE STANDARD TABLE OF zabs_emp_role,
         lt_taba_urole     TYPE STANDARD TABLE OF dd07v,
         lt_taba_pevt      TYPE STANDARD TABLE OF dd07v,
         lt_taba_fconf     TYPE STANDARD TABLE OF dd07v,

         lv_errors         TYPE char1,
         lv_tabix          TYPE sy-tabix,
         lv_file           TYPE rlgrap-filename.

*--FM to convert excel data format to sap format
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = abap_true
      i_tab_raw_data       = lt_tab_raw
      i_filename           = p_path
    TABLES
      i_tab_converted_data = gt_upl_act
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.

    LEAVE LIST-PROCESSING.
    LEAVE TO SCREEN 0.

  ELSE.

*--Fetch User Employee data
    SELECT *
      FROM zabs_emp_role
      INTO TABLE @DATA(lt_emp_role).
    IF sy-subrc = 0.
      SORT lt_emp_role BY pernr.
      DATA(lt_emp_role_temp) = lt_emp_role.
      DELETE ADJACENT DUPLICATES FROM lt_emp_role_temp COMPARING pernr.
    ENDIF.

*--Fetching Personnel Number values
    SELECT pernr
      FROM pa0003
      INTO TABLE @DATA(lt_pernr)
       FOR ALL ENTRIES IN @gt_upl_act
     WHERE pernr EQ @gt_upl_act-pernr.
    IF sy-subrc = 0.
      SORT lt_pernr BY pernr.
    ENDIF.

    PERFORM dom_values USING zcl_abs_abap_maintain=>c_domain_urole "'ZABS_DOM_UROLE'
                    CHANGING lt_taba_urole.

    PERFORM dom_values USING zcl_abs_abap_maintain=>c_domain_pevt "'ZABS_DOM_PEVT'
                    CHANGING lt_taba_pevt.

    PERFORM dom_values USING zcl_abs_abap_maintain=>c_domain_fpcnf "'ZABS_DOM_FPCNF'
                    CHANGING lt_taba_fconf.


*--Processing excel data with validations to fill the final table
    LOOP AT gt_upl_act ASSIGNING
    FIELD-SYMBOL(<fs_upl_act>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_upl_act>-urole
        IMPORTING
          output = <fs_upl_act>-urole.


      lv_tabix = sy-tabix + 1.
      LOOP AT gt_upl_act INTO DATA(ls_upl_act) FROM lv_tabix.
        IF <fs_upl_act>-pernr EQ ls_upl_act-pernr.
          <fs_upl_act>-msgtxt = TEXT-002.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_warning.
          "'@09@'
        ENDIF.
      ENDLOOP.

      IF <fs_upl_act>-msgtxt IS NOT INITIAL.
        CONTINUE.
      ENDIF.

*--Validating whether Personnel Number is mandatory or not
      IF  <fs_upl_act>-pernr IS INITIAL.
        <fs_upl_act>-msgtxt = TEXT-003.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@'
        CONTINUE.
      ENDIF.

*--Validating Personnel Number
      IF <fs_upl_act>-pernr IS NOT INITIAL.
        READ TABLE lt_pernr INTO DATA(lwa_pernr)
        WITH KEY pernr = <fs_upl_act>-pernr
        BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-004.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

      IF <fs_upl_act>-urole IS NOT INITIAL.
*--Validating User Role
        READ TABLE lt_taba_urole TRANSPORTING NO FIELDS
              WITH KEY domvalue_l = <fs_upl_act>-urole.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-009.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.

        ENDIF.
      ENDIF.

*--Validating User Role field
      IF <fs_upl_act>-urole EQ space.
        <fs_upl_act>-msgtxt = TEXT-010.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@' .
        CONTINUE.
      ENDIF.

*--Validating Events Production Values
      IF <fs_upl_act>-pevt IS NOT INITIAL.
        READ TABLE lt_taba_pevt TRANSPORTING NO FIELDS
              WITH KEY domvalue_l = <fs_upl_act>-pevt.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-005.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Event Production Field
      IF <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_leader "'LE'
            OR <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_incharge ."'IN'
        IF <fs_upl_act>-pevt IS INITIAL.
          <fs_upl_act>-msgtxt = TEXT-006.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@' .
          CONTINUE.
        ENDIF.
      ELSE.
*--Validating Farming Confirmatoin Values
        IF <fs_upl_act>-fpcnf IS INITIAL.
          <fs_upl_act>-msgtxt = TEXT-008.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@' .
          CONTINUE.
        ENDIF.

      ENDIF.

*--Validating Farming Confirmatoin Values
      IF <fs_upl_act>-fpcnf IS NOT INITIAL.
        READ TABLE lt_taba_fconf TRANSPORTING NO FIELDS
              WITH KEY domvalue_l = <fs_upl_act>-fpcnf.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-007.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Farming Confirmatoin Values
*      IF <fs_upl_act>-urole NE zcl_abs_abap_maintain=>c_role_leader "'LE'
*                  OR <fs_upl_act>-urole NE zcl_abs_abap_maintain=>c_role_incharge ."'IN'
*        IF <fs_upl_act>-fpcnf IS INITIAL.
*          <fs_upl_act>-msgtxt = TEXT-008.
*          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
*          "'@0A@' .
*          CONTINUE.
*        ENDIF.
*      ENDIF.

*--Inserting records into Employee Role table
      IF <fs_upl_act>-msgtyp IS INITIAL.

        READ TABLE lt_emp_role INTO DATA(lwa_emp_role)
                               WITH KEY pernr = <fs_upl_act>-pernr
                               BINARY SEARCH.
        IF sy-subrc NE 0.
*--Inserting records into ZABS_USR_EMP
          lwa_emp_role-pernr = <fs_upl_act>-pernr.
          lwa_emp_role-urole = <fs_upl_act>-urole.
          lwa_emp_role-pevt  = <fs_upl_act>-pevt.
          lwa_emp_role-fpcnf = <fs_upl_act>-fpcnf.
          lwa_emp_role-ernam = sy-uname.
          lwa_emp_role-erdat = sy-datum.
          lwa_emp_role-erzet = sy-uzeit.
          APPEND lwa_emp_role TO lt_insert.
          CLEAR lwa_emp_role.
          <fs_upl_act>-msgtyp =
          zcl_abs_abap_maintain=>c_icon_success. "'@08@'
          <fs_upl_act>-msgtxt = TEXT-013.
        ELSE.
          <fs_upl_act>-msgtyp =
          zcl_abs_abap_maintain=>c_icon_error.
          <fs_upl_act>-msgtxt = TEXT-014.
        ENDIF. "lt_usr_emp

      ENDIF. "<fs_upl_act>
    ENDLOOP. "gt_upl_act

*--Inserting record from User Employee table
    IF lt_insert IS NOT INITIAL.
      INSERT zabs_emp_role FROM TABLE lt_insert.
    ENDIF.

    COMMIT WORK.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DOM_VALUES
*&---------------------------------------------------------------------*
*& DOM_VALUES
*&---------------------------------------------------------------------*
FORM dom_values  USING pv_dom  TYPE dd01l-domname
              CHANGING ct_taba TYPE tty_taba.

*--Local declaration
  DATA : it_tabn TYPE STANDARD TABLE OF dd07v.

*--Fetching User Role Domain values
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = pv_dom
      langu         = sy-langu
      withtext      = abap_true
    TABLES
      dd07v_tab_a   = ct_taba
      dd07v_tab_n   = it_tabn
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc EQ 0.
    SORT ct_taba BY domvalue_l.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.

  DATA : lr_table   TYPE REF TO cl_salv_table,
         lr_columns TYPE REF TO cl_salv_columns_table,
         lv_msg     TYPE REF TO cx_salv_msg.

*--Displaying data
  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = gt_upl_act ).
    CATCH  cx_salv_msg INTO lv_msg .
  ENDTRY.

  lr_columns = lr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  lr_table->display( ).

ENDFORM.
