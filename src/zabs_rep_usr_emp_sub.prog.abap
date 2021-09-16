************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Include Name   : ZABS_REP_USR_EMP_SUB                                *
* Tcode          : ZABS_TRN_USREMP                                     *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Mario Alfredo                                       *
* Created on     : 05.19.2020                                          *
* TR             : C4DK911368                                          *
* Version        : 001                                                 *
* Description    : Employees User                                      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form GET_EXCEL_DATA
*&---------------------------------------------------------------------*
*& GET_EXCEL_DATA
*&---------------------------------------------------------------------*
FORM get_excel_data.

*--Local data Declaration
  DATA : lwa_usr_emp_temp TYPE zabs_usr_emp,
         lwa_vendor       TYPE lfa1,
         lwa_terrain      TYPE /agri/glflot,

         lt_vendor        TYPE TABLE OF lfa1,
         lt_terrain       TYPE TABLE OF /agri/glflot,
         lt_tab_raw       TYPE truxs_t_text_data,
         lt_insert        TYPE STANDARD TABLE OF zabs_usr_emp,
         lt_taba_urole    TYPE STANDARD TABLE OF dd07v,
         lt_taba_pevt     TYPE STANDARD TABLE OF dd07v,
         lt_taba_fconf    TYPE STANDARD TABLE OF dd07v,

         lv_errors        TYPE char1,
         lv_tabix         TYPE sy-tabix,
         lv_file          TYPE rlgrap-filename.

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

    LOOP AT gt_upl_act INTO DATA(lwa_upl_act).

      lwa_vendor-lifnr  = |{ lwa_upl_act-lifnr ALPHA = IN }|.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = lwa_upl_act-farm
        IMPORTING
          output     = lwa_terrain-tplnr_fl
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      APPEND lwa_vendor  TO lt_vendor.
      APPEND lwa_terrain TO lt_terrain.
      CLEAR: lwa_vendor,
             lwa_terrain.

    ENDLOOP.

*--Fetch User Employee data
    SELECT *
      FROM zabs_usr_emp
      INTO TABLE @DATA(lt_usr_emp).
    IF sy-subrc = 0.
      SORT lt_usr_emp BY bname.
      DATA(lt_usr_emp_temp) = lt_usr_emp.
      DELETE ADJACENT DUPLICATES FROM lt_usr_emp_temp COMPARING bname.
    ENDIF.

*--Fetching User Name Values
    SELECT bname
      FROM usr02
      INTO TABLE @DATA(lt_bname)
       FOR ALL ENTRIES IN @gt_upl_act
     WHERE bname EQ @gt_upl_act-bname.
    IF sy-subrc = 0.
      SORT lt_bname BY bname.
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

*--Fetching Vendor Values
    SELECT lifnr
      FROM lfa1
      INTO TABLE @DATA(lt_lifnr)
      FOR ALL ENTRIES IN @lt_vendor
      WHERE lifnr EQ @lt_vendor-lifnr.
    IF sy-subrc = 0.
      SORT lt_lifnr BY lifnr.
    ENDIF.

*--Fetching Farm Values
    SELECT tplnr_fl
      FROM /agri/glflot
      INTO TABLE @DATA(lt_farm)
      FOR ALL ENTRIES IN @lt_terrain
      WHERE tplnr_fl EQ @lt_terrain-tplnr_fl.
    IF sy-subrc = 0.
      SORT lt_farm BY tplnr_fl.
    ENDIF.

*--Fetching Fleet Values
    SELECT fleet
      FROM zabst_fleet
      INTO TABLE @DATA(lt_fleet)
      FOR ALL ENTRIES IN @gt_upl_act
      WHERE fleet EQ @gt_upl_act-fleet.
    IF sy-subrc = 0.
      SORT lt_fleet BY fleet.
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

      lv_tabix = sy-tabix + 1.
      LOOP AT gt_upl_act INTO DATA(ls_upl_act) FROM lv_tabix.
        IF <fs_upl_act>-bname EQ ls_upl_act-bname.
          <fs_upl_act>-msgtxt = TEXT-002.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_warning.
          "'@09@'
        ENDIF.
      ENDLOOP.

      IF <fs_upl_act>-msgtxt IS NOT INITIAL.
        CONTINUE.
      ENDIF.

*--Validating whether User Name is mandatory or not
      IF  <fs_upl_act>-bname IS INITIAL.
        <fs_upl_act>-msgtxt = TEXT-003.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@'
        CONTINUE.
      ENDIF.

      IF <fs_upl_act>-bname IS NOT INITIAL.
*--Validating User Name
        READ TABLE lt_bname INTO DATA(lwa_bname)
        WITH KEY bname = <fs_upl_act>-bname
        BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-015.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

      IF <fs_upl_act>-bname IS NOT INITIAL.
        READ TABLE lt_usr_emp TRANSPORTING NO FIELDS
       WITH KEY bname = <fs_upl_act>-bname
       BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_upl_act>-msgtxt = TEXT-022.
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
          <fs_upl_act>-msgtxt = TEXT-014.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating User Role field
      IF <fs_upl_act>-urole EQ space.
        <fs_upl_act>-msgtxt = TEXT-007.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@' .
        CONTINUE.
      ENDIF.

*--Validating Personnel Number
      IF <fs_upl_act>-pernr IS INITIAL AND
        ( <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_leader  OR "'LE'
        <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_incharge  )."'IN'
        <fs_upl_act>-msgtxt = TEXT-005.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@' .
        CONTINUE.
      ENDIF.

*--Validating Personnel Number
      IF <fs_upl_act>-pernr IS NOT INITIAL.
        READ TABLE lt_pernr INTO DATA(lwa_pernr)
        WITH KEY pernr = <fs_upl_act>-pernr
        BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-016.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Vendor Field
      IF <fs_upl_act>-lifnr IS INITIAL AND
         <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_loader. "'LD'
        <fs_upl_act>-msgtxt = TEXT-006.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@' .
        CONTINUE.
      ENDIF.

      lwa_vendor-lifnr  = |{ <fs_upl_act>-lifnr ALPHA = IN }|.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = <fs_upl_act>-farm
        IMPORTING
          output     = lwa_terrain-tplnr_fl
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

*--Validating Vendor
      IF lwa_vendor-lifnr IS NOT INITIAL.
        READ TABLE lt_lifnr INTO DATA(lwa_lifnr)
        WITH KEY lifnr = lwa_vendor-lifnr
        BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-017.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Farm
      IF lwa_terrain-tplnr_fl IS NOT INITIAL.
        READ TABLE lt_farm INTO DATA(lwa_farm)
        WITH KEY tplnr_fl = lwa_terrain-tplnr_fl
        BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-018.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Events Production Values
      IF <fs_upl_act>-pevt IS NOT INITIAL.
        READ TABLE lt_taba_pevt TRANSPORTING NO FIELDS
              WITH KEY domvalue_l = <fs_upl_act>-pevt.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-020.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Event Production Field
      IF <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_leader "'LE'
            OR <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_incharge ."'IN'
        IF <fs_upl_act>-pevt IS INITIAL.
          <fs_upl_act>-msgtxt = TEXT-008.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@' .
          CONTINUE.
        ENDIF.
      ENDIF.

      IF <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_loader "'LD'
       OR <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_bin. "'BN'
        IF <fs_upl_act>-pevt IS NOT INITIAL.
          <fs_upl_act>-msgtxt = TEXT-009.
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
          <fs_upl_act>-msgtxt = TEXT-021.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          CONTINUE.
        ENDIF.
      ENDIF.

*--Validating Fleet Field
      IF <fs_upl_act>-fleet IS INITIAL AND
           <fs_upl_act>-urole = zcl_abs_abap_maintain=>c_role_equip. "'EQ'
        <fs_upl_act>-msgtxt = TEXT-010.
        <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
        "'@0A@' .
        CONTINUE.
      ENDIF.

      IF <fs_upl_act>-fleet IS NOT INITIAL.
        READ TABLE lt_fleet INTO DATA(lwa_fleet)
         WITH KEY fleet = <fs_upl_act>-fleet
         BINARY SEARCH.
        IF sy-subrc NE 0.
          <fs_upl_act>-msgtxt = TEXT-019.
          <fs_upl_act>-msgtyp =  zcl_abs_abap_maintain=>c_icon_error.
          "'@0A@'
          CONTINUE.
        ENDIF.
      ENDIF.

*--Inserting records into User Employee table
      IF <fs_upl_act>-msgtyp IS INITIAL.

        READ TABLE lt_usr_emp INTO DATA(lwa_usr_emp)
                               WITH KEY bname = <fs_upl_act>-bname
                               BINARY SEARCH.
        IF sy-subrc NE 0.
*--Inserting records into ZABS_USR_EMP
          lwa_usr_emp-bname = <fs_upl_act>-bname.
          lwa_usr_emp-urole = <fs_upl_act>-urole.
          lwa_usr_emp-pernr = <fs_upl_act>-pernr.
          lwa_usr_emp-lifnr = <fs_upl_act>-lifnr.
          lwa_usr_emp-farm  = <fs_upl_act>-farm.
          lwa_usr_emp-pevt  = <fs_upl_act>-pevt.
          lwa_usr_emp-fpcnf = <fs_upl_act>-fpcnf.
          lwa_usr_emp-fleet = <fs_upl_act>-fleet.
          lwa_usr_emp-ernam = sy-uname.
          lwa_usr_emp-erdat = sy-datum.
          lwa_usr_emp-erzet = sy-uzeit.
          APPEND lwa_usr_emp TO lt_insert.
          CLEAR lwa_usr_emp.
          <fs_upl_act>-msgtyp =
          zcl_abs_abap_maintain=>c_icon_success. "'@08@'
          <fs_upl_act>-msgtxt = TEXT-013.
        ENDIF. "lt_usr_emp

      ENDIF. "<fs_upl_act>
    ENDLOOP. "gt_upl_act

*--Inserting record from User Employee table
    IF lt_insert IS NOT INITIAL.
      INSERT zabs_usr_emp FROM TABLE lt_insert.
    ENDIF.

    COMMIT WORK.

  ENDIF.

ENDFORM.

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
*& Form DOM_VALUES
*&---------------------------------------------------------------------*
*& DOM_VALUES
*&---------------------------------------------------------------------*
FORM dom_values     USING pv_dom  TYPE dd01l-domname
                 CHANGING ct_taba TYPE tty_taba.

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
