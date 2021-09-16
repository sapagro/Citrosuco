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

  CONSTANTS: lc_struc  TYPE tabname    VALUE 'ZABS_STR_ROT_XLS',
             lc_struc1 TYPE tabname    VALUE 'ZABS_STR_ROTTER_XLS',
             lc_struc2 TYPE tabname    VALUE 'ZABS_STR_ROTPERNR_XLS'.

*--Local data Declaration
  DATA : lwa_emp_role_temp TYPE zabs_emp_role,

         lt_tab_raw        TYPE truxs_t_text_data,
         lt_insert         TYPE STANDARD TABLE OF zabs_emp_role,
         lt_taba_urole     TYPE STANDARD TABLE OF dd07v,
         lt_taba_pevt      TYPE STANDARD TABLE OF dd07v,
         lt_taba_fconf     TYPE STANDARD TABLE OF dd07v,
         lt_upl_per        TYPE TABLE OF zabs_str_rotpernr_xls,
         lt_upl_hdr        TYPE TABLE OF zabs_str_rot_xls,
         lt_upl_ter        TYPE TABLE OF zabs_str_rotter_xls,
         lt_usrpernr       TYPE TABLE OF zabs_usrpernr,
         lt_usrpernrtmp    TYPE TABLE OF zabs_usrpernr,
         lti_handle        TYPE bal_t_logh,
         lv_counter        TYPE zabs_pernr_cnt,
         lv_extnumber      TYPE balnrext,
         lv_tabix          TYPE sytabix,
         lv_path           TYPE string,
         lv_tplnrfl        TYPE /agri/gltplnr_fl,
         lv_delta          TYPE c LENGTH 6,
         lv_etype          TYPE c,
         lv_str            TYPE string,
         lt_rtdoc          TYPE /agri/t_glrt_doc,
         lt_messtab        TYPE /agri/t_gprolog,
         lt_dd03l          TYPE thrpad_erd_dd03l,
         lwa_dd03l         TYPE dd03l,
         lv_posnr          TYPE posnr,
         lv_row            TYPE /agri/s_excel_sheet-row,

         lv_errors         TYPE char1,
         lv_owrol          TYPE /agri/glowrol,
         lv_file           TYPE rlgrap-filename.

  DATA: lt_sheet TYPE /agri/t_excel_sheet,
        lt_table TYPE /agri/t_excel_sheet,
        lwa_data TYPE /agri/s_excel_sheet,
        lv_route TYPE char0256.

  FIELD-SYMBOLS: <lv_value>   TYPE any.

  DATA: ref_upload TYPE REF TO /agri/cl_glupload_master_data.
  CREATE OBJECT ref_upload.

******************************************************************************
*-- Excel Upload
******************************************************************************

  lv_route = p_path.
  CALL METHOD /agri/cl_glupload_master_data=>read_excel_file
    EXPORTING
      i_route  = lv_route
    IMPORTING
      et_sheet = lt_sheet.
  IF sy-subrc <> 0.

    LEAVE LIST-PROCESSING.
    LEAVE TO SCREEN 0.

  ELSE.

**-- Crear log
    zabs_cl_logup_utilities=>create_log(
      EXPORTING
        im_v_extnumber  = lv_extnumber
        im_v_object     = gv_object
        im_v_subobject  = gv_subobject
      IMPORTING
        ex_v_log_handle = gv_log_handle ).

* Añadimos detalles al log

    IF p_test IS INITIAL.
      lv_etype = 'R'.
    ELSE.
      lv_etype = 'T'.
    ENDIF.
    lv_path = p_path.
    zabs_cl_logup_utilities=>add_initial_msg(
      EXPORTING
         im_v_path       = lv_path
         im_v_tipo       = lv_etype
         im_v_log_handle = gv_log_handle ).

*-- Route Header
    zabs_cl_logup_utilities=>structure_build(
      EXPORTING
        i_structure = lc_struc
        i_sheet     = 1
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_sheet ).

    lt_table = lt_sheet.
    DELETE lt_table WHERE sheet <> 01.

    LOOP AT lt_table INTO lwa_data.                      "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
*    ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_upl_hdr ASSIGNING FIELD-SYMBOL(<lwa_tr01>).
        lv_posnr = lv_posnr + 1.
        <lwa_tr01>-posnr = lv_posnr.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_tr01> TO <lv_value>.
        <lv_value> = lwa_data-value.

        IF lwa_data-fieldname = 'ROUTE'.
          IF <lv_value> CA '0123456789'.
            IF strlen( <lv_value> ) <= 2.
              lv_str = <lv_value>.
              IF strlen( lv_str ) = 1.
                lv_str = '00' && lv_str.
              ELSEIF strlen( lv_str ) = 2.
                lv_str = '0' && lv_str.
              ENDIF.
              <lv_value> = lv_str.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
    DELETE lt_upl_hdr INDEX 1.
    CLEAR lv_posnr.

******************************************************************************
*-- Tables Set Up
******************************************************************************

*-- Route Terrains
    zabs_cl_logup_utilities=>structure_build(
      EXPORTING
        i_structure = lc_struc1
        i_sheet     = 2
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_sheet ).

    lt_table = lt_sheet.
    DELETE lt_table WHERE sheet <> 02.

    LOOP AT lt_table INTO lwa_data.                      "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
*    ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_upl_ter ASSIGNING FIELD-SYMBOL(<lwa_tr02>).
        lv_posnr = lv_posnr + 1.
        <lwa_tr02>-posnr = lv_posnr.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_tr02> TO <lv_value>.
        IF sy-subrc = 0.
          <lv_value> = lwa_data-value.
          IF lwa_data-fieldname = 'ROUTE'.
            IF <lv_value> CA '0123456789'.
              IF strlen( <lv_value> ) <= 2.
                lv_str = <lv_value>.
                IF strlen( lv_str ) = 1.
                  lv_str = '00' && lv_str.
                ELSEIF strlen( lv_str ) = 2.
                  lv_str = '0' && lv_str.
                ENDIF.
                <lv_value> = lv_str.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
    DELETE lt_upl_ter INDEX 1.
    CLEAR lv_posnr.

*-- Route-Employee Relation

    zabs_cl_logup_utilities=>structure_build(
      EXPORTING
        i_structure = lc_struc2
        i_sheet     = 3
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_sheet ).

    lt_table = lt_sheet.
    DELETE lt_table WHERE sheet <> 03.

    LOOP AT lt_table INTO lwa_data.                      "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
*    ENDAT.
      IF lv_row NE lwa_data-row.
        CLEAR lv_owrol.
        APPEND INITIAL LINE TO lt_upl_per ASSIGNING FIELD-SYMBOL(<lwa_tr03>).
        lv_posnr = lv_posnr + 1.
        <lwa_tr01>-posnr = lv_posnr.
        lv_row = lwa_data-row.
      ENDIF.

      IF lwa_data-fieldname = 'OWROL'.
        lv_owrol = lwa_data-value.
      ENDIF.

      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        IF lwa_data-fieldname <> 'PERNR_LIFNR.'.
          ASSIGN COMPONENT lwa_data-fieldname
                      OF STRUCTURE <lwa_tr03> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = lwa_data-value.
          ENDIF.
        ELSE.
          IF lv_owrol = 'EN'.
            ASSIGN COMPONENT 'PERNR'
                      OF STRUCTURE <lwa_tr03> TO <lv_value>.
            IF sy-subrc = 0.
              <lv_value> = lwa_data-value.
            ENDIF.
          ELSEIF lv_owrol = 'VN'.
            ASSIGN COMPONENT 'LIFNR'
                      OF STRUCTURE <lwa_tr03> TO <lv_value>.
            IF sy-subrc = 0.
              <lv_value> = lwa_data-value.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lwa_data-fieldname = 'ROUTE'.
          IF <lv_value> CA '0123456789'.
            IF strlen( <lv_value> ) <= 2.
              lv_str = <lv_value>.
              IF strlen( lv_str ) = 1.
                lv_str = '00' && lv_str.
              ELSEIF strlen( lv_str ) = 2.
                lv_str = '0' && lv_str.
              ENDIF.
              <lv_value> = lv_str.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
    DELETE lt_upl_per INDEX 1.
    CLEAR lv_posnr.

    LOOP AT lt_upl_per ASSIGNING FIELD-SYMBOL(<ls_upl_per>).

      APPEND INITIAL LINE TO lt_usrpernr ASSIGNING FIELD-SYMBOL(<ls_usrper>).
      MOVE-CORRESPONDING <ls_upl_per> TO <ls_usrper>.
      IF <ls_upl_per>-owrol = 'EN'.
        <ls_usrper>-pernr = <ls_upl_per>-pernr_lifnr.
      ELSEIF <ls_upl_per>-owrol = 'VN'.
        <ls_usrper>-lifnr = <ls_upl_per>-pernr_lifnr.
      ENDIF.

    ENDLOOP.

******************************************************************************
*-- Queries
******************************************************************************

*-- Terrains Fetch for Validation

    LOOP AT lt_upl_ter ASSIGNING FIELD-SYMBOL(<ls_upl_ter>).

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input         = <ls_upl_ter>-tplnr_fl
        IMPORTING
          output        = <ls_upl_ter>-tplnr_fl
        EXCEPTIONS
          error_message = 10.
      IF sy-subrc = 0.
      ENDIF.

    ENDLOOP.

    SELECT *
      FROM /agri/glflot
      INTO TABLE @DATA(lt_flott)
      FOR ALL ENTRIES IN @lt_upl_ter               "#EC CI_NO_TRANSFORM
      WHERE tplnr_fl = @lt_upl_ter-tplnr_fl.
    SORT lt_flott BY tplnr_fl.
    IF sy-subrc = 0.

      SELECT *
        FROM /agri/glrtfla
        INTO TABLE @DATA(lt_glrtfla)
        FOR ALL ENTRIES IN @lt_flott
        WHERE tplnr_fl = @lt_flott-tplnr_fl.
      SORT lt_glrtfla BY route tplnr_fl.

    ENDIF.

*-- Routes Fetch for Validation

    SELECT *
      FROM /agri/glrthdr
      INTO TABLE @DATA(lt_glrthdr)
      FOR ALL ENTRIES IN @lt_upl_hdr
      WHERE route = @lt_upl_hdr-route.
    SORT lt_glrthdr BY route.


*-- Routes Texts Fetch for Validation

    SELECT *
      FROM /agri/glrthdrt
      INTO TABLE @DATA(lt_glrthdrt)
      FOR ALL ENTRIES IN @lt_upl_hdr
      WHERE route = @lt_upl_hdr-route
        AND spras = @lt_upl_hdr-spras.
    SORT lt_glrthdrt BY route.


*--Fetching Personnel Number values
    lt_usrpernrtmp = lt_usrpernr.
    DELETE lt_usrpernrtmp WHERE owrol = 'VN'.
    DELETE lt_usrpernrtmp WHERE owrol = space.

    IF lt_usrpernrtmp IS NOT INITIAL.
      SELECT *
        FROM pa0000
        INTO TABLE @DATA(lt_pernr)
        FOR ALL ENTRIES IN @lt_usrpernrtmp
        WHERE pernr = @lt_usrpernrtmp-pernr.
      IF sy-subrc = 0.
        SORT lt_pernr BY pernr.
      ENDIF.
    ENDIF.
    REFRESH lt_usrpernrtmp.

    lt_usrpernrtmp = lt_upl_per.
    DELETE lt_usrpernrtmp WHERE owrol = 'EN'.
    DELETE lt_usrpernrtmp WHERE owrol = space.

    IF lt_usrpernrtmp IS NOT INITIAL.
      SELECT *
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
        FOR ALL ENTRIES IN @lt_usrpernrtmp
        WHERE lifnr = @lt_usrpernrtmp-lifnr.
      IF sy-subrc = 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.
    REFRESH lt_usrpernrtmp.
  ENDIF.

******************************************************************************
*-- Validations and Data Mapping
******************************************************************************

  LOOP AT lt_upl_hdr ASSIGNING FIELD-SYMBOL(<lwa_defhdr>).

    READ TABLE lt_glrthdr TRANSPORTING NO FIELDS
                          WITH KEY route = <lwa_defhdr>-route
                          BINARY SEARCH.
    IF sy-subrc = 0.
      lv_delta = <lwa_defhdr>-posnr.
      SHIFT lv_delta LEFT DELETING LEADING '0'.
      CONDENSE lv_delta.
      log_add gc_messid c_err '169'
        <lwa_defhdr>-route lv_delta space space space.

      IF <lwa_defhdr>-spras IS NOT INITIAL
        AND <lwa_defhdr>-descr IS NOT INITIAL.

        APPEND INITIAL LINE TO lt_rtdoc                ASSIGNING FIELD-SYMBOL(<fs_glrthdrtt>).
        APPEND INITIAL LINE TO <fs_glrthdrtt>-x-rthdrt ASSIGNING FIELD-SYMBOL(<fs_x_rthdrt>).
        <fs_x_rthdrt>-spras  = <lwa_defhdr>-spras.
        <fs_x_rthdrt>-descr  = <lwa_defhdr>-descr.
        <fs_x_rthdrt>-route  = <lwa_defhdr>-route.

        lv_str = <lwa_defhdr>-route.
        IF strlen( lv_str ) <= 2..
          IF strlen( lv_str ) = 1.
            lv_str = '00' && lv_str.
          ELSEIF strlen( lv_str ) = 2.
            lv_str = '0' && lv_str.
          ENDIF.
          <fs_x_rthdrt>-route = lv_str.
        ENDIF.

        READ TABLE lt_glrthdrt TRANSPORTING NO FIELDS
                               WITH KEY route = <lwa_defhdr>-route
                               BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_x_rthdrt>-updkz  = 'U'.
          lv_delta = <lwa_defhdr>-posnr.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_suc '171'
          <lwa_defhdr>-route lv_delta space space space.
        ELSE.
          <fs_x_rthdrt>-updkz  = 'I'.
          lv_delta = <lwa_defhdr>-posnr.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_suc '170'
          <lwa_defhdr>-route lv_delta space space space.
        ENDIF.

      ENDIF.

    ELSE.

      APPEND INITIAL LINE TO lt_rtdoc ASSIGNING FIELD-SYMBOL(<fs_defdata>).

      <fs_defdata>-x-rthdr-route = <lwa_defhdr>-route.

      lv_str = <lwa_defhdr>-route.
      IF strlen( lv_str ) <= 2.
        IF strlen( lv_str ) = 1.
          lv_str = '00' && lv_str.
        ELSEIF strlen( lv_str ) = 2.
          lv_str = '0' && lv_str.
        ENDIF.
        <fs_defdata>-x-rthdr-route = lv_str.
      ENDIF.

      <fs_defdata>-x-rthdr-rttyp = <lwa_defhdr>-rttyp.
      <fs_defdata>-x-rthdr-ernam = sy-uname.
      <fs_defdata>-x-rthdr-erdat = sy-datum.
      <fs_defdata>-x-rthdr-erzet = sy-uzeit.
      <fs_defdata>-x-rthdr-updkz = 'I'.

      lv_delta = <lwa_defhdr>-posnr.
      SHIFT lv_delta LEFT DELETING LEADING '0'.
      CONDENSE lv_delta.
      log_add gc_messid c_suc '172'
        <lwa_defhdr>-route lv_delta space space space.


      IF <lwa_defhdr>-spras IS NOT INITIAL
        AND <lwa_defhdr>-descr IS NOT INITIAL.

        APPEND INITIAL LINE TO <fs_defdata>-x-rthdrt ASSIGNING FIELD-SYMBOL(<fs_x_rthdrt2>).

        <fs_x_rthdrt2>-spras  = <lwa_defhdr>-spras.
        <fs_x_rthdrt2>-descr  = <lwa_defhdr>-descr.

        <fs_x_rthdrt2>-route = <lwa_defhdr>-route.
        lv_str = <lwa_defhdr>-route.
        IF strlen( lv_str ) <= 2.
          IF strlen( lv_str ) = 1.
            lv_str = '00' && lv_str.
          ELSEIF strlen( lv_str ) = 2.
            lv_str = '0' && lv_str.
          ENDIF.
          <fs_x_rthdrt2>-route  = lv_str.
        ENDIF.

        READ TABLE lt_glrthdrt TRANSPORTING NO FIELDS
                               WITH KEY route = <lwa_defhdr>-route
                               BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_x_rthdrt2>-updkz = 'U'.
          lv_delta             = <lwa_defhdr>-posnr.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_suc '171'
          <lwa_defhdr>-route lv_delta space space space.
        ELSE.
          <fs_x_rthdrt2>-updkz = 'I'.
          lv_delta             = <lwa_defhdr>-posnr.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_suc '170'
          <lwa_defhdr>-route lv_delta space space space.
        ENDIF.

      ENDIF.
*       /AGRI/S_GLRTHDR
*/AGRI/T_GLRTHDRT
*/AGRI/T_GLRTUSR
*/AGRI/T_GLRTFLA

    ENDIF.

  ENDLOOP.

  LOOP AT lt_upl_ter ASSIGNING FIELD-SYMBOL(<lwa_defter>).

    READ TABLE lt_flott TRANSPORTING NO FIELDS
                        WITH KEY tplnr_fl = <lwa_defter>-tplnr_fl
                        BINARY SEARCH.
    IF sy-subrc <> 0.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input         = <lwa_defter>-tplnr_fl
        IMPORTING
          output        = lv_tplnrfl
        EXCEPTIONS
          error_message = 10.

      lv_delta = <lwa_defter>-posnr.
      SHIFT lv_delta LEFT DELETING LEADING '0'.
      CONDENSE lv_delta.
      log_add gc_messid c_err '173'
        lv_tplnrfl lv_delta space space space.
      CONTINUE.
    ENDIF.

    READ TABLE lt_glrtfla TRANSPORTING NO FIELDS
                          WITH KEY route    = <lwa_defter>-route
                                   tplnr_fl = <lwa_defter>-tplnr_fl
                          BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO lt_rtdoc                ASSIGNING FIELD-SYMBOL(<fs_glrthdrty>).
      APPEND INITIAL LINE TO <fs_glrthdrty>-x-rtfla  ASSIGNING FIELD-SYMBOL(<fs_x_rtfla>).

      <fs_x_rtfla>-route = <lwa_defter>-route.
      lv_str = <lwa_defter>-route.
      IF strlen( lv_str ) <= 2.
        IF strlen( lv_str ) = 1.
          lv_str = '00' && lv_str.
        ELSEIF strlen( lv_str ) = 2.
          lv_str = '0' && lv_str.
        ENDIF.
        <fs_x_rtfla>-route    = lv_str.
      ENDIF.
      <fs_x_rtfla>-tplnr_fl = <lwa_defter>-tplnr_fl.
      <fs_x_rtfla>-updkz    = 'I'.

      lv_delta = <lwa_defter>-posnr.
      SHIFT lv_delta LEFT DELETING LEADING '0'.
      CONDENSE lv_delta.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input         = <lwa_defter>-tplnr_fl
        IMPORTING
          output        = lv_tplnrfl
        EXCEPTIONS
          error_message = 10.

      log_add gc_messid c_suc '174'
        lv_tplnrfl <lwa_defter>-route lv_delta space space.
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input         = <lwa_defter>-tplnr_fl
        IMPORTING
          output        = lv_tplnrfl
        EXCEPTIONS
          error_message = 10.

      lv_delta = <lwa_defter>-posnr.
      SHIFT lv_delta LEFT DELETING LEADING '0'.
      CONDENSE lv_delta.
      log_add gc_messid c_err '175'
        lv_tplnrfl lv_delta space space space.
    ENDIF.

  ENDLOOP.

  LOOP AT lt_usrpernr ASSIGNING FIELD-SYMBOL(<lwa_defusr>).
    lv_tabix = sy-tabix + 1.

    IF <lwa_defusr>-owrol = 'EN'.
      READ TABLE lt_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>)
                          WITH KEY pernr = <lwa_defusr>-pernr
                          BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_pernr>-stat2 = '3'.
          APPEND INITIAL LINE TO lt_usrpernrtmp ASSIGNING FIELD-SYMBOL(<fs_usrpernrtmp>).
          <fs_usrpernrtmp>         = <lwa_defusr>.

          lv_delta = lv_tabix.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_suc '177'
          <lwa_defusr>-pernr lv_delta space space space.
        ELSE.

          lv_delta = lv_tabix.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_err '176'
          <lwa_defusr>-pernr lv_delta space space space.
        ENDIF.
      ELSE.

        lv_delta = lv_tabix.
        SHIFT lv_delta LEFT DELETING LEADING '0'.
        CONDENSE lv_delta.
        log_add gc_messid c_err '178'
          <lwa_defusr>-pernr lv_delta space space space.
      ENDIF.

    ELSEIF <lwa_defusr>-owrol = 'VN'.
      READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
                        WITH KEY lifnr = <lwa_defusr>-lifnr
                        BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_lfa1>-loevm <> abap_true.
          APPEND INITIAL LINE TO lt_usrpernrtmp ASSIGNING FIELD-SYMBOL(<fs_usrpernrtm2>).
          <fs_usrpernrtm2>         = <lwa_defusr>.
          <fs_usrpernrtm2>-counter = lv_posnr.

          lv_delta = lv_tabix.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_err '180'
          <lwa_defusr>-lifnr lv_delta space space space.
        ELSE.

          lv_delta = lv_tabix.
          SHIFT lv_delta LEFT DELETING LEADING '0'.
          CONDENSE lv_delta.
          log_add gc_messid c_err '179'
          <lwa_defusr>-lifnr lv_delta space space space.
        ENDIF.
      ELSE.

        lv_delta = lv_tabix.
        SHIFT lv_delta LEFT DELETING LEADING '0'.
        CONDENSE lv_delta.
        log_add gc_messid c_err '181'
          <lwa_defusr>-lifnr lv_delta space space space.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CLEAR lv_route.
  SORT lt_usrpernrtmp BY route.
  LOOP AT lt_usrpernrtmp ASSIGNING FIELD-SYMBOL(<fs_usrtmp>).
    IF lv_route IS INITIAL.
      lv_route = <fs_usrtmp>-route.
      lv_counter = lv_counter + 1.
    ELSE.
      IF lv_route <> <fs_usrtmp>-route.
        CLEAR lv_counter.
        lv_route   = <fs_usrtmp>-route.
        lv_counter = lv_counter + 1.
      ELSE.
        lv_counter = lv_counter + 1.
      ENDIF.
    ENDIF.

    <fs_usrtmp>-counter = lv_counter.

    lv_str = <fs_usrtmp>-route.
    IF strlen( lv_str ) <= 2.
      IF strlen( lv_str ) = 1.
        lv_str = '00' && lv_str.
      ELSEIF strlen( lv_str ) = 2.
        lv_str = '0' && lv_str.
      ENDIF.
      <fs_usrtmp>-route = lv_str.
    ENDIF.

  ENDLOOP.

  IF p_test IS INITIAL
    AND lt_rtdoc IS NOT INITIAL.
    CALL FUNCTION '/AGRI/GLRT_SAVE'
      EXPORTING
        i_set_update_task = abap_true
        i_commit_work     = abap_true
*       IREF_TEXT         =
      CHANGING
        ct_rtdoc          = lt_rtdoc
        ct_messages       = lt_messtab
      EXCEPTIONS
        no_change         = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

  IF lt_usrpernrtmp IS NOT INITIAL
    AND p_test IS INITIAL.
    MODIFY zabs_usrpernr FROM TABLE lt_usrpernrtmp.
    COMMIT WORK AND WAIT.
  ENDIF.

  APPEND gv_log_handle TO lti_handle.
*-- Guarda log para visualizar por la SLG1
  IF p_test IS INITIAL.
    zabs_cl_logup_utilities=>alt_save_log(
        EXPORTING
          im_ti_log_handle = lti_handle ).
  ENDIF.

*-- Muestra el log solo si es una ejecución local.
  IF sy-batch IS INITIAL.
    zabs_cl_logup_utilities=>display_log(
       EXPORTING
        im_ti_log_handle = lti_handle ).
  ENDIF.

  READ TABLE lti_handle ASSIGNING FIELD-SYMBOL(<lwa_log_handle>) INDEX 1.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle  = <lwa_log_handle>
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
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
*  TRY.
*      CALL METHOD cl_salv_table=>factory(
*        IMPORTING
*          r_salv_table = lr_table
*        CHANGING
*          t_table      = gt_upl_act ).
*    CATCH  cx_salv_msg INTO lv_msg .
*  ENDTRY.
*
*  lr_columns = lr_table->get_columns( ).
*  lr_columns->set_optimize( abap_true ).
*
*  lr_table->display( ).

ENDFORM.
