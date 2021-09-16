FUNCTION ZFMRC_MEASUREMENT_GET.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ATNAM) TYPE  ATNAM
*"     REFERENCE(I_ASLVL) TYPE  /AGRI/GLASLVL
*"     REFERENCE(I_MDATE) TYPE  IMRC_IDATE OPTIONAL
*"     REFERENCE(I_TPLNR_FL) TYPE  /AGRI/GLTPLNR_FL OPTIONAL
*"     REFERENCE(I_CMNUM) TYPE  /AGRI/GLCMNUM OPTIONAL
*"     REFERENCE(I_EQUNR) TYPE  /AGRI/GLEQUNR OPTIONAL
*"     REFERENCE(I_AVERAGE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DURAN) TYPE  /AGRI/GLDURAN OPTIONAL
*"     REFERENCE(I_TUNIT) TYPE  MSEHI OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ATFLV) TYPE  ATFLV
*"     REFERENCE(E_UNIT) TYPE  MSEHI
*"     REFERENCE(ES_MDLMP) TYPE  /AGRI/S_GLMDLMP
*"  EXCEPTIONS
*"      INCOMPLETE_PARAMETERS
*"      NO_DATA_EXISTS
*"      INVALID_DATA
*"--------------------------------------------------------------------

**  DATA: lv_lines   TYPE sy-dbcnt,
**        lv_atflv   TYPE atflv,
**        lv_atinn   TYPE atinn,
**        lv_measurement_date TYPE sy-datum,
**        lwa_acdata TYPE /agri/s_FMRClmp,
**        lwa_atinn  TYPE /agri/s_gatinn,
**        lt_atinn   TYPE /agri/t_gatinn,
**        lwa_athdr  TYPE /agri/s_gathdr,
**        lt_athdr   TYPE /agri/t_gathdr,
**        lt_acdata  TYPE TABLE OF /agri/s_FMRClmp,
**
**        rt_mdate TYPE RANGE OF imrc_idate,
**        rt_contr TYPE RANGE OF /agri/gcontr,
**        rt_tplnr TYPE RANGE OF /agri/gltplnr_fl,
**        rt_cmnum TYPE RANGE OF /agri/glcmnum,
**        rt_equnr TYPE RANGE OF /agri/glequnr,
**
**        rs_mdate LIKE LINE OF rt_mdate,
**        rs_contr LIKE LINE OF rt_contr,
**        rs_tplnr LIKE LINE OF rt_tplnr,
**        rs_cmnum LIKE LINE OF rt_cmnum,
**        rs_equnr LIKE LINE OF rt_equnr.
**
**  CLEAR: e_atflv, es_mdlmp.
**
***---Replace Unreleased Interface.
***  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
***    EXPORTING
***      input  = i_atnam
***    IMPORTING
***      output = lv_atinn.
**   CALL FUNCTION '/AGRI/G_CONV_EXIT_ATINN_INPUT'
**     EXPORTING
**       i_input = i_atnam
**     IMPORTING
**      O_OUTPUT = lv_atinn.
***---
**
**  IF lv_atinn IS INITIAL OR
**     i_aslvl IS INITIAL OR
**     ( i_tplnr_fl IS INITIAL AND
**       i_cmnum IS INITIAL AND
**       i_equnr IS INITIAL ) OR
**     ( i_average IS NOT INITIAL AND
**       i_duran IS INITIAL ).
**    RAISE incomplete_parameters.
**  ENDIF.
**
**  IF i_mdate IS INITIAL.
**    lv_measurement_date = sy-datum.
**  ELSE.
**    lv_measurement_date = i_mdate.
**  ENDIF.
**
**  IF i_aslvl EQ c_measurement_level-crop_seasons OR
**     i_aslvl EQ c_measurement_level-harvest.
**    SELECT SINGLE contr
**             FROM /agri/glflcma
**             INTO rs_contr-low
**            WHERE tplnr_fl EQ i_tplnr_fl  "#EC CI_NOORDER
**              AND cmnum    EQ i_cmnum
**              AND datab    LE lv_measurement_date
**              AND datbi    GE lv_measurement_date
**              AND loevm    EQ space.
**    IF sy-subrc NE 0.
**      RAISE no_data_exists.
**    ENDIF.
**  ENDIF.
**
**  rs_contr-option = rs_tplnr-option = rs_cmnum-option =
**  rs_mdate-option = rs_equnr-option = c_operator_word-equalto.
**  rs_contr-sign = rs_tplnr-sign = rs_cmnum-sign =
**  rs_mdate-sign = rs_equnr-sign = c_sign-include.
**
**  IF i_tplnr_fl IS NOT INITIAL.
**    rs_tplnr-low = i_tplnr_fl.
**    APPEND rs_tplnr TO rt_tplnr.
**  ENDIF.
**
**  IF i_cmnum IS NOT INITIAL.
**    rs_cmnum-low = i_cmnum.
**    APPEND rs_cmnum TO rt_cmnum.
**  ENDIF.
**
**  IF i_equnr IS NOT INITIAL.
**    rs_equnr-low = i_equnr.
**    APPEND rs_equnr TO rt_equnr.
**  ENDIF.
**
**  IF rs_contr-low IS NOT INITIAL.
**    APPEND rs_contr TO rt_contr.
**  ENDIF.
**
**  IF i_average IS NOT INITIAL.
**    lwa_atinn-atinn = lv_atinn.
**    APPEND lwa_atinn TO lt_atinn.
**    CALL METHOD /agri/cl_gattr_utils=>attributes_read
**      EXPORTING
**        i_agtyp                       = c_agtyp-measurements
**        i_datuv                       = i_mdate
***       i_language                    = SY-LANGU
**        i_no_attr_descr               = c_true
**        i_no_attr_value               = c_true
**        i_no_attr_value_descr         = c_true
**        i_no_composite_attributes     = c_true
**        i_no_child_attributes         = c_true
**        it_atinn                      = lt_atinn
***       it_atnam                      = lt_atnam
***       it_table_field                =
**      IMPORTING
**        et_attr_header                = lt_athdr
***       et_attr_descr                 =
***       et_attr_value                 =
***       et_ksml_catr                  =
***       et_attr_value_descr           =
***       et_attr_value_rule_ref        =
**      EXCEPTIONS
**        invalid_parameter_combination = 1
**        OTHERS                        = 2.
**    IF sy-subrc <> 0.
**      RAISE invalid_data.
**    ENDIF.
**    READ TABLE lt_athdr INTO lwa_athdr INDEX 1.
**    IF lwa_athdr-atfor EQ 'CHAR' OR
**       lwa_athdr-atfor EQ 'DATE' OR
**       lwa_athdr-atfor EQ'TIME'.
**      RAISE invalid_data.
**    ENDIF.
**  ENDIF.
**
**  IF i_duran IS NOT INITIAL AND
**     i_tunit IS NOT INITIAL.
**    CALL FUNCTION 'START_TIME_DETERMINE'
**      EXPORTING
**        duration                   = i_duran
**        unit                       = i_tunit
***       FACTORY_CALENDAR           =
**      IMPORTING
**        start_date                 = rs_mdate-low
***       START_TIME                 =
**      CHANGING
**        end_date                   = lv_measurement_date
***       END_TIME                   = SY-UZEIT
**      EXCEPTIONS
**        factory_calendar_not_found = 1
**        date_out_of_calendar_range = 2
**        date_not_valid             = 3
**        unit_conversion_error      = 4
**        si_unit_missing            = 5
**        parameters_not_valid       = 6
**        OTHERS                     = 7.
**    IF sy-subrc <> 0.
**      RAISE invalid_data.
**    ENDIF.
**    rs_mdate-high   = lv_measurement_date.
**    rs_mdate-option = c_operator_word-between.
**    APPEND rs_mdate TO rt_mdate.
**  ELSE.
**    rs_mdate-low    = lv_measurement_date.
**    rs_mdate-option = 'LE'.
**    APPEND rs_mdate TO rt_mdate.
**  ENDIF.
**
**  SELECT a~RCNUM a~tplnr_fl a~contr a~cmnum a~equnr
**         a~muser a~mdate a~mtime b~atinn b~atwrt
**         b~atflv "b~cunit
**            INTO CORRESPONDING FIELDS OF TABLE lt_acdata
**            FROM /agri/glRCHDR AS a INNER JOIN
**                 /agri/FMRCatv AS b
**              ON b~RCNUM EQ a~RCNUM
**           WHERE a~aslvl EQ i_aslvl
**             AND a~mdate IN rt_mdate
**             AND a~tplnr_fl IN rt_tplnr
**             AND a~cmnum IN rt_cmnum
**             AND a~contr IN rt_contr
**             AND a~equnr IN rt_equnr
**             AND b~atinn EQ lv_atinn.
**  IF sy-subrc EQ 0.
**    lv_lines = sy-dbcnt.
**    SORT lt_acdata DESCENDING BY mdate mtime.
**    IF i_average IS INITIAL.
**      READ TABLE lt_acdata INTO lwa_acdata INDEX 1.
**      es_mdlmp = lwa_acdata.
**      e_atflv = lwa_acdata-atflv.
**      e_unit  = lwa_acdata-cunit.
**    ELSE.
**      LOOP AT lt_acdata INTO lwa_acdata.
**        lv_atflv = lwa_acdata-atflv + lv_atflv.
**      ENDLOOP.
**      e_atflv = lv_atflv / lv_lines.
**      e_unit  = lwa_acdata-cunit.
**    ENDIF.
**  ELSE.
**    RAISE no_data_exists.
**  ENDIF.

ENDFUNCTION.
