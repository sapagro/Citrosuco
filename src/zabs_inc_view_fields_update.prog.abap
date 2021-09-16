************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_CROP_SEASONS                          *
* Form Name         :  FIELD_VALUE_CONVERSIONS                         *
* Include Name      :  ZABS_INC_VIEW_FIELDS_UPDATE                     *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.19.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Display View Fields for Crop season workbench   *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local variable declarations
 DATA: lv_days   TYPE i,
       lv_months TYPE zabs_del_month,
       lv_years  TYPE zabs_del_yearc,
       lv_date   TYPE datum.

*-- BOC-T_T.KONNO
 DATA l_exception TYPE REF TO cx_sy_arithmetic_overflow.
*-- EOC-T_T.KONNO

 IF lv_int_ext NE 1.
   RETURN.
 ENDIF.

 DATA(lv_pdate) = gs_csdoc_infocus-x-cshdr-zzfazplantio.
 DATA(lv_datbi) = gs_csdoc_infocus-x-cshdr-datbi.

 IF lv_datbi LE sy-datum.
   lv_date = lv_datbi.
 ELSE.
   lv_date = sy-datum.
 ENDIF.

*--Calculating days, months and years
 lv_days = lv_date - lv_pdate.

*-- BOC-T_T.KONNO
* lv_months = lv_days / 30.
* lv_years  = lv_days / 365.
 TRY.
     lv_months = lv_days / 30.
   CATCH cx_sy_arithmetic_overflow INTO l_exception.
     CLEAR lv_months.
 ENDTRY.

 TRY.
     lv_years = lv_days / 365.
   CATCH cx_sy_arithmetic_overflow INTO l_exception.
     CLEAR lv_years.
 ENDTRY.
*-- EOC-T_T.KONNO

 LOOP AT gt_additional_data ASSIGNING <lwa_additional_data>.
*--View Fields Display
   ASSIGN COMPONENT <lwa_additional_data>-fieldname
   OF STRUCTURE /agri/s_glcsview_flds TO <lv_field_value>.
   IF sy-subrc EQ 0.
     CASE <lwa_additional_data>-fieldname.
       WHEN zcl_abs_abap_maintain=>c_cs_af_age_months. "'ZZCSAGE_MONTH'
         <lv_field_value> = lv_months.
       WHEN zcl_abs_abap_maintain=>c_cs_af_age_days.   "'ZZCSAGE'
         <lv_field_value> = lv_days.
       WHEN zcl_abs_abap_maintain=>c_cs_af_years_count.   "'ZZCSAGE'
         <lv_field_value> = lv_years.
       WHEN zcl_abs_abap_maintain=>c_cs_af_plant_year. "'ZZPLANTYEAR'
         <lv_field_value> = gs_csdoc_infocus-x-cshdr-zzfazplantio+0(4).
       WHEN OTHERS.
     ENDCASE.

     ls_table_field-fieldname = <lwa_additional_data>-fieldname.
*--Single Field
     ls_table_field-tabname = <lwa_additional_data>-tabname.

     IF <lwa_additional_data>-fieldtyp EQ zcl_abs_abap_maintain=>c_cust_add_fieldtyp. "'D'
       IF <lv_field_value> EQ '00000000'.
         CLEAR <lwa_additional_data>-fieldval.
       ELSE.
         CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
           EXPORTING
             date_internal            = <lv_field_value>
           IMPORTING
             date_external            = <lwa_additional_data>-fieldval
           EXCEPTIONS
             date_internal_is_invalid = 1
             OTHERS                   = 2.
         IF sy-subrc <> 0.
           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
           message_simple space.
         ENDIF.
       ENDIF.
     ELSE.
       lv_int_value = <lv_field_value>.
*--DS_CONV fm delimts data which has lenght more than 45
       IF <lwa_additional_data>-outputlen LT 46.
         CALL FUNCTION 'RS_DS_CONV_IN_2_EX'
           EXPORTING
             input       = lv_int_value
             table_field = ls_table_field
           IMPORTING
             output      = <lwa_additional_data>-fieldval.
       ELSE.
         CALL FUNCTION '/AGRI/G_CONVRT_FIELD_TO_EXTERN'
           EXPORTING
             i_conve = <lwa_additional_data>-convexit
             i_feld  = lv_int_value
           IMPORTING
             e_feld  = <lwa_additional_data>-fieldval.
       ENDIF.
     ENDIF.
   ENDIF.

 ENDLOOP.
