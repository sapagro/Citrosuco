*************************************************************************
**  Confidential property of Citrosuco                                  *
**  All Rights Reserved                                                 *
*************************************************************************
** Enhancement Name  :  ZABS_IENH_CROP_SEASONS                          *
** Form Name         :  CS_MASS_DATA_DISPLAY                            *
** Include Name      :  ZABS_INC_CS_ADD_FIELDS_UPD                      *
** Created By        :  Jetendra Mantena                                *
** Requested by      :  Mario Alfredo                                   *
** Created on        :  09.12.2019                                      *
** TR                :  C4DK901784                                      *
** Version           :  001                                             *
** Description       :  Updating custom additional fields planting date,*
**                      planting year and age in months in Crop Season  *
**                      Workbench                                       *
**----------------------------------------------------------------------*
**  Modification Log:                                                   *
**----------------------------------------------------------------------*
** MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
**                                                                      *
**&--------------------------------------------------------------------&*

*--Local Variable Declaration
 DATA: lv_days  TYPE i,
       lv_month TYPE zabs_del_month,
       lv_date  TYPE d,
       lv_year  TYPE zabs_del_yearc.

*--Filed Symbol Declarations
 FIELD-SYMBOLS: <fs_cs_mass> TYPE any,
                <fs_date>    TYPE any,
                <fs_value>   TYPE any.

*--Process CropSeason data to update the custom additional view fields
 LOOP AT <gt_cs_mass> ASSIGNING <fs_cs_mass>.

   UNASSIGN <fs_value>.
   ASSIGN COMPONENT zcl_abs_abap_maintain=>c_cs_appl OF STRUCTURE <fs_cs_mass> TO <fs_value>. "'CLASS'
   IF <fs_value> IS ASSIGNED.
     IF <fs_value> NE zcl_abs_abap_maintain=>c_cs_farming_appl.
       CONTINUE.
     ENDIF.
   ENDIF.

*--Updating Planting year from Planting date
   UNASSIGN: <fs_value>, <fs_date>.
   ASSIGN COMPONENT: zcl_abs_abap_maintain=>c_cs_af_plant_date OF STRUCTURE <fs_cs_mass> TO <fs_date>, "'ZZFAZPLANTIO'
                     zcl_abs_abap_maintain=>c_cs_af_plant_year OF STRUCTURE <fs_cs_mass> TO <fs_value>. "'ZZPLANTYEAR'
   IF <fs_value> IS ASSIGNED AND <fs_date> IS ASSIGNED.
     <fs_value> = <fs_date>+0(4).
   ENDIF.

*--Age calculation
   ASSIGN COMPONENT : zcl_abs_abap_maintain=>c_cs_end_date       OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_datbi>), "'DATBI'
                      zcl_abs_abap_maintain=>c_cs_age_days       OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_csage>), "'CSAGE'
                      zcl_abs_abap_maintain=>c_cs_start_date     OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_datab>), "'DATAB'
                      zcl_abs_abap_maintain=>c_cs_af_age_months  OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_month>), "'ZZCSAGE_MONTH'
                      zcl_abs_abap_maintain=>c_cs_af_years_count OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_year>),  "'ZZCSYEAR'
                      zcl_abs_abap_maintain=>c_cs_af_age_days    OF STRUCTURE
                      <fs_cs_mass> TO FIELD-SYMBOL(<fs_zzcsage>). "'ZZCSAGE'

   IF <fs_date>  IS NOT ASSIGNED OR
      <fs_datbi> IS NOT ASSIGNED OR
      <fs_csage> IS NOT ASSIGNED OR
      <fs_month> IS NOT ASSIGNED OR
      <fs_datab> IS NOT ASSIGNED.
     CONTINUE.
   ENDIF.

   IF  <fs_date> GT sy-datum
     OR <fs_date> GT <fs_datbi>
     OR <fs_date> IS INITIAL.
     CLEAR <fs_csage>.
     CONTINUE.
   ENDIF.

   IF <fs_datbi> LE sy-datum.
     lv_date = <fs_datbi>.
   ELSEIF <fs_datbi> GE sy-datum.
     lv_date = sy-datum.
   ENDIF.

   lv_days = lv_date - <fs_date>.
   <fs_csage> = <fs_zzcsage> = lv_days.
   <fs_month> = lv_days / 30.
   <fs_year>  = lv_days / 365.

 ENDLOOP.
