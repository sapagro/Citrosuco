class ZCL_ABS_GLCS_ENHANCEMENT definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_GLCS_ENHANCEMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_GLCS_ENHANCEMENT IMPLEMENTATION.


METHOD /agri/if_glcs_enhancement~additional_fields_populate.

  IF cs_cshdr-ymatnr IS NOT INITIAL.
*--BOC-T_T.KONNO-07.14.21
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = cs_cshdr-ymatnr
      IMPORTING
        output       = cs_cshdr-ymatnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
*--EOC-T_T.KONNO-07.14.21
    SELECT SINGLE maktx
      INTO cs_user_fields-zzmaktx
      FROM makt
     WHERE matnr = cs_cshdr-ymatnr.
  ENDIF.

ENDMETHOD.


  method /AGRI/IF_GLCS_ENHANCEMENT~ADDITIONAL_FIELDS_PREFETCH.
  endmethod.


  method /AGRI/IF_GLCS_ENHANCEMENT~AUTHORITY_CHECK.

  endmethod.


  METHOD /agri/if_glcs_enhancement~crop_season_check.

*****Update Indicators
*CONSTANTS: c_updkz_new(1)    TYPE c VALUE 'I',
*           c_updkz_update(1) TYPE c VALUE 'U',
*           c_updkz_delete(1) TYPE c VALUE 'D',
*           c_updkz_old(1)    TYPE c VALUE ' ',
*****Rel 60E SP6
*           c_updkz_newrow    TYPE c VALUE 'N',
*****
*           c_updkz_propose(1) TYPE c VALUE 'P'. "ESP5 11129 - Generic Fiori app changes,
*
*    ASSIGN ('(/AGRI/SAPLGLCSM)GS_VARIABLES-DATA_CHANGED') TO FIELD-SYMBOL(<lv_changed>).
*    IF sy-subrc EQ 0.
*      IF <lv_changed> EQ abap_true.
**(/AGRI/SAPLGLCSM)GS_CSDOC_INFOCUS-X-CSHDR-ZZFAZPLANTIO
*      ENDIF.
*    ENDIF.
**(/AGRI/SAPLGLCSM)GS_CSDOC_INFOCUS-UPDKZ
**  IF gs_csdoc_infocus-x-cshdr NE lwa_cshdr.
**    MOVE-CORRESPONDING lwa_cshdr TO gs_csdoc_infocus-x-cshdr.
**    IF gs_csdoc_infocus-x-cshdr-updkz NE c_updkz_new.
**      gs_csdoc_infocus-x-cshdr-updkz = c_updkz_update.
**    ENDIF.
**  ENDIF.
**      gs_variables-data_changed = c_true.
**      IF gs_csdoc_infocus-x-cshdr-updkz NE c_updkz_new.
**        gs_csdoc_infocus-x-cshdr-updkz = c_updkz_update.
**      ENDIF.

  ENDMETHOD.


  METHOD /agri/if_glcs_enhancement~header_defaults_fill.

    DATA: lv_years TYPE t5a4a-dlyyr,
          lv_datab TYPE datum.

    FIELD-SYMBOLS: <fs_extend_cs> TYPE any,
                   <fs_year>      TYPE any,
                   <lfs_cshdr>    TYPE /agri/s_glflcma,
                   <lfs_glflcma>  TYPE /agri/s_glflcma.

    CHECK cs_cshdr-class EQ '1'.

*--BOC- T_T.KONNO-05.28.21
    IF cs_cshdr-cmnum EQ 'CITROS'
    AND cs_cshdr-varia EQ 'FORMAÇÃO'.
      SELECT * FROM zabs_csks UP TO 1 ROWS
        INTO @DATA(ls_zabs_csks)
       WHERE cmnum = @cs_cshdr-cmnum
         AND varia = @cs_cshdr-varia
         AND iwerk = @cs_cshdr-iwerk.
      ENDSELECT.
      IF sy-subrc EQ 0.
        cs_cshdr-kostl = ls_zabs_csks-kostl.
      ENDIF.
    ENDIF.
*--EOC- T_T.KONNO-05.28.21

    IF sy-tcode EQ 'ZABS_UPLOAD'.
      RETURN.
    ENDIF.

    ASSIGN ('(/AGRI/SAPLGLCSM)GS_VARIABLES-EXTEND_CROP_SEASON') TO <fs_extend_cs>.
    IF <fs_extend_cs> IS ASSIGNED.
      IF <fs_extend_cs> IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    lv_datab = cs_cshdr-datab.
    ASSIGN ('(/AGRI/SAPLGLCSM)/AGRI/S_GLCSSCRFIELDS-GYEAR') TO <fs_year>.
    IF <fs_year> IS ASSIGNED.
      cs_cshdr-datab+0(4) = <fs_year>.
    ENDIF.

    SELECT SINGLE * FROM /agri/glseason
      INTO @DATA(lwa_season)
     WHERE season EQ @cs_cshdr-season.
    IF sy-subrc EQ 0.
      cs_cshdr-datab+4(4) = lwa_season-sperd.
      lv_years = lwa_season-offst.
      IF lv_datab > cs_cshdr-datab.
        cs_cshdr-datab = lv_datab.
      ENDIF.
      cs_cshdr-datbi = cs_cshdr-datab.
      IF lv_years IS NOT INITIAL.
        CALL FUNCTION '/AGRI/G_RP_CALC_DATE_IN_INTERV'
          EXPORTING
            i_date      = cs_cshdr-datab
            i_days      = 0
            i_months    = 0
*           I_SIGNUM    = '+'
            i_years     = lv_years
          IMPORTING
            e_calc_date = cs_cshdr-datbi.
      ENDIF.
      IF lwa_season-eperd IS NOT INITIAL.
        cs_cshdr-datbi+4(4) = lwa_season-eperd.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method /AGRI/IF_GLCS_ENHANCEMENT~WBS_ELEMENT_CREATE.
  endmethod.
ENDCLASS.
