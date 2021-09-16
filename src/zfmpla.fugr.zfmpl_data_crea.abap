FUNCTION zfmpl_data_crea .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_PLTYP) TYPE  ZFMPLANN
*"     REFERENCE(I_ACNUM) TYPE  ZFMACNUM
*"     REFERENCE(I_DATAB) TYPE  ZFMDATAB
*"     REFERENCE(I_DATBI) TYPE  ZFMDATBI
*"  EXPORTING
*"     REFERENCE(ET_PLDOC) TYPE  ZT_FMPL_DOC
*"  CHANGING
*"     REFERENCE(CT_ZFMACHDR) TYPE  ZT_FMACHDR OPTIONAL
*"     REFERENCE(CT_ZFMACITM) TYPE  ZT_FMACITM OPTIONAL
*"     REFERENCE(CT_FMFPHDR) TYPE  /AGRI/T_FMFPHDR OPTIONAL
*"     REFERENCE(CT_FMFPCOM) TYPE  /AGRI/T_FMFPCOM OPTIONAL
*"     REFERENCE(CT_MAPL) TYPE  TT_MAPL OPTIONAL
*"     REFERENCE(CT_PLPO) TYPE  PLPO_TT OPTIONAL
*"     REFERENCE(CT_CRHD) TYPE  TT_CRHD OPTIONAL
*"     REFERENCE(CT_KAPA) TYPE  COMES_T_DRF_KAPA OPTIONAL
*"     REFERENCE(CT_TC37A) TYPE  TT_TC37A OPTIONAL
*"     REFERENCE(CT_WORK_DAYS) TYPE  WGRC_FABKL_TTY OPTIONAL
*"     REFERENCE(CT_GLFLOTX) TYPE  ZT_GLFLOTX OPTIONAL
*"     REFERENCE(CT_FMFPITM) TYPE  /AGRI/T_FMFPITM OPTIONAL
*"  EXCEPTIONS
*"      DATA_NO_EXITS
*"      TYPE_PLANEJAMENTO_NOT_EXITS
*"      NUMERO_AREA_CULTIVO_NOT_EXITS
*"      INITAL_DATE_NOT_EXITS
*"      FINISH_DATE_NOT_EXITS
*"----------------------------------------------------------------------

  REFRESH: et_pldoc[].

  IF i_pltyp IS INITIAL.
    RAISE type_planejamento_not_exits.
  ENDIF.

  IF i_acnum IS INITIAL.
    RAISE numero_area_cultivo_not_exits.
  ENDIF.
  IF i_datab IS INITIAL.
    RAISE initial_date_not_exits.
  ENDIF.
  IF i_datbi IS INITIAL.
    RAISE finish_date_not_exits.
  ENDIF.
  PERFORM data_planning_get USING    i_acnum
                                     i_pltyp
                            CHANGING  ct_zfmachdr
                                      ct_zfmacitm
                                      ct_fmfphdr
                                      ct_fmfpitm
                                      ct_fmfpcom
                                      ct_mapl
                                      ct_plpo
                                      ct_crhd
                                      ct_kapa
                                      ct_tc37a
                                      ct_glflotx.

  PERFORM work_days_get USING    i_datab
                                 i_datbi
                        CHANGING ct_work_days.
ENDFUNCTION.
