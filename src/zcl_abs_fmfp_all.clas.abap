class ZCL_ABS_FMFP_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_FMFP_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_FMFP_ALL IMPLEMENTATION.


  METHOD /agri/if_ex_badi_fmfp_all~after_save.


  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmfp_all~authority_check.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmfp_all~before_save.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmfp_all~execute_function.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmfp_all~fieldcatalog_modify.

    IF sy-uname EQ 'T_T.KONNO'.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmfp_all~posting_data_check.



  ENDMETHOD.
ENDCLASS.
