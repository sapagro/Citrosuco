class ZCL_ABS_BADI_GLFL_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_GLFL_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_BADI_GLFL_ALL IMPLEMENTATION.


  METHOD /agri/if_ex_badi_glfl_all~after_save.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~archive_check.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~authority_check.




  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~before_save.

    IF cs_flhdr-ownshp IS INITIAL.
*--Atualize o tipo de propriedade!
      MESSAGE e049(zfmfp) DISPLAY LIKE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~crop_season_after_save.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~crop_season_authority_check.



  ENDMETHOD.


METHOD /agri/if_ex_badi_glfl_all~crop_season_before_save.

  DATA: lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack.

****SOC SHAIK 24-09-2020 INC0023569
*-- Populate the Yard material from mass upload program
  IF sy-tcode = '/AGRI/GLUPMD21'
  OR sy-tcode EQ 'ZABS_UPLOAD'.
    IF cs_csasn-zzmatnr IS NOT INITIAL.
      cs_csasn-ymatnr = cs_csasn-zzmatnr.
    ELSE.
      cs_csasn-zzmatnr = cs_csasn-ymatnr.
    ENDIF.
  ENDIF.
****EOC SHAIK 24-09-2020 INC0023569

*--BOC-T_T.KONNO-07.14.21
  lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
  lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).

  DATA(lv_faz_previsao) = abap_false.
  READ TABLE lt_formatted_stack INTO DATA(ls_formatted_stack)
    WITH KEY kind     = 'FUNCTION'
             progname = 'SAPLZABS_FG_ATTR_GROUP_FP'.
  IF sy-subrc EQ 0.
    lv_faz_previsao = abap_true.
  ENDIF.

  IF lv_faz_previsao = abap_true.
    IF cs_csasn-zzmatnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = cs_csasn-zzmatnr
        IMPORTING
          output       = cs_csasn-zzmatnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      cs_csasn-ymatnr = cs_csasn-zzmatnr.
    ENDIF.
  ENDIF.
*--EOC-T_T.KONNO-07.14.21

ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~crop_season_single_check.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~document_check.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~field_catalog_prepare.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~header_data_fill.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~header_defaults_fill.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~status_flow_outcome_set.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~status_flow_step_set.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~status_profile_trigger_set.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~wbs_create.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_glfl_all~yard_order_create.



  ENDMETHOD.
ENDCLASS.
