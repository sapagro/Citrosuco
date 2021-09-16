************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  items_data_prepare                              *
* Include Name      :  ZABS_INC_NWB_ITEM_DATA                          *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Prepare the items for task materials            *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local Variables declaration
  DATA: lv_posnr_new TYPE co_posnr,
        lv_contr_new TYPE /agri/fmccontr.

*--Workarea declaration
  DATA :ls_csprso     TYPE /agri/s_glcsprso,
        ls_fpitm      TYPE /agri/s_fmfpitm,
        ls_fpcom      TYPE /agri/s_fmfpcom,
        ls_resb       TYPE resbd,
        ls_operations TYPE bapi_order_operation1.

*--Internal table declaration
  DATA :lt_resb_new TYPE TABLE OF resbd,
        ltr_mtart   TYPE RANGE OF mtart.

  DATA: lo_range_mtart TYPE REF TO data.

*--Field-Symbols declaration
  FIELD-SYMBOLS: <fs_mtart> TYPE any.

  CALL FUNCTION '/AGRI/G_CO_BC_RESBD_TAB_ORDGET'
    EXPORTING
      i_aufnr_imp = lwa_fpdoc-x-fphdr-aufnr
      i_rsnum_imp = lwa_fpdoc-x-fphdr-rsnum
    TABLES
      resbd_tab   = lt_resb_new.

*--SOC for shuffling between task order and batches
  IF lwa_fpdoc-x-fphdr-autyp EQ c_document_category-production_order AND
     lt_resb_new IS NOT INITIAL.

    CREATE DATA lo_range_mtart LIKE ltr_mtart.

    IF lo_range_mtart IS BOUND.
      CALL METHOD zcl_abs_get_variants=>get_range_constants
        EXPORTING
          iv_mod   = zcl_abs_abap_maintain=>c_custom_mode "'C'
          iv_objid = zcl_abs_abap_maintain=>c_objid_nursery_wb "'NSWB'
          iv_k1val = zcl_abs_abap_maintain=>c_key_task_mat_types "'TMTP'
        IMPORTING
          eo_range = lo_range_mtart.

      ASSIGN lo_range_mtart->* TO <fs_mtart>.
      IF <fs_mtart> IS ASSIGNED.
        ltr_mtart = <fs_mtart>.
      ENDIF.
    ENDIF.

    IF ltr_mtart IS NOT INITIAL.
      SELECT matnr, mtart
        FROM mara
        INTO TABLE @DATA(lt_mara)
         FOR ALL ENTRIES IN @lt_resb_new
       WHERE matnr EQ @lt_resb_new-matnr.
      IF sy-subrc EQ 0.
        SORT lt_mara BY matnr.
      ENDIF.
    ENDIF.
  ENDIF.
*--EOC for shuffling between task order and batches

  DELETE lt_resb_new WHERE xloek IS NOT INITIAL.

  LOOP AT lt_operations INTO ls_operations
                       WHERE routing_no EQ lwa_fpdoc-x-fphdr-aufpl.

    IF lwa_csdoc_infocus IS NOT INITIAL AND
       lwa_fpdoc-x-fphdr-autyp EQ c_document_category-production_order.
      CLEAR: ls_csprso.
      READ TABLE lwa_csdoc_infocus-x-csprso INTO ls_csprso
                 WITH KEY werks = lwa_fpcord-iwerk
                          cpros = lwa_fpcord-cpros
                          vornr = ls_operations-operation_number.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      ls_fpitm-sappl = ls_csprso-sappl.
      ls_fpitm-byprd = ls_csprso-byprd.
    ENDIF.

    ls_fpitm-aufnr = lwa_fpdoc-x-fphdr-aufnr.
    ls_fpitm-vornr = ls_operations-operation_number.
    ls_fpitm-steus = ls_operations-opr_cntrl_key.
    ls_fpitm-ltxa1 = ls_operations-description.
    ls_fpitm-rueck = ls_operations-conf_no.
    ls_fpitm-gamng = ls_operations-quantity.
    IF lwa_fpdoc-x-fphdr-gamng IS NOT INITIAL.
      ls_fpitm-umren = ls_fpitm-gamng / lwa_fpdoc-x-fphdr-gamng.
    ENDIF.
    ls_fpitm-meinh = ls_operations-unit.
    ls_fpitm-arbpl = ls_operations-work_center.
    ls_fpitm-actdt = ls_operations-late_sched_start_date_exec.

    lv_posnr_new   = lv_posnr_new + 1.
    ls_fpitm-posnr = lv_posnr_new.
    ls_fpitm-updkz = c_updkz_new.

*--Reservations
    LOOP AT lt_resb_new INTO ls_resb
                   WHERE vornr EQ ls_operations-operation_number
                     AND xwaok IS NOT INITIAL.
      ls_fpcom-aufnr = lwa_fpdoc-x-fphdr-aufnr.
      ls_fpcom-matnr = ls_resb-matnr.
      ls_fpcom-erfmg = ls_resb-erfmg.
      ls_fpcom-esmng = ls_resb-esmng.
      ls_fpcom-erfme = ls_resb-erfme.
      ls_fpcom-werks = ls_resb-werks.
      ls_fpcom-lgort = ls_resb-lgort.
      ls_fpcom-rspos = ls_resb-rspos.
      ls_fpcom-bwart = ls_resb-bwart.
      ls_fpcom-xwaok = ls_resb-xwaok.
      IF ls_resb-rgekz IS NOT INITIAL.
        CLEAR: ls_fpcom-xwaok.
      ENDIF.

*--SOC for shuffling between task order and batches
      READ TABLE lt_mara INTO DATA(ls_mara)
            WITH KEY matnr = ls_fpcom-matnr
          BINARY SEARCH.
      IF sy-subrc EQ 0 AND ls_mara-mtart IN ltr_mtart.
        ls_fpcom-zztask = abap_true.
      ENDIF.
*--EOC for shuffling between task order and batches

      ls_fpcom-posnr = lv_posnr_new.
      lv_contr_new        = lv_contr_new + 1.
      ls_fpcom-contr = lv_contr_new.
      ls_fpcom-updkz = c_updkz_new.
      APPEND ls_fpcom TO lwa_fpdoc-x-fpcom.
      CLEAR: ls_fpcom.

*--SOC for shuffling between task order and batches
*      IF lwa_fpcord-autyp EQ c_document_category-production_order AND
*         lwa_fpcord-ordlv EQ c_order_level-process_task.
*        EXIT.
*      ENDIF.
*--EOC for shuffling between task order and batches

    ENDLOOP.

    APPEND ls_fpitm TO lwa_fpdoc-x-fpitm.
    CLEAR: ls_fpitm.
    CLEAR: lv_contr_new.

  ENDLOOP.

*--SOC for shuffling between task order and batches
*--If GR is enable set last item as GR enable.
*  IF lwa_fpdoc-x-fphdr-ordlv NE c_order_level-process_task AND
*     lv_posnr IS NOT INITIAL.
  IF lv_posnr_new IS NOT INITIAL.
*--EOC for shuffling between task order and batches

    ls_fpitm-grcre = lwa_fpcord-grcre.
    MODIFY lwa_fpdoc-x-fpitm FROM ls_fpitm INDEX lv_posnr_new
                             TRANSPORTING grcre.
  ENDIF.

  RETURN.
