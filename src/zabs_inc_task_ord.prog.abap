************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  create_task_orders                              *
* Include Name      :  ZABS_INC_TASK_ORD                               *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Skip consumption materials from components while*
*                      creating task orders                            *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local Variables declaration
 DATA:  lv_subrc_new        TYPE sy-subrc,
        lv_success_new,
        lv_posnr_new        TYPE co_posnr.

*--Workarea declaration
 DATA : ls_fpitm            TYPE /agri/s_fmfpitm,
        ls_fpcom            TYPE /agri/s_fmfpcom,
        ls_fpitm_tmp        TYPE /agri/s_fmfpitm,
        ls_fpcord           TYPE /agri/s_fmfpcord,
        ls_order_comm       TYPE /agri/s_glpocomm,
        ls_csdoc            TYPE /agri/s_glcs_doc,
        ls_fpdoc_to         TYPE /agri/s_fmfp_doc,
        ls_messtab          TYPE bdcmsgcoll.

*--Internal table declaration
 DATA : lt_fpdoc_new        TYPE /agri/t_fmfp_doc,
        lt_messtab_new      TYPE tab_bdcmsgcoll.

*--Field-Symbol declaration
  FIELD-SYMBOLS: <ls_fpitm> TYPE /agri/s_fmfpitm,
                 <ls_fpcom> TYPE /agri/s_fmfpcom.

  LOOP AT lwa_fpdoc-x-fpitm ASSIGNING <ls_fpitm>
                                WHERE confm EQ c_true.

    gs_variables-item_infocus = <ls_fpitm>-posnr.
    CLEAR: <ls_fpitm>-confm.
    PERFORM messages_context_set USING lwa_fpdoc-x-fphdr.
    CLEAR: gs_variables-item_infocus.
    PERFORM task_order_create_check USING <ls_fpitm>
                                          lwa_fpdoc-x-fphdr
                                 CHANGING lv_subrc_new.
    IF lv_subrc_new IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: ls_order_comm, ls_fpdoc_to, ls_fpcord.
    MOVE-CORRESPONDING lwa_fpdoc-x-fphdr TO ls_fpcord.
    UNASSIGN <ls_fpcom>.
    READ TABLE lwa_fpdoc-x-fpcom ASSIGNING <ls_fpcom>
                                 WITH KEY aufnr = <ls_fpitm>-aufnr
                                          posnr = <ls_fpitm>-posnr
*--SOC to read data based on newly added custom field
                                          zztask = abap_true.
*                                 BINARY SEARCH.
*--EOC to read data based on newly added custom field
    IF <ls_fpcom> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <ls_fpitm> TO ls_fpcord.
    ls_fpcord-matnr = <ls_fpcom>-matnr.
    ls_fpcord-gamng = <ls_fpitm>-tomng.
    ls_fpcord-gstrp = <ls_fpitm>-actdt.
    ls_fpcord-grcre = lwa_fpdoc-x-fphdr-grcre.
    ls_fpcord-ordlv = c_order_level-process.

    CLEAR: ls_fpcord-aufnr,
           ls_fpcord-schta.
    IF <ls_fpitm>-unpln IS NOT INITIAL.
      <ls_fpitm>-gamng = ls_fpcord-gamng.
    ENDIF.
    ls_fpcord-autyp = c_document_category-task_order.

    MOVE-CORRESPONDING ls_fpcord TO ls_order_comm.
    CLEAR: ls_order_comm-auart.
    CALL METHOD /agri/cl_glco_process=>order_create
      EXPORTING
        is_order_comm = ls_order_comm
      IMPORTING
        e_aufnr       = ls_fpcord-aufnr
        et_messages   = lt_messtab_new.

    IF ls_fpcord-aufnr IS NOT INITIAL.
      REFRESH: lt_fpdoc_new.
      PERFORM document_infocus_prepare USING ls_fpcord
                                             ls_csdoc
                                    CHANGING lt_fpdoc_new.
      READ TABLE lt_fpdoc_new INTO ls_fpdoc_to INDEX 1.
      ls_fpdoc_to-x-fphdr-aufnr_v = lwa_fpdoc-x-fphdr-aufnr.
      ls_fpdoc_to-x-fphdr-posnr_v = <ls_fpitm>-posnr.
      ls_fpdoc_to-x-fphdr-equnr   = <ls_fpitm>-equnr.
      IF <ls_fpitm>-unpln IS NOT INITIAL.
        ls_fpdoc_to-x-fphdr-gamng = ls_fpcord-gamng.
      ENDIF.
      PERFORM document_infocus_save USING    c_true
                                    CHANGING ls_fpdoc_to
                                             lv_subrc_new.
      IF lv_subrc_new EQ 0.
        lv_success_new = c_true.
        REFRESH: lt_fpdoc_new.
        APPEND ls_fpdoc_to TO lt_fpdoc_new.
        PERFORM document_flow_update
                             USING c_document_category-production_order
                                   ls_fpcord
                                   lt_fpdoc_new.
        <ls_fpitm>-aufnr_to = ls_fpdoc_to-aufnr.
*--add new line for remaining quantity
        IF <ls_fpitm>-gamng NE ls_fpcord-gamng.
          CLEAR: ls_fpitm, ls_fpcom.
          ls_fpitm = <ls_fpitm>.
          ls_fpcom = <ls_fpcom>.
          CLEAR: ls_fpitm-gamng,
                 ls_fpitm-confm,
                 ls_fpitm-gwemg,
                 ls_fpitm-aufnr_to,
                 ls_fpitm-tomng.
          ls_fpitm-gamng = <ls_fpitm>-gamng - ls_fpcord-gamng.
          IF lv_posnr_new IS INITIAL.
            LOOP AT lwa_fpdoc-x-fpitm INTO ls_fpitm_tmp.
              IF lv_posnr_new LT ls_fpitm_tmp-posnr.
                lv_posnr_new = ls_fpitm_tmp-posnr.
              ENDIF.
            ENDLOOP.
          ENDIF.
          lv_posnr_new = lv_posnr_new + 1.
          ls_fpitm-posnr = lv_posnr_new.
          ls_fpcom-posnr = lv_posnr_new.
          ls_fpitm-updkz = c_updkz_new.
          ls_fpcom-updkz = c_updkz_new.
          APPEND ls_fpitm TO lwa_fpdoc-x-fpitm.
          APPEND ls_fpcom TO lwa_fpdoc-x-fpcom.
        ENDIF.
        <ls_fpitm>-gamng = ls_fpdoc_to-x-fphdr-gamng.
        IF <ls_fpitm>-updkz NE c_updkz_new.
          <ls_fpitm>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT lt_messtab_new INTO ls_messtab
        WHERE msgtyp = c_msg_type-error.
        MESSAGE ID ls_messtab-msgid
              TYPE ls_messtab-msgtyp
            NUMBER ls_messtab-msgnr
              WITH ls_messtab-msgv1 ls_messtab-msgv2
                   ls_messtab-msgv3 ls_messtab-msgv4
              INTO sy-msgli.
        message_simple space.
      ENDLOOP.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
      MESSAGE ID '/AGRI/FMFP' TYPE c_msg_type-error NUMBER '008'
*                            WITH ls_fpdoc_to-aufnr
                            INTO sy-msgli. "#EC*
*****
      message_simple space.
    ENDIF.

    CLEAR: <ls_fpitm>-confm.

  ENDLOOP.

  IF lv_success_new IS NOT INITIAL AND
     lwa_fpdoc-aufnr IS NOT INITIAL.
    PERFORM document_infocus_save USING  c_true
                                CHANGING lwa_fpdoc
                                         lv_subrc_new.
    IF lv_subrc_new EQ 0.
      lv_set_infocus = c_true.
    ENDIF.
  ENDIF.

  RETURN.
