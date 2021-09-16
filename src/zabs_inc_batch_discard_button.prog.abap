************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Method Name       :  ON_GRID_TOOLBAR                                 *
* Include Name      :  ZABS_INC_BATCH_DISCARD_BUTTON                   *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.19.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Batch Discard Functionality                     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Workareas declarations
DATA : ls_button TYPE stb_button,
       ls_btnmnu TYPE stb_btnmnu.

*--Local Variables
DATA : lv_view_new          TYPE i,
       lv_bcan_text         TYPE gui_text,
       lv_bdis_text         TYPE gui_text,
       lref_fca_details_new TYPE REF TO cl_ctmenu.

CASE sender.
  WHEN ref_grid_items.
    toolbar_button_insert e_object->mt_toolbar ls_button 3
                              space space space space.

    IF gs_variables-overview_mode NE c_mode_display
*--SOC to add batch discard functionality
     AND gs_variables-document_mode NE c_mode_display.
*--EOC to add batch discard functionality
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-delete_itm icon_delete_row
                            TEXT-035 space.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-insert_itm icon_insert_row
                            TEXT-034 space.
      CREATE OBJECT lref_fca_details_new.
      IF gs_fpdoc_infocus-x-fphdr-schta IS NOT INITIAL.
        menu_function_add lref_fca_details_new c_fcode-schedule TEXT-043.
      ENDIF.
      menu_function_add lref_fca_details_new c_fcode-tord_create TEXT-032.
      menu_function_add lref_fca_details_new c_fcode-task_order TEXT-008.
      menu_function_add lref_fca_details_new c_fcode-confirm_mass TEXT-045.
      menu_function_add lref_fca_details_new c_fcode-task_teco TEXT-009.
      menu_function_add lref_fca_details_new c_fcode-task_undo_teco TEXT-017.
      static_context_set e_object->mt_btnmnu ls_btnmnu
                       lref_fca_details_new c_fcode-task_actions.
      toolbar_button_insert e_object->mt_toolbar ls_button 2
                          c_fcode-task_actions icon_order
                          TEXT-041 space.
    ELSE.
      CREATE OBJECT lref_fca_details_new.
      menu_function_add lref_fca_details_new c_fcode-task_order TEXT-008.
      static_context_set e_object->mt_btnmnu ls_btnmnu
                       lref_fca_details_new c_fcode-task_actions.
      toolbar_button_insert e_object->mt_toolbar ls_button 2
                          c_fcode-task_actions icon_order
                          TEXT-041 space.
    ENDIF.

  WHEN ref_grid_confirmations.
    toolbar_button_insert e_object->mt_toolbar ls_button 3
                              space space space space.

    IF gs_variables-task_order_mode NE c_mode_display.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-delete_opr icon_delete_row
                            TEXT-035 space.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-insert_opr icon_insert_row
                            TEXT-034 space.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-save_operations icon_system_save
                            TEXT-039 space.
      toolbar_button_insert e_object->mt_toolbar ls_button 3
                                space space space space.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-confirm icon_operation
                            TEXT-008 space.
    ENDIF.
  WHEN ref_grid_components.
    toolbar_button_insert e_object->mt_toolbar ls_button 3
                              space space space space.
    IF gs_variables-task_order_mode NE c_mode_display.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-delete_comp icon_delete_row
                            TEXT-035 space.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-insert_comp icon_insert_row
                            TEXT-034 space.
    ENDIF.
  WHEN ref_grid_batches.
    toolbar_button_insert e_object->mt_toolbar ls_button 3
                                space space space space.
    IF gs_variables-overview_mode NE c_mode_display AND
       gs_fpdoc_infocus-x-fphdr-aufnr_v IS INITIAL.
      toolbar_button_insert e_object->mt_toolbar ls_button space
                            c_fcode-delete_batch icon_delete_row
                            TEXT-035 space.
    ENDIF.
    CREATE OBJECT lref_fca_details_new.

    IF gs_variables-overview_mode EQ c_mode_display. "LOC available only in display mode
      menu_function_add lref_fca_details_new c_fcode-display_details TEXT-047.
    ENDIF.
    IF gs_variables-overview_mode NE c_mode_display.
      IF gs_fpdoc_infocus-x-fphdr-aufnr_v IS NOT INITIAL.
        menu_function_add lref_fca_details_new c_fcode-reprocess_batch TEXT-046.
      ENDIF.
      menu_function_add lref_fca_details_new c_fcode-batch_single_confirm TEXT-049.
*--SOC to add batch discard functionality
*      menu_function_add lref_fca_details_new zcl_abs_abap_maintain=>c_fcode_btch_canc 'Batch Cancellation'. "'BCAN
*      menu_function_add lref_fca_details_new zcl_abs_abap_maintain=>c_fcode_btch_disc 'Batch Discard'. "'BDIS'
*--Estorno do Lote/Batch Cancellation
      MESSAGE i088(zfmfp) INTO lv_bcan_text.
*--Descarte do Lote/Batch Discard
      MESSAGE i089(zfmfp) INTO lv_bdis_text.
      menu_function_add lref_fca_details_new zcl_abs_abap_maintain=>c_fcode_btch_canc lv_bcan_text. "'BCAN
      menu_function_add lref_fca_details_new zcl_abs_abap_maintain=>c_fcode_btch_disc lv_bdis_text. "'BDIS'
*      menu_function_add lref_fca_details_new c_fcode-batches_mass_confirm TEXT-045.
*--EOC to add batch discard functionality
    ENDIF.
    static_context_set e_object->mt_btnmnu ls_btnmnu
                       lref_fca_details_new c_fcode-batch_actions.
    toolbar_button_insert e_object->mt_toolbar ls_button 2
                          c_fcode-batch_actions icon_batch
                          TEXT-048 space.
  WHEN OTHERS.
ENDCASE.

RETURN.
