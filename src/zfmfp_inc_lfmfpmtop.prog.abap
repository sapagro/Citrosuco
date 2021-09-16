*&---------------------------------------------------------------------*
*& Include ZFMFP_INC_LFMFPMTOP
*&---------------------------------------------------------------------*
*--Tables
TABLES: /agri/s_fmfphdr, sscrfields, /agri/s_gtabstrip_captions,
        /agri/s_fmfpcom, /agri/glflcma, /agri/s_fmfpscrfields,
        /agri/s_fmfp_cnf, /agri/s_fmfpitm, /agri/s_glflot.

DATA : ok_code  TYPE  sy-ucomm,
       fcode    LIKE  ok_code.

CONTROLS: ts_items TYPE TABSTRIP.

DATA: BEGIN OF gs_variables,
       overview_mode,
       document_mode,
       task_order_mode,
       object_text(40),
       header_display,
       details_display,
       items_display,
       refresh_worklist,
       exit_after_save,
       worklist_is_visible,
       item_infocus            TYPE co_posnr,
       refresh_header,
       refresh_items_grid,
       refresh_confirmation_grid,
       refresh_components_grid,
       refresh_activities_grid,
       refresh_create_orders,
       refresh_next_orders,
       refresh_postings_grid,
       refresh_mass_conf_grid,
       refresh_bom, "--10/10/2016
       refresh_colum_bom, "--10/10/2016
       manual_changes,
       errors,
       document_changed,
       external,
       navigator_is_visible,
       call_from_portal,
       display_only,
       transaction_code         TYPE sy-tcode,
       selected_process         TYPE /agri/glcpros,
       initiator                TYPE /agri/gdescr,
       aufnr_in_focus           TYPE /agri/fmfpnum,
       program                  TYPE sy-repid,
       main_screen              TYPE sydynnr,
       subscr_quick_info        TYPE sydynnr,
       subscr_header            TYPE sydynnr,
       subscr_items             TYPE sydynnr,
       docking_screen           TYPE sydynnr,
       subscr_admin             TYPE sy-dynnr,
       admin_program            TYPE sy-repid,
       task_material            TYPE matnr,
       task_description         TYPE maktx,
       basic_start_date         TYPE sy-datum,
       personnel_number         TYPE co_pernr,
      END OF gs_variables.

****Expand and Compress data areas
DATA: BEGIN OF gs_compress_data_area,
        quick_info(3),
        details(3),
        header(3),
        items(3),
      END OF gs_compress_data_area,
****13/10/2016
      BEGIN OF gs_unit,
        tomng TYPE /agri/fmtomng,
        meinh TYPE vorme,
      END OF gs_unit.
****13/10/2016

DATA: gt_csdoc           TYPE /agri/t_glcs_doc,
      gs_fpoc_doc        TYPE /agri/s_fmoc_doc,
      gs_fpdoc_infocus   TYPE /agri/s_fmfp_doc,
      gs_todoc_infocus   TYPE /agri/s_fmfp_doc,
      gt_fpitm_fcat      TYPE /agri/t_fmfpitm_fcat,
      gt_fpcnf_mass      TYPE /agri/t_fmfp_cnf_mass,
      gt_fpcnf_mass_fcat TYPE /agri/t_fmfp_cnf_mass_fcat,
      gt_fpbom_fcat      TYPE /agri/t_fmfpbom_fcat, "--10/10/2016
      gt_fpbom_temp      TYPE /agri/t_fmfpbom_fcat, "--23/03/2017
      gt_components      TYPE /agri/t_fmfpcom_fcat,
      gt_activities_fcat TYPE /agri/t_fmfpact_fcat,
      gt_fpcord_fcat     TYPE /agri/t_fmfpcord_fcat,
      gt_fpcord          TYPE /agri/t_fmfpcord,
      gt_update_aufnr    TYPE /agri/t_fmaufnr,
      gt_next_orders     TYPE /agri/t_fmfpcnprs_ord,
      gt_fmfpcnf         TYPE /agri/t_fmfp_cnf,
      gt_fpcnf_fcat      TYPE /agri/t_fmfp_cnf_fcat,
      gt_dropdown        TYPE lvc_t_drop.

FIELD-SYMBOLS: <gs_fpdoc_infocus> TYPE /agri/s_fmfp_doc.

DATA: gt_search_header               TYPE TABLE OF /agri/s_fmfphdr,
      gt_search_next_header          TYPE TABLE OF /agri/s_fmfphdr,
      gt_worklist_header             TYPE TABLE OF /agri/s_fmfphdr_wl.

DATA: gt_selected_docs               TYPE /agri/t_fmaufnr,
      gt_selected_rows               TYPE lvc_t_row,
      gt_items_mod_rows              TYPE lvc_t_modi,
      gt_comp_mod_rows               TYPE lvc_t_modi,
      gt_acti_mod_rows               TYPE lvc_t_modi,
      gt_next_mod_rows               TYPE lvc_t_modi,
      gt_task_itm_mod_rows           TYPE lvc_t_modi,
      gt_bom_mod_rows                TYPE lvc_t_modi."--13/10/2016

DATA: gs_tabstrip_captions           TYPE /agri/s_gtabstrip_captions,
      gt_tabstrip_fcodes             TYPE /agri/t_gtabstrip.

****Tabstrip settings
DATA: gt_tabstrip_defaults           TYPE /agri/t_gtabstrip,
      gt_tabstrip_texts              TYPE TABLE OF dd07v.

DATA: gs_fphdr_portal                TYPE /agri/s_fmfphdr.

DATA : ref_worklist_container        TYPE REF TO cl_gui_docking_container,
       ref_worklist                  TYPE REF TO /agri/cl_worklist_container,
       ref_log_handler               TYPE REF TO lcl_log_handler,
       ref_event_handler             TYPE REF TO lcl_event_handler,
       ref_grid_items                TYPE REF TO /agri/cl_gui_alv_grid,
       ref_dock_container_task_itm   TYPE REF TO cl_gui_docking_container,
       ref_dock_container_comp       TYPE REF TO cl_gui_docking_container,
       ref_dock_container_actv       TYPE REF TO cl_gui_docking_container,
       ref_items_container           TYPE REF TO cl_gui_custom_container,
       ref_confirmations_container   TYPE REF TO cl_gui_custom_container,
       ref_grid_components           TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_activities           TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_create_orders        TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_next_orders          TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_confirmations        TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_bom                  TYPE REF TO /agri/cl_gui_alv_grid, "--10/10/2016
       ref_container_next_orders     TYPE REF TO cl_gui_custom_container,
       ref_container_create_orders   TYPE REF TO cl_gui_custom_container,
       ref_container_bom             TYPE REF TO cl_gui_custom_container, "--10/10/2016

       ref_grid_mass_conf            TYPE REF TO /agri/cl_gui_alv_grid,
       ref_mass_conf_container       TYPE REF TO cl_gui_custom_container.

**BAdI's
DATA: ref_badi_fmfp_all   TYPE REF TO /agri/badi_fmfp_all.
