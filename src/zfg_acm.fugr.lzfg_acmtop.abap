FUNCTION-POOL ZFG_ACM.                      "MESSAGE-ID ..

* INCLUDE LZFG_ACMD...                       " Local class definition


CLASS: lcl_event_handler DEFINITION DEFERRED.
CLASS: lcl_status_handler DEFINITION DEFERRED.

INCLUDE: LZFG_ACMCON,
          /agri/abgl_constants,
         /agri/global_macros,
         /agri/global_constants,
         /agri/gprolog_macros,
         /agri/global_brf_macros,
         /agri/glprolog_macros.
*--Tables
TABLES: /agri/s_fmachdr, aufk, sscrfields, /agri/s_gtabstrip_captions,/AGRI/s_FMFPHDR,
        zabs_s_ac_scr_fields.

TYPES: BEGIN OF ty_details.
        INCLUDE STRUCTURE /agri/s_fmoc_details.
TYPES:  tplnr_fl TYPE /agri/gltplnr_fl,
        prnum    TYPE /agri/fmprnum,
       END OF ty_details.

FIELD-SYMBOLS: <gt_fmac_fcat> TYPE STANDARD TABLE.

*--Fcodes
DATA: ok_code                         TYPE sy-ucomm,
      io_text                         TYPE string,
      fcode                           LIKE ok_code.

RANGES: gr_aufnr    FOR aufk-aufnr,
        gr_aufnrtmp FOR aufk-aufnr.

*--Tabstrip
DATA: gs_tabstrip_captions           TYPE /agri/s_gtabstrip_captions,
      gt_tabstrip_fcodes             TYPE /agri/t_gtabstrip,
      gt_tfmirts                     TYPE TABLE OF /agri/v_tfmirts,
      gt_fcat                        TYPE lvc_t_fcat,
      gs_tfmactyp                    TYPE /agri/tfmactyp,
      gt_arbpl                       TYPE /agri/t_range_arbpl,
      gt_search_header               TYPE TABLE OF /agri/s_fmachdr,
      gt_worklist_header             TYPE TABLE OF /agri/s_fmachdr_wl,
      gs_acdoc_infocus               TYPE /agri/s_fmacs_doc,
      gs_f4_fields                   TYPE lvc_s_f4.

****Selected Docs and Rows
DATA: gt_selected_docs              TYPE /agri/t_fmacom,
      gt_selected_rows              TYPE lvc_t_row,
      gt_items_layout               TYPE /agri/t_fmacitm_layout,
      gt_details_tmp                TYPE /agri/t_fmoc_details,
      gt_fphdr                      TYPE /agri/t_fmac_fp,
      gt_details                    TYPE TABLE OF ty_details,
      gt_details_fcat               TYPE TABLE OF ty_details,
*     gt_details_trr                TYPE TABLE OF ty_details,
      gt_ac_desc_layout             TYPE /agri/t_fmachdrt_layout_fcat,
      gt_items_mod_rows             TYPE lvc_t_modi,
      gt_desc_mod_rows              TYPE lvc_t_modi,
      gt_returntab                  TYPE TABLE OF ddshretval,
      gt_dropdown_values            TYPE lvc_t_dral,
      gt_f4_fields                  TYPE lvc_t_f4.

****Reference Variables
DATA: ref_worklist_container        TYPE REF TO cl_gui_docking_container,
      ref_worklist                  TYPE REF TO /agri/cl_worklist_container,
      ref_event_handler             TYPE REF TO lcl_event_handler,
      ref_grid_items                TYPE REF TO /agri/cl_gui_alv_grid,
      ref_grid_details              TYPE REF TO /agri/cl_gui_alv_grid,
      ref_container_items           TYPE REF TO cl_gui_custom_container,
      ref_details_container         TYPE REF TO cl_gui_custom_container,
      ref_additional_data_container TYPE REF TO cl_gui_custom_container,
      ref_multi_lang_desc_grid      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_multi_lang_desc_container TYPE REF TO cl_gui_custom_container,
      ref_grid_additional_data      TYPE REF TO /agri/cl_gui_alv_grid.

*--BAdI's
DATA: ref_badi_fmac_all             TYPE REF TO /agri/badi_fmac_all.

****Tabstrip settings
DATA: gt_tabstrip_texts              TYPE TABLE OF dd07v.

*--Additional data
DATA: gt_additional_data        TYPE TABLE OF /agri/s_abgl_user_scrfields.

DATA: ref_status_handler TYPE REF TO lcl_status_handler.

DATA: gr_lstar           TYPE RANGE OF lstar.

*--Controls
CONTROLS: ts_items                    TYPE TABSTRIP.

*--Texts
DATA: ref_text                      TYPE REF TO /agri/cl_gtext_process.

DATA: BEGIN OF gs_variables,
        overview_mode,
        document_mode,
        object_text(25),
        display_only,
        manual_changes,
        document_changed,
        refresh_worklist,
        worklist_is_visible,
        errors,
        copy,
*        curline TYPE i,
*        curfield(40),
*        scrfield(50),
        program                       TYPE sy-repid,
        main_screen                   TYPE sy-dynnr,
        subs_header TYPE sydynnr,
        subs_details TYPE sydynnr,
        exit_after_save,
        wl_srch_count                 TYPE i,
*        user_additional_data TYPE sydynnr,
        subs_items TYPE sydynnr,
        accom_in_focus TYPE /agri/fmaccom,
        actyp_in_focus TYPE /agri/fmactyp,
        data_changed,
        refresh_items_grid,
        refresh_grid_desc,
        refresh_postings_grid,
        display_details,
*        refresh_items_grid,
*        refresh_desc_grid,
*        refresh_attributes,
*        refresh_mea_grid,
*        subs_class TYPE sy-dynnr,
        refresh_additional_data_grid,
        subscr_admin              LIKE sy-dynnr,
        initiator TYPE /agri/gdescr,
        admin_program TYPE sy-repid,
        header_display,
        user_structure(30),
*        user_function1(40),
*        user_function2(40),
*        user_function3(40),
*        user_function4(40),
*        user_function5(40),
        notes_title          TYPE char060,
*        transaction_code     TYPE sy-tcode,
*        call_from_portal,
*        navigator_is_visible,
        external,
      END OF gs_variables.

DATA: gv_posting_date TYPE budat.
