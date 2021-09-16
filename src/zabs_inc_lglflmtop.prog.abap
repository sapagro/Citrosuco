*FUNCTION-POOL /agri/glflm.                  "MESSAGE-ID ..

CLASS : lcl_event_handler DEFINITION DEFERRED.
CLASS : lcl_status_handler DEFINITION DEFERRED.

TABLES: /agri/s_glflot, /agri/s_gtabstrip_captions,
        /agri/s_glflscrfields, "/agri/s_gliflot, /agri/s_gliloa,
        diadr, */agri/s_glflot, /agri/glmdhdr.

CONTROLS: ts_items TYPE TABSTRIP.

***** Environment Vlaues
CONSTANTS: BEGIN OF c_environment,
*           turnaround  TYPE c VALUE 'X',
*           activity TYPE c VALUE ' ',
             change_docs TYPE c VALUE 'C',
           END OF c_environment.

*--Includes
INCLUDE:  /agri/lglflmcon,
          /agri/abgl_constants,
          /agri/global_macros,
          /agri/global_constants,
          /agri/gprolog_macros,
          /agri/global_brf_macros.

DATA: ok_code TYPE sy-ucomm,
      fcode   LIKE ok_code.

DATA: BEGIN OF gs_variables,
        manual_changes,
        document_mode,
        overview_mode,
        attr_manual_changes,
        errors,
        data_changed,
        addr_data_changed,
        worklist_is_visible,
        header_display,
        object_text(40),
        owners_assigned,
*       labeling_active TYPE ilom_cnvrt,
*       alkey       TYPE ilom_alkey,
        main_screen                  TYPE sy-dynnr,
        subs_header                  TYPE sy-dynnr,
        subs_details                 TYPE sy-dynnr,
        subs_items                   TYPE sy-dynnr,
        subs_class                   TYPE sy-dynnr,
        subscr_admin                 TYPE sy-dynnr,
        program                      TYPE sy-repid,
        worklist_refresh,
        initiator                    TYPE /agri/gdescr,
        refresh_attributes,
        refresh_plants_grid,
        refresh_class_grid,
        refresh_desc_grid,
        refresh_additional_data_grid,
        refresh_owners_grid,
        refresh_assignments,
        user_structure(30),
        customer_program             TYPE sy-repid,
        customer_screen              TYPE sy-dynnr,
        wl_srch_count                TYPE i,
        exit_after_save,
        admin_program                TYPE sy-repid,
        user_function1(40),
        user_function2(40),
        user_function3(40),
        user_function4(40),
        user_function5(40),
        external,
        notes_title                  TYPE char060,
        environment                  TYPE c,
        multiple_documents           TYPE c,
        variables_set                TYPE c,
        refresh_cdocs_grid           TYPE c,
        refresh_cdocs_tree           TYPE c,
        prepare_fcat                 TYPE c,
        collapse_tree                TYPE c,
        expand_tree                  TYPE c,
        selections_change            TYPE c,
        all_chngs_tab_prpre          TYPE c,
        view                         TYPE c,
        navigator_is_visible,
        call_from_portal,
        transaction_code             TYPE sy-tcode,
        display_only,
        refresh_worklist,
        refresh_map,
        partners_changed,
        refresh_hierarchy_grid,
        refresh_label_grid,
        refresh_extdfl_tree,
        new_label,
      END OF gs_variables.

*--Tabstrip
DATA: gs_tabstrip_captions TYPE /agri/s_gtabstrip_captions,
      gt_tabstrip_fcodes   TYPE /agri/t_gtabstrip,
      gt_tabstrip_texts    TYPE TABLE OF dd07v,
      gt_tglflts           TYPE TABLE OF /agri/v_tglflts,
      gt_tglcsatl          TYPE TABLE OF /agri/tglcsatl.

*--Control tables
DATA: "gs_tglfltyp TYPE /agri/tglfltyp,
  "gs_t370f    TYPE t370f,
  gs_t370s    TYPE /agri/tgl370s,
  gs_tglfllvl TYPE /agri/tglfllvl.

*--Address
DATA: gs_addr           TYPE addr1_data,
      gv_address_handle TYPE szad_field-handle.

*--Classsifications
DATA: gt_flatg_fcat  TYPE /agri/t_glflatg_fcat,
      gt_flhier_list TYPE /agri/t_glflhier_list,
      gt_athdr       TYPE /agri/t_gathdr,
      gt_atgrp       TYPE TABLE OF /agri/s_glagha,
      gt_attr_vals   TYPE /agri/t_glflatv_fcat,
      gt_owners      TYPE /agri/t_glflown_fcat,
      gt_plants      TYPE /agri/t_glflppl_fcat.

*--Additional data
DATA: gt_additional_data       TYPE TABLE OF /agri/s_abgl_user_scrfields.

DATA: gs_fldoc_infocus    TYPE /agri/s_glfl_doc,
      gt_csdoc_infocus    TYPE /agri/t_glcs_doc,
      gt_fl_desc_layout   TYPE /agri/t_gliflotx_fcat,
      gt_fl_labels_layout TYPE /agri/t_glflos_fcat,
      gt_search_header    TYPE TABLE OF /agri/s_glflot,
      gt_search_text      TYPE TABLE OF /agri/s_gliflotx,
      gt_worklist_header  TYPE TABLE OF /agri/s_glflot_wl.

*--Reference Objects.
DATA: ref_worklist                   TYPE REF TO /agri/cl_worklist_container,
      ref_plants_grid                TYPE REF TO /agri/cl_gui_alv_grid,
      ref_classes_grid               TYPE REF TO /agri/cl_gui_alv_grid,
      ref_attributes_grid            TYPE REF TO /agri/cl_gui_alv_grid,
      ref_owners_grid                TYPE REF TO /agri/cl_gui_alv_grid,
      ref_worklist_container         TYPE REF TO cl_gui_docking_container,
      ref_attributes_container       TYPE REF TO cl_gui_custom_container,
      ref_plants_container           TYPE REF TO cl_gui_custom_container,
      ref_classes_container          TYPE REF TO cl_gui_custom_container,
      ref_owners_container           TYPE REF TO cl_gui_custom_container,
      ref_additional_data_container  TYPE REF TO cl_gui_custom_container,
      ref_additional_data_grid       TYPE REF TO /agri/cl_gui_alv_grid,
      ref_multi_lang_desc_grid       TYPE REF TO /agri/cl_gui_alv_grid,
      ref_multi_lang_desc_container  TYPE REF TO cl_gui_custom_container,
      ref_terrain_labels_container   TYPE REF TO cl_gui_custom_container,
      ref_terrain_labels_grid        TYPE REF TO /agri/cl_gui_alv_grid,
      ref_hierarchy_disply_container TYPE REF TO cl_gui_custom_container,
      ref_hierarchy_disply_grid      TYPE REF TO /agri/cl_gui_alv_grid,
      ref_event_handler              TYPE REF TO lcl_event_handler,
      ref_container_map              TYPE REF TO cl_gui_custom_container,
      ref_html_viewer_map            TYPE REF TO cl_gui_html_viewer,
      ref_container_tree_ext_doc     TYPE REF TO cl_gui_custom_container,
      ref_tree_ext_doc               TYPE REF TO /agri/cl_alv_tree_simple_hier.

DATA: ref_status_handler       TYPE REF TO lcl_status_handler.
DATA: ref_text                 TYPE REF TO /agri/cl_gtext_process.

****Selected Docs and Rows
DATA : gt_selected_terrains TYPE TABLE OF /agri/s_glflot_wl,
       gt_selected_docs     TYPE /agri/t_gltplnr,
       gt_class_mod_rows    TYPE lvc_t_modi,
       gt_selected_rows     TYPE lvc_t_row,
       gt_desc_mod_rows     TYPE lvc_t_modi,
       gt_owner_mod_rows    TYPE lvc_t_modi,
       gt_plant_mod_rows    TYPE lvc_t_modi.

*--BAdI's
DATA: ref_badi_glfl_all TYPE REF TO /agri/badi_glfl_all,
      ref_badi_glfl_cs  TYPE REF TO /agri/badi_glfl_cs.

*****Change Documents
DATA: gt_tplnr_fl         TYPE /agri/t_gltplnr,
      gt_cdhdr            TYPE cdhdr_tab,
      gt_filter_selection TYPE /agri/t_gselfrange,
      gt_knuma_ag         TYPE /agri/t_gltplnr,
      gt_cddetails        TYPE TABLE OF /agri/s_gcdred,
      gt_gcdobjact        TYPE TABLE OF /agri/tgcdobjact,
      gt_ddtypedesc       TYPE TABLE OF ddtypedesc,
      gt_cddetails_final  TYPE TABLE OF cdred.

TYPES: BEGIN OF t_tcdob,
         tabname    TYPE tabname,
         ddtext     TYPE ddtext,
         tab_ref(2) TYPE n,
       END OF t_tcdob.
DATA : gt_tcdob           TYPE TABLE OF t_tcdob.

DATA :  gs_flhdr_portal   TYPE /agri/s_glflot.

DATA: gt_xflptr_chng   TYPE /agri/t_glihpa,
      gt_yflptr_chng   TYPE /agri/t_glihpa,
      gt_glflot_buffer TYPE /agri/t_glflot,
      gt_extdfl_tree   TYPE /agri/t_glfldfl_fcat,
      gt_tplnr         TYPE /agri/t_gltplnr.

DATA: gv_faz TYPE /agri/s_glflot-tplnr_fl.
