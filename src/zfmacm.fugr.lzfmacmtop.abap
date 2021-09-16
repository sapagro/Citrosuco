FUNCTION-POOL zfmacm.                 "MESSAGE-ID ..

CLASS : lcl_event_handler DEFINITION DEFERRED,
        lcl_log_handler   DEFINITION DEFERRED.


TABLES: /agri/s_gtabstrip_captions,zfmachdr,
        zsc_fmachdr, zsc_fmacitm, sscrfields, zsc_fmacvlcl.

*--Includes
INCLUDE: zlfmacmcon,
         lzfmacmsel,
         /agri/abgl_constants,
         /agri/global_macros,
         /agri/global_constants,
         /agri/gprolog_macros,
         /agri/global_brf_macros.

TYPES: BEGIN OF s_actyp,
         actyp TYPE zfmactyp,
       END OF s_actyp,
       t_actyp TYPE TABLE OF s_actyp.

*--Fcodes
DATA : ok_code TYPE  sy-ucomm,
       fcode   LIKE  ok_code.

*--Controls
CONTROLS: ts_items TYPE TABSTRIP.

*--Global Data

DATA: BEGIN OF gs_variables,
        overview_mode,
        document_mode,
        object_text(40),
        display_only,
        copy,
        document_changed,
        manual_changes,
        data_changed,
        refresh_worklist,
        worklist_is_visible,
        external,
        exit_screen,
        messages,
        errors,
        refresh_items_grid,
        refresh_vlc_grid,
        refresh_attributes_grid,
        refresh_attributes_mass_grid,
        refresh_latest_values_grid,
        refresh_output_table,
        refresh_display_output_table,
        navigator_is_visible,
        call_from_portal,
        items_manual_changes,
        measurement_level            TYPE /agri/glaslvl,
        characteristics_display,
        curfield(40),
        scrfield(50),
        style_fname                  TYPE lvc_fname,
        transaction_code             TYPE sy-tcode,
        actyp_in_focus               TYPE zfmactyp,
        program                      TYPE sy-repid,
        subscr_quick_info            TYPE sydynnr,
        subscr_details               TYPE sydynnr,
        exit_after_save,
        user_additional_data         TYPE sydynnr,
        subscr_admin                 LIKE sy-dynnr,
        initiator                    TYPE /agri/gdescr,
        admin_program                TYPE sy-repid,
        mass_action,
        cancelled,
        external_dialog,
        display_mode,                 "TYPE /agri/fmacmod,
        attribute_description,
        user_structure(30),
        user_function1(40),
        user_function2(40),
        user_function3(40),
        user_function4(40),
        user_function5(40),
        notes_title                  TYPE char060,
        header_display,
      END OF gs_variables.

*--Gobal Tables for Grid display
DATA: gt_search_header      TYPE TABLE OF zsc_fmachdr,
      gs_fmactyp            TYPE ztfmactyp,
      gt_search_next_header TYPE TABLE OF zsc_fmachdr,
      gt_worklist_header    TYPE TABLE OF zsc_fmachdr_wl,
      gt_selected_docs      TYPE zt_fmac_key.

****Global Infocus Document Structure
DATA: gs_acdoc_infocus TYPE zsc_fmac_doc.


DATA: gt_achdr        TYPE zt_fmachdr,
      gt_fmacitm_fcat TYPE zt_fmacitm_fcat,
      gt_fmacvlc_fcat TYPE zt_fmacvlcl_fcat,
*-- BOC T_T.KONNO 04.07.21
      gt_glflcma      TYPE STANDARD TABLE OF /agri/glflcma INITIAL SIZE 0,
*-- EOC T_T.KONNO 04.07.21
      gs_tfmactyp     TYPE ztfmactyp.

DATA: gt_acdoc              TYPE zt_fmac_doc.

DATA: gt_fcat TYPE lvc_t_fcat.
DATA: gt_fcat_itm TYPE lvc_t_fcat.

****Reference Variables
DATA : ref_worklist_container TYPE REF TO cl_gui_docking_container,
       ref_worklist           TYPE REF TO /agri/cl_worklist_container,
       ref_event_handler      TYPE REF TO lcl_event_handler,
       ref_log_handler        TYPE REF TO lcl_log_handler,
       ref_container_items    TYPE REF TO cl_gui_custom_container,
       ref_container_vlcl     TYPE REF TO cl_gui_custom_container,
       ref_grid_items         TYPE REF TO /agri/cl_gui_alv_grid,
       ref_grid_vlcl          TYPE REF TO /agri/cl_gui_alv_grid.

****Selected Docs and Rows
DATA :  gt_selected_rows      TYPE lvc_t_row.
DATA :  gt_selected_rows_vldl TYPE lvc_t_row.

DATA:   gt_items_modi  TYPE lvc_t_modi.
DATA:   gt_items_modi_vlcl  TYPE lvc_t_modi.

*--BAdI's
*DATA: ref_badi_FMAC_all              TYPE REF TO /agri/badi_FMAC_all.

*--Tabstrip
DATA: gs_tabstrip_captions TYPE /agri/s_gtabstrip_captions,
      gt_tabstrip_fcodes   TYPE /agri/t_gtabstrip.

*--Texts
DATA: ref_text             TYPE REF TO /agri/cl_gtext_process.

****Tabstrip settings
DATA: gt_tabstrip_texts    TYPE TABLE OF dd07v.

DATA: gs_achdr_portal      TYPE zsc_fmachdr.

TYPES: BEGIN OF type_safras,
         tplnr_fl TYPE /agri/gltplnr_fl,
         contr    TYPE /agri/gcontr,
         datab    TYPE /agri/gldatab,
         datbi    TYPE /agri/gldatbi,
       END OF type_safras,

       type_safras_tab TYPE STANDARD TABLE OF type_safras,

       BEGIN OF type_seasons,
         tplnr_fl TYPE /agri/gltplnr_fl,
         cmnum    TYPE /agri/glcmnum,
         varia    TYPE /agri/glvaria,
         season   TYPE /agri/gl_season,
         datab    TYPE /agri/gldatab,
         datbi    TYPE /agri/gldatbi,
       END OF type_seasons,

       type_seasons_tab TYPE STANDARD TABLE OF type_seasons.
