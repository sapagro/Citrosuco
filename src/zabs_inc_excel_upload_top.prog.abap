*&---------------------------------------------------------------------*
*& Include ZABS_INC_EXCEL_UPLOAD_TOP
*&---------------------------------------------------------------------*

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
*INCLUDE /agri/glpg_upload_process_sel.
INCLUDE zabs_glpg_upload_process_sel.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0a.
INCLUDE /agri/glpg_upload_process_f0c.
INCLUDE /agri/glpg_upload_process_f0d.
INCLUDE /agri/glpg_upload_process_f0f.
INCLUDE /agri/glpg_upload_process_f0i.
INCLUDE /agri/glpg_upload_process_f0m.
INCLUDE /agri/glpg_upload_process_f0t.

* Define Screen Container
DATA: obj_container TYPE REF TO cl_gui_custom_container,
      o_error       TYPE REF TO i_oi_error,
      o_control     TYPE REF TO i_oi_container_control,
      o_document    TYPE REF TO i_oi_document_proxy,
      o_spreadsheet TYPE REF TO i_oi_spreadsheet.

* Data declarations.
DATA: t_files      TYPE filetable,
      t_sheets     TYPE soi_sheets_table,
      t_sheets_out TYPE /agri/t_excel_sheet,
      s_sheets_out TYPE /agri/s_excel_sheet,
      t_ranges     TYPE soi_range_list,
      t_data       TYPE soi_generic_table,
      s_ranges     TYPE soi_range_item,
      s_data       TYPE soi_generic_item,
      v_doc_name   TYPE char256,
      v_changed    TYPE int4,
      v_action     TYPE int4,
      v_columns    TYPE i VALUE 35,
      v_subrc      TYPE sy-subrc,
      v_rows       TYPE i VALUE 15536.
*      v_rows       TYPE i VALUE 65536.
