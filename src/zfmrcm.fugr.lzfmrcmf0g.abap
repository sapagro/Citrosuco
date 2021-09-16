*&---------------------------------------------------------------------*
*&      Form  GRID_DATA_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_data_get   USING lref_grid   TYPE REF TO /agri/cl_gui_alv_grid
                     CHANGING lt_sort     TYPE lvc_t_sort
                              lt_fcat     TYPE lvc_t_fcat
                              ls_row_no   TYPE lvc_s_roid
                              ls_col_no   TYPE lvc_s_col
                              ls_row_info TYPE lvc_s_row
                              ls_col_info TYPE lvc_s_col
                              ls_layout   TYPE lvc_s_layo.
  CHECK NOT lref_grid IS INITIAL.

  CALL METHOD lref_grid->get_sort_criteria
    IMPORTING
      et_sort = lt_sort[].

  CALL METHOD lref_grid->get_scroll_info_via_id
    IMPORTING
      es_row_no   = ls_row_no
      es_row_info = ls_row_info
      es_col_info = ls_col_info.

  CALL METHOD lref_grid->get_current_cell
    IMPORTING
      es_row_no = ls_row_no
      es_col_id = ls_col_no.

  CALL METHOD lref_grid->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat[].

  CALL METHOD lref_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  ls_layout-cwidth_opt = c_true.

ENDFORM.                    " GRID_DATA_GET

FORM grid_data_get_vlc   USING lref_grid   TYPE REF TO /agri/cl_gui_alv_grid
                     CHANGING lt_sort     TYPE lvc_t_sort
                              lt_fcat     TYPE lvc_t_fcat
                              ls_row_no   TYPE lvc_s_roid
                              ls_col_no   TYPE lvc_s_col
                              ls_row_info TYPE lvc_s_row
                              ls_col_info TYPE lvc_s_col
                              ls_layout   TYPE lvc_s_layo.
  CHECK NOT lref_grid IS INITIAL.

  CALL METHOD lref_grid->get_sort_criteria
    IMPORTING
      et_sort = lt_sort[].

  CALL METHOD lref_grid->get_scroll_info_via_id
    IMPORTING
      es_row_no   = ls_row_no
      es_row_info = ls_row_info
      es_col_info = ls_col_info.

  CALL METHOD lref_grid->get_current_cell
    IMPORTING
      es_row_no = ls_row_no
      es_col_id = ls_col_no.

  CALL METHOD lref_grid->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat[].

  CALL METHOD lref_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  ls_layout-cwidth_opt = c_true.

ENDFORM.                    " GRID_DATA_GET

*&---------------------------------------------------------------------*
*&      Form  GRID_DATA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_data_set  USING    lref_grid   TYPE REF TO /agri/cl_gui_alv_grid
                    CHANGING lt_outtab   TYPE table
                      lt_fcat     TYPE lvc_t_fcat
                      lt_sort     TYPE lvc_t_sort
                      ls_row_no   TYPE lvc_s_roid
                      ls_col_no   TYPE lvc_s_col
                      ls_row_info TYPE lvc_s_row
                      ls_col_info TYPE lvc_s_col
                      ls_layout   TYPE lvc_s_layo.


  CHECK NOT lref_grid IS INITIAL.

  CALL METHOD lref_grid->frontend_fieldcatalog_set
    EXPORTING
      it_fieldcatalog = lt_fcat[].

  CALL METHOD lref_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  CALL METHOD lref_grid->tables_display_refresh   "Display the modified grid values
    CHANGING
      it_outtab = lt_outtab[].

  CALL METHOD lref_grid->set_scroll_info_via_id
    EXPORTING
      is_row_info = ls_row_info
      is_col_info = ls_col_info.

  CALL METHOD lref_grid->set_current_cell_via_id
    EXPORTING
      is_column_id = ls_col_no
      is_row_no    = ls_row_no.

  CALL METHOD lref_grid->set_sort_criteria
    EXPORTING
      it_sort                   = lt_sort[]
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  ls_layout-cwidth_opt = c_true.  "

ENDFORM.                    " GRID_DATA_SET

*&---------------------------------------------------------------------*
*&      Form  GRID_DATA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_data_set_vlc  USING    lref_grid   TYPE REF TO /agri/cl_gui_alv_grid
                    CHANGING lt_outtab   TYPE table
                      lt_fcat     TYPE lvc_t_fcat
                      lt_sort     TYPE lvc_t_sort
                      ls_row_no   TYPE lvc_s_roid
                      ls_col_no   TYPE lvc_s_col
                      ls_row_info TYPE lvc_s_row
                      ls_col_info TYPE lvc_s_col
                      ls_layout   TYPE lvc_s_layo.


  CHECK NOT lref_grid IS INITIAL.

  CALL METHOD lref_grid->frontend_fieldcatalog_set
    EXPORTING
      it_fieldcatalog = lt_fcat[].

  CALL METHOD lref_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  CALL METHOD lref_grid->tables_display_refresh   "Display the modified grid values
    CHANGING
      it_outtab = lt_outtab[].

  CALL METHOD lref_grid->set_scroll_info_via_id
    EXPORTING
      is_row_info = ls_row_info
      is_col_info = ls_col_info.

  CALL METHOD lref_grid->set_current_cell_via_id
    EXPORTING
      is_column_id = ls_col_no
      is_row_no    = ls_row_no.

  CALL METHOD lref_grid->set_sort_criteria
    EXPORTING
      it_sort                   = lt_sort[]
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  ls_layout-cwidth_opt = c_true.  "

ENDFORM.                    " GRID_DATA_SET
*&---------------------------------------------------------------------*
*& Form GROUP_ROUTING_OBTAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_MAPL
*&---------------------------------------------------------------------*
FORM group_routing_obtain  CHANGING  lt_mapl TYPE tt_mapl.

  SELECT * FROM mapl
             INTO TABLE lt_mapl
                   WHERE matnr = zsc_fmrchdr-matnr
                    AND  werks = zsc_fmrchdr-werks
             AND  PLNTY = 'N'.

ENDFORM.
