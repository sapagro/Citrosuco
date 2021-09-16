*&---------------------------------------------------------------------*
*& Include ZABS_INC_DEL_GIS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include ZABS_INC_DEL_GIS_TOP
*&---------------------------------------------------------------------*

INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_macros.
INCLUDE /agri/global_constants.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/glpg_upload_process_top.
INCLUDE /agri/glpg_upload_process_cls.
INCLUDE /agri/glpg_upload_process_f0a.
INCLUDE /agri/glpg_upload_process_f0c.
INCLUDE /agri/glpg_upload_process_f0f.
INCLUDE /agri/glpg_upload_process_f0i.
INCLUDE /agri/glpg_upload_process_f0m.
INCLUDE /agri/glpg_upload_process_f0t.

*&---------------------------------------------------------------------*
*&    TABLES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&    DATA
*&---------------------------------------------------------------------*
DATA: gt_guid TYPE STANDARD TABLE OF zabs_map_guid INITIAL SIZE 0.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
*-- Parâmetros de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_date TYPE zabs_map_guid-data DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.
