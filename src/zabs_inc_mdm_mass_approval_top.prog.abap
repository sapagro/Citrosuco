************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_INC_MD_MASS_APPROVALS_TOP                      *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Rapheal                                             *
* Created on     : 11.06.2019                                          *
* TR             : C4DK903886                                          *
* Version        : 001                                                 *
* Description    : Provide the list of measurement documents for mass  *
*                  approvals based the user who executes this program  *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

****Includes
INCLUDE: /irm/gprolog_macros,
         /irm/global_macros,
         /agri/glprolog_macros,
         /irm/global_constants,
         /agri/abgl_constants.

*&---------------------------------------------------------------------*
*&                     TABLES
*&---------------------------------------------------------------------*
TABLES: sscrfields,
        /agri/glflot,
        /agri/glmdhdr,
        lfa1.

*&---------------------------------------------------------------------*
*&                      TYPE-POOLS
*&---------------------------------------------------------------------*
TYPE-POOLS: icon.

*&---------------------------------------------------------------------*
*&                      TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_fname,
         fieldname(30) TYPE c,
         atinn         TYPE atinn,
         atnam         TYPE atnam,
       END OF ty_fname,
       tty_fname TYPE STANDARD TABLE OF ty_fname.

TYPES : BEGIN OF ty_cabn,
          atinn TYPE atinn,
          atnam TYPE atnam,
          atbez TYPE atbez,
        END OF ty_cabn,
        tty_cabn TYPE STANDARD TABLE OF ty_cabn.

TYPES : BEGIN OF ty_f4_region,
          atwrt TYPE atwrt,
          atwtb TYPE atwtb,
        END OF ty_f4_region.

TYPES : BEGIN OF ty_mdhdr,
          mdocm      TYPE /agri/glmdocm,
          mdtyp      TYPE /agri/glmdtyp,
          tplnr_fl   TYPE /agri/gltplnr_fl,
          tplnr_out  TYPE /agri/gltplnr_fl,
          contr      TYPE /agri/gcontr,
          mpgrp      TYPE /agri/glmpgrp,
          kfrst      TYPE /agri/glkfrst,
          stsma      TYPE j_stsma,
          objnr      TYPE j_objnr,
          ustat      TYPE /agri/gustat,
          imovel     TYPE telf0,
          imovel_std TYPE /agri/gltplnr_fl,
          talhao     TYPE zabs_del_talhao,
          lifnr      TYPE lifnr,
          ymatnr     TYPE /agri/glymatnr,
          safra      TYPE gjahr,
        END OF ty_mdhdr,

        BEGIN OF ty_doctos,
          imovel      TYPE telf0,
          ustat       TYPE /agri/gustat,
          est_inicial TYPE zabs_del_estini,
        END OF ty_doctos,

        BEGIN OF ty_gsfstp,
          stsma TYPE j_stsma,
          stsfl TYPE /agri/gstsfl,
          flstp TYPE /agri/gsfflstp,
          deflt TYPE /agri/gsfstrt,
          sstat TYPE /agri/gsfsstat,
        END OF ty_gsfstp,

        BEGIN OF ty_msg_out,
          mdocm TYPE /agri/glmdocm,
        END OF ty_msg_out.

CLASS: lcl_log_handler DEFINITION DEFERRED.

*--Global data declarations
DATA : gt_fcat    TYPE lvc_t_fcat,
       gt_mdhdr   TYPE TABLE OF ty_mdhdr INITIAL SIZE 0,
       gt_doctos  TYPE STANDARD TABLE OF ty_doctos INITIAL SIZE 0,
       gt_gsfstp  TYPE TABLE OF ty_gsfstp INITIAL SIZE 0,
       gt_gsfocm  TYPE TABLE OF /agri/tgsfocm INITIAL SIZE 0,
       gt_tj30    TYPE TABLE OF tj30 INITIAL SIZE 0,
       gt_sort    TYPE lvc_t_sort,
       gr_stsma   TYPE RANGE OF j_stsma,
       gt_output  TYPE STANDARD TABLE OF zabs_str_mdm_mass_approvals INITIAL SIZE 0,
       gt_copy    LIKE gt_output,
       gt_aux     LIKE gt_output,

*--Variable declaration
       gv_stsma   TYPE j_stsma,
       gv_toolbar TYPE abap_bool,
       gv_safra   TYPE gjahr,

*--Object declaration
       gobj_alv   TYPE REF TO /agri/cl_gui_alv_grid,
       gobj_cont  TYPE REF TO cl_gui_custom_container,

*--Screen attribute declarations
       fcode      TYPE sy-ucomm,
       ok_code    TYPE sy-ucomm.

DATA: BEGIN OF gs_variables,
        refresh_items_grid,
        initiator          TYPE /irm/gdescr,
      END OF gs_variables,
      ref_log_handler TYPE REF TO lcl_log_handler.

**--Constants
CONSTANTS: BEGIN OF c_object,
             bor              LIKE /agri/tgabo-object VALUE '/AGRI/GLMD',
             log              LIKE balobj-object      VALUE '/AGRI/GLMD',
             change_documents TYPE cdobjectcl         VALUE '/AGRI/GLMD',
             esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/GLMD',
             text_object      TYPE thead-tdobject     VALUE '/AGRI/GLMD',
           END OF c_object,

           BEGIN OF c_status,
             approved TYPE /agri/gustat VALUE 'E0005',
             rejected TYPE /agri/gustat VALUE 'E0006',
           END OF c_status,

           BEGIN OF c_log_subobject,
             save TYPE balsubobj VALUE 'SAVE',
           END OF c_log_subobject,

           BEGIN OF c_proposal,
             bukrs TYPE bukrs  VALUE 'FAI',
             bstyp TYPE ebstyp VALUE 'K',
             bsart TYPE esart  VALUE 'ZEST',
           END OF c_proposal,

           BEGIN OF c_sort,
             fornecedor TYPE fieldname VALUE 'LIFNR',
*-- BOC-T_T.KONNO-04.19.21
             forn_desc  TYPE fieldname VALUE 'LIFNRDESC',
*-- EOC-T_T.KONNO-04.19.21
             imovel     TYPE fieldname VALUE 'IMOVEL',
             material   TYPE fieldname VALUE 'YMATNR',
             talhao     TYPE fieldname VALUE 'TALHAO',
             florada    TYPE fieldname VALUE 'FLORADA',
             docto      TYPE fieldname VALUE 'MDOCM',
             motivo     TYPE fieldname VALUE 'MOT_EST',
             status     TYPE fieldname VALUE 'KFRST',
             status_sf  TYPE fieldname VALUE 'ETAPA_ATUAL',
             grupo      TYPE fieldname VALUE 'MPRGRP',
             total      TYPE fieldname VALUE 'TOTAL',
             var_perc   TYPE fieldname VALUE 'VAR_PERC_ATUAL',
             var_qtd    TYPE fieldname VALUE 'VAR_QTD_ATUAL',
           END OF c_sort.

*--Field-symbol declarations
FIELD-SYMBOLS : <gfs_dyn_smdm> TYPE any,
                <gfs_dyn_tmdm> TYPE STANDARD TABLE,
                <gfs_tcopy>    TYPE STANDARD TABLE,
                <gfs_scopy>    TYPE any,
                <gfs_taux>     TYPE STANDARD TABLE,
                <gfs_saux>     TYPE any,
                <gfs_value>    TYPE any.

*----------------------------------------------------------------------*
*       CLASS lcl_log_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_log_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: on_log_display_profile
      FOR EVENT log_display_profile
                  OF /irm/cl_process_log_manager
      IMPORTING eref_event_data.

ENDCLASS.                    "lcl_log_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_log_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_log_handler IMPLEMENTATION.

  METHOD on_log_display_profile.
    DATA: ls_fcat TYPE bal_s_fcat.

    READ TABLE eref_event_data->ms_display_profile-mess_fcat
    WITH KEY ref_field = 'MDOCM' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_fcat-ref_table  = 'ZABS_STR_GLMD_CONTEXT'.
      ls_fcat-ref_field  = 'MDOCM'.
      ls_fcat-outputlen  = 12.
*      ls_fcat-coltext    = TEXT-005.
      ls_fcat-coltext    = TEXT-011.
      ls_fcat-col_pos    = 1.
      APPEND ls_fcat TO eref_event_data->ms_display_profile-mess_fcat.
    ENDIF.

  ENDMETHOD.                    "on_log_display_profile
ENDCLASS.                    "lcl_log_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&---------------------------------------------------------------------*
*SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_mdtyp  TYPE /agri/glmdhdr-mdtyp,
            p_mpgrp  TYPE /agri/glmdhdr-mpgrp,
            p_werks  TYPE t001w-werks,
            p_year   TYPE gjahr MATCHCODE OBJECT rscalyear OBLIGATORY,
            p_compra TYPE zabs_str_mdm_mass_approvals-comprador_imovel.

SELECT-OPTIONS: so_mdocm FOR /agri/glmdhdr-mdocm,
                so_imov  FOR /agri/glmdhdr-tplnr_fl,
                s_region FOR /agri/glflot-beber1,
                s_lifnr  FOR lfa1-lifnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-008.
PARAMETERS: rb_detfr RADIOBUTTON GROUP rb01 USER-COMMAND tipo_alv DEFAULT 'X',
            rb_confr RADIOBUTTON GROUP rb01.
PARAMETERS p_disp AS CHECKBOX DEFAULT '' MODIF ID t6.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-012.
*PARAMETERS: rb_n_all RADIOBUTTON GROUP rb02 MODIF ID t5 USER-COMMAND exib_imov DEFAULT 'X',
PARAMETERS: rb_e002  RADIOBUTTON GROUP rb02 MODIF ID t5 USER-COMMAND exib_imov DEFAULT 'X',
            rb_e003  RADIOBUTTON GROUP rb02 MODIF ID t5,
            rb_all   RADIOBUTTON GROUP rb02 MODIF ID t5.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-009.
PARAMETERS p_alcada AS CHECKBOX MODIF ID t3.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-010.
PARAMETERS p_layout AS CHECKBOX MODIF ID t4.
SELECTION-SCREEN END OF BLOCK b4.
