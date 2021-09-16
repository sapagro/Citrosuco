************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_INC_APROVA_DOC_MED_TOP                         *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Raphael                                             *
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
TABLES: /agri/glmdhdr.

*&---------------------------------------------------------------------*
*&                                TYPES
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

        tty_cabn TYPE STANDARD TABLE OF ty_cabn,

        BEGIN OF ty_glmdhdr,
          mdocm    TYPE /agri/glmdocm,
          mdtyp    TYPE /agri/glmdtyp,
          aslvl    TYPE /agri/glaslvl,
          tplnr_fl TYPE /agri/gltplnr_fl,
          contr    TYPE /agri/gcontr,
          cmnum    TYPE /agri/glcmnum,
          equnr    TYPE /agri/glequnr,
          mpgrp    TYPE /agri/glmpgrp,
          mdate    TYPE /agri/glmdate,
          mtime    TYPE imrc_itime,
          stsma    TYPE j_stsma,
          ustat    TYPE /agri/gustat,
          atzhl    TYPE atzhl,
          atwrt    TYPE atwrt,
          atflv    TYPE atflv,
          atinn    TYPE atinn,
          atnam    TYPE atnam,
          atwtb    TYPE atwtb,
        END OF ty_glmdhdr.

TYPES : BEGIN OF ty_cabn_new,
          atinn TYPE atinn,
          adzhl	TYPE adzhl,
          atnam	TYPE atnam,
          atidn	TYPE atidn,
          atfor	TYPE atfor,
          anzst	TYPE anzst,
          anzdz	TYPE anzdz,
          atvor	TYPE atvor,
          atsch	TYPE atsch,
          atbez TYPE atbez,
        END OF ty_cabn_new,
        tty_cabn_new TYPE STANDARD TABLE OF ty_cabn_new.

TYPES : BEGIN OF ty_f4_region,
          atwrt TYPE atwrt,
          atwtb TYPE atwtb,
        END OF ty_f4_region.

TYPES : BEGIN OF ty_mdhdr,
          mdocm    TYPE /agri/glmdocm,
          mdtyp    TYPE /agri/glmdtyp,
          tplnr_fl TYPE /agri/gltplnr_fl,
          contr    TYPE /agri/gcontr,
          mpgrp    TYPE /agri/glmpgrp,
          stsma    TYPE j_stsma,
          objnr    TYPE j_objnr,
          ustat    TYPE /agri/gustat,
        END OF ty_mdhdr,

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
DATA : gt_fcat      TYPE lvc_t_fcat,
       gt_mdhdr     TYPE TABLE OF ty_glmdhdr,
*-- BOC T_T.KONNO-07.12.21
       gt_mdhdr_mem TYPE STANDARD TABLE OF /agri/glmdhdr INITIAL SIZE 0,
*-- EOC T_T.KONNO-07.12.21
       gt_gsfstp    TYPE TABLE OF ty_gsfstp,
       gt_gsfocm    TYPE TABLE OF /agri/tgsfocm,
       gt_tj30      TYPE TABLE OF tj30,

*--Variable declaration
       gv_stsma     TYPE j_stsma,

*--Object declaration
       gobj_alv     TYPE REF TO /agri/cl_gui_alv_grid,
       gobj_cont    TYPE REF TO cl_gui_custom_container,

*--Screen attribute declarations
       fcode        TYPE sy-ucomm,
       ok_code      TYPE sy-ucomm.

DATA: BEGIN OF gs_variables,
        refresh_items_grid,
        initiator          TYPE /irm/gdescr,
      END OF gs_variables,
      ref_log_handler TYPE REF TO lcl_log_handler.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : BEGIN OF c_log_subobject,
              save TYPE balsubobj VALUE 'SAVE',
            END OF c_log_subobject,

            BEGIN OF c_object,
              log LIKE balobj-object  VALUE '/AGRI/FMFP',
            END OF c_object,

            BEGIN OF c_crop_season,
              formacao   TYPE /agri/glvaria VALUE 'FORMAÇÃO',
              manutencao TYPE /agri/glvaria VALUE 'MANUT&COLHEITA',
            END OF c_crop_season,

            BEGIN OF c_crop_process,
              formacao    TYPE /agri/glcpros VALUE 'FORMAÇÃO',
              manutencao  TYPE /agri/glcpros VALUE 'MANUTEÇÃO',
              colheita    TYPE /agri/glcpros VALUE 'COLHEITA',
              implantacao TYPE /agri/glcpros VALUE 'IMPLANT',
              close_all   TYPE /agri/glcpros VALUE abap_true,
            END OF c_crop_process.

*--Field-symbol declarations
FIELD-SYMBOLS : <gfs_dyn_smdm> TYPE any,
                <gfs_dyn_tmdm> TYPE STANDARD TABLE,
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*-- BOC T_T.KONNO-07.12.21
*PARAMETERS: p_mdtyp  TYPE /agri/glmdhdr-mdtyp NO-DISPLAY DEFAULT 'ZPTA',
PARAMETERS: p_mdtyp  TYPE /agri/glmdhdr-mdtyp NO-DISPLAY DEFAULT 'ZTYP',
*-- EOC T_T.KONNO-07.12.21
            p_mpgrp  TYPE /agri/glmdhdr-mpgrp NO-DISPLAY DEFAULT 'FAZ_PREVISAO' MODIF ID id1,
            p_werks  TYPE iwerk,
            p_region TYPE atwrt,
            p_etapa  TYPE /agri/tgsfstp-flstp OBLIGATORY.

SELECT-OPTIONS: so_mdocm FOR /agri/glmdhdr-mdocm.
SELECTION-SCREEN END OF BLOCK b1.
