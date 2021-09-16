************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRR_MONITOR_TOP                        *
* Tcode             :  ZABS_TRN_IRRMON                                 *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  01.24.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Monitor global data declaration and  *
*                      Selection Screen                                *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Types declaration
TYPES : BEGIN OF ty_shift,
          vornr TYPE vornr,
          ltxa1 TYPE ltxa1,
        END OF ty_shift,

        BEGIN OF ty_eqtyp,
          equnr    TYPE /agri/glequnr,
          tplnr_fl TYPE /agri/gltplnr_fl,
          irtyp    TYPE /agri/fmirtyp,
        END OF ty_eqtyp,
        tty_shift TYPE TABLE OF ty_shift,

        BEGIN OF ty_ordhdr,
          ordno TYPE /agri/fmfpnum,
          werks TYPE werks_d,
          equnr TYPE /agri/glequnr,
          datab TYPE datab,
          datbi TYPE datbi,
        END OF ty_ordhdr,
        tty_ordhdr TYPE TABLE OF ty_ordhdr,

        BEGIN OF ly_irhdr,
          equnr TYPE /agri/glequnr,
          irtyp TYPE /agri/fmirtyp,
          txtgr TYPE txtgr,
          descr TYPE /agri/gdescr_40,
        END OF ly_irhdr,

        BEGIN OF ty_ordcnf,
          ordno    TYPE /agri/fmfpnum,
          contr    TYPE /irm/gcontr,
          vornr    TYPE vornr,
          cdate    TYPE zabs_del_cdate,
          idate    TYPE zabs_del_idate,
          fdate    TYPE zabs_del_fdate,
          ihour    TYPE zabs_del_ihour,
          fhour    TYPE zabs_del_fhour,
          tot_iqty TYPE zabs_del_irrqty,
        END OF ty_ordcnf,
        tty_ordcnf TYPE TABLE OF ty_ordcnf,

        BEGIN OF ty_zfmacitm,
          acnum    TYPE zfmacnum,
          acpos    TYPE zfmacpos,
          tplnr_fl TYPE /agri/gltplnr_fl,
          iwerk    TYPE werks_d,
          contr    TYPE /agri/gcontr,
          cmnum    TYPE /agri/glcmnum,
          season   TYPE /agri/gl_season,
          astat    TYPE /agri/glastat,
          aarea    TYPE /agri/glaarea,
          ajahr    TYPE ajahr,
          actyp    TYPE zfmactyp,
          acdes    TYPE zfmacdes,
          werks    TYPE werks_d,
          datab    TYPE zfmdatav,
          datbi    TYPE zfmdatvi,
        END OF ty_zfmacitm,
        tty_zfmacitm TYPE TABLE OF ty_zfmacitm,

        BEGIN OF ty_tplnr,
          equnr     TYPE /agri/glequnr,
          tplnr_fl  TYPE /agri/gltplnr_fl,
          zzcshift1 TYPE zabs_del_cshift1,
          zzcshift2 TYPE zabs_del_cshift2,
          zzcshift3 TYPE zabs_del_cshift3,
          zzcshift4 TYPE zabs_del_cshift4,
          zzcshift5 TYPE zabs_del_cshift5,
          zzcshift6 TYPE zabs_del_cshift6,
        END OF ty_tplnr,
        tty_tplnr TYPE TABLE OF ty_tplnr,

        BEGIN OF ty_fmirhdr,
          equnr    TYPE /agri/glequnr,
          irtyp    TYPE /agri/fmirtyp,
          zzrefcty TYPE zabs_del_refcty,
        END OF ty_fmirhdr,
        tty_fmirhdr TYPE TABLE OF ty_fmirhdr,

        BEGIN OF ty_descr,
          equnr TYPE /agri/glequnr,
          descr TYPE /agri/gldescr,
        END OF ty_descr,
        tty_descr TYPE TABLE OF ty_descr,

        BEGIN OF ty_irrmon,
          ordno       TYPE /agri/fmfpnum,
          werks       TYPE werks_d,
          equnr       TYPE /agri/glequnr,
          irrdate     TYPE zabs_del_irrdate,
          shift       TYPE vornr,
          irr_mm      TYPE zabs_del_irrmm,
          hyd_bal     TYPE zabs_del_hbal,
          acum_hydbal TYPE zabs_del_ahbal,
        END OF ty_irrmon,
        tty_irrmon TYPE STANDARD TABLE OF zabst_irrmon INITIAL SIZE 0,

        BEGIN OF ty_glmdhdr,
          mdocm    TYPE /agri/glmdocm,
          tplnr_fl TYPE /agri/gltplnr_fl,
          equnr    TYPE /agri/glequnr,
          mdate    TYPE /agri/glmdate,
          mtime    TYPE imrc_itime,
          class    TYPE /agri/gatgrp,
        END OF ty_glmdhdr,
        tty_glmdhdr TYPE TABLE OF ty_glmdhdr,

        BEGIN OF ty_glmdatv,
          mdocm TYPE /agri/glmdocm,
          atzhl TYPE atzhl,
          atwrt TYPE atwrt,
          atflv TYPE atflv,
          atinn TYPE atinn,
          atnam TYPE atnam,
        END OF ty_glmdatv,
        tty_glmdatv TYPE TABLE OF ty_glmdatv,

        BEGIN OF ty_glflot,
          tplnr_fl TYPE /agri/gltplnr_fl,
          garea    TYPE /agri/glgarea,
          strno    TYPE /agri/glstrno,
        END OF ty_glflot,
        tty_glflot TYPE TABLE OF ty_glflot,

        BEGIN OF ty_glflatv,
          tplnr_fl TYPE /agri/gltplnr_fl,
          atwrt    TYPE atwrt,
          atflv    TYPE atflv,
          atinn    TYPE atinn,
        END OF ty_glflatv,
        tty_glflatv TYPE TABLE OF ty_glflatv,
        tty_final   TYPE TABLE OF zabs_str_irr_monitor_fcat,

        BEGIN OF ty_equp_tplnr,
          equnr    TYPE /agri/glequnr,
          tplnr_fl TYPE /agri/gltplnr_fl,
        END OF ty_equp_tplnr,
        tty_equp_tplnr TYPE TABLE OF ty_equp_tplnr,

        BEGIN OF ty_atval_desc,
          atinn TYPE atinn,
          atwrt TYPE atwrt,
          atwtb TYPE atwtb,
        END OF ty_atval_desc,
        tty_atval_desc TYPE TABLE OF ty_atval_desc.

TABLES: /agri/s_fmirhdr,
        zabst_irrmon.

*--Constant declarations
CONSTANTS: c_irr_monitor_0100_cc(30) TYPE c VALUE 'IRR_MONITOR_0100_CC',
           c_shift1                  TYPE zabs_del_shift VALUE '0010',
           c_shift2                  TYPE zabs_del_shift VALUE '0020',
           c_shift3                  TYPE zabs_del_shift VALUE '0030',
           c_shift4                  TYPE zabs_del_shift VALUE '0040',
           c_shift5                  TYPE zabs_del_shift VALUE '0050',
           c_shift6                  TYPE zabs_del_shift VALUE '0060'.

*--Global table declarations
DATA: gt_final      TYPE tty_final,
      gt_irhdr      TYPE STANDARD TABLE OF ly_irhdr INITIAL SIZE 0,
      gt_shift      TYPE tty_shift,
      gt_atval_desc TYPE tty_atval_desc,
      gv_new_shift  TYPE abap_bool,
      gv_new_equnr  TYPE abap_bool,
      fcode         TYPE sy-ucomm,
      ok_code       TYPE sy-ucomm.

DATA: o_alv TYPE REF TO cl_gui_alv_grid.

DEFINE popup_to_confirm.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = &1
      text_question         = &2
      display_cancel_button = &3
    IMPORTING
      answer                = &4
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

END-OF-DEFINITION.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_werks TYPE werks_d.
SELECT-OPTIONS: so_irr   FOR /agri/s_fmirhdr-equnr,
                so_shift FOR zabst_irrmon-shift NO-EXTENSION,
                so_date  FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:
      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS. "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION .

  METHOD handle_toolbar.

    DATA: lwa_toolbar  TYPE stb_button.

    CLEAR lwa_toolbar.
    MOVE 3 TO lwa_toolbar-butn_type.
    APPEND lwa_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
*-------------------------------------------------------------------
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'SAVE'.
        PERFORM fcode_save.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command

ENDCLASS. "lcl_event_handler IMPLEMENTATION

DATA: gr_event_handler TYPE REF TO lcl_event_handler.
