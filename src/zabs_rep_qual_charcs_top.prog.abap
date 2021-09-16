*&---------------------------------------------------------------------*
*& Include          ZABS_REP_QUAL_CHARCS_TOP
*&---------------------------------------------------------------------*

*--Table declarations.
TABLES : qals,qapp,/agri/glflot.

*--Types for mic's.
TYPES : BEGIN OF ty_mic,
          prueflos   TYPE qibplosnr,
          operatn    TYPE qibpvornr,
          qibpprobe  TYPE qibpprobe,
          mstr_char  TYPE qmstr_char,
          mean_value TYPE qmean_val,
          char_descr TYPE qmkkurztxt,
          inspchar   TYPE qibpmerknr,
        END OF ty_mic.

*--Types for Plpo.
TYPES : BEGIN OF ty_plpo,
          plnty TYPE plpo-plnty,
          plnnr TYPE plpo-plnnr,
          plnkn TYPE plpo-plnkn,
          zaehl TYPE plpo-zaehl,
          vornr TYPE plpo-vornr,
          steus TYPE plpo-steus,
          werks TYPE plpo-werks,
        END OF ty_plpo.

*--Types for Qapp.
TYPES : BEGIN OF ty_qapp,
          prueflos  TYPE qapp-prueflos,
          vorglfnr  TYPE qapp-vorglfnr,
          ppsortkey TYPE qapp-ppsortkey,
          probenr   TYPE qapp-probenr,
        END OF ty_qapp.

*--Types for Qals.
TYPES : BEGIN OF ty_qals,
          prueflos TYPE qals-prueflos,
          werk     TYPE qals-werk,
          selmatnr TYPE qals-selmatnr,
          aufnr    TYPE qals-aufnr,
        END OF ty_qals.

*--Types for Results operation validation
TYPES : BEGIN OF ty_pruef,
          prueflos TYPE qals-prueflos,
          flag     TYPE flag,
        END OF ty_pruef.

*--Types for fmfphdr.
TYPES : BEGIN OF ty_fmfphdr,
          aufnr    TYPE /agri/fmfphdr-aufnr,
          tplnr_fl TYPE /agri/fmfphdr-tplnr_fl,
          erdat    TYPE erdat,
        END OF ty_fmfphdr.

*--Types for Glflot.
TYPES : BEGIN OF ty_glflot,
          tplnr_fl TYPE /agri/glflot-tplnr_fl,
          bukrs    TYPE /agri/glflot-bukrs,
          iwerk    TYPE /agri/glflot-iwerk,
        END OF ty_glflot.

TYPES: BEGIN OF gty_mapl,
         matnr TYPE	matnr,
         werks TYPE werks_d,
         plnty TYPE plnty,
         plnnr TYPE plnnr,
         plnal TYPE plnal,
         zaehl TYPE	cim_count,
       END OF gty_mapl,

       gtt_mapl TYPE STANDARD TABLE OF gty_mapl.

TYPES: BEGIN OF gty_plpo,
         plnty TYPE	plnty,
         plnnr TYPE plnnr,
         plnkn TYPE plnkn,
         zaehl TYPE cim_count,
         vornr TYPE	vornr,
         steus TYPE steus,
         werks TYPE werks_d,
       END OF gty_plpo,

       gtt_plpo TYPE STANDARD TABLE OF gty_plpo.

**--Types for status.
*TYPES: BEGIN OF ty_status,  "Task order status
*         status TYPE j_txt04,
*       END OF ty_status.

*--Table Type declarations
TYPES : tty_mic     TYPE TABLE OF ty_mic,
        tty_qals    TYPE TABLE OF ty_qals,
        tty_qapp    TYPE TABLE OF ty_qapp,
        tty_fmfphdr TYPE TABLE OF ty_fmfphdr.

*--Global Table Declaration.
DATA : gt_mic_qm01   TYPE TABLE OF ty_mic,
       gt_mic_qm03   TYPE TABLE OF ty_mic,
       gt_mic_n_qm01 TYPE TABLE OF ty_mic,
       gt_mic_n_qm03 TYPE TABLE OF ty_mic,
       gt_glflot     TYPE TABLE OF ty_glflot,
       gt_pruef      TYPE TABLE OF ty_pruef.

*--Global Variables.
DATA : gv_oper_qm01 TYPE vornr,
       gv_oper_qm03 TYPE vornr,
       gt_mapl      TYPE gtt_mapl,
       gt_plpo      TYPE gtt_plpo.

*--Constant declarations
CONSTANTS: c_qual_char_0100_cc(30) TYPE c VALUE 'QUAL_CHAR_0100_CC'.

*--Screen attribute declarations
DATA: fcode   TYPE sy-ucomm,
      ok_code TYPE sy-ucomm.

*--FieldCatalog.
DATA : gt_fcat_qm01 TYPE lvc_t_fcat,
       gt_fcat_qm03 TYPE lvc_t_fcat.

*--Global reference declarations
DATA: gobj_ref_grid_qm01  TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_ref_grid_qm03  TYPE REF TO /agri/cl_gui_alv_grid,
      gobj_ref_cust_cont  TYPE REF TO cl_gui_custom_container,
      gobj_ref_split_cont TYPE REF TO cl_gui_splitter_container,
      gobj_container_qm01 TYPE REF TO cl_gui_container,
      gobj_container_qm03 TYPE REF TO cl_gui_container.

*--Field-Symbols.
FIELD-SYMBOLS : <fs_tqual_char_qm01> TYPE STANDARD TABLE,
                <fs_tqual_char_qm03> TYPE STANDARD TABLE,
                <fs_squal_char_qm01> TYPE any,
                <fs_squal_char_qm03> TYPE any,
                <fs_vqual_char>      TYPE any.

*--Local Class.
CLASS lcl_qual DEFINITION.
  PUBLIC SECTION.
    METHODS: get_qual_charcs_data,
      display_qual_charcs_data.
ENDCLASS.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS handle_hotspot_click                " HOTSPOT_CLICK
          FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
          e_row_id
          e_column_id.

ENDCLASS.                    "cl_event_receiver DEFINITION

*--Global reference declarations
DATA : go_qual TYPE REF TO lcl_qual.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.

PARAMETERS     : p_art    TYPE qals-art DEFAULT '03'.   "Inspection Type
SELECT-OPTIONS : so_datum FOR sy-datum,                 " Date
                 so_matnr FOR qals-matnr,               " Material Number
                 so_tplnr FOR /agri/glflot-tplnr_fl,    " Terrain
                 so_ilot  FOR qals-prueflos.            " Inspection Lot Number

SELECTION-SCREEN : END OF BLOCK b1.


SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-006.
PARAMETERS : r_disp   RADIOBUTTON GROUP rg1 DEFAULT 'X'.
PARAMETERS : r_create RADIOBUTTON GROUP rg1.
SELECTION-SCREEN : END OF BLOCK b2.
*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN OUTPUT
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name EQ 'P_ART'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
