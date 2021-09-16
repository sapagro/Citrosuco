*&---------------------------------------------------------------------*
*&  Include           ZLFMACMNCON
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZLFMACMCON
*&---------------------------------------------------------------------*
*--Program Names
CONSTANTS: c_program_acm   TYPE sy-repid VALUE 'SAPLZFMACM'.

*--Object
CONSTANTS: BEGIN OF c_object,
             bor              LIKE /agri/tgabo-object      VALUE 'ZFMAC',
             log              LIKE balobj-object           VALUE 'ZFMAC',
             change_documents TYPE cdobjectcl              VALUE 'ZFMAC',
             esh_object       TYPE /agri/geshobjtp         VALUE 'ZFMAC',
             text_object      TYPE thead-tdobject          VALUE 'ZFMAC',
           END OF c_object.

*--Log Sub-object
CONSTANTS : BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
            END OF c_log_subobject.

*--Screens
CONSTANTS: BEGIN OF c_screen,
             main_screen          TYPE sy-dynnr VALUE '0100',
             create_mass_dialog   TYPE sy-dynnr VALUE '0101',
             create_dialog        TYPE sy-dynnr VALUE '0102',
             quick_info           TYPE sy-dynnr VALUE '0200',
             create_crop_area     TYPE sy-dynnr VALUE '0201',
             hierarchy_items      TYPE sy-dynnr VALUE '0202',
             volumen_calda        TYPE sy-dynnr VALUE '0203',
             mass_quick_info      TYPE sy-dynnr VALUE '0205',
             details              TYPE sy-dynnr VALUE '0300',
             items                TYPE sy-dynnr VALUE '0301',
             texts                TYPE sy-dynnr VALUE '0302',
             admin                TYPE sy-dynnr VALUE '0303',
             mass_details         TYPE sy-dynnr VALUE '0304',
             list                 TYPE sy-dynnr VALUE '0305',
             latest_measurements  TYPE sy-dynnr VALUE '0306',
             measurements_display TYPE sy-dynnr VALUE '0307',
             update_mass          TYPE sy-dynnr VALUE '0310',
             ATTRIBUTES           TYPE sy-dynnr VALUE '0311',
             dummy                TYPE sy-dynnr VALUE '0999',
           END OF c_screen.

CONSTANTS : BEGIN OF c_fcode,
              wl_search             TYPE sy-ucomm VALUE 'SRCH',
              wl_search_more        TYPE sy-ucomm VALUE 'SRCH_MORE',
              worklist_hotspot      TYPE sy-ucomm VALUE 'WLHC',
              save                  TYPE sy-ucomm VALUE 'SAVE',
              create                TYPE sy-ucomm VALUE 'CREA',
              display_change_toggle TYPE sy-ucomm VALUE 'DICH',
              exit                  TYPE sy-ucomm VALUE 'EXIT',
              back                  TYPE sy-ucomm VALUE 'BACK',
              cancel                TYPE sy-ucomm VALUE 'CANC',
              update_mass           TYPE sy-ucomm VALUE 'UPMS',
              copy                  TYPE sy-ucomm VALUE 'COPY',
              reno                  TYPE sy-ucomm VALUE 'RENO',
              delete                TYPE sy-ucomm VALUE 'DELE',
              continue              TYPE sy-ucomm VALUE 'CONT',
              continue_mass         TYPE sy-ucomm VALUE 'CONM',
              volumen_cald          TYPE sy-ucomm VALUE 'VLCL',
              enter                 TYPE sy-ucomm VALUE 'ENTR',
              select                TYPE sy-ucomm VALUE 'SELE',
              where_used_list       TYPE sy-ucomm VALUE 'WUSD',
              tab_items             TYPE sy-ucomm VALUE 'TAB_ITEM',
              tab_vlcld             TYPE sy-ucomm VALUE 'TAB_VLCL',
              tab_texts             TYPE sy-ucomm VALUE 'TAB_TEXT',
              tab_admin             TYPE sy-ucomm VALUE 'TAB_ADMN',
              tab_notes             TYPE sy-ucomm VALUE 'TAB_NOTES',
              TAB_ATTRIBUTES        TYPE SY-UCOMM VALUE 'TAB_ATRBU',
              expand_hdr            TYPE sy-ucomm VALUE 'EXPH',
              compress_hdr          TYPE sy-ucomm VALUE 'CMPH',
              refresh_objects       TYPE sy-ucomm VALUE 'RFSH',
              reset_attr_values     TYPE sy-ucomm VALUE 'RSAV',
              delete_rows           TYPE sy-ucomm VALUE 'DLRW',
              insert_rows           TYPE sy-ucomm VALUE 'INRW',
              ac_set_values         TYPE sy-ucomm VALUE 'SETVAL',
              vol_calda_recalcular  TYPE sy-ucomm VALUE 'VOLR',
              attr_val_descr_switch TYPE sy-ucomm VALUE 'ZFMRC_DCVL',
****User Functions
              user_function1        TYPE syucomm VALUE 'USF1',
              user_function2        TYPE syucomm VALUE 'USF2',
              user_function3        TYPE syucomm VALUE 'USF3',
              user_function4        TYPE syucomm VALUE 'USF4',
              user_function5        TYPE syucomm VALUE 'USF5',
            END OF c_fcode.

****Grid Variant Handles
CONSTANTS: BEGIN OF c_variant_handle,
             worklist      TYPE slis_handl VALUE 'WKLT',
             items         TYPE slis_handl VALUE 'ITMS',
             volumen_calda TYPE slis_handl VALUE 'VLCL',
           END OF c_variant_handle.

*--Structures
CONSTANTS: BEGIN OF c_structure_name,
             work_list_header TYPE dd02l-tabname
                                 VALUE 'ZSC_FMACHDR_WL',
             items            TYPE dd02l-tabname
                                 VALUE 'ZSC_FMACITM_FCAT',
             vlcl             TYPE dd02l-tabname
                                 VALUE 'ZSC_FMACVLCL_FCAT',
           END OF c_structure_name.

****Screen Groups
CONSTANTS : BEGIN OF c_screen_group,
              display_only(3)  VALUE 'DIO',
              off(3)           VALUE 'OFF',
              on(2)            VALUE 'ON',
              data_required(3) VALUE 'DTR',
              crop_number(3)   VALUE 'CNM',
              copy(3)          VALUE 'CPY',
            END OF c_screen_group.

CONSTANTS: BEGIN OF c_tcode,
             acm_wb   TYPE sy-tcode VALUE 'ZFMACM',
             acm_mass TYPE sy-tcode VALUE 'ZFMRC23',
           END OF c_tcode.

CONSTANTS: BEGIN OF c_int_data_type,
             char    VALUE 'C',
             integer VALUE 'I',
             date    VALUE 'D',
             time    VALUE 'T',
             packed  VALUE 'P',
             numeric VALUE 'N'.
CONSTANTS: END OF c_int_data_type.

CONSTANTS: BEGIN OF c_data_type,
             char     TYPE datatype_d VALUE 'CHAR',
             date     TYPE datatype_d VALUE 'DATS',
             unit     TYPE datatype_d VALUE 'UNIT',
             currency TYPE datatype_d VALUE 'CURR',
             time     TYPE datatype_d VALUE 'TIMS'.
CONSTANTS: END OF c_data_type.

CONSTANTS: BEGIN OF c_fldgrp,
             attributes(4) VALUE 'AT',
           END OF c_fldgrp.

CONSTANTS: BEGIN OF c_program_constant,
             tank_capasity TYPE int4 VALUE 2000,
           END OF c_program_constant.

CONSTANTS: BEGIN OF c_unit_of_measurement,
             unit    TYPE meins VALUE 'UN',
             hectare TYPE meins VALUE 'HAR',
           END OF c_unit_of_measurement.
