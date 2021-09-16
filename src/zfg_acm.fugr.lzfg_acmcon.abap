*&---------------------------------------------------------------------*
*& Include          LZFG_ACMCON
*&---------------------------------------------------------------------*

*--Program Names
CONSTANTS: c_transac   TYPE sy-repid VALUE '/AGRI/FMAC',
           c_program   TYPE sy-repid VALUE 'SAPLZFG_ACM',
           c_no        TYPE /agri/fm_bill VALUE 'NO'.

CONSTANTS: BEGIN OF c_object,
             bor              LIKE /agri/tgabo-object VALUE '/AGRI/FMAC',
             log              LIKE balobj-object      VALUE '/AGRI/FMAC',
             change_documents TYPE cdobjectcl         VALUE '/AGRI/FMAC',
             esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/FMAC',
             text_object      TYPE thead-tdobject     VALUE '/AGRI/FMAC',
           END OF c_object.

****Log Sub-object
CONSTANTS: BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_fcode,
              wl_search             TYPE sy-ucomm VALUE 'SRCH',
              wl_search_more        TYPE sy-ucomm VALUE 'SRCH_MORE',
              worklist_hotspot      TYPE sy-ucomm VALUE 'WLHC',
              save                  TYPE sy-ucomm VALUE 'SAVE',
              create                TYPE sy-ucomm VALUE 'CREA',
              display_change_toggle TYPE sy-ucomm VALUE 'DICH',
              exit                  TYPE sy-ucomm VALUE 'EXIT',
              back                  TYPE sy-ucomm VALUE 'BACK',
              cancel                TYPE sy-ucomm VALUE 'CANC',
              set_values            TYPE sy-ucomm VALUE 'SETV',
              delete                TYPE sy-ucomm VALUE 'DELE',
              tab_items             TYPE sy-ucomm VALUE 'TAB_ITEMS',
              order_confirm         TYPE sy-ucomm VALUE 'CONF',
              order_distributed     TYPE sy-ucomm VALUE 'DIST',
              order_closed          TYPE sy-ucomm VALUE 'CLOS',
              order_reversed        TYPE sy-ucomm VALUE 'REVE',
              tab_notes             TYPE sy-ucomm VALUE 'TAB_NOTES',
*              tab_admin_data        TYPE sy-ucomm VALUE 'TAB_ADMIN',
              enter                 TYPE sy-ucomm VALUE 'ENTR',
              continue              TYPE sy-ucomm VALUE 'CONT',
              none                  TYPE sy-ucomm VALUE 'NONE',
              crea                  TYPE sy-ucomm VALUE 'CRET',
              copy                  TYPE sy-ucomm VALUE 'COPY',
              change_docs           TYPE sy-ucomm VALUE 'CDOC',
              desc_delete           TYPE sy-ucomm VALUE 'DDEL',
              desc_insert           TYPE sy-ucomm VALUE 'DINS',
              item_delete           TYPE sy-ucomm VALUE 'IDEL',
              item_insert           TYPE sy-ucomm VALUE 'IINS',
              conf_reversed         TYPE sy-ucomm VALUE 'REVC',
              where_used_list       TYPE sy-ucomm VALUE 'WUSD',
              tab_status            TYPE sy-ucomm VALUE 'TAB_STAT',
              tab_postings          TYPE sy-ucomm VALUE 'TAB_POST',
*              tab_assignments       TYPE sy-ucomm VALUE 'TAB_ASGN',
              tab_texts             TYPE sy-ucomm VALUE 'TAB_TEXT',
              tab_additional_data1  TYPE sy-ucomm VALUE 'TAB_ADD1',
              tab_admin             TYPE sy-ucomm VALUE 'TAB_ADMN',
              expand_hdr            TYPE sy-ucomm VALUE 'EXPH',
              compress_hdr          TYPE sy-ucomm VALUE 'CMPH',
*****User Functions
              user_function1        TYPE syucomm VALUE 'USF1',
              user_function2        TYPE syucomm VALUE 'USF2',
              user_function3        TYPE syucomm VALUE 'USF3',
              user_function4        TYPE syucomm VALUE 'USF4',
              user_function5        TYPE syucomm VALUE 'USF5',
           END OF c_fcode.

*--Screens
CONSTANTS: BEGIN OF c_screen,
            create               TYPE sy-dynnr VALUE '0010',
            main_screen          TYPE sy-dynnr VALUE '0100',
            overview             TYPE sy-dynnr VALUE '0101',
            header               TYPE sy-dynnr VALUE '0200',
            create_accom         TYPE sy-dynnr VALUE '0201',
            multi_lang_desc      TYPE sy-dynnr VALUE '0203',
            header_compressed    TYPE sy-dynnr VALUE '0204',
            items                TYPE sy-dynnr VALUE '0300',
            ac_items             TYPE sy-dynnr VALUE '0301',
            ac_postings          TYPE sy-dynnr VALUE '0302',
            ac_status            TYPE sy-dynnr VALUE '0304',
            ac_texts             TYPE sy-dynnr VALUE '0305',
            ac_admin             TYPE sy-dynnr VALUE '0306',
            user_additional_data TYPE sy-dynnr VALUE '0307',
            dummy                TYPE sy-dynnr VALUE '0999',

           END OF c_screen.

*--Structures
CONSTANTS: BEGIN OF c_structure_name,
            worklist_header         TYPE dd02l-tabname
                                    VALUE '/AGRI/S_FMACHDR_WL',
            details                 TYPE dd02l-tabname
                                    VALUE '/AGRI/S_FMOC_DETAILS_FCAT',
            ac_multi_lang_desc      TYPE dd02l-tabname
                                    VALUE '/AGRI/S_FMACHDRT_LAYOUT_FCAT',
            ac_items                TYPE dd02l-tabname
                                    VALUE '/AGRI/S_FMACITM_LAYOUT',
           END OF c_structure_name.

****Grid Variant Handles
CONSTANTS: BEGIN OF c_variant_handle,
             worklist           TYPE slis_handl VALUE 'WKLT',
             ac_additional_data TYPE slis_handl VALUE 'ACAD',
             items              TYPE slis_handl VALUE 'ITEM',
             details            TYPE slis_handl VALUE 'DETA',
           END OF c_variant_handle.

****Screen Groups
CONSTANTS: BEGIN OF c_screen_group,
              display_only(3)  VALUE 'DIO',
              off(3)           VALUE 'OFF',
              on(2)            VALUE 'ON',
              data_required(3) VALUE 'DTR',
              aufnr(3)         VALUE 'AUF',
              wonum(3)         VALUE 'WON',
              accom(3)         VALUE 'ACC',
              copy(3)          VALUE 'CPY',
            END OF c_screen_group.

CONSTANTS: BEGIN OF c_status_ord,
              rel  TYPE j_istat VALUE 'I0002',
              pcnf TYPE j_istat VALUE 'I0010',
           END OF c_status_ord.

CONSTANTS: BEGIN OF c_rstype,
              labor  TYPE /agri/fmrstype VALUE 'A',
              equnr  TYPE /agri/fmrstype VALUE 'B',
           END OF c_rstype.

CONSTANTS: BEGIN OF c_movements,
              mov_101  TYPE bwart VALUE '101',
              mov_102  TYPE bwart VALUE '102',
           END OF c_movements.

CONSTANTS: BEGIN OF c_process_status,
              ctd  TYPE /agri/fm_status VALUE 'CTD',
              cnf  TYPE /agri/fm_status VALUE 'CNF',
              dis  TYPE /agri/fm_status VALUE 'DIS',
              cls  TYPE /agri/fm_status VALUE 'CLS',
              rev  TYPE /agri/fm_status VALUE 'REV',
              del  TYPE /agri/fm_status VALUE 'DEL',
           END OF c_process_status.

CONSTANTS: BEGIN OF c_brf_object,
            fmac TYPE /agri/gbrfobjtp VALUE '/AGRI/FMAC',
           END OF c_brf_object.

CONSTANTS: BEGIN OF c_status_transaction,
             released          TYPE tj01-vrgng VALUE 'XGGR',
             blocked           TYPE tj01-vrgng VALUE 'XGGB',
           END OF c_status_transaction.

CONSTANTS: BEGIN OF c_accom_appli,
             wonum          TYPE /agri/fmacapp VALUE 'WO',
             prnum          TYPE /agri/fmacapp VALUE 'PR',
             aufnr          TYPE /agri/fmacapp VALUE 'FP',
           END OF c_accom_appli.

CONSTANTS: BEGIN OF c_accom_id,
           employee         TYPE /agri/fmacrstyp VALUE 'EM',
           equipment        TYPE /agri/fmacrstyp VALUE 'EQ',
           END OF c_accom_id.
                   " NOTES_REFRESH

CONSTANTS: BEGIN OF c_grmm,
             move_type TYPE bwart    VALUE '101',
             stck_type TYPE mb_insmk VALUE 'F',
             mvt_ind   TYPE kzbew    VALUE 'B',
             mvt_code  TYPE char2    VALUE '01',
           END OF c_grmm.

**** ESP6 Task #30035 - Global Text Engine Integration
CONSTANTS : c_switch_object_type TYPE /agri/b0swobjtp
                  VALUE /agri/if_global_constants=>mc_switch_object_type-accomplishment.
****
*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*CLASS lcl_event_handler DEFINITION.
*  PUBLIC SECTION.
*
*
*ENDCLASS.                    "lcl_event_handler DEFINITION
**---------------------------------------------------------------------*
**       CLASS lcl_event_handler IMPLEMENTATION
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*CLASS lcl_event_handler IMPLEMENTATION.
*
*
*                  "on_user_command
*
*ENDCLASS.
