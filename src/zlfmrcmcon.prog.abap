*&---------------------------------------------------------------------*
*&  Include           ZLFMRCMNCON
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZLFMRCMCON
*&---------------------------------------------------------------------*
*--Program Names
CONSTANTS: c_program_rcm   TYPE sy-repid VALUE 'SAPLZFMRCM'.

*--Object
CONSTANTS: BEGIN OF c_object,
             bor              LIKE /agri/tgabo-object      VALUE 'ZFMRC',
             log              LIKE balobj-object           VALUE 'ZFMRC',
             change_documents TYPE cdobjectcl              VALUE 'ZFMRC',
             esh_object       TYPE /agri/geshobjtp         VALUE 'ZFMRC',
             text_object      TYPE thead-tdobject          VALUE 'ZFMRC',
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
             create_recipe        TYPE sy-dynnr VALUE '0201',
             popup_recipe_create  TYPE sy-dynnr VALUE '0204',
             popup_version_create TYPE sy-dynnr VALUE '0202',
             popup_version_modify TYPE sy-dynnr VALUE '0206',
             grid_bom             TYPE sy-dynnr VALUE '0203',
             version              TYPE sy-dynnr VALUE '0205',
             details              TYPE sy-dynnr VALUE '0300',
             grid_dose            TYPE sy-dynnr VALUE '0301',
             texts                TYPE sy-dynnr VALUE '0302',
             admin                TYPE sy-dynnr VALUE '0303',
             produto_similar      TYPE sy-dynnr VALUE '0309',
             mass_details         TYPE sy-dynnr VALUE '0304',
             list                 TYPE sy-dynnr VALUE '0305',
             latest_measurements  TYPE sy-dynnr VALUE '0306',
             measurements_display TYPE sy-dynnr VALUE '0307',
             update_mass          TYPE sy-dynnr VALUE '0310',
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
              vrs_create            TYPE sy-ucomm VALUE 'VERS',
              vrs_modify            TYPE sy-ucomm VALUE 'VERM',
              bom_create            TYPE sy-ucomm VALUE 'CBOM',
              bom_update            TYPE sy-ucomm VALUE 'BUPD',
              delete                TYPE sy-ucomm VALUE 'DELE',
              delete_alternative    TYPE sy-ucomm VALUE 'DELA',
              delete_version        TYPE sy-ucomm VALUE 'DLVR',
              continue              TYPE sy-ucomm VALUE 'CONT',
              enter                 TYPE sy-ucomm VALUE 'ENTR',
              select                TYPE sy-ucomm VALUE 'SELE',
              where_used_list       TYPE sy-ucomm VALUE 'WUSD',
              tab_version           TYPE sy-ucomm VALUE 'TAB_VERS',
              tab_dose              TYPE sy-ucomm VALUE 'TAB_DOSE',
              tab_bom               TYPE sy-ucomm VALUE 'TAB_BOM',
              tab_texts             TYPE sy-ucomm VALUE 'TAB_TEXT',
              tab_admin             TYPE sy-ucomm VALUE 'TAB_ADMN',
              tab_similar           TYPE sy-ucomm VALUE 'TAB_SIMILAR',
              tab_notes             TYPE sy-ucomm VALUE 'TAB_NOTES',
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
             Dose          TYPE slis_handl VALUE 'DOSE',
             bom           TYPE slis_handl VALUE 'BOM',
           END OF c_variant_handle.

*--Structures
CONSTANTS: BEGIN OF c_structure_name,
             work_list_header TYPE dd02l-tabname
                                 VALUE 'ZSC_FMRCHDR_WL',
             doce                 TYPE dd02l-tabname
                                 VALUE 'ZSC_FMRCLST_FCAT',
             bom             TYPE dd02l-tabname
                                 VALUE 'ZSC_FMRCBOM_FCAT',
             ver             TYPE dd02l-tabname
                                 VALUE 'ZSC_FMRCVRS_FCAT',
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
             acm_wb   TYPE sy-tcode VALUE 'ZFMRCM',
             acm_mass TYPE sy-tcode VALUE 'ZFMRC23',
           END OF c_tcode.
