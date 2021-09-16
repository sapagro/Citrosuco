*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLMCON
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF c_program,
             funloc   TYPE sy-repid VALUE '/AGRI/SAPLGLFLM',
             address  TYPE sy-repid VALUE 'SAPLSZA1',
             partners TYPE sy-repid VALUE 'SAPLIPAR',
           END OF c_program.

CONSTANTS: BEGIN OF c_agtyp,
             functional_location TYPE klassenart VALUE 'X91',
             measurement_point   TYPE klassenart VALUE 'X90',
           END OF c_agtyp.

*--Object
CONSTANTS: BEGIN OF c_object,
             func_loc         TYPE tcla-obtab        VALUE '/AGRI/GLFLOT',
             bor              LIKE /agri/tgabo-object VALUE '/AGRI/GLFL',
             log              LIKE balobj-object     VALUE '/AGRI/GLFL',
             change_documents TYPE cdobjectcl        VALUE '/AGRI/GLFL',
*             change_documents_fa    TYPE cdobjectcl        VALUE '/AGRI/GLFA',
*             change_documents_iflo  TYPE cdobjectcl        VALUE 'IFLO',
*             fl_change_documents    TYPE cdobjectcl        VALUE '/AGRI/GLFL',
             esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/GLFL',
             text_object      TYPE thead-tdobject    VALUE '/AGRI/GLFL',
           END OF c_object.

CONSTANTS: BEGIN OF c_dropdown_id,
*            funcloc TYPE vrm_id VALUE '/AGRI/S_GLFLOT-FLTYP',
             class         TYPE vrm_id VALUE '/AGRI/S_GLFLOT-CLASS',
*            struct_ind TYPE vrm_id VALUE '/AGRI/S_GLFLOT-TPLKZ',
             terrain_level TYPE vrm_id VALUE '/AGRI/S_GLFLOT-TPLVL',
           END OF c_dropdown_id.

****Log Sub-object
CONSTANTS : BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_structure_name,
             worklist_header    TYPE dd02l-tabname VALUE '/AGRI/S_GLFLOT_WL',
             class_assignment   TYPE dd02l-tabname VALUE '/AGRI/S_GLFLATG_FCAT',
             attributes         TYPE dd02l-tabname VALUE '/AGRI/S_GLFLATV_FCAT',
             fl_multi_lang_desc TYPE dd02l-tabname VALUE '/AGRI/S_GLIFLOTX',
             terrain_labels     TYPE dd02l-tabname VALUE '/AGRI/S_GLFLOS_FCAT',
             hierarchy_display  TYPE dd02l-tabname VALUE '/AGRI/S_GLFLHIER_LIST',
             assignments        TYPE dd02l-tabname VALUE '/AGRI/S_GLFLCMA_FCAT',
             owners             TYPE dd02l-tabname VALUE '/AGRI/S_GLFLOWN_FCAT',
             plants             TYPE dd02l-tabname VALUE '/AGRI/S_GLFLPPL_FCAT',
             extdfl_fcat        TYPE dd02l-tabname VALUE '/AGRI/S_GLFLDFL_FCAT',
           END OF c_structure_name.

CONSTANTS: BEGIN OF c_variant_handle,
             worklist   TYPE slis_handl VALUE 'WKLT',
             attributes TYPE disvariant-handle VALUE 'ATTR',
             owners     TYPE disvariant-handle VALUE 'OWNR',
             plants     TYPE disvariant-handle VALUE 'TPPL',
           END OF c_variant_handle.

CONSTANTS: BEGIN OF c_screen,
             overview             TYPE sy-dynnr VALUE '0100',
             main_screen          TYPE sy-dynnr VALUE '0101',
             header               TYPE sy-dynnr VALUE '0200',
             create_floc          TYPE sy-dynnr VALUE '0201',
             multi_lang_desc      TYPE sy-dynnr VALUE '0203',
             header_compressed    TYPE sy-dynnr VALUE '0204',
             terrain_labels       TYPE sy-dynnr VALUE '0205',
             hierarchy_display    TYPE sy-dynnr VALUE '0206',
             new_label            TYPE sy-dynnr VALUE '0207',
             cam                  TYPE sy-dynnr VALUE '0300',
             items                TYPE sy-dynnr VALUE '0300',
             items_fullscr        TYPE sy-dynnr VALUE '0314',
             general              TYPE sy-dynnr VALUE '0301',
             organization         TYPE sy-dynnr VALUE '0302',
             address              TYPE sy-dynnr VALUE '0303',
             status               TYPE sy-dynnr VALUE '0304',
             texts                TYPE sy-dynnr VALUE '0305',
             admin                TYPE sy-dynnr VALUE '0306',
             user_additional_data TYPE sy-dynnr VALUE '0307',
             partners             TYPE sy-dynnr VALUE '0308',
             attributes           TYPE sy-dynnr VALUE '0309',
             classification       TYPE sy-dynnr VALUE '0311',
             assignments          TYPE sy-dynnr VALUE '0310',
             class_assignment     TYPE sy-dynnr VALUE '0312',
             owners               TYPE sy-dynnr VALUE '0313',
             change_superior_fl   TYPE sy-dynnr VALUE '0315',
             gis_map              TYPE sy-dynnr VALUE '0316',
             plants               TYPE sy-dynnr VALUE '0317',
             adddata_cust_screen  TYPE sy-dynnr VALUE '0318',
             reference_summary    TYPE sy-dynnr VALUE '0319',
             partners_fl          TYPE sy-dynnr VALUE '0201',
             address_fl           TYPE sy-dynnr VALUE '0300',
             dummy                TYPE sy-dynnr VALUE '0999',
           END OF c_screen.

****Screen Groups
CONSTANTS : BEGIN OF c_screen_group,
              display_only(3)  VALUE 'DIO',
              off(3)           VALUE 'OFF',
              on(2)            VALUE 'ON',
              data_required(3) VALUE 'DTR',
              company_code(3)  VALUE 'BUK',
            END OF c_screen_group.

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
              delete                TYPE sy-ucomm VALUE 'DELE',
              enter                 TYPE sy-ucomm VALUE 'ENTR',
              select                TYPE sy-ucomm VALUE 'SELE',
              continue              TYPE sy-ucomm VALUE 'CONT',
              copy                  TYPE sy-ucomm VALUE 'COPY',
              undo_delete           TYPE sy-ucomm VALUE 'UNDL',
              superior_funloc       TYPE sy-ucomm VALUE 'PRED',
              measurement_doc       TYPE sy-ucomm VALUE 'MEDO',
              measure_doc_display   TYPE sy-ucomm VALUE 'MDSP',
              class_assign          TYPE sy-ucomm VALUE 'KLAA',
              class_insert          TYPE sy-ucomm VALUE 'KLAI',
              class_delete          TYPE sy-ucomm VALUE 'KLAD',
              desc_delete           TYPE sy-ucomm VALUE 'DDEL',
              desc_insert           TYPE sy-ucomm VALUE 'DINS',
              label_create          TYPE sy-ucomm VALUE 'LCRE',
              label_activate        TYPE sy-ucomm VALUE 'LACT',
              owner_insert          TYPE sy-ucomm VALUE 'OINS',
              owner_delete          TYPE sy-ucomm VALUE 'ODEL',
              search_and_replace    TYPE sy-ucomm VALUE 'SNRP',
              select_and_compute    TYPE sy-ucomm VALUE 'SNCP',
              hierarchy_click       TYPE sy-ucomm VALUE 'HIR_CLK',
              change_docs           TYPE sy-ucomm VALUE 'CDOC',
              gis_map               TYPE sy-ucomm VALUE 'TMAP',
              hierarchy_display     TYPE sy-ucomm VALUE 'HIDS',
              where_used_list       TYPE sy-ucomm VALUE 'WUSD',
              expand_hdr            TYPE sy-ucomm VALUE 'EXPH',
              compress_hdr          TYPE sy-ucomm VALUE 'CMPH',
              alternative_label     TYPE sy-ucomm VALUE 'LBFO',
              plant_insert          TYPE sy-ucomm VALUE 'PPLI',
              plant_delete          TYPE sy-ucomm VALUE 'PPLD',
              tab_details           TYPE sy-ucomm VALUE 'TAB_DETL',
              tab_address           TYPE sy-ucomm VALUE 'TAB_ADR',
              tab_plants            TYPE sy-ucomm VALUE 'TAB_PPL',
              tab_organization      TYPE sy-ucomm VALUE 'TAB_ORG',
              tab_partners          TYPE sy-ucomm VALUE 'TAB_PTR',
              tab_classification    TYPE sy-ucomm VALUE 'TAB_CLF',
              tab_owners            TYPE sy-ucomm VALUE 'TAB_OWN',
              tab_assignments       TYPE sy-ucomm VALUE 'TAB_ASGN',
              tab_notes             TYPE sy-ucomm VALUE 'TAB_NOTES',
              tab_texts             TYPE sy-ucomm VALUE 'TAB_TEXT',
              tab_status            TYPE sy-ucomm VALUE 'TAB_STAT',
              tab_additional_data   TYPE sy-ucomm VALUE 'TAB_ADD',
              tab_additional_data2  TYPE sy-ucomm VALUE 'TAB_ADD2',
              tab_admin             TYPE sy-ucomm VALUE 'TAB_ADMN',
              mass_header_change    TYPE sy-ucomm VALUE 'MASS_HDR_CHNG',

****User Functions
              user_function1        TYPE syucomm VALUE 'USF1',
              user_function2        TYPE syucomm VALUE 'USF2',
              user_function3        TYPE syucomm VALUE 'USF3',
              user_function4        TYPE syucomm VALUE 'USF4',
              user_function5        TYPE syucomm VALUE 'USF5',
            END OF c_fcode.

CONSTANTS: BEGIN OF c_tcode,
             funloc TYPE sytcode VALUE '/AGRI/GLFLM',
           END OF c_tcode.

CONSTANTS: c_cam_group TYPE ad_group VALUE 'PM01',
           BEGIN OF c_cam_mode,
             create  TYPE ad_mntmd VALUE 'CREATE',
             display TYPE ad_mntmd VALUE 'DISPLAY',
             change  TYPE ad_mntmd VALUE 'CHANGE',
             empty   TYPE ad_mntmd VALUE 'EMPTY',
           END OF c_cam_mode.

CONSTANTS: BEGIN OF c_activity,
             delete      TYPE imas_act_type VALUE 'LVMS',
             undo_delete TYPE imas_act_type VALUE 'LVMZ',
           END OF c_activity.

CONSTANTS: BEGIN OF c_brf_object,
             glfl TYPE /agri/gbrfobjtp VALUE '/AGRI/GLFL',
           END OF c_brf_object.

CONSTANTS: BEGIN OF c_status_transaction,
             released TYPE tj01-vrgng VALUE 'XAGR',
             blocked  TYPE tj01-vrgng VALUE 'XAGB',
           END OF c_status_transaction.

**** ESP6 Task #30035 - Global Text Engine Integration
CONSTANTS : c_switch_object_type TYPE /agri/b0swobjtp
                  VALUE /agri/if_global_constants=>mc_switch_object_type-terrain.
****

CONSTANTS: c_posnr LIKE vbrp-posnr VALUE '000000'.
