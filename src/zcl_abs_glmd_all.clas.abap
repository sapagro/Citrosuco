class ZCL_ABS_GLMD_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_GLMD_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_GLMD_ALL IMPLEMENTATION.


METHOD /agri/if_ex_badi_glmd_all~after_save.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_SHP_EXIT_ATTR                           *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.11.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  The attribute values from measurement document  *
*                      workbench gets updated in the terrain workbench *
*                      based on custom table.                          *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
  TYPES: BEGIN OF ly_atributos,
           safra          TYPE atinn,
           data_encerra   TYPE atinn,
           estimativa     TYPE atinn,
           cxpl_estimada  TYPE atinn,
           prod_real      TYPE atinn,
           cxpl_real      TYPE atinn,
           cod_imovel     TYPE atinn,
           talhao         TYPE atinn,
           florada        TYPE atinn,
           estimativa_ini TYPE atinn,
           estimativa_fim TYPE atinn,
           variacao_perc  TYPE atinn,
           area_talhao    TYPE atinn,
         END OF ly_atributos.

*--Local data declaration
  DATA: lo_mdtype       TYPE REF TO data,
        lt_cs_messages  TYPE /agri/t_gprolog,
        lt_all_messages TYPE /agri/t_gprolog,
        lt_tplnr        TYPE /agri/t_gltplnr,
        lt_flhdr        TYPE /agri/t_glflot,
        lt_iflotx       TYPE /agri/t_gliflotx,
        lt_adrc         TYPE /agri/t_gladrc,
        lt_ihpa         TYPE /agri/t_glihpa,
        lt_flppl        TYPE /agri/t_glflppl,
        lt_flatg        TYPE /agri/t_glflatg,
        lt_mdhdr        TYPE STANDARD TABLE OF /agri/glmdhdr INITIAL SIZE 0,
        lt_flatv        TYPE /agri/t_glflatv,
        lt_flown        TYPE /agri/t_glflown,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_klah         TYPE tt_klah,
        lt_flos         TYPE /agri/t_glflos,
        lrt_tplnr_fl    TYPE RANGE OF /agri/gltplnr_fl,
        lt_messages     TYPE /agri/t_gprolog,
        ltr_mdtype      TYPE RANGE OF /agri/glmdtyp,
        ls_atributo     TYPE ly_atributos,
        ls_glfldoc      TYPE /agri/s_glfl_doc,
        lref_date       TYPE REF TO data,
        lv_subrc        TYPE sysubrc,
        lv_i            TYPE i,
        lv_data_char    TYPE atwrt.

  DATA: gt_message TYPE /agri/t_gprolog.

*-- Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

*--Field symbol declaration
  FIELD-SYMBOLS: <fs_mdtyp> TYPE any,
                 <lt_mdhdr> TYPE ANY TABLE,
                 <fs_date>  TYPE any.

  DO 13 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-SAFRA'
          IMPORTING
            output = ls_atributo-safra.
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-ENCERRA'
          IMPORTING
            output = ls_atributo-data_encerra.
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESTIMATIVA'
          IMPORTING
            output = ls_atributo-estimativa.
      WHEN 4.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-EST'
          IMPORTING
            output = ls_atributo-cxpl_estimada.
      WHEN 5.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-PROD-REAL'
          IMPORTING
            output = ls_atributo-prod_real.
      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-REAL'
          IMPORTING
            output = ls_atributo-cxpl_real.
      WHEN 7.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_COD_IM'
          IMPORTING
            output = ls_atributo-cod_imovel.
      WHEN 8.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_TALH'
          IMPORTING
            output = ls_atributo-talhao.
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-FLORADA'
          IMPORTING
            output = ls_atributo-florada.
      WHEN 10.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-INICIAL'
          IMPORTING
            output = ls_atributo-estimativa_ini.
      WHEN 11.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-FINAL'
          IMPORTING
            output = ls_atributo-estimativa_fim.
      WHEN 12.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-VAR-PERC'
          IMPORTING
            output = ls_atributo-variacao_perc.
      WHEN 13.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_AREA_TALHAO'
          IMPORTING
            output = ls_atributo-area_talhao.
    ENDCASE.
  ENDDO.

  ASSIGN ('(ZABS_REP_MD_MASS_APPROVALS)GT_MDHDR[]') TO <lt_mdhdr>.
*-- BOC T_T.KONNO-07.12.21
  IF sy-subrc NE 0.
    ASSIGN ('(ZABS_REP_APROVA_DOC_MED)GT_MDHDR_MEM[]') TO <lt_mdhdr>.
  ENDIF.
*-- EOC T_T.KONNO-07.12.21

  IF <lt_mdhdr> IS ASSIGNED.
    lt_mdhdr[] = <lt_mdhdr>[].
*-- BOC T_T.KONNO-07.12.21
  ELSE.
    SELECT *
      FROM /agri/glmdhdr
      INTO TABLE @lt_mdhdr
     WHERE mdocm = @is_mdhdr-mdocm.
*-- EOC T_T.KONNO-07.12.21
  ENDIF.

  IF flt_val EQ 'ZPTA'.
    IF is_mdhdr-mpgrp EQ 'FAZ-PLANTIO'.
      IF i_updkz EQ 'I'.
        APPEND is_mdhdr-tplnr_fl TO lt_tplnr.

*-- Getting all the terrain records
        CALL FUNCTION '/AGRI/GLFL_READ'
          EXPORTING
            it_tplnr       = lt_tplnr
          IMPORTING
            et_flhdr       = lt_flhdr
            et_iflotx      = lt_iflotx
            et_adrc        = lt_adrc
            et_ihpa        = lt_ihpa
            et_flppl       = lt_flppl
            et_flatg       = lt_flatg
            et_flatv       = lt_flatv
            et_flown       = lt_flown
            et_flcma       = lt_flcma
            et_flos        = lt_flos
          EXCEPTIONS
            no_data_exists = 1
            OTHERS         = 2.

        IF sy-subrc EQ 0.
*--Fetch class header data
          SELECT *
            FROM klah
            INTO TABLE @lt_klah
            WHERE class = @is_mdhdr-mpgrp.

          IF sy-subrc EQ 0.
*--Calling meathod to get attribute header data
            CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
              EXPORTING
                it_klah  = lt_klah
                i_agtyp  = zcl_abs_abap_maintain=>c_agtyp_measurement_doc
              IMPORTING
                et_athdr = DATA(lt_athdr).
            IF lt_athdr IS NOT INITIAL.
              SORT lt_athdr BY atnam.
            ENDIF.
          ENDIF.

          DATA(lt_mdatv) = it_mdatv[].
          READ TABLE lt_mdatv ASSIGNING FIELD-SYMBOL(<ls_talhao>)
            WITH KEY atinn = ls_atributo-area_talhao.
          IF sy-subrc EQ 0.
            IF <ls_talhao>-atflv IS NOT INITIAL.
              READ TABLE lt_athdr INTO DATA(ls_athdr)
                WITH KEY atinn = ls_atributo-area_talhao.
              IF sy-subrc EQ 0.
                CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
                ASSIGN lref_date->* TO <fs_date>.
                <fs_date> = <ls_talhao>-atflv.
                <ls_talhao>-atwrt = <fs_date>.
                CONDENSE <ls_talhao>-atwrt NO-GAPS.
              ENDIF.
            ENDIF.
          ENDIF.

*-- Atualiza Área Talhão /AGRI/GLFLCMA
*-- Calling FM to update attribute values
          REFRESH lt_cs_messages.
          CALL FUNCTION 'ZABS_FM_CS_FIELDS_UPDATE'
            EXPORTING
              is_mdhdr    = is_mdhdr
              it_mditm    = it_mditm
              it_mdatv    = lt_mdatv
            IMPORTING
              et_messages = lt_cs_messages.

          APPEND LINES OF lt_cs_messages TO lt_all_messages.

*-- Atualiza Área Talhão /AGRI/GLFLOT
          IF <ls_talhao> IS ASSIGNED.
            READ TABLE lt_flhdr INTO DATA(ls_flhdr) INDEX 1.
            IF sy-subrc EQ 0.
              DATA(lv_locked) = abap_true.
              IF ls_flhdr-tplnr_fl IS NOT INITIAL.
                CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFL'
                  EXPORTING
                    tplnr_fl       = ls_flhdr-tplnr_fl
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.
                IF sy-subrc NE 0.
                  lv_locked = abap_false.
                ENDIF.
              ENDIF.

              IF ls_flhdr-strno IS NOT INITIAL.
                CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLFLS'
                  EXPORTING
                    strno          = ls_flhdr-strno
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.
                IF sy-subrc NE 0.
                  lv_locked = abap_false.
                ENDIF.
              ENDIF.

              IF lv_locked = abap_true
              AND <ls_talhao>-atflv IS NOT INITIAL.
                ls_flhdr-updkz = 'U'.
                ls_flhdr-garea = <ls_talhao>-atwrt.
*-- Terrain Change
                REFRESH lt_messages.
                CALL FUNCTION '/AGRI/GLFL_CHANGE'
                  EXPORTING
                    is_flhdr                = ls_flhdr "Cabeçalho do terreno
                    it_flatg                = lt_flatg "Grupos de atributo de terreno
                    it_flatv                = lt_flatv "Características de terreno
                  IMPORTING
                    et_messages             = lt_messages
                  CHANGING
                    cs_glfl_doc             = ls_glfldoc
                  EXCEPTIONS
                    no_documents_to_process = 1
                    no_authorization        = 2
                    change_failed           = 3
                    terrain_locked          = 4
                    OTHERS                  = 5.

                APPEND LINES OF lt_messages TO lt_all_messages.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*--BOC-T_T.KONNO-05.28.21
*--BOC-T_T.KONNO-07.12.21
*    ELSEIF is_mdhdr-mpgrp EQ 'FAZ_PREVISAO'.
*      IF i_updkz EQ 'U'.
*        READ TABLE lt_mdhdr INTO DATA(ls_mdhdr)
*          WITH KEY mdocm = is_mdhdr-mdocm BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          IF is_mdhdr-ustat NE ls_mdhdr-ustat
*          AND is_mdhdr-ustat EQ 'E0004'.
*            CALL FUNCTION 'FUNCTION_EXISTS'
*              EXPORTING
*                funcname           = 'ZABS_FM_CS_FIELDS_UPDATE_FP'
*              EXCEPTIONS
*                function_not_exist = 1
*                OTHERS             = 2.
*            IF sy-subrc EQ 0.
*              CALL FUNCTION 'ZABS_FM_CS_FIELDS_UPDATE_FP'
*                EXPORTING
*                  is_mdhdr    = is_mdhdr
*                  it_mditm    = it_mditm
*                  it_mdatv    = it_mdatv
*                IMPORTING
*                  et_messages = lt_cs_messages[].
*
*              APPEND LINES OF lt_cs_messages TO lt_all_messages.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*--EOC-T_T.KONNO-07.12.21
    ENDIF.
  ELSEIF flt_val EQ 'ZTYP'.
    IF is_mdhdr-mpgrp EQ 'FAZ-PREPARO-SOLO'.
      IF i_updkz EQ 'I'.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = 'ZABS_FM_CS_FIELDS_UPDATE_FPS'
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'ZABS_FM_CS_FIELDS_UPDATE_FPS'
            EXPORTING
              is_mdhdr    = is_mdhdr
              it_mditm    = it_mditm
              it_mdatv    = it_mdatv
            IMPORTING
              et_messages = lt_cs_messages[].
        ENDIF.
      ENDIF.
*-- BOC T_T.KONNO-07.12.21
    ELSEIF is_mdhdr-mpgrp EQ 'FAZ_PREVISAO'.
      IF i_updkz EQ 'U'.
        READ TABLE lt_mdhdr INTO DATA(ls_mdhdr)
          WITH KEY mdocm = is_mdhdr-mdocm BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF is_mdhdr-ustat NE ls_mdhdr-ustat
          AND is_mdhdr-ustat EQ 'E0004'.
            CALL FUNCTION 'FUNCTION_EXISTS'
              EXPORTING
                funcname           = 'ZABS_FM_CS_FIELDS_UPDATE_FP'
              EXCEPTIONS
                function_not_exist = 1
                OTHERS             = 2.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'ZABS_FM_CS_FIELDS_UPDATE_FP'
                EXPORTING
                  is_mdhdr    = is_mdhdr
                  it_mditm    = it_mditm
                  it_mdatv    = it_mdatv
                IMPORTING
                  et_messages = lt_cs_messages[].

              APPEND LINES OF lt_cs_messages TO lt_all_messages.
            ENDIF.
          ENDIF.
        ENDIF.
*-- EOC T_T.KONNO-07.12.21
      ENDIF.
    ENDIF.
  ELSEIF flt_val EQ 'ZARV'.
    IF is_mdhdr-mpgrp EQ 'FAZ-INV-PLANTAS'.
      IF i_updkz EQ 'U'.
        READ TABLE lt_mdhdr INTO ls_mdhdr
          WITH KEY mdocm = is_mdhdr-mdocm BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF is_mdhdr-ustat NE ls_mdhdr-ustat
          AND is_mdhdr-ustat EQ 'E0004'.
            CALL FUNCTION 'FUNCTION_EXISTS'
              EXPORTING
                funcname           = 'ZABS_FM_CS_FIELDS_UPDATE_FIP'
              EXCEPTIONS
                function_not_exist = 1
                OTHERS             = 2.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'ZABS_FM_CS_FIELDS_UPDATE_FIP'
                EXPORTING
                  is_mdhdr    = is_mdhdr
                  it_mditm    = it_mditm
                  it_mdatv    = it_mdatv
                IMPORTING
                  et_messages = lt_cs_messages[].

              APPEND LINES OF lt_cs_messages TO lt_all_messages.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*--EOC-T_T.KONNO-05.28.21
  ENDIF.

  CALL FUNCTION 'ZABS_FM_MDM_MESSAGE'
    EXPORTING
      it_messages = lt_all_messages[].

ENDMETHOD.


METHOD /agri/if_ex_badi_glmd_all~before_save.
**************************************************************************
*  Confidential property of Citrosuco                                    *
*  All Rights Reserved                                                   *
**************************************************************************
* Enhancement Name    :                                                  *
* Implementation Class:  ZCL_ABS_GLMD_ALL                                *
* Interface           : /AGRI/IF_EX_BADI_GLMD_ALL                        *
* Method Name         :  before_save                                     *
* Created By          :  Jetendra Mantena                                *
* Requested by        :  Mario Alfredo                                   *
* Created on          :  08.04.2019                                      *
* TR                  :  C4DK901784                                      *
* Version             :  001                                             *
* Description         :  Validating Formula in measurement document      *
*------------------------------------------------------------------------*
*  Modification Log:                                                     *
*------------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description               *
*                                                                        *
*&----------------------------------------------------------------------&*
  TYPES: BEGIN OF ly_atributos,
           safra          TYPE atinn,
           data_encerra   TYPE atinn,
           estimativa     TYPE atinn,
           cxpl_estimada  TYPE atinn,
           prod_real      TYPE atinn,
           cxpl_real      TYPE atinn,
           cod_imovel     TYPE atinn,
           talhao         TYPE atinn,
           florada        TYPE atinn,
           estimativa_ini TYPE atinn,
           estimativa_fim TYPE atinn,
           variacao_perc  TYPE atinn,
           cit_quimicos   TYPE atinn,
           dias_carencia  TYPE atinn,
           data_aplicacao TYPE atinn,
         END OF ly_atributos,

         BEGIN OF ly_ausp,
           objek TYPE cuobn,
           atinn TYPE atinn,
           atzhl TYPE wzaehl,
           mafid TYPE klmaf,
           klart TYPE klassenart,
           adzhl TYPE adzhl,
           atwrt TYPE atwrt,
           atflv TYPE atflv,
         END OF ly_ausp,

         BEGIN OF ly_matdoc,
           werks TYPE werks_d,
           menge TYPE menge_d,
           budat TYPE budat,
           bwart TYPE bwart,
           matnr TYPE matnr,
           charg TYPE charg_d,
           lifnr TYPE elifn,
           atwrt TYPE atwrt,
         END OF ly_matdoc,

         BEGIN OF ly_mch1,
           matnr    TYPE matnr,
           charg    TYPE charg_d,
           cuobj_bm TYPE cuobj_bm,
           objek    TYPE cuobn,
         END OF ly_mch1.

  DATA: lt_cskey           TYPE /agri/t_glcs_key,
        lt_fl_doc          TYPE /agri/t_glfl_doc,
        lt_tplnr           TYPE /agri/t_gltplnr,
        lt_amhdr           TYPE /agri/t_glamhdr,
        lt_ativos          TYPE /agri/t_glamhdr,
        lt_ausp            TYPE STANDARD TABLE OF ly_ausp INITIAL SIZE 0,
        lrt_objek          TYPE RANGE OF cuobn,
        lrs_objek          LIKE LINE OF lrt_objek,
        lt_cshdr           TYPE STANDARD TABLE OF /agri/glflcma,
        lt_mch1            TYPE STANDARD TABLE OF ly_mch1 INITIAL SIZE 0,
        lt_matdoc          TYPE STANDARD TABLE OF ly_matdoc INITIAL SIZE 0,
        lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        lwa_atributo       TYPE ly_atributos,
        lv_menge_x         TYPE menge_d,
        lv_cod_imovel      TYPE /agri/gltplnr_fl,
        lv_imovel_atwrt    TYPE atwrt,
        lv_talhao_atwrt    TYPE atwrt,
        lv_cod_imovel_in   TYPE /agri/gltplnr_fl,
        lv_talhao          TYPE /agri/gltplnr_fl,
        lv_talhao_in       TYPE /agri/gltplnr_fl,
        lwa_attributes     TYPE zabs_str_attr_forml,
        lwa_summary_list   TYPE /agri/s_glam_summary_fcat,
        lt_asset_summary   TYPE /agri/t_glam_summary_fcat,
        lt_attributes      TYPE zabs_tty_attr_forml,
        lr_mpgrp           TYPE RANGE OF /agri/glmpgrp,
        lr_cuobj           TYPE RANGE OF cuobj_bm,
        lr_werks           TYPE RANGE OF werks_d,
        lwa_werks          LIKE LINE OF lr_werks,
        lr_bwart           TYPE RANGE OF bwart,
        lwa_bwart          LIKE LINE OF lr_bwart,
        lr_budat           TYPE RANGE OF budat,
        lwa_budat          LIKE LINE OF lr_budat,
        lr_matnr           TYPE RANGE OF matnr,
        lr_batch           TYPE RANGE OF atnam,
        lr_cx_liq          TYPE RANGE OF atnam,
        lr_imovel          TYPE RANGE OF atnam,
        lr_atnam           TYPE RANGE OF atnam,
        lr_atinn           TYPE RANGE OF atinn,
        lr_atinn_imovel    TYPE RANGE OF atinn,
        lr_atinn_cx_liq    TYPE RANGE OF atinn,
        lr_atinn_batch     TYPE RANGE OF atinn,
        lwa_atinn          LIKE LINE OF lr_atinn,
        lwa_matnr          LIKE LINE OF lr_matnr,
        lv_terreno         TYPE /agri/gltplnr_fl,
        lv_data_temp       TYPE i,
        lv_data_encerra    TYPE sydatum,
        lv_cod_imov_f      TYPE atflv,
        lv_talhao_f        TYPE atflv,
        lv_sum_menge       TYPE menge_d,
        lv_estimativa_x    TYPE menge_d,
        lv_var_perc        TYPE menge_d,
        lv_var_qtd         TYPE menge_d,
        lv_libera_colheita TYPE atinn,
        lv_menge           TYPE menge_d,
        lv_menge_char      TYPE char16,
        lv_data_apl_char   TYPE char8,
        lv_data_char       TYPE char8,
        lv_safra_char      TYPE char4,
        lv_caixa_char      TYPE char20,
        lv_data_apl_datum  TYPE sydatum,
        lv_data_datum      TYPE sydatum,
        lv_data_fim        TYPE sydatum,
        lv_data_inicio     TYPE sydatum,
        lv_i               TYPE i,
        lv_dias_carencia   TYPE i,
        lv_gyear           TYPE /agri/gyear,
        lv_gyear_tela      TYPE /agri/gyear,
        lv_matnr_in        TYPE matnr,
        lv_matnr           TYPE cuobn,
        lv_subrc           TYPE sysubrc,
        lv_message         TYPE char80,
        lv_safra_float     TYPE atflv,
        lv_safra           TYPE atinn,
        lv_atinn_imovel    TYPE atinn,
        lv_atinn_talhao    TYPE atinn,
        lv_atinn_data      TYPE atinn,
        lv_cod_imovel_flt  TYPE atflv,
        lv_cod_talhao_flt  TYPE atflv,
        lv_cod_talhao_char TYPE atwrt,
        lv_florada_1       TYPE atwrt VALUE '1',
        lv_florada_2       TYPE atwrt VALUE '2',
        lv_cod_imovel_out  TYPE /agri/gltplnr_fl,
        lv_value           TYPE atflv.

  TYPES: BEGIN OF ty_notes_buffer,
           objtyp    TYPE tojtb-name,
           objkey    TYPE /agri/gnotes-object,
           subobject TYPE /agri/gnotes-subobject,
           posnr     TYPE numc06,
           notes     TYPE /agri/t_gnote,
           subscreen TYPE sy-dynnr,
           changed   TYPE abap_bool,
         END OF ty_notes_buffer,

         tt_notes_buffer TYPE TABLE OF ty_notes_buffer.

  FIELD-SYMBOLS: <lwa_mddoc_infocus>   TYPE /agri/s_glmd_doc,
                 <lt_std_notes_buffer> TYPE tt_notes_buffer,
                 <lt_cst_notes_buffer> TYPE tt_notes_buffer.

  CONSTANTS: BEGIN OF lc_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF lc_tipo_safra,

             BEGIN OF lc_propriedade,
               proprio   TYPE /agri/glownshp VALUE 'OW',
               terceiros TYPE /agri/glownshp VALUE 'TP',
             END OF lc_propriedade,

             BEGIN OF c_action,
               add    TYPE /agri/glivact VALUE 'A',
               remove TYPE /agri/glivact VALUE 'D',
               reset  TYPE /agri/glivact VALUE 'R',
             END OF c_action,

             lc_cultura     TYPE /agri/glcmnum VALUE 'CITROS',
             lc_cl_material TYPE klassenart VALUE '001'.

  IF ct_mdatv[] IS INITIAL.
    RETURN.
  ENDIF.

  DO 15 TIMES.
    DATA(lv_index) = sy-index.
    CASE lv_index.
      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-SAFRA'
          IMPORTING
            output = lwa_atributo-safra.
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-ENCERRA'
          IMPORTING
            output = lwa_atributo-data_encerra.
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-ESTIMATIVA'
          IMPORTING
            output = lwa_atributo-estimativa.
      WHEN 4.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-EST'
          IMPORTING
            output = lwa_atributo-cxpl_estimada.
      WHEN 5.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-PROD-REAL'
          IMPORTING
            output = lwa_atributo-prod_real.
      WHEN 6.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-CXPL-REAL'
          IMPORTING
            output = lwa_atributo-cxpl_real.
      WHEN 7.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_COD_IM'
          IMPORTING
            output = lwa_atributo-cod_imovel.
      WHEN 8.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'RC_FR_TALH'
          IMPORTING
            output = lwa_atributo-talhao.
      WHEN 9.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-FLORADA'
          IMPORTING
            output = lwa_atributo-florada.
      WHEN 10.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-INICIAL'
          IMPORTING
            output = lwa_atributo-estimativa_ini.
      WHEN 11.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-EST-FINAL'
          IMPORTING
            output = lwa_atributo-estimativa_fim.
      WHEN 12.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-VAR-PERC'
          IMPORTING
            output = lwa_atributo-variacao_perc.
      WHEN 13.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-QUIMICOS'
          IMPORTING
            output = lwa_atributo-cit_quimicos.
      WHEN 14.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'FAZ_DIAS_CARENCIA'
          IMPORTING
            output = lwa_atributo-dias_carencia.
      WHEN 15.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'CIT-DATA-APLIC'
          IMPORTING
            output = lwa_atributo-data_aplicacao.
    ENDCASE.
  ENDDO.

*--BOC-T_T.KONNO-06.02.21
  IF flt_val EQ 'ZPTA'
  AND i_updkz EQ 'I'
  AND cs_mdhdr-mpgrp EQ 'CIT-ESTIMATIVA'.
    CALL FUNCTION 'ZABS_FM_CIT_ESTIMATIVA'
      CHANGING
        cs_mdhdr = cs_mdhdr
        ct_mditm = ct_mditm[]
        ct_mdatv = ct_mdatv[].
  ELSEIF flt_val EQ 'ZPTA'
     AND i_updkz EQ 'I'
     AND cs_mdhdr-mpgrp EQ 'ENCERRAR_FLORADA'.
    CALL FUNCTION 'ZABS_FM_ENCERRAR_FLORADA'
      CHANGING
        cs_mdhdr = cs_mdhdr
        ct_mditm = ct_mditm[]
        ct_mdatv = ct_mdatv[].
  ELSEIF flt_val EQ 'ZPTA'
     AND i_updkz EQ 'I'
     AND cs_mdhdr-mpgrp EQ 'ENCERRAR_COLHEITA'.
    CALL FUNCTION 'ZABS_FM_ENCERRAR_COLHEITA'
      CHANGING
        cs_mdhdr = cs_mdhdr
        ct_mditm = ct_mditm[]
        ct_mdatv = ct_mdatv[].
*--EOC-T_T.KONNO-06.02.21
  ELSE.
*--BOC- T_T.KONNO-05.27.21
    APPEND INITIAL LINE TO lt_tplnr
     ASSIGNING FIELD-SYMBOL(<lwa_tplnr>).
    IF sy-subrc EQ 0.
      <lwa_tplnr> = cs_mdhdr-tplnr_fl.
    ENDIF.

    CALL FUNCTION '/AGRI/GLFL_VIEW'
      EXPORTING
        it_tplnr       = lt_tplnr
      IMPORTING
        et_fldoc       = lt_fl_doc
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    READ TABLE lt_fl_doc INTO DATA(lwa_terrain) INDEX 1.
    IF sy-subrc EQ 0.
      IF lwa_terrain-x-flhdr-ownshp EQ lc_propriedade-proprio.
        DATA(lv_proprio) = abap_true.
      ELSEIF lc_propriedade-terceiros EQ lc_propriedade-terceiros.
        lv_proprio = abap_false.
      ENDIF.
    ENDIF.
*--EOC- T_T.KONNO-05.27.21

    IF cs_mdhdr-mdtyp EQ 'ZPTA'.
      IF cs_mdhdr-mpgrp EQ 'FAZ-ESTIMATIVA'.
        DO 2 TIMES.
          lv_index = sy-index.
          INSERT INITIAL LINE INTO TABLE lr_mpgrp
            ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
          IF sy-subrc EQ 0.
            <lwa_mpgrp> = 'IEQ'.
            CASE lv_index.
              WHEN '1'.
                <lwa_mpgrp>-low = 'CIT-ESTIMATIVA'.
              WHEN '2'.
                <lwa_mpgrp>-low = 'FAZ-ESTIMATIVA'.
            ENDCASE.
          ENDIF.
        ENDDO.

        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = cs_mdhdr-tplnr_fl
          IMPORTING
            output = lv_terreno.

        lv_cod_imovel = lv_terreno+0(6).
        lv_talhao = lv_terreno+7(5).
      ENDIF.

*-- Data de Liberação de Colheita
      IF cs_mdhdr-mpgrp EQ 'APLIC_AGROQUIMICOS'
      AND i_updkz EQ 'I'.
        READ TABLE ct_mdatv INTO DATA(lwa_mdatv_ref)
          WITH KEY atinn = lwa_atributo-cit_quimicos.
        IF sy-subrc EQ 0.
          IF lwa_mdatv_ref-atwrt IS NOT INITIAL.
            lv_matnr = lwa_mdatv_ref-atwrt(18).

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = lv_matnr
              IMPORTING
                output       = lv_matnr_in
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            IF sy-subrc <> 0.
              lv_matnr_in = lv_matnr.
            ENDIF.

            SELECT objek, atinn, atzhl, mafid, klart,
                   adzhl, dec_value_from, dec_value_to UP TO 1 ROWS
              FROM ausp
              INTO @DATA(lwa_ausp)
             WHERE objek = @lv_matnr_in
               AND atinn = @lwa_atributo-dias_carencia
               AND klart = @lc_cl_material.
            ENDSELECT.

            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = 'CIT-DT-LIBERA-COL'
                IMPORTING
                  output = lv_libera_colheita.

              READ TABLE ct_mdatv INTO DATA(lwa_data_aplic)
                WITH KEY atinn = lwa_atributo-data_aplicacao.
              IF sy-subrc EQ 0.
                CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
                  EXPORTING
                    float_imp  = lwa_data_aplic-atflv
                    format_imp = lv_i
                  IMPORTING
                    char_exp   = lv_data_apl_char.
              ENDIF.

              IF lv_data_apl_char IS NOT INITIAL.
                lv_data_apl_datum = lv_data_apl_char.
                lv_dias_carencia = lwa_ausp-dec_value_from.
                ADD lv_dias_carencia TO lv_data_apl_datum.
                CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                  EXPORTING
                    date                      = lv_data_apl_datum
                  EXCEPTIONS
                    plausibility_check_failed = 1
                    OTHERS                    = 2.

                IF sy-subrc EQ 0.
                  READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<lwa_libera_colheita>)
                    WITH KEY atinn = lv_libera_colheita.
                  IF sy-subrc NE 0.
                    INSERT INITIAL LINE INTO TABLE ct_mdatv
                      ASSIGNING <lwa_libera_colheita>.
                    <lwa_libera_colheita>-updkz = 'I'.
                  ELSE.
                    IF <lwa_libera_colheita>-updkz IS INITIAL.
                      <lwa_libera_colheita>-updkz = 'U'.
                    ENDIF.
                  ENDIF.
                  IF <lwa_libera_colheita> IS ASSIGNED.
                    <lwa_libera_colheita>-mdocm = cs_mdhdr-mdocm.
                    <lwa_libera_colheita>-atinn = lv_libera_colheita.
                    <lwa_libera_colheita>-atcod = lwa_mdatv_ref-atcod.
                    lv_data_apl_char = lv_data_apl_datum.
                    <lwa_libera_colheita>-atflv = lv_data_apl_char.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE ct_mdatv INTO lwa_mdatv_ref
            WITH KEY atinn = lwa_atributo-data_aplicacao.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
              EXPORTING
                input  = 'CIT-DT-LIBERA-COL'
              IMPORTING
                output = lv_libera_colheita.

            UNASSIGN <lwa_libera_colheita>.
            READ TABLE ct_mdatv ASSIGNING <lwa_libera_colheita>
              WITH KEY atinn = lv_libera_colheita.
            IF sy-subrc NE 0.
              INSERT INITIAL LINE INTO TABLE ct_mdatv
                ASSIGNING <lwa_libera_colheita>.
              <lwa_libera_colheita>-updkz = 'I'.
            ELSE.
              IF <lwa_libera_colheita>-updkz  IS INITIAL.
                <lwa_libera_colheita>-updkz = 'U'.
              ENDIF.
            ENDIF.
            IF <lwa_libera_colheita> IS ASSIGNED.
              MOVE-CORRESPONDING lwa_mdatv_ref TO <lwa_libera_colheita>.
              <lwa_libera_colheita>-atinn = lv_libera_colheita.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: lwa_mdatv_ref, lv_matnr, lwa_ausp,
               lv_libera_colheita, lv_data_apl_char.
        UNASSIGN: <lwa_libera_colheita>.
      ENDIF.
    ENDIF.

    SELECT SINGLE clint
      FROM klah
      INTO @DATA(lv_clint)
      WHERE class = @cs_mdhdr-mpgrp.

    IF sy-subrc = 0.
      SELECT *
        FROM zabst_ksml
        INTO TABLE @DATA(lt_ksml)
        WHERE clint = @lv_clint.
      IF sy-subrc = 0.
        SELECT atinn, adzhl, atnam,
               atfor, anzst, anzdz
          FROM cabn
          INTO TABLE @DATA(lt_cabn)
          FOR ALL ENTRIES IN @lt_ksml
         WHERE atnam = @lt_ksml-atnam.
      ENDIF.
    ENDIF.

    LOOP AT ct_mdatv INTO DATA(lwa_mdatv).
      READ TABLE lt_cabn INTO DATA(lwa_cabn)
        WITH KEY atinn = lwa_mdatv-atinn.
      IF sy-subrc EQ 0.
        READ TABLE lt_ksml INTO DATA(lwa_ksml)
          WITH KEY atnam = lwa_cabn-atnam.
        IF sy-subrc = 0.
          DATA(lv_data) = lwa_ksml-ident(1).
          lwa_attributes-atflv = lwa_mdatv-atflv.
          lwa_attributes-atnam = lwa_ksml-atnam.
          lwa_attributes-ident = lwa_ksml-ident.
          APPEND lwa_attributes TO lt_attributes.
        ENDIF.
      ENDIF.
      CLEAR: lwa_attributes, lwa_ksml, lwa_cabn, lwa_mdatv.
    ENDLOOP.

    IF lt_attributes[] IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_ksml INTO lwa_ksml WHERE formula IS NOT INITIAL.
      CLEAR: lwa_cabn.
      READ TABLE lt_cabn INTO lwa_cabn
        WITH KEY atnam = lwa_ksml-atnam.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<fs_mdatv>) INDEX 1.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      lwa_mdatv = <fs_mdatv>.
      lwa_mdatv-atinn = lwa_cabn-atinn.

      CALL FUNCTION 'ZABS_FM_EVAL_FORMULA'
        EXPORTING
          iv_formula    = lwa_ksml-formula
          it_attributes = lt_attributes
        IMPORTING
          ev_subrc      = lv_subrc
          ev_message    = lv_message
          ev_value      = lv_value.
      IF lv_subrc IS INITIAL.
        lwa_mdatv-atflv = lv_value.
        APPEND lwa_mdatv TO ct_mdatv.
      ENDIF.
    ENDLOOP.

    lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
    lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).

    DATA(lv_insert_notes) = abap_false.
    LOOP AT lt_formatted_stack INTO DATA(ls_formatted_stack).
      IF ls_formatted_stack-progname EQ 'ZABS_REP_MEASUREM_DOC_CREATE'.
        lv_insert_notes = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_insert_notes EQ abap_true
    AND i_updkz EQ 'I'.
      ASSIGN ('(/AGRI/SAPLGNOTES)GT_NOTES_BUFFER[]') TO <lt_std_notes_buffer>.
      ASSIGN ('(ZABS_REP_MEASUREM_DOC_CREATE)GT_NOTES_BUFFER[]') TO <lt_cst_notes_buffer>.
      IF <lt_std_notes_buffer> IS ASSIGNED
      AND <lt_cst_notes_buffer> IS ASSIGNED.
        READ TABLE <lt_cst_notes_buffer> ASSIGNING FIELD-SYMBOL(<ls_custom>) INDEX 1.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'OBJKEY' OF STRUCTURE <ls_custom> TO FIELD-SYMBOL(<lv_objkey>).
          IF sy-subrc EQ 0.
            <lv_objkey> = cs_mdhdr-mdocm.
            <lt_std_notes_buffer>[] = <lt_cst_notes_buffer>[].
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_glmd_all~document_check.

  DATA: lv_safra   TYPE char4,
        lv_florada TYPE char2,
        lv_mpgrp   TYPE /agri/glmpgrp,
        lv_msgno   TYPE msgno,
        ls_message LIKE LINE OF ct_messages.

  IF flt_val EQ 'ZPTA'
  AND is_mdhdr-mpgrp EQ 'ENCERRAR_FLORADA'.
    SELECT mdocm
      FROM /agri/glmdhdr
      INTO TABLE @DATA(tl_mdhdr)
      WHERE mpgrp EQ 'ENCERRAR_FLORADA'
        AND canceled EQ @space.

    IF it_mdatv IS NOT INITIAL.
      READ TABLE it_mdatv INTO DATA(sl_mdatv) WITH KEY atinn = '0000015025'.
      IF sy-subrc EQ 0.
        lv_safra = sl_mdatv-atzhl.
      ENDIF.

      READ TABLE it_mdatv INTO sl_mdatv WITH KEY atinn = '0000014996'.
      IF sy-subrc EQ 0.
        lv_florada = sl_mdatv-atzhl.
      ENDIF.
    ENDIF.

    IF tl_mdhdr IS NOT INITIAL.
      SELECT mdocm, atinn, atwrt
        FROM /agri/glmdatv
        INTO TABLE @DATA(tl_mdatv)
        FOR ALL ENTRIES IN @tl_mdhdr
        WHERE mdocm EQ @tl_mdhdr-mdocm.
    ENDIF.

    DELETE tl_mdatv WHERE atinn NE lv_safra.
    DELETE tl_mdatv WHERE atinn NE lv_florada.
    IF tl_mdatv IS NOT INITIAL.
      lv_msgno = '198'.
      ls_message-msgid = 'ZABS_MSGCLS'.
      ls_message-msgno = lv_msgno.
      ls_message-msgty = 'E'.
      APPEND ls_message TO ct_messages.
      EXIT.
    ENDIF.

*-- Checking for the Second condition
    SELECT tplnr_fl,ownshp
      FROM /agri/glflot
      INTO TABLE @DATA(lt_flot)
      WHERE tplnr_fl EQ @is_mdhdr-tplnr_fl.

    SORT lt_flot BY tplnr_fl.
    DELETE ADJACENT DUPLICATES FROM lt_flot COMPARING tplnr_fl.
    READ TABLE lt_flot INTO DATA(ls_flot) INDEX 1.
    IF ls_flot-ownshp = 'OW'.
      lv_mpgrp = 'FAZ-ESTIMATA'.
    ELSEIF ls_flot-ownshp = 'TP'.
      lv_mpgrp = 'CIT-ESTIMATA'.
    ENDIF.

    CLEAR tl_mdhdr.
    SELECT mdocm
     FROM /agri/glmdhdr
     INTO TABLE tl_mdhdr
     WHERE mpgrp EQ lv_mpgrp
       AND canceled EQ space.
    IF sy-subrc EQ 0.
      CLEAR tl_mdatv.
      SELECT mdocm, atinn, atwrt
       FROM /agri/glmdatv
       INTO TABLE @tl_mdatv
       FOR ALL ENTRIES IN @tl_mdhdr
       WHERE mdocm EQ @tl_mdhdr-mdocm.
      DELETE tl_mdatv WHERE atinn NE lv_safra.
      DELETE tl_mdatv WHERE atinn NE lv_florada.
      IF tl_mdatv IS INITIAL.
        lv_msgno = '199'.
        ls_message-msgid = 'ZABS_MSGCLS'.
        ls_message-msgno = lv_msgno.
        ls_message-msgty = 'E'.
        APPEND ls_message TO ct_messages.
        EXIT.
      ENDIF.

    ENDIF.

  ELSEIF flt_val EQ 'ZPTA'
     AND is_mdhdr-mpgrp EQ 'ENCERRAR_COLHEITA'.
    SELECT mdocm
      FROM /agri/glmdhdr
      INTO TABLE @tl_mdhdr
      WHERE mpgrp EQ 'ENCERRAR_COLHEITA'
        AND canceled EQ @space.

    IF it_mdatv IS NOT INITIAL.
      READ TABLE it_mdatv INTO sl_mdatv WITH KEY atinn = '0000015025'.
      IF sy-subrc EQ 0.
        lv_safra = sl_mdatv-atzhl.
      ENDIF.

    ENDIF.

    IF tl_mdhdr IS NOT INITIAL.
      SELECT mdocm, atinn, atwrt
        FROM /agri/glmdatv
        INTO TABLE @tl_mdatv
        FOR ALL ENTRIES IN @tl_mdhdr
        WHERE mdocm EQ @tl_mdhdr-mdocm.
    ENDIF.

    DELETE tl_mdatv WHERE atinn NE lv_safra.
    IF tl_mdatv IS NOT INITIAL.
      lv_msgno = '200'.
      ls_message-msgid = 'ZABS_MSGCLS'.
      ls_message-msgno = lv_msgno.
      ls_message-msgty = 'E'.
      APPEND ls_message TO ct_messages.
      EXIT.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_glmd_all~status_flow_outcome_set.

  IF sy-cprog EQ 'ZABS_REP_MD_MASS_APPROVALS'
  OR sy-cprog EQ 'ZABS_REP_MDM_MASS_APPROVALS'
*--BOC-T_T.KONNO-05.28.21
  OR sy-cprog EQ 'ZABS_REP_APROVA_DOC_MED'
*--EOC-T_T.KONNO-05.28.21
  OR sy-ucomm EQ space.

    CALL FUNCTION 'ZABS_FM_CHECK_PROCESSOR'
      EXPORTING
        is_stsfl         = is_step_outcome-stsfl
        is_flstp         = is_step_outcome-flstp
        is_soutc         = is_step_outcome-soutc
        it_step_outcomes = it_step_outcomes
      CHANGING
        ct_messages      = ct_messages
      EXCEPTIONS
        outcome_not_set  = 1
        not_a_processor  = 2
        OTHERS           = 3.

    CASE sy-subrc.
      WHEN 1.
        c_stop_set = abap_true.
      WHEN 2.
        PERFORM f_get_susbst IN PROGRAM saplzabs_fg_md IF FOUND
                                         CHANGING c_stop_set.
        IF c_stop_set EQ abap_true.
          READ TABLE ct_messages INTO DATA(lwa_message) INDEX 1.
          IF sy-subrc EQ 0.
            MESSAGE ID lwa_message-msgid
              TYPE lwa_message-msgty NUMBER lwa_message-msgno
              WITH lwa_message-msgv1 lwa_message-msgv2
              lwa_message-msgv3 lwa_message-msgv4.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDMETHOD.


METHOD /agri/if_ex_badi_glmd_all~status_flow_step_set.
ENDMETHOD.


METHOD /agri/if_ex_badi_glmd_all~status_profile_trigger_set.
ENDMETHOD.
ENDCLASS.
