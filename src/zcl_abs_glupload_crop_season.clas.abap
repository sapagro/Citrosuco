class ZCL_ABS_GLUPLOAD_CROP_SEASON definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_ABS_GLUPLOAD_CROP_SEASON
*"* do not include other source files here!!!
  methods ATTRIBUTE_GROUPS_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods MEASUREMENT_DOCUMENT_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  class-methods READ_EXCEL_FILE
    importing
      !I_ROUTE type CHAR0256
    exporting
      !ET_SHEET type /AGRI/T_EXCEL_SHEET .
  methods TERRAINS_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ATTRIBUTE_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CROP_MASTER_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ES_GLCM_DOC type /AGRI/S_GLCM_DOC
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CROP_SEASON_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG
      !ET_TABLE type /AGRI/T_EXCEL_SHEET .
  methods IRRIGATION_EQUIP_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
protected section.
*"* protected components of class ZCL_ABS_GLUPLOAD_CROP_SEASON
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_ABS_GLUPLOAD_CROP_SEASON
*"* do not include other source files here!!!
  methods STRUCTURE_BUILD
    importing
      !I_STRUCTURE type TABNAME
      !I_SHEET type NUMC2
    exporting
      !ET_DD03L type THRPAD_ERD_DD03L
    changing
      !CT_TABLE type /AGRI/T_EXCEL_SHEET .
ENDCLASS.



CLASS ZCL_ABS_GLUPLOAD_CROP_SEASON IMPLEMENTATION.


METHOD ATTRIBUTE_CREATE.

  DATA:
        lt_table        TYPE /agri/t_excel_sheet,
        lwa_data        TYPE /agri/s_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l, "ishmed_t_dd03l,
        lwa_dd03l       TYPE dd03l,
        lt_gathdr       TYPE /agri/t_gathdr,
        lwa_gathdr      TYPE /agri/s_gathdr,
        lt_atda         TYPE /agri/t_gatda,
        lwa_atda        TYPE /agri/s_gatda,
        lt_cawn         TYPE /agri/t_gcawn,
        lwa_cawn        TYPE /agri/s_gcawn,
        lt_cawnt        TYPE /agri/t_gcawnt,
        lwa_cawnt       TYPE /agri/s_gcawnt,
        lwa_gcabnt      TYPE /agri/s_gcabnt,
        lt_gcabnt       TYPE /agri/t_gcabnt,
        lt_agcat        TYPE TABLE OF /agri/glagcat,
        lwa_agcat       TYPE  /agri/glagcat,
        lt_messages     TYPE /agri/t_gprolog,
        lwa_message     TYPE /agri/s_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_at01         TYPE TABLE OF /agri/s_at_sheet1,
        lt_at02         TYPE TABLE OF /agri/s_at_sheet2,
        lt_at03         TYPE TABLE OF /agri/s_at_sheet3,
        lt_at04         TYPE TABLE OF /agri/s_at_sheet4,
        lt_at05         TYPE TABLE OF /agri/s_at_sheet5,
        lv_row          TYPE /agri/s_excel_sheet-row.
*        lt_at06    type table of /agri/s_at_sheet6.
*        lt_tr07    TYPE TABLE OF /agri/s_tr_sheet7.


  FIELD-SYMBOLS: <lwa_at01> TYPE /agri/s_at_sheet1,
                 <lwa_at02> TYPE /agri/s_at_sheet2,
                 <lwa_at03> TYPE /agri/s_at_sheet3,
                 <lwa_at04> TYPE /agri/s_at_sheet4,
                 <lwa_at05> TYPE /agri/s_at_sheet5,
                 <lv_value> TYPE any.
*                 <lwa_at06> type /agri/s_at_sheet6,
*                 <lwa_tr07> TYPE /agri/s_tr_sheet7,



  CONSTANTS: BEGIN OF c_structures,
      at_01 TYPE tabname VALUE '/AGRI/S_AT_SHEET1',
      at_02 TYPE tabname VALUE '/AGRI/S_AT_SHEET2',
      at_03 TYPE tabname VALUE '/AGRI/S_AT_SHEET3',
      at_04 TYPE tabname VALUE '/AGRI/S_AT_SHEET4',
      at_05 TYPE tabname VALUE '/AGRI/S_AT_SHEET5',
*      at_06 type tabname value '/AGRI/S_AT_SHEET6',
*      ag_07 TYPE tabname VALUE '/AGRI/S_TR_SHEET7',
      ag_in TYPE updkz_d VALUE 'I',
      END OF c_structures.

  lt_table[] = it_table[].


  DELETE lt_table WHERE row BETWEEN 1 AND 15.    "#EC CI_STDSEQ
  DELETE lt_table WHERE column = 1.              "#EC CI_STDSEQ

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_at01 ASSIGNING <lwa_at01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at01 ASSIGNING <lwa_at01>.
      lv_row = lwa_data-row.
    ENDIF.

    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_at01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 02.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_at02 ASSIGNING <lwa_at02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at02 ASSIGNING <lwa_at02>.
      lv_row = lwa_data-row.
    ENDIF.

    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_at02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  "Sheet 3
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_03
      i_sheet     = 3
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 03.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_at03 ASSIGNING <lwa_at03>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at03 ASSIGNING <lwa_at03>.
      lv_row = lwa_data-row.
    ENDIF.

    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_at03> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  "Sheet 4
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_04
      i_sheet     = 4
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 04.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_at04 ASSIGNING <lwa_at04>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at04 ASSIGNING <lwa_at04>.
      lv_row = lwa_data-row.
    ENDIF.

    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_at04> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.


  "Sheet 5
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_05
      i_sheet     = 5
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 05.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_at05 ASSIGNING <lwa_at05>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at05 ASSIGNING <lwa_at05>.
      lv_row = lwa_data-row.
    ENDIF.

    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_at05> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_at01 ASSIGNING <lwa_at01>.
    "Attribute Header Data
    MOVE-CORRESPONDING  <lwa_at01> TO lwa_gathdr.
    IF lwa_gathdr-einhe NE abap_false.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lwa_gathdr-einhe
          language       = sy-langu
        IMPORTING
          output         = lwa_gathdr-einhe
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
        CLEAR lwa_message.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_LUNIT_OUTPUT'
        EXPORTING
          input          = lwa_gathdr-einhe
          language       = sy-langu
        IMPORTING
*         LONG_TEXT      =
          output         = lwa_gathdr-einhe
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
        CLEAR lwa_message.
      ENDIF.
    ENDIF.

    APPEND lwa_gathdr TO lt_gathdr.

    "Attributes Complex Data Type
    LOOP AT lt_at02 ASSIGNING <lwa_at02> WHERE atinn = <lwa_at01>-atinn.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_at02> TO lwa_atda.
      lwa_atda-updkz = c_structures-ag_in.
      APPEND lwa_atda TO lt_atda.
      CLEAR lwa_atda.
    ENDLOOP.

    "Characteristic Values
    LOOP AT lt_at03 ASSIGNING <lwa_at03> WHERE atinn = <lwa_at01>-atinn.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_at03> TO lwa_cawn.
      lwa_cawn-updkz = c_structures-ag_in.
      APPEND lwa_cawn TO lt_cawn.
      CLEAR lwa_cawn.
    ENDLOOP.

    "Characteristic Values Texts
    LOOP AT lt_at04 ASSIGNING <lwa_at04> WHERE atinn = <lwa_at01>-atinn.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_at04> TO lwa_cawnt.
      lwa_cawnt-updkz = c_structures-ag_in.
      APPEND lwa_cawnt TO lt_cawnt.
      CLEAR lwa_cawnt.
    ENDLOOP.

    "Attribute Header Descriptions
    LOOP AT lt_at05 ASSIGNING <lwa_at05> WHERE atinn = <lwa_at01>-atinn.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_at05> TO lwa_gcabnt.
      lwa_gcabnt-updkz = c_structures-ag_in.
      APPEND lwa_gcabnt TO lt_gcabnt.
      CLEAR lwa_gcabnt.
    ENDLOOP.

*ENHANCEMENT-POINT ATTRIBUTE_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/GLAT_CREATE_MASS'
      EXPORTING
        i_agcat               = <lwa_at01>-agcat
        i_message_log_init    = 'X'
*       i_plcrule             =
        i_commit              = 'X'
        i_set_update_task     = 'X'
        it_athdr              = lt_gathdr
        it_cabnt              = lt_gcabnt
        it_atda               = lt_atda
        it_cawn               = lt_cawn
        it_cawnt              = lt_cawnt
*       IT_ATVA               =
*       IT_ATKW               =
*       IT_ATTXT              =
      IMPORTING
*       ET_ATHDR              =
        et_messages           = lt_messages
      EXCEPTIONS
        incomplete_parameters = 1
        invalid_parameters    = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
      lwa_message-msgid = sy-msgid.
      lwa_message-msgno = sy-msgno.
      lwa_message-msgty = sy-msgty.
      lwa_message-msgv1 = sy-msgv1.
      lwa_message-msgv2 = sy-msgv2.
      lwa_message-msgv3 = sy-msgv3.
      lwa_message-msgv4 = sy-msgv4.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    APPEND LINES OF lt_messages TO lt_mess_collect.
    CLEAR: lt_gathdr,
           lt_atda[],
           lt_cawn[],
           lt_cawnt[],
           lwa_message.
  ENDLOOP.

  et_messages = lt_mess_collect.

ENDMETHOD.


METHOD ATTRIBUTE_GROUPS_CREATE.
  DATA: lwa_data         TYPE /agri/s_excel_sheet,
        lwa_klah         TYPE /agri/s_gklah,
        lt_klah          TYPE /agri/t_gklah,
        lt_klah2         TYPE /agri/t_gklah,
        lwa_agha         TYPE /agri/s_glagha,
        lt_agha          TYPE /agri/t_glagha,
        lt_agha2         TYPE /agri/t_glagha,
        lwa_swor         TYPE /agri/s_gswor,
        lt_swor          TYPE /agri/t_gswor,
        lwa_ksml         TYPE /agri/s_gksml,
        lt_ksml          TYPE /agri/t_gksml,
        lt_ksml2         TYPE /agri/t_gksml,
        lt_klah_e        TYPE /agri/t_gklah,
        lt_messages      TYPE /agri/t_gprolog,
        lwa_message      TYPE /agri/s_gprolog,
        lt_mass_messages TYPE /agri/t_gprolog,
        lv_clint         TYPE clint,
        lv_posnr         TYPE kposnr,
        lt_table         TYPE /agri/t_excel_sheet,
        lt_dd03l         TYPE thrpad_erd_dd03l,  "ishmed_t_dd03l,
        lwa_dd03l        TYPE dd03l,
        lt_ag01          TYPE STANDARD TABLE OF /agri/s_ag_sheet1,
        lt_ag02          TYPE STANDARD TABLE OF /agri/s_ag_sheet2,
        lv_row           TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_ag01>  TYPE /agri/s_ag_sheet1,
                 <lwa_ag02>  TYPE /agri/s_ag_sheet2,
                 <lwa_gklah> TYPE /agri/s_gklah,
                 <lwa_ksml>  TYPE /agri/s_gksml,
                 <lwa_agha>  TYPE /agri/s_glagha,
                 <lv_value>  TYPE any.

  CONSTANTS: BEGIN OF c_structures,
    ag_01 TYPE tabname VALUE '/AGRI/S_AG_SHEET1',
    ag_02 TYPE tabname VALUE '/AGRI/S_AG_SHEET2',
    ag_in TYPE updkz_d VALUE 'I',
    END OF c_structures.


  lt_table[] = it_table[].
  DELETE lt_table WHERE row BETWEEN 1 AND 15          "#EC CI_STDSEQ
             OR column EQ 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ag_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 1.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_ag01 ASSIGNING <lwa_ag01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ag01 ASSIGNING <lwa_ag01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_ag01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_ag01 ASSIGNING <lwa_ag01>.
    lv_clint = sy-tabix.
    MOVE-CORRESPONDING <lwa_ag01> TO lwa_klah.
    lwa_klah-clint = lv_clint.
    lwa_klah-updkz = c_structures-ag_in.
    MOVE-CORRESPONDING <lwa_ag01> TO lwa_agha.
    lwa_agha-updkz = c_structures-ag_in.

    MOVE-CORRESPONDING <lwa_ag01> TO lwa_swor.
    lwa_swor-clint = lv_clint.
    lwa_swor-spras = sy-langu.
    lwa_swor-updkz = c_structures-ag_in.

    APPEND lwa_agha TO lt_agha. CLEAR lwa_agha.
    APPEND lwa_klah TO lt_klah. CLEAR lwa_klah.
    APPEND lwa_swor TO lt_swor. CLEAR lwa_swor.
  ENDLOOP.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ag_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 2.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_ag02 ASSIGNING <lwa_ag02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ag02 ASSIGNING <lwa_ag02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_ag02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_ag02 ASSIGNING <lwa_ag02>.
    AT NEW class.
      CLEAR lv_posnr.
    ENDAT.
    ADD 1 TO lv_posnr.
    MOVE-CORRESPONDING <lwa_ag02> TO lwa_ksml.
    READ TABLE lt_klah INTO lwa_klah                   "#EC CI_STDSEQ
                        WITH KEY class = <lwa_ag02>-class.
    IF sy-subrc = 0.
      lwa_ksml-clint = lwa_klah-clint.
      lwa_ksml-posnr = lv_posnr.
      lwa_ksml-updkz = c_structures-ag_in.
      APPEND lwa_ksml TO lt_ksml. CLEAR lwa_ksml.
    ENDIF.


  ENDLOOP.

  CLEAR lt_swor[].

  LOOP AT lt_klah ASSIGNING <lwa_gklah>.
    APPEND <lwa_gklah> TO lt_klah2.
    READ TABLE lt_ag01 ASSIGNING <lwa_ag01> WITH KEY class = <lwa_gklah>-class.  "#EC CI_STDSEQ
    IF sy-subrc = 0.
      LOOP AT lt_ksml ASSIGNING <lwa_ksml> WHERE clint = <lwa_gklah>-clint.     "#EC CI_STDSEQ
        APPEND <lwa_ksml> TO lt_ksml2.
      ENDLOOP.

      LOOP AT lt_agha ASSIGNING <lwa_agha> WHERE class = <lwa_gklah>-class.     "#EC CI_STDSEQ
        APPEND <lwa_agha> TO lt_agha2.
      ENDLOOP.
*{   INSERT         SS8K900620                                        1
*****Don't Create attribute group without attributes
      IF lt_klah2 IS NOT INITIAL AND
         lt_ksml2 IS INITIAL.
        lwa_message-msgid = '/AGRI/GLAG'.
        lwa_message-msgno = '002'.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_gklah>-class.
        APPEND lwa_message TO lt_mass_messages.
        CONTINUE.
      ENDIF.
*}   INSERT

*ENHANCEMENT-POINT ATTRIBUTE_GROUPS_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

      CALL FUNCTION '/AGRI/GLAG_CREATE_MASS'
        EXPORTING
          i_agcat               = <lwa_ag01>-agcat "'MP'
*         I_MESSAGE_LOG_INIT    = 'X'
*         I_PLCRULE             =
*         I_COMMIT              = 'X'
*         I_SET_UPDATE_TASK     = 'X'
          it_klah               = lt_klah2
          it_swor               = lt_swor
          it_ksml               = lt_ksml2
*         IT_AGTLN              =
          it_agha               = lt_agha2
        IMPORTING
          et_klah               = lt_klah_e
          et_messages           = lt_messages
        EXCEPTIONS
          invalid_parameters    = 1
          incomplete_parameters = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      APPEND LINES OF lt_messages TO lt_mass_messages.
    ENDIF.
    CLEAR: lt_klah2[],
           lt_ksml2[],
           lt_agha2[].
  ENDLOOP.

  et_messages = lt_mass_messages.

ENDMETHOD.


METHOD CROP_MASTER_CREATE.

  CONSTANTS: BEGIN OF c_structures,
               cm_01 TYPE tabname VALUE '/AGRI/S_CM_SHEET1',
               cm_02 TYPE tabname VALUE '/AGRI/S_CM_SHEET2',
               cm_03 TYPE tabname VALUE '/AGRI/S_CM_SHEET3',
               cm_04 TYPE tabname VALUE '/AGRI/S_CM_SHEET4',
               cm_05 TYPE tabname VALUE '/AGRI/S_CM_SHEET5',
               cm_06 TYPE tabname VALUE '/AGRI/S_CM_SHEET6',
               cm_07 TYPE tabname VALUE '/AGRI/S_CM_SHEET7',
               cm_08 TYPE tabname VALUE '/AGRI/S_CM_SHEET8',
               cm_09 TYPE tabname VALUE '/AGRI/S_CM_SHEET9',
               cm_in TYPE updkz_d VALUE 'I',
             END OF c_structures.

  TYPES: BEGIN OF ls_fldty,
           fldty TYPE /agri/glcmtyp,
           numki TYPE numki,
         END OF ls_fldty.

  DATA:   lt_table       TYPE /agri/t_excel_sheet,
          lwa_data       TYPE /agri/s_excel_sheet,
          lt_dd03l       TYPE thrpad_erd_dd03l,   "ishmed_t_dd03l,
          lwa_dd03l      TYPE dd03l,
          lv_balognr     TYPE balognr,
          lwa_message    TYPE /agri/s_gprolog,
          lt_messages    TYPE /agri/t_gprolog,
          lv_row         TYPE /agri/s_excel_sheet-row,

          lt_cm01        TYPE TABLE OF /agri/s_cm_sheet1,
          lt_cm02        TYPE TABLE OF /agri/s_cm_sheet2,
          lt_cm03        TYPE TABLE OF /agri/s_cm_sheet3,
          lt_cm04        TYPE TABLE OF /agri/s_cm_sheet4,
          lt_cm05        TYPE TABLE OF /agri/s_cm_sheet5,
          lt_cm06        TYPE TABLE OF /agri/s_cm_sheet6,
          lt_cm07        TYPE TABLE OF /agri/s_cm_sheet7,
          lt_cm08        TYPE TABLE OF /agri/s_cm_sheet8,
          lt_cm09        TYPE TABLE OF /agri/s_cm_sheet9,
          lt_tmpcmhdr    TYPE TABLE OF /agri/s_cm_sheet1,
          lt_tmpcmwrk    TYPE TABLE OF /agri/s_cm_sheet2,
          lt_tmpcmqch    TYPE TABLE OF /agri/s_cm_sheet3,
          lt_tmpcmvar    TYPE TABLE OF /agri/s_cm_sheet4,
          lt_tmpcmprs    TYPE TABLE OF /agri/s_cm_sheet5,
          lt_tmpcmpvr    TYPE TABLE OF /agri/s_cm_sheet6,
          lt_tmpcmprso   TYPE TABLE OF /agri/s_cm_sheet7,
          lt_tmpcmprst   TYPE TABLE OF /agri/s_cm_sheet8,
          lt_tmpcmdesc   TYPE TABLE OF /agri/s_cm_sheet9,
          lt_fldty       TYPE TABLE OF ls_fldty,
          ls_glcm_doc    TYPE /agri/s_glcm_doc,
          ls_tmpglcm_doc TYPE /agri/s_glcm_doc,
          lwa_cmhdr      TYPE /agri/s_glcmhdr,
          lwa_cmwrk      TYPE /agri/s_glcmwrk,
          lwa_cmqch      TYPE /agri/s_glcmqch,
          lwa_cmvar      TYPE /agri/s_glcmvar,
          lwa_cmprs      TYPE /agri/s_glcmprs,
          lwa_cmpvr      TYPE /agri/s_glcmpvr,
          lwa_cmprso     TYPE /agri/s_glcmprso,
          lwa_cmprst     TYPE /agri/s_glcmprst,
          lwa_cmdesc     TYPE /agri/s_glcmhdrt,
          lwa_fldty      TYPE ls_fldty.

  FIELD-SYMBOLS: <lwa_glcm_doc> TYPE /agri/s_glcm_doc,
                 <lwa_cmhdr>    TYPE /agri/s_glcmhdr,
                 <lwa_cm01>     TYPE /agri/s_cm_sheet1,
                 <lwa_cm02>     TYPE /agri/s_cm_sheet2,
                 <lwa_cm03>     TYPE /agri/s_cm_sheet3,
                 <lwa_cm04>     TYPE /agri/s_cm_sheet4,
                 <lwa_cm05>     TYPE /agri/s_cm_sheet5,
                 <lwa_cm06>     TYPE /agri/s_cm_sheet6,
                 <lwa_cm07>     TYPE /agri/s_cm_sheet7,
                 <lwa_cm08>     TYPE /agri/s_cm_sheet8,
                 <lwa_cm09>     TYPE /agri/s_cm_sheet9,
                 <lv_value>     TYPE any.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15
              OR column EQ 1.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                      WITH KEY sheet = 01.
  CHECK sy-subrc EQ 0.
  MOVE c_structures-cm_in TO es_glcm_doc-updkz.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-cm_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_cm01 ASSIGNING <lwa_cm01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_cm01 ASSIGNING <lwa_cm01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_cm01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_cm01 IS NOT INITIAL.
    SELECT fldty numki
      FROM /agri/tglcmtyp
      INTO TABLE lt_fldty
      FOR ALL ENTRIES IN lt_cm01
       WHERE fldty EQ lt_cm01-fldty.
    SORT lt_cm01 BY fldty.
  ENDIF.

  LOOP AT lt_cm01 ASSIGNING <lwa_cm01>.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = <lwa_cm01>-msehi
        language       = sy-langu
      IMPORTING
        output         = <lwa_cm01>-msehi
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
      lwa_message-msgid = sy-msgid.
      lwa_message-msgno = sy-msgno.
      lwa_message-msgty = sy-msgty.
      lwa_message-msgv1 = sy-msgv1.
      lwa_message-msgv2 = sy-msgv2.
      lwa_message-msgv3 = sy-msgv3.
      lwa_message-msgv4 = sy-msgv4.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_fldty INTO lwa_fldty                   "#EC CI_SORTED
                WITH KEY fldty = <lwa_cm01>-fldty.
*                BINARY SEARCH.
    IF sy-subrc = 0.
      IF lwa_fldty-numki IS NOT INITIAL.
        CLEAR <lwa_cm01>-cmnum.
      ENDIF.
      <lwa_cm01>-updkz = c_structures-cm_in.
      APPEND <lwa_cm01> TO lt_tmpcmhdr.
    ELSE.
      lwa_message-msgid  = 'ZUPDMD'.
      lwa_message-msgno  = '000'.
      lwa_message-msgty  = 'E'.
      lwa_message-msgv1 = <lwa_cm01>-fldty.
      APPEND lwa_message TO lt_messages.
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

  ENDLOOP.


  READ TABLE lt_table TRANSPORTING NO FIELDS
                       WITH KEY sheet = 02.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_02
        i_sheet     = 2
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 02.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm02 ASSIGNING <lwa_cm02>.
*      ENDAT.
     IF lv_row NE lwa_data-row.
       APPEND INITIAL LINE TO lt_cm02 ASSIGNING <lwa_cm02>.
       lv_row = lwa_data-row.
     ENDIF.

      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm02> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
   CLEAR lv_row.

    LOOP AT lt_cm02 ASSIGNING <lwa_cm02>.
      <lwa_cm02>-updkz = c_structures-cm_in.
      APPEND <lwa_cm02> TO lt_tmpcmwrk.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 03.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_03
        i_sheet     = 3
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 03.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm03 ASSIGNING <lwa_cm03>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
         APPEND INITIAL LINE TO lt_cm03 ASSIGNING <lwa_cm03>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm03> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm03 ASSIGNING <lwa_cm03>.
      <lwa_cm03>-updkz = c_structures-cm_in.
      APPEND <lwa_cm03> TO lt_tmpcmqch.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 04.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_04
        i_sheet     = 4
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 04.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm04 ASSIGNING <lwa_cm04>.
*      ENDAT.
       IF lv_row NE lwa_data-row.
         APPEND INITIAL LINE TO lt_cm04 ASSIGNING <lwa_cm04>.
         lv_row = lwa_data-row.
       ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm04> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm04 ASSIGNING <lwa_cm04>.
      <lwa_cm04>-updkz = c_structures-cm_in.
      APPEND <lwa_cm04> TO lt_tmpcmvar.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 05.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_05
        i_sheet     = 5
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 05.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm05 ASSIGNING <lwa_cm05>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm05 ASSIGNING <lwa_cm05>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm05> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA' OR lwa_data-fieldname = 'CPROS' OR lwa_data-fieldname = 'MATNR'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ELSEIF lwa_data-fieldname = 'MIUNT' OR lwa_data-fieldname = 'MAUNT'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = lwa_data-value
              language       = sy-langu
            IMPORTING
              output         = lwa_data-value
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                       INTO sy-msgli.
            lwa_message-msgid = sy-msgid.
            lwa_message-msgno = sy-msgno.
            lwa_message-msgty = sy-msgty.
            lwa_message-msgv1 = sy-msgv1.
            lwa_message-msgv2 = sy-msgv2.
            lwa_message-msgv3 = sy-msgv3.
            lwa_message-msgv4 = sy-msgv4.
            APPEND lwa_message TO lt_messages.
          ENDIF.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm05 ASSIGNING <lwa_cm05>.
      <lwa_cm05>-updkz = c_structures-cm_in.
      APPEND <lwa_cm05> TO lt_tmpcmprs.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 06.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_06
        i_sheet     = 6
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 06.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm06 ASSIGNING <lwa_cm06>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm06 ASSIGNING <lwa_cm06>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm06> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm06 ASSIGNING <lwa_cm06>.
      <lwa_cm06>-updkz = c_structures-cm_in.
      APPEND <lwa_cm06> TO lt_tmpcmpvr.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 07.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_07
        i_sheet     = 7
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 07.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm07 ASSIGNING <lwa_cm07>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm07 ASSIGNING <lwa_cm07>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm07> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm07 ASSIGNING <lwa_cm07>.
      <lwa_cm07>-updkz = c_structures-cm_in.
      APPEND <lwa_cm07> TO lt_tmpcmprso.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 08.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_08
        i_sheet     = 8
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 08.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm08 ASSIGNING <lwa_cm08>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm08 ASSIGNING <lwa_cm08>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm08> TO <lv_value>.
        IF lwa_data-fieldname EQ 'SPRAS'.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lwa_data-value
            IMPORTING
              output = lwa_data-value.
        ENDIF.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.
      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_cm08 ASSIGNING <lwa_cm08>.
      <lwa_cm08>-updkz = c_structures-cm_in.
      APPEND <lwa_cm08> TO lt_tmpcmprst.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 09.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_09
        i_sheet     = 9
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 09.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm09 ASSIGNING <lwa_cm09>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm09 ASSIGNING <lwa_cm09>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm09> TO <lv_value>.
        IF lwa_data-fieldname EQ 'SPRAS'.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lwa_data-value
            IMPORTING
              output = lwa_data-value.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm09 ASSIGNING <lwa_cm09>.
      <lwa_cm09>-updkz = c_structures-cm_in.
      APPEND <lwa_cm09> TO lt_tmpcmdesc.
    ENDLOOP.
  ENDIF.

  CHECK es_glcm_doc IS NOT INITIAL.

  LOOP AT lt_tmpcmhdr ASSIGNING <lwa_cm01>.
    MOVE-CORRESPONDING <lwa_cm01> TO ls_glcm_doc-x-cmhdr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_glcm_doc-x-cmhdr-cmnum
      IMPORTING
        output = ls_glcm_doc-x-cmhdr-cmnum.
    IF lt_tmpcmwrk IS NOT INITIAL.
      LOOP AT lt_tmpcmwrk ASSIGNING <lwa_cm02>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm02> TO lwa_cmwrk.
        lwa_cmwrk-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmwrk TO ls_glcm_doc-x-cmwrk.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmqch IS NOT INITIAL.
      LOOP AT lt_tmpcmqch ASSIGNING <lwa_cm03>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm03> TO lwa_cmqch.
        lwa_cmqch-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_cmqch-version
          IMPORTING
            output = lwa_cmqch-version.
        APPEND lwa_cmqch TO ls_glcm_doc-x-cmqch.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmvar IS NOT INITIAL.
      LOOP AT lt_tmpcmvar ASSIGNING <lwa_cm04>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm04> TO lwa_cmvar.
        lwa_cmvar-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmvar TO ls_glcm_doc-x-cmvar.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprs IS NOT INITIAL.
      LOOP AT lt_tmpcmprs ASSIGNING <lwa_cm05>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm05> TO lwa_cmprs.
        lwa_cmprs-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprs TO ls_glcm_doc-x-cmprs.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmpvr IS NOT INITIAL.
      LOOP AT lt_tmpcmpvr ASSIGNING <lwa_cm06>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm06> TO lwa_cmpvr.
        lwa_cmpvr-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmpvr TO ls_glcm_doc-x-cmpvr.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprso IS NOT INITIAL.
      LOOP AT lt_tmpcmprso ASSIGNING <lwa_cm07>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm07> TO lwa_cmprso.
        lwa_cmprso-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprso TO ls_glcm_doc-x-cmprso.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprst IS NOT INITIAL.
      LOOP AT lt_tmpcmprst ASSIGNING <lwa_cm08>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm08> TO lwa_cmprst.
        lwa_cmprst-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprst TO ls_glcm_doc-x-cmprst.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmdesc IS NOT INITIAL.
      LOOP AT lt_tmpcmdesc ASSIGNING <lwa_cm09>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm09> TO lwa_cmdesc.
        lwa_cmdesc-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmdesc TO ls_glcm_doc-x-cmdesc.
      ENDLOOP.
    ENDIF.
*{   INSERT         SS8K900620                                        1
***Date check
    DATA: lv_subrc TYPE sy-subrc.

    LOOP AT ls_glcm_doc-x-cmwrk INTO DATA(ls_cmwrk).
      IF ls_cmwrk-datab IS NOT INITIAL.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                            = ls_cmwrk-datab
         EXCEPTIONS
           plausibility_check_failed       = 1
           OTHERS                          = 2.
        IF sy-subrc <> 0.
          lwa_message-msgid = '/AGRI/GLAG'.
          lwa_message-msgno = '003'.
          lwa_message-msgty = 'E'.
          lwa_message-msgv1 = ls_glcm_doc-x-cmhdr-cmnum.
          lwa_message-msgv2 = ls_cmwrk-werks.
          APPEND lwa_message TO et_messages.
          lv_subrc = sy-subrc.
        ENDIF.
      ENDIF.

      IF ls_cmwrk-datbi IS NOT INITIAL.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                            = ls_cmwrk-datbi
         EXCEPTIONS
           plausibility_check_failed       = 1
           OTHERS                          = 2.
        IF sy-subrc <> 0.
          lwa_message-msgid = '/AGRI/GLAG'.
          lwa_message-msgno = '004'.
          lwa_message-msgty = 'E'.
          lwa_message-msgv1 = ls_glcm_doc-x-cmhdr-cmnum.
          lwa_message-msgv2 = ls_cmwrk-werks.
          APPEND lwa_message TO et_messages.
          lv_subrc = sy-subrc.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_subrc IS NOT INITIAL.
     CLEAR ls_glcm_doc.
     CONTINUE.
    ENDIF.
*}   INSERT

*ENHANCEMENT-POINT CROP_MASTER_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/GLCM_CREATE'
      EXPORTING
*       i_messages_display      = ' '
*       i_save_messages         = ' '
        i_commit_work           = 'X'
        is_cmhdr                = ls_glcm_doc-x-cmhdr
        it_cmwrk                = ls_glcm_doc-x-cmwrk
        it_cmqch                = ls_glcm_doc-x-cmqch
        it_cmvar                = ls_glcm_doc-x-cmvar
        it_cmprs                = ls_glcm_doc-x-cmprs
        it_cmpvr                = ls_glcm_doc-x-cmpvr
        it_cmprso               = ls_glcm_doc-x-cmprso
        it_cmprst               = ls_glcm_doc-x-cmprst
        it_cmdesc               = ls_glcm_doc-x-cmdesc
      IMPORTING
        es_glcm_doc             = ls_tmpglcm_doc
        et_messages             = lt_messages
        e_log_number            = lv_balognr
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.

    IF sy-subrc <> 0.
      IF sy-msgid IS INITIAL.
        lwa_message-msgid  = 'ZUPDMD'.
        lwa_message-msgno  = '001'.
        lwa_message-msgty  = 'E'.
        APPEND lwa_message TO lt_messages.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.


    APPEND LINES OF lt_messages TO et_messages.
    CLEAR: ls_glcm_doc,
           lwa_message.

  ENDLOOP.


ENDMETHOD.


METHOD crop_season_create.

  DATA: lt_cs         TYPE TABLE OF /agri/s_cs_sheet,
        lt_flcma      TYPE /agri/t_glflcma,
        lt_csdoc      TYPE /agri/t_glcs_doc,
        lwa_flcma     TYPE /agri/s_glflcma,
        lv_log_number TYPE balognr,
        lwa_data      TYPE /agri/s_excel_sheet,
        lt_dd03l      TYPE thrpad_erd_dd03l,   "ishmed_t_dd03l,
        lwa_dd03l     TYPE dd03l,
        lv_internal   TYPE n LENGTH 6,
        lwa_message   TYPE /agri/s_gprolog.

  FIELD-SYMBOLS: <lwa_cs>   TYPE /agri/s_cs_sheet,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_cons,
               cs    TYPE tabname VALUE '/AGRI/S_CS_SHEET',
               cs_in TYPE updkz   VALUE 'I',
             END OF c_cons.

  et_table[] = it_table[].
  DELETE et_table WHERE row BETWEEN 1 AND 15.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_cons-cs
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = et_table ).

  LOOP AT et_table INTO lwa_data.
    AT NEW row.
      APPEND INITIAL LINE TO lt_cs ASSIGNING <lwa_cs>.
    ENDAT.
    LOOP AT lt_dd03l INTO lwa_dd03l
      WHERE position = lwa_data-column.
      ASSIGN COMPONENT lwa_data-fieldname
        OF STRUCTURE <lwa_cs> TO <lv_value>.
      IF sy-subrc EQ 0.
*        IF lwa_dd03l-domname EQ 'DATUM'
*        OR lwa_dd03l-domname EQ 'DATS'.
*          CALL FUNCTION 'KCD_EXCEL_DATE_CONVERT'
*            EXPORTING
*              excel_date  = lwa_data-value
*              date_format = 'TMJ'
*            IMPORTING
*              sap_date    = <lv_value>.
*        ELSEIF lwa_dd03l-domname EQ 'ZABS_DOM_MENG30'.
        IF lwa_dd03l-domname EQ 'ZABS_DOM_MENG30'.
          TRANSLATE lwa_data-value USING ',.'.
          <lv_value> = lwa_data-value.
        ELSEIF lwa_dd03l-domname EQ 'MATNR'
            OR lwa_dd03l-domname EQ '/AGRI/GLYMATNR'.
          IF lwa_data-value IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = lwa_data-value
              IMPORTING
                output       = <lv_value>
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
          ENDIF.
        ELSEIF lwa_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = lwa_data-value
            IMPORTING
              output     = <lv_value>
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ELSEIF lwa_dd03l-domname EQ 'AUFNR'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_data-value
            IMPORTING
              output = <lv_value>.
        ELSE.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_cs ASSIGNING <lwa_cs>.
    MOVE-CORRESPONDING <lwa_cs> TO lwa_flcma.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_flcma-cmnum
      IMPORTING
        output = lwa_flcma-cmnum.
*    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
*      EXPORTING
*        i_input  = lwa_flcma-tplnr_fl
**       I_NO_MESSAGE       =
*      IMPORTING
*        e_output = lwa_flcma-tplnr_fl
**       ES_RETURN          =
*      EXCEPTIONS
*        OTHERS   = 1.
*    IF sy-subrc EQ 0.
    lwa_flcma-datab(4) = <lwa_cs>-gyear.
    lwa_flcma-updkz    = c_cons-cs_in.
    APPEND lwa_flcma TO lt_flcma.
*    ENDIF.
    CLEAR lwa_flcma.
  ENDLOOP.

*ENHANCEMENT-POINT CROP_SEASON_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

  CALL FUNCTION '/AGRI/GLCS_CREATE'
    EXPORTING
*     I_MESSAGES_DISPLAY      = ' '
*     I_SAVE_MESSAGES         = ' '
*     I_COMMIT_WORK           = 'X'
*     I_SYNCHRONOUS           = ' '
      it_flcma                = lt_flcma
    IMPORTING
      et_csdoc                = lt_csdoc
      et_messages             = et_messages
      e_log_number            = lv_log_number
    EXCEPTIONS
      no_documents_to_process = 1
      no_authorization        = 2
      creation_failed         = 3
      OTHERS                  = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
    lwa_message-msgid = sy-msgid.
    lwa_message-msgno = sy-msgno.
    lwa_message-msgty = sy-msgty.
    lwa_message-msgv1 = sy-msgv1.
    lwa_message-msgv2 = sy-msgv2.
    lwa_message-msgv3 = sy-msgv3.
    lwa_message-msgv4 = sy-msgv4.
    APPEND lwa_message TO et_messages.
  ENDIF.


ENDMETHOD.


METHOD IRRIGATION_EQUIP_CREATE.

  CONSTANTS: BEGIN OF c_structures,
      ie_01 TYPE tabname VALUE '/AGRI/S_IE_SHEET1',
      ie_02 TYPE tabname VALUE '/AGRI/S_IE_SHEET2',
      ie_03 TYPE tabname VALUE '/AGRI/S_IE_SHEET3',
      ie_04 TYPE tabname VALUE '/AGRI/S_IE_SHEET4',
      ie_05 TYPE tabname VALUE '/AGRI/S_IE_SHEET5',
      ie_in TYPE updkz_d VALUE 'I',
      END OF c_structures.

  TYPES: BEGIN OF ls_irtyp,
           irtyp TYPE /agri/fmirtyp,
           numki TYPE numki,
         END OF ls_irtyp.

  DATA:   lt_table        TYPE /agri/t_excel_sheet,
          lwa_data        TYPE /agri/s_excel_sheet,
          lt_dd03l        TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
          lwa_dd03l       TYPE dd03l,
          lv_balognr      TYPE balognr,
          lwa_message     TYPE /agri/s_gprolog,
          lt_messages     TYPE /agri/t_gprolog,
          lv_row          TYPE /agri/s_excel_sheet-row,

          lt_ie01         TYPE TABLE OF /agri/s_ie_sheet1,
          lt_ie02         TYPE TABLE OF /agri/s_ie_sheet2,
          lt_ie03         TYPE TABLE OF /agri/s_ie_sheet3,
          lt_ie04         TYPE TABLE OF /agri/s_ie_sheet4,
          lt_ie05         TYPE TABLE OF /agri/s_ie_sheet5,
          lt_tpmirhdr     TYPE TABLE OF /agri/s_ie_sheet1,
          lt_tmpirwrk     TYPE TABLE OF /agri/s_ie_sheet2,
          lt_tmpirflo     TYPE TABLE OF /agri/s_ie_sheet3,
          lt_tmpirmea     TYPE TABLE OF /agri/s_ie_sheet4,
          lt_tmpirhdrt    TYPE TABLE OF /agri/s_ie_sheet5,
          lt_klah         TYPE TABLE OF klah,
          lt_irtyp        TYPE TABLE OF ls_irtyp,
          ls_klah         TYPE klah,
          ls_fmir_doc     TYPE /agri/s_fmir_doc,
          ls_tmpfmir_doc  TYPE /agri/s_fmir_doc,
          lwa_irwrk       TYPE /agri/s_fmirwrk,
          lwa_irflo       TYPE /agri/s_fmirflo,
          lwa_irmea       TYPE /agri/s_fmirmea,
          lwa_irhdrt      TYPE /agri/s_fmirhdrt,
          lwa_irtyp       TYPE ls_irtyp.

  FIELD-SYMBOLS: <lwa_ie01>     TYPE /agri/s_ie_sheet1,
                 <lwa_ie02>     TYPE /agri/s_ie_sheet2,
                 <lwa_ie03>     TYPE /agri/s_ie_sheet3,
                 <lwa_ie04>     TYPE /agri/s_ie_sheet4,
                 <lwa_ie05>     TYPE /agri/s_ie_sheet5,
                 <lv_value>     TYPE any.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15   "#EC CI_STDSEQ
              OR column EQ 1.

  READ TABLE lt_table TRANSPORTING NO FIELDS   "#EC CI_STDSEQ
                      WITH KEY sheet = 01.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_01
        i_sheet     = 1
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 01.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie01 ASSIGNING <lwa_ie01>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie01 ASSIGNING <lwa_ie01>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie01> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    IF lt_ie01 IS NOT INITIAL.
      SELECT irtyp numki
        FROM /agri/tfmirtyp
       INTO TABLE lt_irtyp
        FOR ALL ENTRIES IN lt_ie01
        WHERE irtyp EQ lt_ie01-irtyp.
      SORT lt_irtyp BY irtyp.
    ENDIF.

    LOOP AT lt_ie01 ASSIGNING <lwa_ie01>.
      READ TABLE lt_irtyp INTO lwa_irtyp
                     WITH KEY irtyp = <lwa_ie01>-irtyp
                     BINARY SEARCH.
      IF lwa_irtyp-numki IS NOT INITIAL.
        CLEAR <lwa_ie01>-equnr.
      ENDIF.
      <lwa_ie01>-updkz = c_structures-ie_in.
      APPEND <lwa_ie01> TO lt_tpmirhdr.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 02.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_02
        i_sheet     = 2
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 02.   "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie02 ASSIGNING <lwa_ie02>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie02 ASSIGNING <lwa_ie02>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie02> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_ie02 ASSIGNING <lwa_ie02>.
      <lwa_ie02>-updkz = c_structures-ie_in.
      APPEND <lwa_ie02> TO lt_tmpirwrk.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 03.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_03
        i_sheet     = 3
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 03.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie03 ASSIGNING <lwa_ie03>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie03 ASSIGNING <lwa_ie03>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie03> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

*BREAK-POINT.
    LOOP AT lt_ie03 ASSIGNING <lwa_ie03>.
      <lwa_ie03>-updkz = c_structures-ie_in.
      APPEND <lwa_ie03> TO lt_tmpirflo.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 04.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_04
        i_sheet     = 4
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 04. "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie04 ASSIGNING <lwa_ie04>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie04 ASSIGNING <lwa_ie04>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie04> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    IF lt_ie04 IS NOT INITIAL.
      SELECT * FROM  klah "#EC CI_ALL_FIELDS_NEEDED
        INTO TABLE lt_klah
        FOR ALL ENTRIES IN lt_ie04
        WHERE class EQ lt_ie04-class.
      SORT lt_klah BY class.
    ENDIF.

*BREAK-POINT.
    LOOP AT lt_ie04 ASSIGNING <lwa_ie04>.
      READ TABLE lt_klah INTO ls_klah
                    WITH KEY class = <lwa_ie04>-class
                    BINARY SEARCH.
      <lwa_ie04>-class = ls_klah-class.
      <lwa_ie04>-updkz = c_structures-ie_in.
      APPEND <lwa_ie04> TO lt_tmpirmea.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS   "#EC CI_STDSEQ
                       WITH KEY sheet = 05.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_05
        i_sheet     = 5
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 05.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie05 ASSIGNING <lwa_ie05>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie05 ASSIGNING <lwa_ie05>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie05> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_ie05 ASSIGNING <lwa_ie05> WHERE spras <> sy-langu. "#EC CI_STDSEQ
      <lwa_ie05>-updkz = c_structures-ie_in.
      APPEND <lwa_ie05> TO lt_tmpirhdrt.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_tpmirhdr ASSIGNING <lwa_ie01>.
    MOVE-CORRESPONDING <lwa_ie01> TO ls_fmir_doc-x-irhdr.
    IF lt_tmpirwrk IS NOT INITIAL.
      LOOP AT lt_tmpirwrk ASSIGNING <lwa_ie02>
                        WHERE contr EQ <lwa_ie01>-contr.  "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie02> TO lwa_irwrk.
        APPEND lwa_irwrk TO ls_fmir_doc-x-irwrk.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirflo IS NOT INITIAL.
      LOOP AT lt_tmpirflo ASSIGNING <lwa_ie03>
                        WHERE contr EQ <lwa_ie01>-contr. "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie03> TO lwa_irflo.
        APPEND lwa_irflo TO ls_fmir_doc-x-irflo.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirmea IS NOT INITIAL.
      LOOP AT lt_tmpirmea ASSIGNING <lwa_ie04>
                        WHERE contr EQ <lwa_ie01>-contr. "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie04> TO lwa_irmea.
        APPEND lwa_irmea TO ls_fmir_doc-x-irmea.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirhdrt IS NOT INITIAL.
      LOOP AT lt_tmpirhdrt ASSIGNING <lwa_ie05>
                        WHERE contr EQ <lwa_ie01>-contr.  "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie05> TO lwa_irhdrt.
        APPEND lwa_irhdrt TO ls_fmir_doc-x-irdes.
      ENDLOOP.
    ENDIF.

*ENHANCEMENT-POINT IRRIGATION_EQUIP_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/FMIR_CREATE'
      EXPORTING
        i_messages_display      = ' '
        i_save_messages         = ' '
        i_commit_work           = 'X'
        is_irhdr                = ls_fmir_doc-x-irhdr
        it_irwrk                = ls_fmir_doc-x-irwrk
        it_irflo                = ls_fmir_doc-x-irflo
        it_irmea                = ls_fmir_doc-x-irmea
        it_irdes                = ls_fmir_doc-x-irdes
      IMPORTING
        es_fmir_doc             = ls_fmir_doc
        et_messages             = lt_messages
*       e_log_number            =
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
      lwa_message-msgid = sy-msgid.
      lwa_message-msgno = sy-msgno.
      lwa_message-msgty = sy-msgty.
      lwa_message-msgv1 = sy-msgv1.
      lwa_message-msgv2 = sy-msgv2.
      lwa_message-msgv3 = sy-msgv3.
      lwa_message-msgv4 = sy-msgv4.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    APPEND LINES OF lt_messages TO et_messages.
    CLEAR: ls_fmir_doc-x-irhdr, ls_tmpfmir_doc, ls_fmir_doc.

  ENDLOOP.






ENDMETHOD.


METHOD MEASUREMENT_DOCUMENT_CREATE.
  DATA: lt_md01     TYPE TABLE OF /agri/s_md_sheet1,
        lt_md02     TYPE TABLE OF /agri/s_md_sheet2,
        lt_mdtyp    TYPE TABLE OF /agri/tglmdtyp,
        ls_mdtyp    TYPE /agri/tglmdtyp,
        lwa_data    TYPE /agri/s_excel_sheet,
        lv_posnr    TYPE /agri/glmdocitm,
        lwa_mdhdr   TYPE /agri/s_glmdhdr,
        lt_mdhdr    TYPE /agri/t_glmdhdr,
        lwa_mditm   TYPE /agri/s_glmditm,
        lt_mditm    TYPE /agri/t_glmditm,
        lwa_mdatv   TYPE /agri/s_glmdatv,
        lt_mdatv    TYPE /agri/t_glmdatv,
        lt_mditx    TYPE /agri/t_glmditm,
        lwa_mddoc   TYPE /agri/s_glmd_doc,
        lt_mddoc    TYPE /agri/t_glmd_doc,
        lwa_message TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog,
        lt_table    TYPE /agri/t_excel_sheet,
        lt_dd03l    TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
        lwa_dd03l   TYPE dd03l,
        lv_row      TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_md01> TYPE /agri/s_md_sheet1,
                 <lwa_md02> TYPE /agri/s_md_sheet2,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               md_01 TYPE tabname VALUE '/AGRI/S_MD_SHEET1',
               md_02 TYPE tabname VALUE '/AGRI/S_MD_SHEET2',
               md_in TYPE updkz_d VALUE 'I',
             END OF c_structures.

  REFRESH: et_messages.

  lt_table[] = it_table[].
  DELETE lt_table WHERE row BETWEEN 1 AND 15   "#EC CI_STDSEQ
             OR column EQ 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-md_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 1.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_md01 ASSIGNING <lwa_md01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_md01 ASSIGNING <lwa_md01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_md01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_md01 ASSIGNING <lwa_md01>.
    MOVE-CORRESPONDING <lwa_md01> TO lwa_mdhdr.
    lwa_mdhdr-updkz = c_structures-md_in.
*    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
*      EXPORTING
*        input                      = lwa_mdhdr-tplnr_fl
*     IMPORTING
*       OUTPUT                     = lwa_mdhdr-tplnr_fl
*     EXCEPTIONS
*       NOT_FOUND                  = 1
*       OTHERS                     = 2
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
      EXPORTING
        i_input  = lwa_mdhdr-tplnr_fl
*       I_NO_MESSAGE       =
      IMPORTING
        e_output = lwa_mdhdr-tplnr_fl
*       ES_RETURN          =
      .

    APPEND lwa_mdhdr TO lt_mdhdr. CLEAR lwa_mdhdr.
  ENDLOOP.

  IF lt_mdhdr IS NOT INITIAL.
    SELECT * FROM /agri/tglmdtyp
   INTO TABLE lt_mdtyp
      FOR ALL ENTRIES IN lt_mdhdr
      WHERE mdtyp EQ lt_mdhdr-mdtyp.
    SORT lt_mdtyp BY mdtyp.
  ENDIF.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-md_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 2.
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_md02 ASSIGNING <lwa_md02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_md02 ASSIGNING <lwa_md02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column. "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_md02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mdatv.
    lwa_mdatv-updkz = c_structures-md_in.
    SELECT SINGLE c~atinn g~atbez_uc INTO (lwa_mdatv-atinn, lwa_mdatv-atbez)  "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = <lwa_md02>-atnam AND g~spras = sy-langu."#EC CI_NOORDER
    APPEND lwa_mdatv TO lt_mdatv.
    CLEAR lwa_mdatv.
  ENDLOOP.


  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mditm.
    lwa_mditm-updkz = c_structures-md_in.
    SELECT SINGLE c~msehi g~atbez_uc INTO (lwa_mditm-cunit, lwa_mditm-atbez)  "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = lwa_mditm-atnam AND g~spras = sy-langu."#EC CI_NOORDER
    APPEND lwa_mditm TO lt_mditm. CLEAR lwa_mditm.
  ENDLOOP.

*{   INSERT         SS8K900620                                        1
****Date check
  LOOP AT lt_mdhdr INTO DATA(ls_mdhdr).
    IF ls_mdhdr-mdate IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                           = ls_mdhdr-mdate
       EXCEPTIONS
         plausibility_check_failed       = 1
         OTHERS                          = 2.
      IF sy-subrc <> 0.
        lwa_message-msgid = '/AGRI/GLAG'.
        lwa_message-msgno = '005'.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = ls_mdhdr-mdocm.
      APPEND lwa_message TO et_messages.
      DELETE lt_mdhdr WHERE mdocm = ls_mdhdr-mdocm.
      DELETE lt_mditm WHERE mdocm = ls_mdhdr-mdocm.
      ENDIF.
    ENDIF.
  ENDLOOP.
*}   INSERT

*ENHANCEMENT-POINT MEASUREMENT_DOCUMENT_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

  CALL FUNCTION '/AGRI/GLMD_CREATE_MASS'
    EXPORTING
*     I_MESSAGES_DISPLAY = ' '
*     I_REFRESH_MESSAGES = 'X'
*     I_SAVE_MESSAGES   = 'X'
      it_mdhdr          = lt_mdhdr
      it_mdatv          = lt_mdatv
    IMPORTING
      et_mddoc          = lt_mddoc
      et_messages       = lt_messages
*     E_LOG_NUMBER      =
    EXCEPTIONS
      inconsistent_data = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.
    lwa_message-msgid = sy-msgid.
    lwa_message-msgno = sy-msgno.
    lwa_message-msgty = sy-msgty.
    lwa_message-msgv1 = sy-msgv1.
    lwa_message-msgv2 = sy-msgv2.
    lwa_message-msgv3 = sy-msgv3.
    lwa_message-msgv4 = sy-msgv4.
    APPEND lwa_message TO lt_messages.
  ENDIF.

  APPEND LINES OF lt_messages TO et_messages.


*  LOOP AT lt_mdhdr INTO lwa_mdhdr.
*    lt_mditx[] = lt_mditm[].
*    DELETE lt_mditx WHERE mdocm NE lwa_mdhdr-mdocm.
*    READ TABLE lt_mdtyp INTO ls_mdtyp
*              WITH KEY mdtyp = lwa_mdhdr-mdtyp
*              BINARY SEARCH.
*    IF sy-subrc = 0.
*
*
*
*
*      CALL FUNCTION '/AGRI/GLMD_CREATE'
*        EXPORTING
**         i_messages_display = 'X'
**         i_dialog           = 'X'
**         i_refresh_messages = 'X'
**         i_save_messages    = 'X'
*          i_aslvl            = ls_mdtyp-aslvl
*          i_mpgrp            = lwa_mdhdr-mpgrp
*          is_mdhdr           = lwa_mdhdr
*          it_mditm           = lt_mditx
*        IMPORTING
*          es_mddoc           = lwa_mddoc
*          et_messages        = lt_messages
*        EXCEPTIONS
*          creation_failed    = 1
*          action_canceled    = 2
*          invalid_parameters = 3
*          inconsistent_data  = 4
*          OTHERS             = 5.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                INTO sy-msgli.
*        lwa_message-msgid = sy-msgid.
*        lwa_message-msgno = sy-msgno.
*        lwa_message-msgty = sy-msgty.
*        lwa_message-msgv1 = sy-msgv1.
*        lwa_message-msgv2 = sy-msgv2.
*        lwa_message-msgv3 = sy-msgv3.
*        lwa_message-msgv4 = sy-msgv4.
*        APPEND lwa_message TO lt_messages.
*      ENDIF.
*
*      APPEND lwa_mddoc TO lt_mddoc. CLEAR lwa_mddoc.
*      APPEND LINES OF lt_messages TO et_messages.
*      CLEAR: lt_messages[], lt_mditx[].
*      WAIT UP TO 2 SECONDS.
*    ENDIF.
*
*  ENDLOOP.

ENDMETHOD.


METHOD READ_EXCEL_FILE.

  DATA:
    ref_container   TYPE REF TO cl_gui_custom_container,
    ref_control     TYPE REF TO i_oi_container_control,
    ref_document    TYPE REF TO i_oi_document_proxy,
    ref_spreadsheet TYPE REF TO i_oi_spreadsheet,
    ref_error       TYPE REF TO i_oi_error,
    lt_sheets_out   TYPE  /agri/t_excel_sheet,
    ls_sheets_out   TYPE /agri/s_excel_sheet,

    ls_sheets TYPE soi_sheets_table,
    lt_data   TYPE soi_generic_table,
    ls_ranges TYPE soi_range_list,

    lv_rows TYPE i VALUE 3000, "Rows (Maximum 65536)
    lv_cols TYPE i VALUE 30, "Columns (Maximum 256)
    lv_subrc TYPE sy-tabix,
    lv_excel  TYPE char0256.

  FIELD-SYMBOLS: <lwa_sheet> TYPE soi_sheets,
                 <lwa_data> TYPE soi_generic_item.


  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = ref_control
      error   = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  CREATE OBJECT ref_container
    EXPORTING
      container_name              = 'CONT'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
***Extended Additional Syntax Check ATC  1709 PQ
        MESSAGE e001(00) WITH 'Error while creating container'(001). "#EC MG_MISSING
****
        ENDIF.

  CALL METHOD ref_control->init_control
    EXPORTING
      inplace_enabled      = 'X'
      r3_application_name  = 'EXCEL CONTAINER'
      parent               = ref_container
    IMPORTING
      error                = ref_error
    EXCEPTIONS
      javabeannotsupported = 1
      OTHERS               = 2.
  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.



  CALL METHOD ref_control->get_document_proxy
    EXPORTING
      document_type  = soi_doctype_excel_sheet
    IMPORTING
      document_proxy = ref_document
      error          = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.


  CONCATENATE 'FILE://' i_route INTO lv_excel.

  CALL METHOD ref_document->open_document
    EXPORTING
      document_title = 'Excel'(002)
      document_url   = lv_excel
      open_inplace   = 'X'
    IMPORTING
      error          = ref_error.


  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ' '
    IMPORTING
      error           = ref_error
      sheet_interface = ref_spreadsheet.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_spreadsheet->get_sheets
    EXPORTING
      no_flush = ' '
    IMPORTING
      sheets   = ls_sheets
      error    = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


*loop for all exel sheets
  LOOP AT ls_sheets ASSIGNING <lwa_sheet>.
    lv_subrc = sy-tabix.

    CALL METHOD ref_spreadsheet->select_sheet
      EXPORTING
        name  = <lwa_sheet>-sheet_name
      IMPORTING
        error = ref_error.

    IF ref_error->has_failed = 'X'.
      EXIT.
    ENDIF.

    CALL METHOD ref_spreadsheet->set_selection
      EXPORTING
        top     = 1
        left    = 1
        rows    = lv_rows
        columns = lv_cols.

    CALL METHOD ref_spreadsheet->insert_range
      EXPORTING
        name     = 'Test'(003)
        rows     = lv_rows
        columns  = lv_cols
        no_flush = ''
      IMPORTING
        error    = ref_error.

    IF ref_error->has_failed = 'X'.
      EXIT.
    ENDIF.

    REFRESH lt_data.

    CALL METHOD ref_spreadsheet->get_ranges_data
      EXPORTING
        all      = 'X'
      IMPORTING
        contents = lt_data
        error    = ref_error
      CHANGING
        ranges   = ls_ranges.

* Remove ranges not to be processed else the data keeps on adding up
    CALL METHOD ref_spreadsheet->delete_ranges
      EXPORTING
        ranges = ls_ranges.

    DELETE lt_data WHERE value IS INITIAL. "OR value = space.


    LOOP AT lt_data ASSIGNING <lwa_data>.

      ls_sheets_out-sheet = lv_subrc.
      MOVE-CORRESPONDING <lwa_data> TO ls_sheets_out.
      APPEND ls_sheets_out TO lt_sheets_out.

    ENDLOOP.
  ENDLOOP.


  CALL METHOD ref_document->close_document
    IMPORTING
      error = ref_error.


  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_document->release_document
*  EXPORTING
*    no_flush = ' '
     IMPORTING
       error    = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  et_sheet = lt_sheets_out.

ENDMETHOD.


METHOD STRUCTURE_BUILD.

  DATA: lwa_dd03l   TYPE dd03l,
        lv_position TYPE position.

  FIELD-SYMBOLS: <lwa_data> TYPE /agri/s_excel_sheet.

  SELECT *
    INTO TABLE @DATA(lt_dd03l)
    FROM dd03l
   WHERE tabname = @i_structure.

  DELETE lt_dd03l WHERE fieldname = '.INCLUDE'
                     OR fieldname = 'DUMMY2'
                     OR fieldname = '.INCLU--AP'.

  LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<lwa_dd03l>).
    ADD 1 TO lv_position.
    <lwa_dd03l>-position = lv_position.
  ENDLOOP.

  SORT lt_dd03l BY position.
  et_dd03l[] = lt_dd03l[].

  LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = i_sheet.
    READ TABLE et_dd03l INTO lwa_dd03l
      WITH KEY position = <lwa_data>-column BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_data>-fieldname = lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD TERRAINS_CREATE.

  DATA: lt_glflot       TYPE TABLE OF /agri/s_glflot,
        lt_klah         TYPE TABLE OF klah,
        lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
        lwa_dd03l       TYPE dd03l,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_glflot      TYPE /agri/s_glflot,
        lwa_klah        TYPE klah,
        lv_row          TYPE /agri/s_excel_sheet-row,

*        lt_gliflot      TYPE TABLE OF /agri/s_gliflot,
*        lwa_gliflot     TYPE /agri/s_gliflot,

        lt_bapiaddr1    TYPE TABLE OF bapiaddr1,
        lwa_bapiaddr1   TYPE  bapiaddr1,
        lwa_gladrc      TYPE /agri/s_gladrc,

*        lt_gliloa       TYPE TABLE OF /agri/s_gliloa,
*        lwa_gliloa      TYPE /agri/s_gliloa,

        lt_gliflotx2    TYPE TABLE OF /agri/glflotx,
        lwa_gliflotx    TYPE /agri/s_gliflotx,
        lt_gliflotx     TYPE  /agri/t_gliflotx,

        lwa_glflown     TYPE /agri/s_glflown,
        lt_glflown      TYPE /agri/t_glflown,
        ls_glfl_doc     TYPE /agri/s_glfl_doc,

        lwa_glihpa      TYPE /agri/s_glihpa,
        lt_glihpa       TYPE /agri/t_glihpa,
        lt_glflatg      TYPE /agri/t_glflatg,
        lwa_glflatg     TYPE /agri/s_glflatg,
        lt_glflatv      TYPE /agri/t_glflatv,
        lwa_glflatv     TYPE /agri/s_glflatv,
*        lt_glihpa2 TYPE /agri/t_glihpa,

*        lt_glfl_doc TYPE TABLE OF /agri/s_glfl_doc,
        lwa_message     TYPE /agri/s_gprolog,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,

        lt_tr01         TYPE TABLE OF /agri/s_tr_sheet1,
        lt_tr02         TYPE TABLE OF /agri/s_tr_sheet2,
        lt_tr03         TYPE TABLE OF /agri/s_tr_sheet3,
        lwa_tr03        TYPE /agri/s_tr_sheet3,
        lt_tr04         TYPE TABLE OF /agri/s_tr_sheet4,
        lt_tr05         TYPE TABLE OF /agri/s_tr_sheet5,
        lt_tr06         TYPE TABLE OF /agri/s_tr_sheet6,
        lt_tr07         TYPE TABLE OF /agri/s_tr_sheet7,
        lt_tr08         TYPE TABLE OF /agri/s_tr_sheet8,
        lt_tr09         TYPE TABLE OF /agri/s_tr_sheet9.


  FIELD-SYMBOLS: <lwa_tr01>   TYPE /agri/s_tr_sheet1,
                 <lwa_tr02>   TYPE /agri/s_tr_sheet2,
                 <lwa_tr03>   TYPE /agri/s_tr_sheet3,
                 <lwa_tr04>   TYPE /agri/s_tr_sheet4,
                 <lwa_tr05>   TYPE /agri/s_tr_sheet5,
                 <lwa_tr06>   TYPE /agri/s_tr_sheet6,
                 <lwa_iflotx> TYPE /agri/glflotx,
                 <lwa_tr07>   TYPE /agri/s_tr_sheet7,
                 <lwa_tr08>   TYPE /agri/s_tr_sheet8,
                 <lwa_tr09>   TYPE /agri/s_tr_sheet9,
                 <lv_value>   TYPE any.



  CONSTANTS: BEGIN OF c_structures,
               tr_01   TYPE tabname    VALUE '/AGRI/S_TR_SHEET1',
               tr_02   TYPE tabname    VALUE '/AGRI/S_TR_SHEET2',
               tr_03   TYPE tabname    VALUE '/AGRI/S_TR_SHEET3',
               tr_04   TYPE tabname    VALUE '/AGRI/S_TR_SHEET4',
               tr_05   TYPE tabname    VALUE '/AGRI/S_TR_SHEET5',
               tr_06   TYPE tabname    VALUE '/AGRI/S_TR_SHEET6',
               tr_07   TYPE tabname    VALUE '/AGRI/S_TR_SHEET7',
               tr_08   TYPE tabname    VALUE '/AGRI/S_TR_SHEET8',
               tr_09   TYPE tabname    VALUE '/AGRI/S_TR_SHEET9',
****Terrain Class Type Issue Fix
*              klart   TYPE klassenart VALUE '003',
               klart   TYPE klassenart VALUE 'X91',
****
               tr_in   TYPE updkz_d    VALUE 'I',
               tr_up   TYPE updkz_d    VALUE 'U',
               success LIKE sy-msgty   VALUE 'S',
             END OF c_structures.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15.  "#EC CI_STDSEQ
  DELETE lt_table WHERE column = 1.  "#EC CI_STDSEQ


  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr01> TO <lv_value>.

      IF lwa_data-fieldname = 'PSPNR' .
*        CALL FUNCTION '/AGRI/G_CONV_EXIT_KONPD_INPUT' "#EC ARG_OK
*          EXPORTING
*            i_input     = lwa_data-value
*          IMPORTING
*            e_output    = lwa_data-value
**           PROJWA    =
*          EXCEPTIONS
*            not_found = 1
*            OTHERS    = 2.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_KONPD_INPUT'
          EXPORTING
            i_input            = lwa_data-value
*           I_NO_MESSAGE       =
          IMPORTING
            e_output           = lwa_data-value.
*           ES_RETURN          =
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO sy-msgli.
          lwa_message-msgid = sy-msgid.
          lwa_message-msgno = sy-msgno.
          lwa_message-msgty = sy-msgty.
          lwa_message-msgv1 = sy-msgv1.
          lwa_message-msgv2 = sy-msgv2.
          lwa_message-msgv3 = sy-msgv3.
          lwa_message-msgv4 = sy-msgv4.
          APPEND lwa_message TO lt_mess_collect.
          CLEAR: lwa_message.
          EXIT.
        ENDIF.
      ENDIF.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  UNASSIGN: <lwa_tr01>, <lv_value>.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 02.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr02 ASSIGNING <lwa_tr02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr02 ASSIGNING <lwa_tr02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr02>, <lv_value>.

  "Sheet 3
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_03
      i_sheet     = 3
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 03.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr03 ASSIGNING <lwa_tr03>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr03 ASSIGNING <lwa_tr03>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr03> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr03>, <lv_value>.

  "Sheet 4
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_04
      i_sheet     = 4
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 04.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr04 ASSIGNING <lwa_tr04>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr04 ASSIGNING <lwa_tr04>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr04> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr04>, <lv_value>.

  "Sheet 5
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_05
      i_sheet     = 5
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 05.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr05 ASSIGNING <lwa_tr05>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr05 ASSIGNING <lwa_tr05>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr05> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr05>, <lv_value>.

  "Sheet 6
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_06
      i_sheet     = 6
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 06.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr06 ASSIGNING <lwa_tr06>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr06 ASSIGNING <lwa_tr06>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr06> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr06>, <lv_value>.

  "Sheet 7
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_07
      i_sheet     = 7
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 07.    "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr07 ASSIGNING <lwa_tr07>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr07 ASSIGNING <lwa_tr07>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr07> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr07>, <lv_value>.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_08
      i_sheet     = 8
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 08.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr08 ASSIGNING <lwa_tr08>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr08 ASSIGNING <lwa_tr08>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ


      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr08> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr08>, <lv_value>.

  IF lt_tr08 IS NOT INITIAL.
    SELECT * FROM klah "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE lt_klah
      FOR ALL ENTRIES IN lt_tr08
      WHERE class EQ lt_tr08-class.
  ENDIF.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_09
      i_sheet     = 9
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 09.    "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr09 ASSIGNING <lwa_tr09>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr09 ASSIGNING <lwa_tr09>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr09> TO <lv_value>.
      IF lwa_data-fieldname EQ 'ATINN'.
*---Replace Unreleased Interfaces
*  call function 'CONVERSION_EXIT_ATINN_INPUT'
*    exporting
*      input  = lwa_data-value
*    importing
*      output = lwa_data-value.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_ATINN_INPUT'
          EXPORTING
            i_input  = lwa_data-value
          IMPORTING
            o_output = lwa_data-value.
*---
      ENDIF.
      <lv_value> = lwa_data-value.
    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr09>, <lv_value>.


  LOOP AT lt_tr01 ASSIGNING <lwa_tr01>.

    "Terrain Header
    MOVE-CORRESPONDING <lwa_tr01> TO lwa_glflot.
    lwa_glflot-updkz = c_structures-tr_in.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = lwa_glflot-msehi
*       LANGUAGE       =
      IMPORTING
        output         = lwa_glflot-msehi
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
      lwa_message-msgid = sy-msgid.
      lwa_message-msgno = sy-msgno.
      lwa_message-msgty = sy-msgty.
      lwa_message-msgv1 = sy-msgv1.
      lwa_message-msgv2 = sy-msgv2.
      lwa_message-msgv3 = sy-msgv3.
      lwa_message-msgv4 = sy-msgv4.
      APPEND lwa_message TO lt_messages.
      APPEND LINES OF lt_messages TO lt_mess_collect.
      CLEAR: lwa_message, lt_messages.
    ENDIF.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glflot-owrol
*      IMPORTING
*        output = lwa_glflot-owrol.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glflot-owrol
      IMPORTING
        o_parvw = lwa_glflot-owrol.
*---


    "Terrain Table
*    READ TABLE lt_tr02 ASSIGNING <lwa_tr02> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING <lwa_tr02> TO lwa_gliflot.
*      lwa_gliflot-updkz = c_structures-tr_in.
*    ENDIF.

    "BAPI Reference Structure for Addresses
    READ TABLE lt_tr03 ASSIGNING <lwa_tr03> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING <lwa_tr03> TO lwa_bapiaddr1.
    ENDIF.

    "PM Object Location and Account Assignment
*    READ TABLE lt_tr04 ASSIGNING <lwa_tr04> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING <lwa_tr04> TO lwa_gliloa.
*      lwa_glflot-eqfnr = lwa_gliloa-eqfnr.
*      lwa_gliloa-updkz = c_structures-tr_in.
*    ENDIF.

    "Terrain: Short Texts
    LOOP AT lt_tr05 ASSIGNING <lwa_tr05> WHERE tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr05> TO lwa_gliflotx.
      lwa_gliflotx-updkz = c_structures-tr_in.
      APPEND lwa_gliflotx TO lt_gliflotx.
      CLEAR lwa_gliflotx.
    ENDLOOP.

    "Plant Maintenance: Partners
    LOOP AT lt_tr06 ASSIGNING <lwa_tr06> WHERE tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr06> TO lwa_glihpa.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glihpa-parvw
*      IMPORTING
*        output = lwa_glihpa-parvw.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glihpa-parvw
      IMPORTING
        o_parvw = lwa_glihpa-parvw.
*---

      CHECK lwa_glihpa-parvw IS NOT INITIAL.

      lwa_glihpa-updkz = c_structures-tr_in.
      APPEND lwa_glihpa TO lt_glihpa.
      CLEAR lwa_glihpa.
    ENDLOOP.

    "Terrain Owners
    LOOP AT lt_tr07 ASSIGNING <lwa_tr07> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr07> TO lwa_glflown.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glflot-owrol
*      IMPORTING
*        output = lwa_glflot-owrol.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glflown-owrol
      IMPORTING
        o_parvw = lwa_glflown-owrol.
*---
      CHECK lwa_glflown-owrol IS NOT INITIAL.

      lwa_glflown-updkz = c_structures-tr_in.
      APPEND lwa_glflown TO lt_glflown.
      CLEAR lwa_glflown.
    ENDLOOP.

    LOOP AT lt_tr08 ASSIGNING <lwa_tr08> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      READ TABLE lt_klah INTO lwa_klah
                WITH KEY klart = c_structures-klart
                         class = <lwa_tr08>-class.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <lwa_tr08> TO lwa_glflatg.
        MOVE-CORRESPONDING lwa_klah   TO lwa_glflatg.
        lwa_glflatg-updkz = c_structures-tr_in.
        APPEND lwa_glflatg TO lt_glflatg.
      ENDIF.
      CLEAR lwa_glflatg.
    ENDLOOP.

    LOOP AT lt_tr09 ASSIGNING <lwa_tr09> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl. "#EC CI_STDSEQ
      READ TABLE lt_klah INTO lwa_klah
                WITH KEY klart = c_structures-klart
                         class = <lwa_tr09>-class.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <lwa_tr09> TO lwa_glflatv.
        lwa_glflatv-clint = lwa_klah-clint.
        lwa_glflatv-updkz = c_structures-tr_in.
        APPEND lwa_glflatv TO lt_glflatv.
      ENDIF.
      CLEAR lwa_glflatv.
    ENDLOOP.

*ENHANCEMENT-POINT TERRAINS_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

*    BREAK-POINT.
    CALL FUNCTION '/AGRI/GLFL_CREATE'
      EXPORTING
        i_messages_display      = ' '
        i_save_messages         = ' '
        i_commit_work           = 'X'
        i_check_address         = 'X'
        i_dialog                = ' '
        is_flhdr                = lwa_glflot
*        is_iflot                = lwa_gliflot
        is_adrc                 = lwa_bapiaddr1
*        is_iloa                 = lwa_gliloa
        it_iflotx               = lt_gliflotx
        it_ihpa                 = lt_glihpa
        it_flown                = lt_glflown
      IMPORTING
        es_glfl_doc             = ls_glfl_doc
        et_messages             = lt_messages
**   E_LOG_NUMBER                  =
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO sy-msgli.
      lwa_message-msgid = sy-msgid.
      lwa_message-msgno = sy-msgno.
      lwa_message-msgty = sy-msgty.
      lwa_message-msgv1 = sy-msgv1.
      lwa_message-msgv2 = sy-msgv2.
      lwa_message-msgv3 = sy-msgv3.
      lwa_message-msgv4 = sy-msgv4.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    APPEND LINES OF lt_messages TO lt_mess_collect.
    CLEAR lt_messages.

    IF sy-msgty EQ c_structures-success
      AND lt_glflatg[] IS NOT INITIAL.
      MOVE-CORRESPONDING: ls_glfl_doc-x-flhdr  TO lwa_glflot.
*      MOVE-CORRESPONDING: ls_glfl_doc-x-iflot TO lwa_gliflot.
*      MOVE-CORRESPONDING: ls_glfl_doc-x-iloa  TO lwa_gliloa.
      lwa_glflot-updkz    = c_structures-tr_up.
      CALL FUNCTION '/AGRI/GLFL_CHANGE'
        EXPORTING
*         I_MESSAGES_DISPLAY      = ' '
*         I_SAVE_MESSAGES         = ' '
          is_flhdr                = lwa_glflot
*          is_iflot                = lwa_gliflot
*          is_iloa                 = lwa_gliloa
          it_flatg                = lt_glflatg
          it_flatv                = lt_glflatv
        IMPORTING
*         es_glfl_doc             = ls_glfl_doc
          et_messages             = lt_messages
*         E_LOG_NUMBER            =
*       CHANGING
*         CS_GLFL_DOC             =
        EXCEPTIONS
          no_documents_to_process = 1
          no_authorization        = 2
          change_failed           = 3
          terrain_locked          = 4
          OTHERS                  = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.
      APPEND LINES OF lt_messages TO lt_mess_collect.
      CLEAR lt_messages.
*      READ TABLE lt_messages WITH KEY msgty = c_structures-success
*                 TRANSPORTING NO FIELDS.
*      IF sy-subrc NE 0.
*        APPEND LINES OF lt_messages TO lt_mess_collect.
*      ENDIF.

    ENDIF.

    CLEAR: lwa_glflot,
*           lwa_gliflot,
           lwa_bapiaddr1,
           lwa_bapiaddr1,
           lt_gliflotx[],
           lt_glihpa[],
           lt_glflown[],
           lt_glflatg[],
           lt_glflatv[].

  ENDLOOP.

  et_messages = lt_mess_collect.

ENDMETHOD.
ENDCLASS.
