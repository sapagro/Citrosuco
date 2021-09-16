FUNCTION zabs_fm_glflm_change.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_TERRAIN) TYPE  /AGRI/GLTPLNR_FL
*"     REFERENCE(IT_CHARACTERISTICS) TYPE  ZABS_TTY_AT_SHEET1 OPTIONAL
*"     REFERENCE(IT_CABN) TYPE  ZABS_TTY_CABN OPTIONAL
*"     REFERENCE(IS_ADDITIONAL_FIEDLS) TYPE  ZABS_STR_AF_GLFLOTCA
*"       OPTIONAL
*"     REFERENCE(IT_DD03L) TYPE  THRPAD_ERD_DD03L OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

*-- Data Declarations
*-- Internal Tables
  DATA: lt_tplnr     TYPE /agri/t_gltplnr,
        lt_flhdr     TYPE /agri/t_glflot,
        lt_iflotx    TYPE /agri/t_gliflotx,
        lt_adrc      TYPE /agri/t_gladrc,
        lt_ihpa      TYPE /agri/t_glihpa,
        lt_flppl     TYPE /agri/t_glflppl,
        lt_flatg     TYPE /agri/t_glflatg,
        lt_flatv     TYPE /agri/t_glflatv,
        lt_flown     TYPE /agri/t_glflown,
        lt_flcma     TYPE /agri/t_glflcma,
        lt_flos      TYPE /agri/t_glflos,
        lrt_tplnr_fl TYPE RANGE OF /agri/gltplnr_fl,
        lt_messages  TYPE /agri/t_gprolog.

*-- Work Areas
  DATA: ls_iflot   TYPE /agri/s_gliflot,
        ls_adrc    TYPE bapiaddr1,
        ls_iloa    TYPE /agri/s_gliloa,
        ls_glfldoc TYPE /agri/s_glfl_doc,
        ls_message LIKE LINE OF lt_messages.

*-- Variables
  DATA: lv_tplnr TYPE /agri/gltplnr_fl,
        lv_subrc TYPE sysubrc.

*-- Constants
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
               klart   TYPE klassenart VALUE 'X91',
               tr_in   TYPE updkz_d    VALUE 'I',
               tr_up   TYPE updkz_d    VALUE 'U',
               success LIKE sy-msgty   VALUE 'S',
             END OF c_structures.

*-- Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

*-- Field Symbols
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

  lv_tplnr = iv_terrain.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = iv_terrain
    IMPORTING
      output     = lv_tplnr
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.

  IF sy-subrc NE 0.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '051'
        WITH iv_terrain sy-msgv2 sy-msgv3 sy-msgv4
        INTO sy-msgli.
    ls_message-msgid = '/AGRI/GLFL'.
    ls_message-msgno = '051'.
    ls_message-msgty = 'E'.
    ls_message-msgv1 = iv_terrain.
    APPEND ls_message TO lt_messages.
    et_messages[] = lt_messages[].
    RETURN.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lrt_tplnr_fl
    ASSIGNING FIELD-SYMBOL(<lrs_tplnr_fl>).
  IF sy-subrc EQ 0.
    <lrs_tplnr_fl> = 'IEQ'.
    <lrs_tplnr_fl>-low = lv_tplnr.
  ENDIF.

*-- Read terrain data from Terrain Header table
  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE lt_tplnr
   WHERE tplnr_fl IN lrt_tplnr_fl.

  IF sy-subrc NE 0.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '051'
        WITH iv_terrain sy-msgv2 sy-msgv3 sy-msgv4
        INTO sy-msgli.
    ls_message-msgid = '/AGRI/GLFL'.
    ls_message-msgno = '051'.
    ls_message-msgty = 'E'.
    ls_message-msgv1 = iv_terrain.
    APPEND ls_message TO lt_messages.
    et_messages[] = lt_messages[].
    RETURN.
  ENDIF.

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

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ls_message-msgid = sy-msgid.
    ls_message-msgno = sy-msgno.
    ls_message-msgty = sy-msgty.
    ls_message-msgv1 = sy-msgv1.
    ls_message-msgv2 = sy-msgv2.
    ls_message-msgv3 = sy-msgv3.
    ls_message-msgv4 = sy-msgv4.
    APPEND ls_message TO lt_messages.
    et_messages[] = lt_messages[].
    RETURN.
  ELSE.
    LOOP AT lt_flatg ASSIGNING FIELD-SYMBOL(<lwa_flatg>).
      IF sy-subrc EQ 0.
        <lwa_flatg>-updkz = c_structures-tr_up.
      ENDIF.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_flhdr INTO DATA(ls_flhdr) INDEX 1.

  IF sy-subrc EQ 0.
    IF it_characteristics[] IS SUPPLIED
    AND it_cabn[] IS SUPPLIED.
      SORT: lt_flatg BY class,
            lt_flatv BY tplnr_fl class atinn.

      SORT lt_flatg BY class.
      LOOP AT it_characteristics INTO DATA(lwa_char).
        DATA(lv_new) = abap_false.
        READ TABLE lt_flatv ASSIGNING FIELD-SYMBOL(<lwa_flatv>)
          WITH KEY tplnr_fl = lwa_char-tplnr_fl_in
                   class    = lwa_char-class
                   atinn    = lwa_char-atinn. "BINARY SEARCH.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE lt_flatv ASSIGNING <lwa_flatv>.
          lv_new = abap_true.
        ENDIF.

*-- Record found or inserted
        IF sy-subrc EQ 0.
          READ TABLE it_cabn INTO DATA(lwa_cabn)
            WITH KEY class = lwa_char-class
                     atnam = lwa_char-atnam
                     atinn = lwa_char-atinn BINARY SEARCH.
          IF sy-subrc NE 0.
            CLEAR lwa_cabn.
            CONTINUE.
          ELSE.
            <lwa_flatv>-atwrt = lwa_char-atsch.
          ENDIF.
          <lwa_flatv>-tplnr_fl = lwa_char-tplnr_fl_in.
          <lwa_flatv>-clint = lwa_cabn-clint.
          <lwa_flatv>-atinn = lwa_char-atinn.
          <lwa_flatv>-class = lwa_char-class.
          <lwa_flatv>-atcod = '1'.
          <lwa_flatv>-aenam = sy-uname.
          <lwa_flatv>-aedat = sy-datum.
          <lwa_flatv>-aezet = sy-uzeit.
          IF lv_new EQ abap_false.
            <lwa_flatv>-updkz = c_structures-tr_up.
          ELSE.
            <lwa_flatv>-updkz = c_structures-tr_in.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA(lt_flatv_x) = lt_flatv[].
      SORT: lt_flatv_x BY class,
            lt_flatg BY class.
      DELETE ADJACENT DUPLICATES FROM lt_flatv_x COMPARING class.

      LOOP AT lt_flatv_x INTO DATA(lwa_flatv_x).
        READ TABLE lt_flatg ASSIGNING <lwa_flatg>
          WITH KEY class = lwa_flatv_x-class.
        IF sy-subrc NE 0.
          READ TABLE it_cabn INTO lwa_cabn
            WITH KEY class = lwa_flatv_x-class BINARY SEARCH.
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE lt_flatg ASSIGNING <lwa_flatg>.
            IF sy-subrc EQ 0.
              <lwa_flatg>-tplnr_fl = ls_flhdr-tplnr_fl.
              <lwa_flatg>-clint    = lwa_cabn-clint.
              <lwa_flatg>-class    = lwa_cabn-class.
              <lwa_flatg>-updkz    = c_structures-tr_in.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF is_additional_fiedls IS SUPPLIED.
      LOOP AT it_dd03l INTO DATA(ls_dd03l).
        IF ls_dd03l-fieldname EQ 'TPLNR_FL'.
          CONTINUE.
        ENDIF.
        ASSIGN COMPONENT ls_dd03l-fieldname OF STRUCTURE ls_flhdr
          TO FIELD-SYMBOL(<lv_old_value>).
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT ls_dd03l-fieldname OF STRUCTURE is_additional_fiedls
            TO FIELD-SYMBOL(<lv_new_value>).
          IF sy-subrc EQ 0.
            <lv_old_value> = <lv_new_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    PERFORM document_infocus_lock USING ls_flhdr-tplnr_fl
                                        ls_flhdr-strno
                                        c_msg_type-info
                               CHANGING lt_messages
                                        lv_subrc.

    IF lv_subrc NE 0.
      et_messages[] = lt_messages[].
      RETURN.
    ENDIF.

    ls_flhdr-updkz = c_structures-tr_up.

*-- Terrain Change
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

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.
      ls_message-msgid = sy-msgid.
      ls_message-msgno = sy-msgno.
      ls_message-msgty = sy-msgty.
      ls_message-msgv1 = sy-msgv1.
      ls_message-msgv2 = sy-msgv2.
      ls_message-msgv3 = sy-msgv3.
      ls_message-msgv4 = sy-msgv4.
      APPEND ls_message TO lt_messages.
    ENDIF.

    PERFORM document_infocus_unlock USING ls_flhdr-tplnr_fl
                                          ls_flhdr-strno.
  ENDIF.

  et_messages[] = lt_messages[].

ENDFUNCTION.
