*----------------------------------------------------------------------*
*       CLASS lcl_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION       ##CLASS_FINAL.
*----------------------------------------------------------------------*
*                   Definición de la clase LCL_UTIL
*----------------------------------------------------------------------*
* Clase para el uso de herramientas útiles que consultan o modifican
* la base de datos (Funciones, BAPIs, etc)
*----------------------------------------------------------------------*
  PUBLIC SECTION.

*...Datos globales de la clase
    CLASS-DATA:
      go_util TYPE REF TO lcl_util.

*...Métodos de instancia de la clase
    CLASS-METHODS:
*.....Método para establecer la instancia de la clase
      set_instance
        IMPORTING
          im_o_util TYPE REF TO lcl_util,

*.....Método para obtener la instancia de la clase
      get_instance
        RETURNING
          VALUE(ro_util) TYPE REF TO lcl_util.

*...Métodos de utilidades
    METHODS:

*.....Método para cargar archivo local
      upload_local
        IMPORTING im_v_path   TYPE string
        CHANGING  ch_ti_table TYPE ANY TABLE,

*.....Método para cargar archivo desde el servidor
      upload_server
        IMPORTING im_v_path   TYPE string
        CHANGING  ch_ti_table TYPE STANDARD TABLE,

*.....Método para crear el log
      log_create
        IMPORTING im_v_extnumber  TYPE balnrext
                  im_v_object     TYPE balobj_d
                  im_v_subobject  TYPE balsubobj
        EXPORTING ex_v_log_handle TYPE balloghndl,

*.....Método para agregar mensaje al log
      add_msg
        IMPORTING im_v_log_handle TYPE balloghndl
                  im_wa_msg       TYPE bal_s_msg,

*.....Método para salvar el log
      save_log
        IMPORTING im_ti_log_handle TYPE bal_t_logh,

*.....Método para salvar el log
      alt_save_log
        IMPORTING im_ti_log_handle TYPE bal_t_logh,

*.....Método para descargar al PC local
      download_local
        IMPORTING im_v_path   TYPE string
        CHANGING  ch_ti_table TYPE ANY TABLE,

*.....Método para descargar al servidor
      download_server
        IMPORTING im_v_path   TYPE string
        CHANGING  ch_ti_table TYPE ANY TABLE,

*.....Método para mostrar el LOG
      display_log
        IMPORTING im_ti_log_handle TYPE bal_t_logh,

      commit,

      rollback.

ENDCLASS.                    "lcl_util DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_dao DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dao DEFINITION        ##NEEDED ##CLASS_FINAL.
*----------------------------------------------------------------------*
*                   Definición de la clase LCL_DAO
*----------------------------------------------------------------------*
*           Clase para las consultas a la base de datos
*----------------------------------------------------------------------*
  PUBLIC SECTION.

*...Declaración de datos de la clase
    CLASS-DATA:
      go_dao               TYPE REF TO lcl_dao.

*...Métodos de instancia de la clase
    CLASS-METHODS:
*.....Método para establecer la instancia de la clase
      set_instance
        IMPORTING
          im_o_dao TYPE REF TO lcl_dao,
*.....Método para obtener la instancia de la clase
      get_instance
        RETURNING
          VALUE(ro_dao) TYPE REF TO lcl_dao.

*...Métodos tipo DAO(Consultas y otros)
    METHODS:
*.....Obtener campos obligatorios en la carga.
      select_zabs_mig_field_ob
        IMPORTING im_v_wricef_id TYPE zabs_e_gapid
        CHANGING  ch_ti_obligat  TYPE zabs_cl_logup_utilities=>gtt_camp_obl_descr .

ENDCLASS.                    "lcl_dao DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.

*.Método para establecer la instancia de la clase
  METHOD set_instance.

*...Se establece la instancia
    go_util = im_o_util.

  ENDMETHOD.                    "set_instance

*.Método para obtener la instancia de la clase
  METHOD get_instance.

*...Sí no se ha creado la instancia, se crea
    IF ( go_util IS INITIAL ).
      CREATE OBJECT go_util.
    ENDIF.

*...Se retorna la instancia
    ro_util = go_util.

  ENDMETHOD.                    "get_instance

  METHOD upload_local.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = im_v_path
        filetype                = 'ASC'
        has_field_separator     = abap_true
      CHANGING
        data_tab                = ch_ti_table
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11 ##NUMBER_OK
        unknown_dp_error        = 12 ##NUMBER_OK
        access_denied           = 13 ##NUMBER_OK
        dp_out_of_memory        = 14 ##NUMBER_OK
        disk_full               = 15 ##NUMBER_OK
        dp_timeout              = 16 ##NUMBER_OK
        not_supported_by_gui    = 17 ##NUMBER_OK
        error_no_gui            = 18 ##NUMBER_OK
        OTHERS                  = 19 ##NUMBER_OK.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "upload_local

  METHOD upload_server.
*--------------------------------------------------------------------*
*...Declaración de variables locales
    DATA:
*.........Variables
      lv_filename TYPE string,
      lv_linea    TYPE string,
      lv_tabix    TYPE syindex,
      lv_index    TYPE syindex,
      lv_xchar    TYPE char4 VALUE '000D',
      lv_hexa     TYPE char4,
      lv_nreg     TYPE i,
      lv_char     TYPE char1,
      lv_filen    TYPE fileextern,
      lv_lon      TYPE i,
*.........Tablas internas
      lti_campos  TYPE zabs_cl_logup_utilities=>gtt_campos.

*...Declaración de field symbols
    FIELD-SYMBOLS:
      <lfs_data_tab> TYPE any,
      <lfs_field>    TYPE any,
      <lfs>          TYPE any.
*--------------------------------------------------------------------*

    lv_filen = im_v_path.
    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        program          = sy-cprog
        activity         = 'READ'
        filename         = lv_filen
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*...Se abre set de datos
    OPEN DATASET lv_filen FOR INPUT IN TEXT MODE ENCODING DEFAULT
    IGNORING CONVERSION ERRORS.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    MOVE im_v_path TO lv_filename.

    DO.

      READ DATASET lv_filename INTO lv_linea.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      REFRESH lti_campos.
      APPEND INITIAL LINE TO ch_ti_table.
      DESCRIBE TABLE ch_ti_table LINES lv_tabix. "registro Actual

*.....Se asigna registro actual
      READ TABLE ch_ti_table ASSIGNING <lfs_data_tab> INDEX lv_tabix.

*.....Registro transpuesto
      SPLIT lv_linea AT cl_abap_char_utilities=>horizontal_tab
      INTO TABLE lti_campos.
      CLEAR lv_index.
      lv_nreg = lines( lti_campos ).

      LOOP AT lti_campos ASSIGNING FIELD-SYMBOL(<lfs_campo>).

        ADD 1 TO lv_index.

*.......Asigna campo a campo
        ASSIGN COMPONENT lv_index OF STRUCTURE <lfs_data_tab>
        TO <lfs_field>.

        IF sy-tabix EQ lv_nreg.

          lv_lon = strlen( <lfs_campo> ).

          IF lv_lon GT 0.

            lv_lon = lv_lon - 1.
            lv_char = <lfs_campo>+lv_lon.
            ASSIGN lv_char TO <lfs> CASTING TYPE x.
            lv_hexa = <lfs>.

            IF lv_hexa EQ lv_xchar.

              IF lv_lon = 0.
                CONTINUE.
              ENDIF.

              <lfs_field> = <lfs_campo>(lv_lon).

            ELSE.

              <lfs_field> = <lfs_campo>.

            ENDIF.

          ENDIF.

        ELSE.

          <lfs_field> = <lfs_campo>.

        ENDIF.

      ENDLOOP.

    ENDDO.

    CLOSE DATASET im_v_path.

  ENDMETHOD.                    "upload_server

  METHOD log_create.
*--------------------------------------------------------------------*
*...Declaración de variables locales
    DATA:
*.........Estructuras
          lwa_log   TYPE bal_s_log.
*--------------------------------------------------------------------*

    lwa_log-extnumber = im_v_extnumber.                     "#EC NOTEXT
    lwa_log-aluser    = sy-uname.
    lwa_log-alprog    = sy-repid.
    lwa_log-object    = im_v_object.
    lwa_log-subobject = im_v_subobject.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lwa_log
      IMPORTING
        e_log_handle = ex_v_log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "log_create

  METHOD add_msg.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle  = im_v_log_handle
        i_s_msg       = im_wa_msg
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "add_msg

  METHOD save_log.
*--------------------------------------------------------------------*
    DATA: lv_dummy  TYPE c.
*--------------------------------------------------------------------*

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_t_log_handle   = im_ti_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_dummy.
    ENDIF.

    READ TABLE im_ti_log_handle ASSIGNING FIELD-SYMBOL(<lwa_log_handle>) INDEX 1.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = <lwa_log_handle>
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.                    "save_log

  METHOD alt_save_log.
*--------------------------------------------------------------------*
    DATA: lv_dummy  TYPE c.
*--------------------------------------------------------------------*

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_t_log_handle   = im_ti_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_dummy.
    ENDIF.

  ENDMETHOD.                    "save_log

  METHOD download_local.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = im_v_path
        filetype                = 'ASC'
        write_field_separator   = cl_abap_char_utilities=>horizontal_tab
        trunc_trailing_blanks   = abap_true     "@0001
      CHANGING
        data_tab                = ch_ti_table
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11 ##NUMBER_OK
        dp_error_send           = 12 ##NUMBER_OK
        dp_error_write          = 13 ##NUMBER_OK
        unknown_dp_error        = 14 ##NUMBER_OK
        access_denied           = 15 ##NUMBER_OK
        dp_out_of_memory        = 16 ##NUMBER_OK
        disk_full               = 17 ##NUMBER_OK
        dp_timeout              = 18 ##NUMBER_OK
        file_not_found          = 19 ##NUMBER_OK
        dataprovider_exception  = 20 ##NUMBER_OK
        control_flush_error     = 21 ##NUMBER_OK
        not_supported_by_gui    = 22 ##NUMBER_OK
        error_no_gui            = 23 ##NUMBER_OK
        OTHERS                  = 24 ##NUMBER_OK.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "download_local

  METHOD download_server.
*--------------------------------------------------------------------*
*...Declaración de variables locales
    DATA: lv_linea    TYPE string,
          lv_filename TYPE localfile    ##NEEDED,
          lv_tabix    TYPE syindex,
          lv_value    TYPE string.

    FIELD-SYMBOLS: <lfs_table> TYPE any,
                   <lfs_field> TYPE any.
*--------------------------------------------------------------------*

    OPEN DATASET im_v_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT ch_ti_table ASSIGNING <lfs_table>.

      DO.

        ADD 1 TO lv_tabix.
*.......Se asigna cada campo de la estructura
        ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_table> TO
                                               <lfs_field>.
        IF sy-subrc EQ 0.

          MOVE <lfs_field> TO lv_value.

          IF sy-index EQ 1.
            CONCATENATE lv_linea lv_value
            INTO lv_linea.
          ELSE.
            CONCATENATE lv_linea lv_value
            INTO lv_linea
            SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
          ENDIF.

        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      SHIFT lv_linea LEFT DELETING LEADING space.
      TRANSFER lv_linea TO im_v_path.
      CLEAR: lv_linea, lv_tabix.

    ENDLOOP.

    CLOSE DATASET im_v_path.

  ENDMETHOD.                    "download_server

  METHOD display_log.
*--------------------------------------------------------------------*
    DATA: lv_dummy  TYPE c.
*--------------------------------------------------------------------*

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
*       I_S_DISPLAY_PROFILE  =
        i_t_log_handle       = im_ti_log_handle
*       I_T_MSG_HANDLE       =
*       I_S_LOG_FILTER       =
*       I_S_MSG_FILTER       =
*       I_T_LOG_CONTEXT_FILTER              =
*       I_T_MSG_CONTEXT_FILTER              =
*       I_AMODAL             = ' '
*       I_SRT_BY_TIMSTMP     = ' '
*       I_MSG_CONTEXT_FILTER_OPERATOR       = 'A'
*     IMPORTING
*       E_S_EXIT_COMMAND     =
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_dummy.
    ENDIF.


  ENDMETHOD.                    "display_log

  METHOD commit.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.

  METHOD rollback.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDMETHOD.

ENDCLASS.                    "lcl_util IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_dao IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dao IMPLEMENTATION.
*----------------------------------------------------------------------*
*                   Implementación de la clase LCL_DAO
*----------------------------------------------------------------------*
*Método para establecer la instancia de la clase
  METHOD set_instance.

*...Se establece la instancia
    go_dao = im_o_dao.

  ENDMETHOD.                    "set_instance

*.Método para obtener la instancia de la clase
  METHOD get_instance.

*...Sí no se ha creado la instancia, se crea
    IF ( go_dao IS INITIAL ).
      CREATE OBJECT go_dao.
    ENDIF.

*...Se retorna la instancia
    ro_dao = go_dao.

  ENDMETHOD.                    "get_instance

*.Obtener campos obligatorios en la carga.
  METHOD select_zabs_mig_field_ob.

*   Obtener campos obligatorios en la carga.
*    SELECT  campo
*            ind_obligatorio
*            descripcion
*      FROM  ztbc_mig_camp_ob
*      INTO  TABLE ch_ti_obligat
*      WHERE wricef_id       EQ im_v_wricef_id
*        AND ind_obligatorio EQ abap_true.

  ENDMETHOD.                    "select_ztbc_mig_camp_ob

ENDCLASS.                    "lcl_dao IMPLEMENTATION
