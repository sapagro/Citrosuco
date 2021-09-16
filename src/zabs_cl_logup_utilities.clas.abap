class ZABS_CL_LOGUP_UTILITIES definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gty_camp_obl_descr,
          campo           TYPE zabs_mig_field_ob-field,
          ind_obligatorio TYPE zabs_mig_field_ob-ind_mandatory,
          descripcion     TYPE zabs_mig_field_ob-description,
        END OF gty_camp_obl_descr .
  types:
    gtt_camp_obl_descr  TYPE TABLE OF gty_camp_obl_descr .
  types GTT_CAMPOS type CME_T_DTYPE_NAME .

  class-methods VALIDATE_MAND_FIELDS
    importing
      !IM_WA_CONTR type ANY
      !IM_V_LINE_NO type INT4
      !IM_V_MSGID type BAL_S_MSG-MSGID
      !IM_V_MSGNO type BAL_S_MSG-MSGNO
      !IM_TI_OBLIGATORIOS type GTT_CAMP_OBL_DESCR
    changing
      !CH_TI_ERRORES type BAL_T_MSG .
  class-methods GET_LOCAL_PATH
    changing
      !CH_V_PATH type STRING .
  class-methods GET_SERVER_PATH
    changing
      !CH_V_PATH type STRING .
  class-methods UPLOAD_INFO
    importing
      !IM_V_PATH type STRING
      !IM_V_SOURCE type CHAR1
    changing
      !CH_TI_TABLE type STANDARD TABLE .
  class-methods CREATE_LOG
    importing
      !IM_V_EXTNUMBER type BALNREXT
      !IM_V_OBJECT type BALOBJ_D
      !IM_V_SUBOBJECT type BALSUBOBJ
    exporting
      !EX_V_LOG_HANDLE type BALLOGHNDL .
  class-methods ADD_MSG
    importing
      !IM_WA_MSG type BAL_S_MSG
      !IM_V_LOG_HANDLE type BALLOGHNDL .
  class-methods SAVE_LOG
    importing
      !IM_TI_LOG_HANDLE type BAL_T_LOGH .
  class-methods DOWNLOAD_ERRORS
    importing
      !IM_V_PATH type STRING
      !IM_V_SOURCE type CHAR1
    changing
      !CH_TI_TABLE type ANY TABLE .
  class-methods DISPLAY_LOG
    importing
      !IM_TI_LOG_HANDLE type BAL_T_LOGH .
  class-methods GET_MAND_FIELDS
    importing
      !IM_V_WRICEF_ID type ZABS_E_GAPID
    changing
      !CH_TI_OBLIGAT type GTT_CAMP_OBL_DESCR .
  class-methods ADD_INITIAL_MSG
    importing
      !IM_V_PATH type STRING optional
      !IM_V_TIPO type CHAR1
      !IM_V_LOG_HANDLE type BALLOGHNDL
      !IM_V_MSJ_OPC_1 type CHAR200 optional
      !IM_V_MSJ_OPC_2 type CHAR200 optional
      !IM_V_MSJ_OPC_3 type CHAR200 optional
      !IM_V_MSJ_OPC_4 type CHAR200 optional .
  class-methods ALT_SAVE_LOG
    importing
      !IM_TI_LOG_HANDLE type BAL_T_LOGH .
  class-methods STRUCTURE_BUILD
    importing
      !I_STRUCTURE type TABNAME
      !I_SHEET type NUMC2
    exporting
      !ET_DD03L type THRPAD_ERD_DD03L
    changing
      !CT_TABLE type /AGRI/T_EXCEL_SHEET .
protected section.
ENDCLASS.



CLASS ZABS_CL_LOGUP_UTILITIES IMPLEMENTATION.


METHOD add_initial_msg.
*--------------------------------------------------------------------*
  DATA:
*     Definición de estructuras.
    lwa_msg          TYPE bal_s_msg,
*     Definición de variables.
    lv_tipo          TYPE char4,
    lv_err_dummy     TYPE char200,
    lv_path          TYPE char200,
*     Definición de constantes.
    lv_tipo_test     TYPE char1 VALUE 'T',
    lv_tipo_test_txt TYPE char4 VALUE 'Test',
    lv_tipo_real_txt TYPE char4 VALUE 'Real'.

*.Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

*   Obtener ruta:
  lv_path = im_v_path.
  IF lv_path IS NOT INITIAL.
*   Lanzar mensaje con Ruta de la carga.
    lwa_msg-msgty = 'I'.
    lwa_msg-msgid = 'ZABS_MSGCLS'.
    lwa_msg-msgno = '182'.
    lwa_msg-msgv1 = lv_path(50).
    lwa_msg-msgv2 = lv_path+50(50).
    lwa_msg-msgv3 = lv_path+100(50).
*...Se crea la instrancia de LOG
    lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                  im_wa_msg       = lwa_msg ).
  ENDIF.

*   Validar tipo de carga.
  IF im_v_tipo EQ lv_tipo_test.
*     Asiganr tipo en texto: TEST.
    lv_tipo = lv_tipo_test_txt.
  ELSE.
*     Asiganr tipo en texto: REAL.
    lv_tipo = lv_tipo_real_txt.
  ENDIF.

*   Lanzar mensaje con tipo de la carga.
  lwa_msg-msgty = 'I'.
  lwa_msg-msgid = 'ZABS_MSGCLS'.
  lwa_msg-msgno = '183'.
  lwa_msg-msgv1 = im_v_tipo.
  lwa_msg-msgv2 = lv_tipo.
*...Se crea la instrancia de LOG
  lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                im_wa_msg = lwa_msg ).

*   Validar si ha llegado mensaje opcional 1.
  IF im_v_msj_opc_1 IS NOT INITIAL.
*     Lanzar mensaje con tipo de la carga.
    lwa_msg-msgty = 'I'.
    lwa_msg-msgid = '00'.
    lwa_msg-msgno = '001'.
    lwa_msg-msgv1 = im_v_msj_opc_1(50).
    lwa_msg-msgv2 = im_v_msj_opc_1+50(50).
    lwa_msg-msgv3 = im_v_msj_opc_1+100(50).
    lwa_msg-msgv4 = im_v_msj_opc_1+150(50).
*.....Se crea la instrancia de LOG
    lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                  im_wa_msg = lwa_msg ).
  ENDIF.

*   Validar si ha llegado mensaje opcional 2.
  IF im_v_msj_opc_2 IS NOT INITIAL.
*     Lanzar mensaje con tipo de la carga.
    lwa_msg-msgty = 'I'.
    lwa_msg-msgid = '00'.
    lwa_msg-msgno = '001'.
    lwa_msg-msgv1 = im_v_msj_opc_2(50).
    lwa_msg-msgv2 = im_v_msj_opc_2+50(50).
    lwa_msg-msgv3 = im_v_msj_opc_2+100(50).
    lwa_msg-msgv4 = im_v_msj_opc_2+150(50).
*.....Se crea la instrancia de LOG
    lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                  im_wa_msg = lwa_msg ).
  ENDIF.

*   Validar si ha llegado mensaje opcional 3.
  IF im_v_msj_opc_3 IS NOT INITIAL.
*     Lanzar mensaje con tipo de la carga.
    lwa_msg-msgty = 'I'.
    lwa_msg-msgid = '00'.
    lwa_msg-msgno = '001'.
    lwa_msg-msgv1 = im_v_msj_opc_3(50).
    lwa_msg-msgv2 = im_v_msj_opc_3+50(50).
    lwa_msg-msgv3 = im_v_msj_opc_3+100(50).
    lwa_msg-msgv4 = im_v_msj_opc_3+150(50).
*.....Se crea la instrancia de LOG
    lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                  im_wa_msg = lwa_msg ).
  ENDIF.

*   Validar si ha llegado mensaje opcional 4.
  IF im_v_msj_opc_4 IS NOT INITIAL.
*     Lanzar mensaje con tipo de la carga.
    lwa_msg-msgty = 'I'.
    lwa_msg-msgid = '00'.
    lwa_msg-msgno = '001'.
    lwa_msg-msgv1 = im_v_msj_opc_4(50).
    lwa_msg-msgv2 = im_v_msj_opc_4+50(50).
    lwa_msg-msgv3 = im_v_msj_opc_4+100(50).
    lwa_msg-msgv4 = im_v_msj_opc_4+150(50).
*.....Se crea la instrancia de LOG
    lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                  im_wa_msg = lwa_msg ).
  ENDIF.

ENDMETHOD.


METHOD add_msg.

*.Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).

*.Se crea la instrancia de LOG
  lo_util->add_msg( EXPORTING   im_v_log_handle = im_v_log_handle
                                im_wa_msg = im_wa_msg ).


ENDMETHOD.


METHOD alt_save_log.
*--------------------------------------------------------------------*
*.Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

*.Se crea la instrancia de LOG
  lo_util->alt_save_log( EXPORTING im_ti_log_handle = im_ti_log_handle ).

ENDMETHOD.


METHOD create_log.

*-- Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

*-- Se crea la instrancia de LOG
  lo_util->log_create( EXPORTING im_v_extnumber = im_v_extnumber
                                 im_v_object    = im_v_object
                                 im_v_subobject = im_v_subobject
                       IMPORTING ex_v_log_handle = ex_v_log_handle ).

ENDMETHOD.


METHOD display_log.
*--------------------------------------------------------------------*
*-- Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

  lo_util->display_log(
    EXPORTING im_ti_log_handle = im_ti_log_handle ).

ENDMETHOD.


METHOD download_errors.

*.Declaración de constantes locales
  CONSTANTS:
    lc_local  TYPE char1 VALUE 'L',
    lc_server TYPE char1 VALUE 'S',
    lc_err    TYPE char5 VALUE '_err.'.

*.Declaración de variables locales
  DATA:
*.......Variables
    lv_path   TYPE string,
    lv_split1 TYPE string,
    lv_split2 TYPE string.

*.Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

*.Cambiar path de .txt a .err
  SPLIT im_v_path AT '.' INTO lv_split1 lv_split2.
  IF lv_split1 IS NOT INITIAL.
    CONCATENATE lv_split1 lc_err lv_split2 INTO lv_path.
  ELSE.
    CONCATENATE im_v_path lc_err INTO lv_path.
  ENDIF.

  IF im_v_source  EQ lc_local.        "Descarga a ruta local

    lo_util->download_local(
      EXPORTING im_v_path     = lv_path
      CHANGING  ch_ti_table   = ch_ti_table ).

  ELSEIF im_v_source EQ lc_server.    "Descarga al servidor

    lo_util->download_server(
      EXPORTING im_v_path     = lv_path
      CHANGING  ch_ti_table   = ch_ti_table ).

  ENDIF.

ENDMETHOD.


METHOD get_local_path.
*--------------------------------------------------------------------*
*.Declaración de variables locales
  DATA:
*.......Tablas internas
    lti_filetable TYPE filetable,
*.......Variables
    lv_subrc      TYPE i.
*--------------------------------------------------------------------*

*.Se abre browser del cliente
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      initial_directory = 'C:\'
    CHANGING
      file_table        = lti_filetable
      rc                = lv_subrc.

*.Se asigna ruta obtenida
  READ TABLE lti_filetable ASSIGNING FIELD-SYMBOL(<lfs_filetable>)
  INDEX 1.

  IF <lfs_filetable> IS ASSIGNED.

    ch_v_path = <lfs_filetable>-filename.

  ENDIF.

ENDMETHOD.


METHOD get_mand_fields.
*--------------------------------------------------------------------*
*.Se instancia la clase DAO
  DATA(lo_dao) = lcl_dao=>get_instance( ).
*--------------------------------------------------------------------*

* Obtener campos obligatorios en la carga.
  lo_dao->select_zabs_mig_field_ob(
    EXPORTING im_v_wricef_id  = im_v_wricef_id
    CHANGING  ch_ti_obligat   = ch_ti_obligat ).

ENDMETHOD.


METHOD get_server_path.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = ch_v_path
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDMETHOD.


METHOD save_log.
*--------------------------------------------------------------------*
*.Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*
*-- Se crea la instrancia de LOG
  lo_util->save_log( EXPORTING im_ti_log_handle = im_ti_log_handle ).

ENDMETHOD.


METHOD structure_build.

  DATA: lwa_dd03l TYPE dd03l.
  FIELD-SYMBOLS: <lwa_data> TYPE /agri/s_excel_sheet.

  SELECT * INTO TABLE @et_dd03l                "#EC CI_ALL_FIELDS_NEEDED
    FROM dd03l
    WHERE tabname = @i_structure.

  LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = i_sheet. "#EC CI_STDSEQ
    <lwa_data>-column = <lwa_data>-column.
    READ TABLE et_dd03l INTO lwa_dd03l
                        WITH KEY position = <lwa_data>-column. "#EC CI_STDSEQ
    IF sy-subrc EQ 0.
      <lwa_data>-fieldname = lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD upload_info.

*-- Declaración de constantes locales
  CONSTANTS:
    lc_local  TYPE char1 VALUE 'L',
    lc_server TYPE char1 VALUE 'S'.

*-- Se instancia la clase UTIL
  DATA(lo_util) = lcl_util=>get_instance( ).
*--------------------------------------------------------------------*

*.Se identifica si la carga se realiza desde archivo local o servidor
  IF im_v_source  EQ lc_local.

    lo_util->upload_local(
      EXPORTING im_v_path   = im_v_path
      CHANGING ch_ti_table  = ch_ti_table ).

  ELSEIF im_v_source  EQ lc_server.

    lo_util->upload_server(
      EXPORTING im_v_path   = im_v_path
      CHANGING ch_ti_table  = ch_ti_table ).

  ENDIF.

ENDMETHOD.


METHOD validate_mand_fields.
*--------------------------------------------------------------------*
  DATA:
*     Definición de objetos loclaes.
    lo_structdescr   TYPE REF TO cl_abap_structdescr,
*     Definición de tablas locales.
    lti_components   TYPE cl_abap_structdescr=>component_table,
*     Definición de estructiras locales.
    lwa_errores      TYPE  bal_s_msg,
    lwa_obligatorios TYPE gty_camp_obl_descr.

*   Definición de apuntadores.
  FIELD-SYMBOLS:
    <lfs_comp>  LIKE LINE OF lti_components,
    <lfs_valor> TYPE any.
*--------------------------------------------------------------------*

*   Obtener detalle de los campos de la estructura.
  lo_structdescr ?= cl_abap_typedescr=>describe_by_data( im_wa_contr ).
*   Obtener los camponentes de la estructura.
  lti_components = lo_structdescr->get_components( ).

*   Validar si se han obtenido componentes (Tabla de campos).
  IF lti_components IS NOT INITIAL.
*     Recorrer componentes.
    LOOP AT lti_components ASSIGNING <lfs_comp>.
*       Limpiar estructuras.
      CLEAR:
        lwa_errores,
        lwa_obligatorios.
*       Asignar el valor del campo.
      ASSIGN COMPONENT sy-tabix
        OF STRUCTURE im_wa_contr
        TO <lfs_valor>.
*       Leer campo en tabla de campos obligatorios.
      READ TABLE im_ti_obligatorios
        INTO lwa_obligatorios
        WITH KEY campo = <lfs_comp>-name.
*       Validar si se encuentra el campo en tabla de obligatorios.
      IF lwa_obligatorios IS NOT INITIAL.
*         Validar si el campo NO tiene valor.
        IF <lfs_valor> IS INITIAL.
*           Asignar error: El campo &1 es obligatorio y debe ser llenado. Linea &2.
          lwa_errores-msgty = 'E'.
          lwa_errores-msgid = im_v_msgid.
          lwa_errores-msgno = im_v_msgno.
          lwa_errores-msgv1 = lwa_obligatorios-descripcion.
          lwa_errores-msgv2 = im_v_line_no.
          CONDENSE lwa_errores-msgv2 NO-GAPS.
*           Añadir error a tabla de retorno de errores.
          APPEND lwa_errores TO ch_ti_errores.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDMETHOD.
ENDCLASS.
