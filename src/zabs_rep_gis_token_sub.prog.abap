*&---------------------------------------------------------------------*
*& Include          ZABS_REP_GIS_TOKEN_SUB
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form UPDATE_GISTOKEN
*&---------------------------------------------------------------------*
*& UPDATE_GISTOKEN
*&---------------------------------------------------------------------*
FORM update_gistoken.

*--Local Declarations
  DATA: lv_http_url    TYPE string,
        lv_json        TYPE string,
        lv_http_status TYPE i,
        lv_token       TYPE string,
        lo_http_client TYPE REF TO if_http_client,
        ls_gis         TYPE zabs_s_gistoken,
        ls_gistoken    TYPE zabst_gistoken.

  CONCATENATE 'https://gis.citrosuco.com.br/portal/sharing/generateToken?'
              'referer=https://gis.citrosuco.com.br/server/rest/services/shpTalhaoCTS/MapServer'
              '&username='
*              'SAPFARM@citrosuco.com.br'
              p_uname
              '&password='
              p_pass
*              '@citrosuco162019@zpalqmkseicnvtgrhg'
              '&expiration=20160&f=json'
         INTO lv_http_url.

*  lv_http_url =
*'https://gis.citrosuco.com.br/portal/sharing/generateToken?referer=https://gis.citrosuco.com.br/server/rest/services/shpTalhaoCTS/MapServer&username=SAPFARM@citrosuco.com.br&password=@citrosuco162019@zpalqmkseicnvtgrhg&expiration=60&f=json'.

*-- Create URL
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_http_url
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc EQ 0.
*-- Request and Post
    lo_http_client->request->set_header_field( name  = '~request_method'
                                               value = 'POST' ).
*-- Send the request
    lo_http_client->send( ).

*-- Reterive the result
    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc EQ 0.
*-- Read status code
      lv_http_status = lo_http_client->response->get_header_field( '~status_code' ).
      IF lv_http_status EQ '200'.
*-- Get the response
        lv_json = lo_http_client->response->get_cdata( ).
        IF lv_json IS NOT INITIAL.
*-- Extract the JSON data
          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json = lv_json
            CHANGING
              data = ls_gis.
        ENDIF.
      ENDIF.
    ENDIF.

*-- Close the request
    lo_http_client->close( ).
  ENDIF.

  IF ls_gis IS NOT INITIAL.
    SELECT SINGLE *
      FROM zabst_gistoken
      INTO ls_gistoken
     WHERE sys EQ zcl_abs_abap_maintain=>c_gis_system. "'GIS'
    IF sy-subrc EQ 0.
      ls_gistoken-token = ls_gis-token.
      ls_gistoken-edate = ls_gis-expires.
      MODIFY zabst_gistoken FROM ls_gistoken.
    ELSE.
      ls_gistoken-sys = zcl_abs_abap_maintain=>c_gis_system. "'GIS'
      ls_gistoken-token = ls_gis-token.
      ls_gistoken-edate = ls_gis-expires.
      INSERT zabst_gistoken FROM ls_gistoken.
    ENDIF.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.
