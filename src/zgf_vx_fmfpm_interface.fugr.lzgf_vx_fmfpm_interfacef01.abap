*----------------------------------------------------------------------*
***INCLUDE LZGF_VX_PROCESS_ORDERF01.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                    SOFTWARE FACTORY <Vistex>                         *
*----------------------------------------------------------------------*
* Requirement ID    : Gap ID 04                                        *
* Program           : PROCESS_ORDER_CREATION                           *
* Created By        : Adonis Rossato                                   *
* Creation Date     : 15.04.2021                                       *
* Descriptión       : Logical Crop Creation                            *
*----------------------------------------------------------------------*
*                    LOG OF MODIFICATIONS                              *
*----------------------------------------------------------------------*
* Changed by        :                                                  *
* Changed Date      :                                                  *
* Transport         :                                                  *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form validate_input_data
*&---------------------------------------------------------------------*
FORM validate_input_data CHANGING cs_indata   TYPE zvxs_prc_order
                                  ct_messages TYPE zvxtt_message_gprolog
                                  ct_cskey    TYPE /agri/t_glcs_key.

*-- Check input if any field is missing
  IF cs_indata-ziwerk IS INITIAL OR
     cs_indata-zstrno IS INITIAL OR
     cs_indata-zcmnum IS INITIAL OR
     cs_indata-zdatab IS INITIAL OR
     cs_indata-zvaria IS INITIAL OR
     cs_indata-zindfield IS INITIAL.

*-- Build messages
    PERFORM build_msgs USING gc_constants-msgty_error '001'
                             space space
                    CHANGING ct_messages[].
  ENDIF.

*  IF ( cs_indata-zindfield EQ gc_constants-delete ) AND
*     ( cs_indata-zcpros IS INITIAL ).
*
**-- Build messages
*    PERFORM build_msgs USING gc_constants-msgty_error '001'
*                             space space
*                    CHANGING ct_messages[].
*  ENDIF.
*
  CHECK ct_messages[] IS INITIAL.

*-- Validate Plant
  PERFORM validate_plant USING cs_indata-ziwerk
                      CHANGING ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate Terrain
  PERFORM validate_terrain USING cs_indata
                        CHANGING ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Convert crop date to internal format and validate
  PERFORM convert_date CHANGING cs_indata-zdatab
                                ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate Crop
  PERFORM validate_crop CHANGING cs_indata
                                 ct_messages[]
                                 ct_cskey[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate Indicator Field
  IF cs_indata-zindfield NE gc_constants-insert AND
     cs_indata-zindfield NE gc_constants-delete.

*-- Build messages
    PERFORM build_msgs USING gc_constants-msgty_error '042'
                             space space
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_plant
*&---------------------------------------------------------------------*
FORM validate_plant USING pv_ziwerk   TYPE char4
                 CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local variables
  DATA: lv_msgv1 TYPE msgv1.

*-- Get Plant
  SELECT SINGLE bwkey
    FROM t001k
    INTO @DATA(lv_bwkey)
   WHERE bwkey EQ @pv_ziwerk.
  IF lv_bwkey IS INITIAL.
*-- Build messages
    lv_msgv1 = pv_ziwerk.
    PERFORM build_msgs USING gc_constants-msgty_error '037'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_crop
*&---------------------------------------------------------------------*
FORM validate_crop CHANGING cs_indata   TYPE zvxs_prc_order
                            ct_messages TYPE zvxtt_message_gprolog
                            ct_cskey    TYPE /agri/t_glcs_key.

*-- Local Variables
  DATA: lv_msgv1 TYPE msgv1,
        lv_msgv2 TYPE msgv2.

  CLEAR: gt_cpros[].

*-- Validate Crop data
  SELECT SINGLE cmnum
    FROM /agri/glcmhdr
    INTO @DATA(lv_cmnum)   ##NEEDED
   WHERE cmnum EQ @cs_indata-zcmnum
     AND loevm EQ @space.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-zcmnum.
    PERFORM build_msgs USING gc_constants-msgty_error '017'
                             lv_msgv1 space
                    CHANGING ct_messages[].

  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate Crop & Variant Master data
  SELECT SINGLE cmnum
    FROM /agri/glcmvar
    INTO lv_cmnum
   WHERE cmnum EQ cs_indata-zcmnum
     AND varia EQ cs_indata-zvaria.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-zcmnum.
    lv_msgv2 = cs_indata-zvaria.
    PERFORM build_msgs USING gc_constants-msgty_error '040'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate Crop & variant with in validate date or not and get counter
  SELECT tplnr_fl contr
    FROM /agri/glflcma
    INTO TABLE ct_cskey
   WHERE tplnr_fl EQ gv_tplnr
     AND cmnum    EQ cs_indata-zcmnum
     AND varia    EQ cs_indata-zvaria
     AND ( datab  LE cs_indata-zdatab
     AND   datbi  GE cs_indata-zdatab )
     AND loevm    EQ space.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-zcmnum.
    PERFORM build_msgs USING gc_constants-msgty_error '034'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

*-- Get Crop Master Processes
*  IF cs_indata-zindfield EQ gc_constants-delete.
*
**Check Input RFC CPROS is valid or nor during TECHO
*    SELECT cmnum varia cpros
*      FROM /agri/glcmprs
*      INTO TABLE gt_cpros
*     WHERE cmnum EQ cs_indata-zcmnum
*       AND varia EQ cs_indata-zvaria
*       AND cpros EQ cs_indata-zcpros
*     ORDER BY cmnum varia cpros.
*    IF sy-subrc NE 0.
**-- Build messages
*      lv_msgv1 = cs_indata-zcpros.
*      lv_msgv2 = cs_indata-zvaria.
*      PERFORM build_msgs USING gc_constants-msgty_error '041'
*                               lv_msgv1 lv_msgv2
*                      CHANGING ct_messages[].
*    ENDIF.

  IF cs_indata-zindfield EQ gc_constants-insert.  " elseif

*-- Fetch all CPROS assignt to crop and variant.
    SELECT cmnum varia cpros                       "#EC CI_NO_TRANSFORM
      FROM /agri/glcmprs
      INTO TABLE gt_cpros
     WHERE cmnum EQ cs_indata-zcmnum
       AND varia EQ cs_indata-zvaria
     ORDER BY cmnum varia cpros.
    IF gt_cpros IS INITIAL.
*-- Build messages
      lv_msgv1 = cs_indata-zcmnum.
      lv_msgv2 = cs_indata-zvaria.
      PERFORM build_msgs USING gc_constants-msgty_error '046'
                               lv_msgv1 lv_msgv2
                      CHANGING ct_messages[].
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_date
*&---------------------------------------------------------------------*
FORM convert_date CHANGING cv_date     TYPE char10
                           ct_messages TYPE zvxtt_message_gprolog.

*-- Local variables
  DATA: lv_date  TYPE datum,
        lv_msgv1 TYPE msgv1.

*-- Convert date to SAP internal format
  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = cv_date
    IMPORTING
      date_internal            = lv_date
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cv_date.
    PERFORM build_msgs USING gc_constants-msgty_error '013'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ELSE.
    cv_date = lv_date.
    CONDENSE cv_date.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_terrain
*&---------------------------------------------------------------------*
FORM validate_terrain USING ps_indata   TYPE zvxs_prc_order
                   CHANGING ct_messages TYPE zvxtt_message_gprolog.
*-- Local variables
  DATA: lv_msgv1 TYPE msgv1,
        lv_msgv2 TYPE msgv2.

*-- Conversion terrain to SAP internal format.
  CLEAR gv_tplnr.
  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = ps_indata-zstrno
    IMPORTING
      output     = gv_tplnr
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.
  IF sy-subrc EQ 0.
*-- Get terrain
    SELECT tplnr_fl, iwerk
      FROM /agri/glflot
      INTO TABLE @DATA(lt_glflot)
     WHERE tplnr_fl EQ @gv_tplnr
       AND loevm    EQ @space
     ORDER BY tplnr_fl, iwerk.
  ENDIF.

  IF lt_glflot IS INITIAL.

*-- Build messages
    lv_msgv1 = ps_indata-zstrno.
    PERFORM build_msgs USING gc_constants-msgty_error '015'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ELSE.

*-- Validating Plant with terrain
    READ TABLE lt_glflot TRANSPORTING NO FIELDS
    WITH KEY   tplnr_fl = gv_tplnr
               iwerk    = ps_indata-ziwerk
    BINARY SEARCH.
    IF sy-subrc NE 0.
*-- Build messages
      lv_msgv1 = ps_indata-zstrno.
      lv_msgv2 = ps_indata-ziwerk.
      PERFORM build_msgs USING gc_constants-msgty_error '039'
                               lv_msgv1 lv_msgv2
                      CHANGING ct_messages[].
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_process_order
*&---------------------------------------------------------------------*
FORM create_process_order  USING ps_indata   TYPE zvxs_prc_order
                                 pt_cskey    TYPE /agri/t_glcs_key
                        CHANGING ct_messages TYPE zvxtt_message_gprolog
                                 ct_outdata  TYPE zvxtt_prc_order_out.

*-- Local Internal tables:
  DATA: lt_messages TYPE /agri/t_gprolog,
        lt_fpdoc    TYPE /agri/t_fmfp_doc,

*-- Local workareas:
        ls_mesg     TYPE zvxs_message_gprolog,
        ls_outdata  TYPE zvxs_prc_order_out,

*-- Local Variables:
        lv_gstrp    TYPE co_gstrp,
        lv_msgv1    TYPE msgv1,
        lv_msgv2    TYPE msgv2.

*-- Check if any existing processing order exit for Input data
  PERFORM check_order USING ps_indata.

*-- Create process orders for all crop process
  LOOP AT gt_cpros INTO DATA(gs_cpros).
    CLEAR : lv_msgv1, lv_msgv2.

*-- Check if any active process order exist for that cpros, variant
    READ TABLE gt_fmfphdr INTO DATA(gs_fmfphdr)
         WITH KEY cmnum = gs_cpros-cmnum
                  varia = gs_cpros-varia
                  cpros = gs_cpros-cpros.
    IF sy-subrc EQ 0.

*-- Append to exporting table
      ls_outdata-cpros = gs_fmfphdr-cpros.
      ls_outdata-aufnr = gs_fmfphdr-aufnr.
      APPEND ls_outdata TO ct_outdata.

*-- Build messages
      lv_msgv1 = gs_fmfphdr-aufnr.
      lv_msgv2 = gs_fmfphdr-cpros.
      PERFORM build_msgs USING gc_constants-msgty_error '043'
                               lv_msgv1 lv_msgv2
                      CHANGING ct_messages[].

    ELSE.

*-- If not active process order found, then create process order
      lv_gstrp = ps_indata-zdatab.
      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = abap_true
          i_commit_work         = abap_true
          i_cpros               = gs_cpros-cpros
          i_gstrp               = lv_gstrp
          it_cskey              = pt_cskey[]
        IMPORTING
          et_fpdoc              = lt_fpdoc[]
          et_messages           = lt_messages[]
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.
      IF sy-subrc EQ 0.
*-- Build messages
        LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
          lv_msgv1         = ls_fpdoc-aufnr.
          ls_outdata-cpros = gs_cpros-cpros.
          ls_outdata-aufnr = ls_fpdoc-aufnr.
          APPEND ls_outdata TO ct_outdata.
          PERFORM build_msgs USING gc_constants-msgty_success '038'
                                   lv_msgv1 space
                          CHANGING ct_messages[].
          CLEAR : lv_msgv1, ls_outdata, ls_fpdoc.
        ENDLOOP.
      ENDIF.

      IF lt_fpdoc IS INITIAL.
        LOOP AT lt_messages INTO DATA(ls_message).
          CLEAR : ls_mesg.
          ls_mesg = CORRESPONDING #( ls_message ).
          APPEND ls_mesg TO ct_messages.
        ENDLOOP.
      ENDIF.

      CLEAR : gs_cpros, lt_fpdoc[], lt_messages[].

    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form inactivate_process_order
*&---------------------------------------------------------------------*
FORM inactivate_process_order USING ps_indata   TYPE zvxs_prc_order
                           CHANGING ct_messages TYPE zvxtt_message_gprolog
                                    ct_outdata  TYPE zvxtt_prc_order_out.

*-- Local Internal tables:
  DATA : lt_aufnr    TYPE /agri/t_fmaufnr,
         lt_messages TYPE /agri/t_gprolog,
         lt_fpdoc    TYPE /agri/t_fmfp_doc.

**-- Local workareas:
  DATA : ls_mesg    TYPE zvxs_message_gprolog,
         ls_outdata TYPE zvxs_prc_order_out.

*-- Local Variables:
  DATA : lv_msgv1 TYPE msgv1,
         lv_msgv2 TYPE msgv2.

*-- When indicator field is 'D', we need to fetch all active
*-- Process orders based on RFC input to do Techo
  SELECT aufnr
    FROM /agri/fmfphdr
    INTO CORRESPONDING FIELDS OF TABLE lt_aufnr
   WHERE autyp    EQ gc_constants-autyp     "Document category - AO
     AND tplnr_fl EQ gv_tplnr
     AND cmnum    EQ ps_indata-zcmnum
     AND varia    EQ ps_indata-zvaria
*     AND cpros    EQ ps_indata-zcpros
     AND ( datab  LE ps_indata-zdatab
     AND   datbi  GE ps_indata-zdatab )
     AND iwerk    EQ ps_indata-ziwerk
     AND tecom    EQ space.

  IF lt_aufnr IS INITIAL.
*-- Build messages
    lv_msgv1 = ps_indata-zcmnum.
*    lv_msgv2 = ps_indata-zcpros.
    PERFORM build_msgs USING gc_constants-msgty_success '162'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages.

  ELSE.

*-- Order Mass Techincal Complete
    CALL FUNCTION '/AGRI/FMFP_ORDER_MASS_TECO'
      EXPORTING
        it_aufnr          = lt_aufnr[]
      IMPORTING
        et_fpdoc          = lt_fpdoc[]
        et_messages       = lt_messages[]
      EXCEPTIONS
        inconsistent_data = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
*-- Build messages
      LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
        lv_msgv1         = ls_fpdoc-aufnr.
*        ls_outdata-cpros = ps_indata-zcpros.
        ls_outdata-aufnr = ls_fpdoc-aufnr.
        APPEND ls_outdata TO ct_outdata.
        PERFORM build_msgs USING gc_constants-msgty_success '044'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
        CLEAR : lv_msgv1, ls_outdata, ls_fpdoc.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty NE 'S'.
      CLEAR : ls_mesg.
      ls_mesg = CORRESPONDING #( ls_message ).
      APPEND ls_mesg TO ct_messages.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form check_order
*&---------------------------------------------------------------------*
FORM check_order USING ps_indata TYPE zvxs_prc_order.

  CLEAR: gt_fmfphdr.

  IF gt_cpros IS NOT INITIAL.
*-- Fetch all acvtive process orders to check before creation
    SELECT aufnr cmnum varia cpros                 "#EC CI_NO_TRANSFORM
      FROM /agri/fmfphdr
      INTO TABLE gt_fmfphdr
       FOR ALL ENTRIES IN gt_cpros
     WHERE autyp    EQ gc_constants-autyp     "Document category - AO
       AND tplnr_fl EQ gv_tplnr
       AND cmnum    EQ ps_indata-zcmnum
       AND varia    EQ ps_indata-zvaria
       AND cpros    EQ gt_cpros-cpros
       AND ( datab  LE ps_indata-zdatab
       AND   datbi  GE ps_indata-zdatab )
       AND iwerk    EQ ps_indata-ziwerk
       AND tecom    EQ space.

    IF sy-subrc EQ 0.
      SORT gt_fmfphdr BY cmnum varia cpros.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_msgs
*&---------------------------------------------------------------------*
FORM build_msgs USING pv_msgty    TYPE msgty
                      pv_msgno    TYPE msgno
                      pv_msgv1    TYPE msgv1
                      pv_msgv2    TYPE msgv2
             CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local work areas
  DATA: ls_messages TYPE zvxs_message_gprolog.

  ls_messages-msgid = gc_constants-msgid.
  ls_messages-msgty = pv_msgty.
  ls_messages-msgno = pv_msgno.
  ls_messages-msgv1 = pv_msgv1.
  ls_messages-msgv2 = pv_msgv2.
  APPEND ls_messages TO ct_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_log
*&---------------------------------------------------------------------*
FORM save_log USING pt_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA : lt_message_log TYPE /agri/t_gprolog,

*-- Local work areas
         ls_message_log TYPE /agri/s_gprolog.

*-- Maintain message log for internal tracking purpose
  IF pt_messages IS NOT INITIAL.

    LOOP AT pt_messages ASSIGNING FIELD-SYMBOL(<fs_message>).
      ls_message_log = CORRESPONDING #( <fs_message> ).
      APPEND ls_message_log TO lt_message_log.
      CLEAR : ls_message_log.
      MESSAGE ID <fs_message>-msgid  TYPE <fs_message>-msgty
                                   NUMBER <fs_message>-msgno
                                     WITH <fs_message>-msgv1
                                          <fs_message>-msgv2
                                          <fs_message>-msgv3
                                          <fs_message>-msgv4
                                     INTO <fs_message>-message.
    ENDLOOP.

    CALL FUNCTION '/AGRI/G_PROLOG_MESSAGES_SAVE'
      EXPORTING
        i_aplobj   = gc_constants-aplobj
        i_subobj   = gc_constants-subobj
      TABLES
        t_messages = lt_message_log[].

  ENDIF.

ENDFORM.
