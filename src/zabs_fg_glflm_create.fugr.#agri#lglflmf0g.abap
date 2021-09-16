*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0G .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GENERAL_DESCRIPTIONS_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM general_descriptions_fill .

*  IF /agri/s_gliflot-begru IS NOT INITIAL.
*    keytext_get_simple 'T370B_T' 'BEGRU' /agri/s_gliflot-begru
*              /agri/s_glflscrfields-begtx.
*  ENDIF.

  IF /agri/s_glflot-tplma IS NOT INITIAL.
    keytext_get_simple '/AGRI/GLFLOS' 'TPLNR_FL' /agri/s_glflot-tplma
              /agri/s_glflscrfields-pltxt_ma.
  ENDIF.

  PERFORM owner_text_get USING /agri/s_glflot-owrol /agri/s_glflot-owner
                      CHANGING /agri/s_glflscrfields-owner_txt.

ENDFORM.                    " GENERAL_DESCRIPTIONS_FILL
*&---------------------------------------------------------------------*
*&      Form  GET_SUPERIOR_FUNLOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_superior_funloc  USING    lv_tplnr TYPE /agri/glstrno
                                   lv_stufm TYPE /agri/glstufm
                          CHANGING lv_tplma TYPE tplma
                                   lv_subrc.

  DATA:
    lv_strno    TYPE /agri/glstrno,
    lv_offset   TYPE i,
    lv_generic  TYPE i,
    lv_strno_ma TYPE /agri/glstrno,
    lv_txxma    TYPE /agri/gltplnr_fl,
    lv_loevm    TYPE /agri/glflot-loevm.
*      ls_iflos_ma TYPE iflos.
*      r_strno     TYPE ilox_r_strno,
*      rt_strno    TYPE ilox_rt_strno,
*      lt_iflos    TYPE ilox_t_iflos.

  FIELD-SYMBOLS: <lv_pos_stufm>.

  IF lv_stufm IS INITIAL.
    PERFORM fill_edit_mask.
  ENDIF.
  lv_strno = lv_tplnr.
  lv_generic = strlen( lv_strno ).
  lv_offset = lv_generic - 1.

  DO.

    IF lv_offset = 0.
      EXIT.
    ENDIF.

    lv_offset = lv_offset - 1.
    ASSIGN lv_stufm+lv_offset(1) TO <lv_pos_stufm>.
    IF <lv_pos_stufm> IS NOT INITIAL.

      lv_generic = lv_offset + 1.
      MOVE lv_strno(lv_generic) TO lv_strno_ma.
      CLEAR lv_txxma.
      SELECT SINGLE tplnr_fl FROM /agri/glflot     "#EC CI_SEL_NESTED
               INTO lv_txxma
              WHERE strno EQ lv_strno_ma. "#EC CI_NOORDER

*      MOVE: 'I'        TO r_strno-sign,
*            'EQ'       TO r_strno-option,
*            lv_strno_ma TO r_strno-low.
*      APPEND r_strno TO rt_strno.
*      REFRESH lt_iflos.
*      CALL FUNCTION 'ILOX_IFLOS_READ_BY_STRNO'
*        EXPORTING
*          it_strno = rt_strno
**         i_alkey  = l_iflos-alkey
*        IMPORTING
*          et_iflos = lt_iflos
*        EXCEPTIONS
*          OTHERS   = 1.
*      IF sy-subrc = 0.
*        LOOP AT lt_iflos INTO ls_iflos_ma
*             WHERE actvs = 'X'.
*          MOVE ls_iflos_ma-tplnr TO lv_txxma.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
      IF lv_txxma IS INITIAL.
        MOVE lv_strno_ma TO lv_txxma.
      ENDIF.

      IF lv_txxma IS INITIAL.
        sy-subrc = 4.
*      ELSE.
*        CALL FUNCTION 'FUNC_LOCATION_READ'
*          EXPORTING
*            tplnr           = lv_txxma
**          IMPORTING
**           iflo_wa         = lwa_iflo
**           PLTXT           =
*          EXCEPTIONS
*            iflot_not_found = 1
*            iloa_not_found  = 2
*            no_authority    = 3
*            OTHERS          = 4.
*        IF sy-subrc <> 0.
** Implement suitable error handling here
*        ENDIF.
      ENDIF.

      EXIT.
    ENDIF.
  ENDDO.

  IF lv_txxma IS NOT INITIAL.
    SELECT SINGLE tplnr_fl loevm
             INTO (lv_tplma,lv_loevm)
             FROM /agri/glflot
            WHERE tplnr_fl EQ lv_txxma.
    IF sy-subrc NE 0.
      lv_subrc = 4.
      MESSAGE e015(/agri/glfl) WITH lv_txxma
                           INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ELSEIF lv_loevm IS NOT INITIAL.
      lv_subrc = 4.
      MESSAGE e056(/agri/glfl) WITH lv_txxma
                           INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_variables-new_label IS NOT INITIAL AND
     lv_subrc IS INITIAL AND
     */agri/s_glflot-tplma NE lv_tplma.
    MESSAGE i058(/agri/glfl) WITH lv_strno_ma
                     INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " GET_SUPERIOR_FUNLOC
*&---------------------------------------------------------------------*
*&      Form  GET_FUNLOC_LEVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_funloc_level USING lv_tplnr TYPE /agri/glstrno
                            lv_stufm TYPE /agri/glstufm
                   CHANGING lv_tplvl TYPE /agri/gltplvl
                            lv_subrc.

  DATA:
    lv_strno   TYPE /agri/glstrno,
    lv_offset  TYPE i,
    lv_generic TYPE i,
    lv_maxlen  TYPE i.

  FIELD-SYMBOLS: <lv_pos_stufm>.

  IF lv_stufm IS INITIAL.
    PERFORM fill_edit_mask.
  ENDIF.
  lv_strno = lv_tplnr.
  lv_generic = strlen( lv_strno ).
  lv_offset = lv_generic - 1.

  lv_maxlen = strlen( lv_stufm ).
  DO.

    IF lv_offset GT lv_maxlen.
      EXIT.
    ENDIF.

    ASSIGN lv_stufm+lv_offset(1) TO <lv_pos_stufm>.
    IF <lv_pos_stufm> IS NOT INITIAL.
      lv_tplvl = <lv_pos_stufm>.
      EXIT.
    ENDIF.
    lv_offset = lv_offset + 1.

  ENDDO.
  IF lv_tplvl IS INITIAL.
    lv_subrc = 4.
    MESSAGE e031(/agri/glfl) INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_FUNLOC_LEVEL
*&---------------------------------------------------------------------*
*&      Form  GRID_CHANGES_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_changes_check  CHANGING lv_modified.

  IF NOT ref_owners_grid IS INITIAL.
    lv_modified = ref_owners_grid->data_modified_check( ).
  ENDIF.

  CHECK lv_modified IS INITIAL.
  IF NOT ref_plants_grid IS INITIAL.
    lv_modified = ref_plants_grid->data_modified_check( ).
  ENDIF.

  CHECK lv_modified IS INITIAL.
  IF NOT ref_classes_grid IS INITIAL.
    lv_modified = ref_classes_grid->data_modified_check( ).
  ENDIF.

  CHECK lv_modified IS INITIAL.
  IF NOT ref_attributes_grid IS INITIAL.
    lv_modified = ref_attributes_grid->data_modified_check( ).
  ENDIF.

ENDFORM.                    " GRID_CHANGES_CHECK
*&---------------------------------------------------------------------*
*& Form GET_STRUCTURE_INDICATOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> /AGRI/S_GLFLOT_TPLKZ
*&      <-- LS_TGL370S
*&---------------------------------------------------------------------*
FORM get_edit_mask  USING    lv_tplkz TYPE /agri/gltplkz
                              CHANGING ls_tgl370s TYPE /agri/tgl370s.

  STATICS: ls_tgl370s_buff TYPE /agri/tgl370s.

  CHECK lv_tplkz IS NOT INITIAL.
  IF lv_tplkz NE ls_tgl370s_buff-tplkz.
    CLEAR ls_tgl370s_buff.
    SELECT SINGLE * FROM /agri/tgl370s
      INTO ls_tgl370s_buff WHERE tplkz EQ lv_tplkz.
  ENDIF.
  ls_tgl370s = ls_tgl370s_buff.

ENDFORM.
