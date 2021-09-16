*----------------------------------------------------------------------*
***INCLUDE LZFMPLAF0D.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form data_planning_get
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM data_planning_get USING lv_acnum TYPE zfmacnum
                             lv_plann TYPE zfmplann
                       CHANGING lt_zfmachdr TYPE zt_fmachdr
                                lt_zfmacitm TYPE zt_fmacitm
                                lt_fmfphdr  TYPE /agri/t_fmfphdr
                                lt_fmfpitm  TYPE /agri/t_fmfpitm
                                lt_fmfpcom  TYPE /agri/t_fmfpcom
                                lt_mapl     TYPE tt_mapl
                                lt_plpo     TYPE plpo_tt
                                lt_crhd     TYPE tt_crhd
                                lt_kapa     TYPE comes_t_drf_kapa
                                lt_tc37a    TYPE tt_tc37a
                                lt_glflotx  TYPE zt_glflotx.

  DATA: ls_fmpltyp TYPE ztfmpltyp.

  PERFORM customizing_planning_get USING lv_plann
                                 CHANGING ls_fmpltyp.
*--- Header
  IF lv_acnum IS NOT INITIAL.
    SELECT * FROM zfmachdr
             INTO TABLE lt_zfmachdr
                        WHERE acnum = lv_acnum.
  ENDIF.
*--- Items
  IF lt_zfmachdr[] IS NOT INITIAL.
    SELECT * FROM zfmaitm
             INTO TABLE lt_zfmacitm
                        WHERE acnum = lv_acnum.
  ENDIF.
*--- Process order header
  IF lt_zfmacitm[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfphdr
             INTO TABLE lt_fmfphdr
                       FOR ALL ENTRIES IN lt_zfmacitm[]
                       WHERE tplnr_fl = lt_zfmacitm-tplnr_fl
                 AND   auart    NE ls_fmpltyp-auart
                 AND   autyp    EQ ls_fmpltyp-autyp.
  ENDIF.
*--- Process order items
  IF lt_fmfphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpitm
             INTO TABLE lt_fmfpitm
                       FOR ALL ENTRIES IN lt_fmfpitm[]
                       WHERE  aufnr EQ lt_fmfpitm-aufnr.
  ENDIF.
*--- Descriptions
  IF lt_zfmacitm[] IS NOT INITIAL.
    SELECT * FROM /agri/glflotx
             INTO TABLE lt_glflotx
                       FOR ALL ENTRIES IN lt_zfmacitm[]
                       WHERE tplnr_fl = lt_zfmacitm-tplnr_fl
                 AND   spras   EQ sy-langu.
  ENDIF.
*--- Components
  IF lt_fmfphdr[] IS NOT INITIAL.
    SELECT * FROM /agri/fmfpcom
               INTO TABLE lt_fmfpcom
                         FOR ALL ENTRIES IN lt_fmfphdr[]
                         WHERE aufnr = lt_fmfphdr-aufnr.
  ENDIF.
*--- Routing
  IF lt_fmfpcom[] IS NOT INITIAL.
    SELECT * FROM mapl
             INTO TABLE lt_mapl
                   FOR ALL ENTRIES IN lt_fmfpcom
                    WHERE matnr EQ lt_fmfpcom-matnr
                             AND  plnty  EQ ls_fmpltyp-plnty
                             AND  werks EQ lt_fmfpcom-werks.
  ENDIF.
*--- Work Center
  IF lt_mapl[] IS NOT INITIAL.
    SELECT * FROM plpo
             INTO TABLE lt_plpo
                  FOR ALL ENTRIES IN lt_mapl[]
                     WHERE plnnr EQ lt_mapl-plnnr
                     AND ( plnty EQ ls_fmpltyp-plnty ).
  ENDIF.
*--- Description W.C. and Aviability
  IF lt_plpo[] IS NOT INITIAL.
    SELECT * FROM crhd
             INTO TABLE lt_crhd
                  FOR ALL ENTRIES IN lt_plpo[]
                      WHERE objid EQ lt_plpo-arbid
                      AND ( objty EQ ls_fmpltyp-objty ).
  ENDIF.
*--- Turns
  IF  lt_crhd[] IS NOT INITIAL.
    SELECT * FROM kapa
                   INTO TABLE lt_kapa
                          FOR ALL ENTRIES IN lt_crhd[]
                               WHERE kapid EQ lt_crhd-kapid.
  ENDIF.
*--- Definição de turno
  IF lt_kapa[] IS NOT INITIAL.
    SELECT * FROM tc37a
                   INTO TABLE lt_tc37a
                               WHERE schgrup EQ ls_fmpltyp-schgrup.
  ENDIF.

ENDFORM.
