*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0U .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form UPDATE_MASS_VOLUMEN_CALDA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_mass_volumen_calda .

  DATA: lwa_vlcl       TYPE zsc_fmacvlcl,
        lwa_vcl_layout LIKE LINE OF gt_fmacvlc_fcat,
        ls_fmvlcl      TYPE zsc_fmacvlcl,
        lt_vlcl        TYPE zt_fmacvlcl,
        lwa_mod_row    TYPE lvc_s_modi,
        lv_typ         TYPE char2 VALUE '02',
        lv_tabix       TYPE sy-tabix,
        lv_modified,
        lv_subrc       TYPE int4,
        lv_valid.

  FIELD-SYMBOLS: <lwa_vlcl>       TYPE zsc_fmacvlcl,
                 <lwa_vcl_layout> TYPE zsc_fmacvlcl_fcat.

  LOOP AT gt_fmacvlc_fcat INTO lwa_vcl_layout.
    MOVE-CORRESPONDING lwa_vcl_layout TO lwa_vlcl.
    READ TABLE gs_acdoc_infocus-x-acvlc ASSIGNING <lwa_vlcl>
                                      WITH KEY acnum = lwa_vcl_layout-acnum
                                      vornr = lwa_vcl_layout-vornr
                                      matnr = lwa_vcl_layout-matnr
                                      posnr = lwa_vcl_layout-posnr
                                      tplnr_fl = lwa_vcl_layout-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_vlcl NE <lwa_vlcl>.
        gs_variables-document_changed = c_true.
        IF <lwa_vlcl>-updkz NE c_updkz_new.
          MOVE lwa_vlcl TO <lwa_vlcl>.
          <lwa_vlcl>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF gs_variables-document_changed IS NOT INITIAL.
    gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
  ENDIF.

ENDFORM.
