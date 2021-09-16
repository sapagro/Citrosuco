*&---------------------------------------------------------------------*
*& Form RANGE_TERRAIN_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TEMPFMACITM
*&      <-- LT_RMG_TPLNR_FL
*&---------------------------------------------------------------------*
FORM range_terrain_create  USING    lt_tempfmacitm TYPE zt_fmacitm
                           CHANGING lt_rng_tplnr_fl TYPE zfmac_terrain_range_t.

  DATA: lwa_fmacitm      TYPE zsc_fmacitm,
        lwa_rng_tplnr_fl TYPE zfmac_terrain_range_s.

   LOOP AT lt_tempfmacitm INTO lwa_fmacitm.
     lwa_rng_tplnr_fl-sign = 'I'.
     lwa_rng_tplnr_fl-option = 'EQ'.
     lwa_rng_tplnr_fl-low = lwa_fmacitm-tplnr_fl.
     APPEND lwa_rng_tplnr_fl to lt_rng_tplnr_fl.
   ENDLOOP.

   SORT lt_rng_tplnr_fl ASCENDING by low.
   DELETE ADJACENT DUPLICATES FROM lt_rng_tplnr_fl COMPARING low.

ENDFORM.
