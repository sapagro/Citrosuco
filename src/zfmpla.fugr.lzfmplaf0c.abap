*&---------------------------------------------------------------------*
*& Include          LZFMACAF0C
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form customizing_planning_get
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM customizing_planning_get USING lv_plann TYPE zfmplann
                              CHANGING ls_fmpltyp TYPE ztfmpltyp.
  SELECT SINGLE *  FROM ztfmpltyp
               INTO ls_fmpltyp
                   WHERE plann = lv_plann.
ENDFORM.
