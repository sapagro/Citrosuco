*&---------------------------------------------------------------------*
*& Report ZABS_REP_GIS_TOKEN
*&---------------------------------------------------------------------*

REPORT zabs_rep_gis_token.

*--Global data declarations
INCLUDE zabs_rep_gis_token_top.

*--Processing data
INCLUDE zabs_rep_gis_token_sub.

START-OF-SELECTION.

*--Update GIS Token
  PERFORM update_gistoken.
