*&---------------------------------------------------------------------*
*&  Include           ZABS_INC_CHECKIN_FRUTA_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_centro TYPE /agri/fmprhdr-werks,
            p_caval  TYPE /agri/fmprhdr-lic_plate,
            p_zrems  TYPE likp-vbeln,
            p_arrend TYPE /agri/fmprhdr-zarrend AS LISTBOX VISIBLE LENGTH 20,
*            p_dest   TYPE /agri/fmprhdr-werks,
            p_reb1   TYPE /agri/fmprhdr-semireb1,
            p_reb2   TYPE /agri/fmprhdr-semireb2,
            p_tara   TYPE /agri/fmprhdr-zfmprftw,
            p_tipo   TYPE /agri/fmprhdr-tipo_viagem MODIF ID m1,
            p_nro    TYPE /agri/fmprhdr-prnum MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b0.
