*&---------------------------------------------------------------------*
*& Include          LZFMRCMSEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 0010.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-034.
PARAMETERS: p_actyp TYPE zfmachdr-actyp,
            p_acdes TYPE zfmachdr-acdes,
            p_ajahr TYPE zfmachdr-ajahr,
            p_datab TYPE zfmachdr-datab,
            p_datbi TYPE zfmachdr-datbi.

SELECT-OPTIONS: so_werks FOR zfmachdr-werks.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0010.
