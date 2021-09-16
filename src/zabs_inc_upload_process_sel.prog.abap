*&---------------------------------------------------------------------*
*&  Include           /AGRI/GLPG_UPLOAD_PROCESS_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_file TYPE char255 OBLIGATORY DEFAULT 'C:',
            p_at   RADIOBUTTON GROUP rb01 DEFAULT 'X' MODIF ID upl,
            p_af   RADIOBUTTON GROUP rb01 MODIF ID upl,
            p_ag   RADIOBUTTON GROUP rb01 MODIF ID up1,
            p_cm   RADIOBUTTON GROUP rb01 MODIF ID up1,
            p_cs   RADIOBUTTON GROUP rb01 MODIF ID up1,
            p_te   RADIOBUTTON GROUP rb01 MODIF ID up1,
            p_ro   RADIOBUTTON GROUP rb01 MODIF ID up1,
            p_md   RADIOBUTTON GROUP rb01 MODIF ID up2,
            p_ie   RADIOBUTTON GROUP rb01 MODIF ID up2,
            p_sol  RADIOBUTTON GROUP rb01 MODIF ID up1.
SELECTION-SCREEN END OF BLOCK b0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM file_path CHANGING p_file.
