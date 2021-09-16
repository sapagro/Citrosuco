*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0H.
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  HEADER_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_get  TABLES lt_selection TYPE /agri/t_bsel_values.

*  DATA: lt_flot_db   TYPE /agri/t_glflot,
*        lt_iflot_db  TYPE /agri/t_gliflot,
*        lt_adrc_db   TYPE /agri/t_gladrc,
*        lt_iloa_db   TYPE /agri/t_gliloa,
*        lt_iflotx_db TYPE /agri/t_gliflotx,
*        lt_flatg_db  TYPE /agri/t_glflatg,
*        lt_flatv_db  TYPE /agri/t_glflatv,
*        lt_flown_db  TYPE /agri/t_glflown,
*        lt_flot_hdb  TYPE /agri/t_glflot,
*        lt_flot      TYPE /agri/t_glflot,
*        lv_subrc     TYPE sy-subrc,
*        lv_flag.
*
*  DATA: lwa_search_hdr        TYPE /agri/glflot,
*        lt_header_sel_opt     TYPE TABLE OF char72,
*        lt_iloa_sel_opt       TYPE TABLE OF char72,
*        lv_hdr_sel_specified,
*        lv_iloa_sel_specified.
*
*  FIELD-SYMBOLS: <lwa_flot_db>   TYPE /agri/s_glflot,
*                 <lwa_iflot_db>  TYPE /agri/s_gliflot,
*                 <lwa_adrc_db>   TYPE /agri/s_gladrc,
*                 <lwa_iloa_db>   TYPE /agri/s_gliloa,
*                 <lwa_iflotx_db> TYPE /agri/s_gliflotx,
*                 <lwa_flatg_db>  TYPE /agri/s_glflatg,
*                 <lwa_flatv_db>  TYPE /agri/s_glflatv,
*                 <lwa_flown_db>  TYPE /agri/s_glflown,
*                 <lwa_flot>      TYPE /agri/s_glflot.
*
*  STATICS: ls_flot_fields   TYPE rsfs_tab_fields,
*           ls_flot_where    TYPE rsds_where,
*           ls_iflot_fields  TYPE rsfs_tab_fields,
*           ls_iflot_where   TYPE rsds_where,
*           ls_adrc_fields   TYPE rsfs_tab_fields,
*           ls_adrc_where    TYPE rsds_where,
*           ls_iloa_fields   TYPE rsfs_tab_fields,
*           ls_iloa_where    TYPE rsds_where,
*           ls_iflotx_fields TYPE rsfs_tab_fields,
*           ls_iflotx_where  TYPE rsds_where,
*           ls_flatg_fields  TYPE rsfs_tab_fields,
*           ls_flatg_where   TYPE rsds_where,
*           ls_flatv_fields  TYPE rsfs_tab_fields,
*           ls_flatv_where   TYPE rsds_where,
*           ls_flown_fields  TYPE rsfs_tab_fields,
*           ls_flown_where   TYPE rsds_where.
*
** STATICS FLAG.
*  IF lv_flag = space.
*    lv_flag = 'X'.
*
*****Declarations for field selection for node /AGRI/GLFLOT
*    MOVE c_tabname-header TO ls_flot_fields-tablename.
*
*****Declarations for dynamic selections for node /AGRI/GLFLOT
*    MOVE c_tabname-header TO ls_flot_where-tablename.
*
*****Declarations for field selection for node IFLOT
*    MOVE c_tabname-iflot TO ls_iflot_fields-tablename.
*
*****Declarations for dynamic selections for node IFLOT
*    MOVE c_tabname-iflot TO ls_iflot_where-tablename.
**
******Declarations for field selection for node ADRC
*    MOVE c_tabname-adrc TO ls_adrc_fields-tablename.
*
****Declarations for dynamic selections for node ADRC
*    MOVE c_tabname-adrc TO ls_adrc_where-tablename.
*
*****Declarations for field selection for node ILOA
*    MOVE c_tabname-iloa TO ls_iloa_fields-tablename.
*
*****Declarations for dynamic selections for node ILOA
*    MOVE c_tabname-iloa TO ls_iloa_where-tablename.
*
*****Declarations for field selection for node IFLOTX
*    MOVE c_tabname-iflotx TO ls_iflotx_fields-tablename.
*
*****Declarations for dynamic selections for node IFLOTX
*    MOVE c_tabname-iflotx TO ls_iflotx_where-tablename.
*
****Declarations for field selections for node /AGRI/GLFLATG
*    MOVE c_tabname-flatg TO ls_flatg_fields-tablename.
*
*****Declarations for dynamic selections for node /AGRI/GLFLATG
*    MOVE c_tabname-flatg TO ls_flatg_where-tablename.
*
****Declarations for field selections for node /AGRI/GLFLATV
*    MOVE c_tabname-flatv TO ls_flatv_fields-tablename.
*
*****Declarations for dynamic selections for node /AGRI/GLFLATV
*    MOVE c_tabname-flatv TO ls_flatv_where-tablename.
*
****Declarations for field selections for node /AGRI/GLFLOWN
*    MOVE c_tabname-flown TO ls_flown_fields-tablename.
*
*****Declarations for dynamic selections for node /AGRI/GLFLOWN
*    MOVE c_tabname-flown TO ls_flown_where-tablename.
*  ENDIF.
*
*  IF  lt_selection[] IS NOT INITIAL.
*    PERFORM where_clause_prepare    TABLES lt_selection
*                                  CHANGING ls_flot_where-where_tab
*                                           ls_iflot_where-where_tab
*                                           ls_adrc_where-where_tab
*                                           ls_iloa_where-where_tab
*                                           ls_iflotx_where-where_tab
*                                           ls_flatg_where-where_tab
*                                           ls_flatv_where-where_tab
*                                           ls_flown_where-where_tab.
*  ENDIF.
*
*  IF        so_strno[] IS INITIAL
*         AND so_pltxt[] IS INITIAL
*         AND so_fltyp[] IS INITIAL
*         AND so_tplvl[] IS INITIAL
*         AND so_pspnr[] IS INITIAL
*         AND so_tplkz[] IS INITIAL
*         AND so_tplma[] IS INITIAL
*         AND so_owrol[] IS INITIAL
*         AND so_owner[] IS INITIAL
*         AND so_ownsh[] IS INITIAL
*         AND so_garea[] IS INITIAL
*         AND so_eqfnr[] IS INITIAL
*         AND so_grup1[] IS INITIAL
*         AND so_grup2[] IS INITIAL
*         AND so_grup3[] IS INITIAL
*         AND so_stort[] IS INITIAL
*         AND so_swerk[] IS INITIAL
*         AND so_bukrs[] IS INITIAL
*         AND so_iwerk[] IS INITIAL
*         AND so_ernam[] IS INITIAL
*         AND so_erdat[] IS INITIAL
*         AND so_aenam[] IS INITIAL
*         AND so_aedat[] IS INITIAL
*         AND ls_flot_where-where_tab[]   IS INITIAL
*         AND ls_iflot_where-where_tab[]  IS INITIAL
*         AND ls_adrc_where-where_tab[]   IS INITIAL
*         AND ls_iloa_where-where_tab[]   IS INITIAL
*         AND ls_iflotx_where-where_tab[] IS INITIAL
*         AND ls_flatg_where-where_tab[]  IS INITIAL
*         AND ls_flatv_where-where_tab[]  IS INITIAL
*         AND ls_flown_where-where_tab[]  IS INITIAL.
**    EXIT.
*  ENDIF.
*
**  PERFORM where_table_prepare TABLES lt_header_sel_opt[]
**                                     lt_iloa_sel_opt[]
**                               USING lv_hdr_sel_specified
**                                     lv_iloa_sel_specified.
*
*  IF    so_strno[] IS NOT INITIAL
*     OR so_pltxt[] IS NOT INITIAL
*     OR so_fltyp[] IS NOT INITIAL
*     OR so_tplvl[] IS NOT INITIAL
*     OR so_pspnr[] IS NOT INITIAL
*     OR so_tplkz[] IS NOT INITIAL
*     OR so_tplma[] IS NOT INITIAL
*     OR so_owrol[] IS NOT INITIAL
*     OR so_owner[] IS NOT INITIAL
*     OR so_ownsh[] IS NOT INITIAL
*     OR so_garea[] IS NOT INITIAL
*     OR so_eqfnr[] IS NOT INITIAL
*     OR so_grup1[] IS NOT INITIAL
*     OR so_grup2[] IS NOT INITIAL
*     OR so_grup3[] IS NOT INITIAL
*     OR so_stort[] IS NOT INITIAL
*     OR so_bukrs[] IS NOT INITIAL
*     OR so_iwerk[] IS NOT INITIAL
*     OR so_ernam[] IS NOT INITIAL
*     OR so_erdat[] IS NOT INITIAL
*     OR so_aenam[] IS NOT INITIAL
*     OR so_aedat[] IS NOT INITIAL.
*
*    SELECT tplnr_fl adrnr
*      INTO CORRESPONDING FIELDS OF TABLE lt_flot_db
*      FROM /agri/glflot
*      UP TO p_mxhit ROWS
*      WHERE strno    IN so_strno
*        AND pltxt    IN so_pltxt
*        AND tplkz    IN so_tplkz
*        AND fltyp    IN so_fltyp
*        AND tplvl    IN so_tplvl
*        AND tplma    IN so_tplma
*        AND pspnr    IN so_pspnr
*        AND bukrs    IN so_bukrs
*        AND iwerk    IN so_iwerk
*        AND garea    IN so_garea
*        AND stort    IN so_stort
*        AND grup1    IN so_grup1
*        AND grup2    IN so_grup2
*        AND grup3    IN so_grup3
*        AND owrol    IN so_owrol
*        AND owner    IN so_owner
*        AND ownshp   IN so_ownsh
*        AND eqfnr    IN so_eqfnr
*        AND ernam    IN so_ernam
*        AND erdat    IN so_erdat
*        AND aenam    IN so_aenam
*        AND aedat    IN so_aedat.
*  ENDIF.
*
*  IF ls_flot_where-where_tab IS NOT INITIAL.
*    SELECT * FROM /agri/glflot
*      INTO CORRESPONDING FIELDS OF TABLE lt_flot_hdb
*       UP TO p_mxhit ROWS
*      WHERE (ls_flot_where-where_tab).
*  ENDIF.
*
*  IF ls_iflot_where-where_tab IS NOT INITIAL.
*    SELECT * FROM iflot
*      INTO CORRESPONDING FIELDS OF TABLE lt_iflot_db
*       UP TO p_mxhit ROWS
*      WHERE (ls_iflot_where-where_tab).
*  ENDIF.
*
*  IF ls_adrc_where-where_tab IS NOT INITIAL.
*    SELECT * FROM adrc
*      INTO CORRESPONDING FIELDS OF TABLE lt_adrc_db
*       UP TO p_mxhit ROWS
*      WHERE (ls_adrc_where-where_tab).
*  ENDIF.
*
*  IF ls_iloa_where-where_tab IS NOT INITIAL
*  OR so_swerk[]              IS NOT INITIAL.
*    SELECT * FROM iloa
*      INTO CORRESPONDING FIELDS OF TABLE lt_iloa_db
*       UP TO p_mxhit ROWS
*      WHERE  swerk IN so_swerk
*        AND (ls_iloa_where-where_tab).
*  ENDIF.
*
*  IF ls_iflotx_where-where_tab IS NOT INITIAL.
*    SELECT * FROM iflotx
*      INTO CORRESPONDING FIELDS OF TABLE lt_iflotx_db
*       UP TO p_mxhit ROWS
*      WHERE (ls_iflotx_where-where_tab).
*  ENDIF.
*
*  IF ls_flatg_where-where_tab IS NOT INITIAL.
*    SELECT * FROM /agri/glflatg
*      INTO CORRESPONDING FIELDS OF TABLE lt_flatg_db
*      WHERE (ls_flatg_where-where_tab).
*  ENDIF.
*
*  IF ls_flatv_where-where_tab IS NOT INITIAL.
*    SELECT * FROM /agri/glflatv
*      INTO CORRESPONDING FIELDS OF TABLE lt_flatv_db
*       UP TO p_mxhit ROWS
*      WHERE (ls_flatv_where-where_tab).
*  ENDIF.
*
*  IF ls_flown_where-where_tab IS NOT INITIAL.
*    SELECT *  FROM /agri/glflown
*      INTO CORRESPONDING FIELDS OF TABLE lt_flown_db
*       UP TO p_mxhit ROWS
*      WHERE (ls_flown_where-where_tab).
*  ENDIF.
*
*  LOOP AT lt_flot_db ASSIGNING <lwa_flot_db>.
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
**    IF ls_iflot_where-where_tab IS NOT INITIAL.
**      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
**      IF sy-subrc <> 0 .
**        lv_subrc = 1.
**      ENDIF.
**    ENDIF.
*
*    IF ls_adrc_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_adrc_db TRANSPORTING NO FIELDS WITH KEY addrnumber = <lwa_flot_db>-adrnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
**    IF ls_iloa_where-where_tab IS NOT INITIAL
**      OR so_swerk[]            IS NOT INITIAL .
**      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
**      IF sy-subrc <> 0 .
**        lv_subrc = 1.
**      ENDIF.
**    ENDIF.
*
**    IF ls_iflotx_where-where_tab IS NOT INITIAL.
**      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
**      IF sy-subrc <> 0 .
**        lv_subrc = 1.
**      ENDIF.
**    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      <lwa_flot>-adrnr    = <lwa_flot_db>-adrnr.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  UNASSIGN <lwa_flot_db>.
*
*  LOOP AT lt_flot_hdb ASSIGNING <lwa_flot_db>.
*    IF     so_strno[] IS NOT INITIAL
*        OR so_pltxt[] IS NOT INITIAL
*        OR so_fltyp[] IS NOT INITIAL
*        OR so_tplvl[] IS NOT INITIAL
*        OR so_pspnr[] IS NOT INITIAL
*        OR so_tplkz[] IS NOT INITIAL
*        OR so_tplma[] IS NOT INITIAL
*        OR so_owrol[] IS NOT INITIAL
*        OR so_owner[] IS NOT INITIAL
*        OR so_ownsh[] IS NOT INITIAL
*        OR so_garea[] IS NOT INITIAL
*        OR so_eqfnr[] IS NOT INITIAL
*        OR so_grup1[] IS NOT INITIAL
*        OR so_grup2[] IS NOT INITIAL
*        OR so_grup3[] IS NOT INITIAL
*        OR so_stort[] IS NOT INITIAL
*        OR so_bukrs[] IS NOT INITIAL
*        OR so_iwerk[] IS NOT INITIAL
*        OR so_ernam[] IS NOT INITIAL
*        OR so_erdat[] IS NOT INITIAL
*        OR so_aenam[] IS NOT INITIAL
*        OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_adrc_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_adrc_db TRANSPORTING NO FIELDS WITH KEY addrnumber = <lwa_flot_db>-adrnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[]           IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflotx_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_flot_db>-tplnr_fl.
*      <lwa_flot>-adrnr   = <lwa_flot_db>-adrnr.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_iflot_db ASSIGNING <lwa_iflot_db>.
*    IF  so_strno[] IS NOT INITIAL
*     OR so_pltxt[] IS NOT INITIAL
*     OR so_fltyp[] IS NOT INITIAL
*     OR so_tplvl[] IS NOT INITIAL
*     OR so_pspnr[] IS NOT INITIAL
*     OR so_tplkz[] IS NOT INITIAL
*     OR so_tplma[] IS NOT INITIAL
*     OR so_owrol[] IS NOT INITIAL
*     OR so_owner[] IS NOT INITIAL
*     OR so_ownsh[] IS NOT INITIAL
*     OR so_garea[] IS NOT INITIAL
*     OR so_eqfnr[] IS NOT INITIAL
*     OR so_grup1[] IS NOT INITIAL
*     OR so_grup2[] IS NOT INITIAL
*     OR so_grup3[] IS NOT INITIAL
*     OR so_stort[] IS NOT INITIAL
*     OR so_bukrs[] IS NOT INITIAL
*     OR so_iwerk[] IS NOT INITIAL
*     OR so_ernam[] IS NOT INITIAL
*     OR so_erdat[] IS NOT INITIAL
*     OR so_aenam[] IS NOT INITIAL
*     OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflotx_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflot_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_iflot_db>-tplnr.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_adrc_db ASSIGNING <lwa_adrc_db>.
*    IF  so_strno[] IS NOT INITIAL
*     OR so_pltxt[] IS NOT INITIAL
*     OR so_fltyp[] IS NOT INITIAL
*     OR so_tplvl[] IS NOT INITIAL
*     OR so_pspnr[] IS NOT INITIAL
*     OR so_tplkz[] IS NOT INITIAL
*     OR so_tplma[] IS NOT INITIAL
*     OR so_owrol[] IS NOT INITIAL
*     OR so_owner[] IS NOT INITIAL
*     OR so_ownsh[] IS NOT INITIAL
*     OR so_garea[] IS NOT INITIAL
*     OR so_eqfnr[] IS NOT INITIAL
*     OR so_grup1[] IS NOT INITIAL
*     OR so_grup2[] IS NOT INITIAL
*     OR so_grup3[] IS NOT INITIAL
*     OR so_stort[] IS NOT INITIAL
*     OR so_bukrs[] IS NOT INITIAL
*     OR so_iwerk[] IS NOT INITIAL
*     OR so_ernam[] IS NOT INITIAL
*     OR so_erdat[] IS NOT INITIAL
*     OR so_aenam[] IS NOT INITIAL
*     OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY adrnr = <lwa_adrc_db>-addrnumber.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY adrnr = <lwa_adrc_db>-addrnumber.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY adrnr = <lwa_adrc_db>-addrnumber.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-adrnr = <lwa_adrc_db>-addrnumber.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_iloa_db ASSIGNING <lwa_iloa_db>.
*    IF   so_strno[] IS NOT INITIAL
*      OR so_pltxt[] IS NOT INITIAL
*      OR so_fltyp[] IS NOT INITIAL
*      OR so_tplvl[] IS NOT INITIAL
*      OR so_pspnr[] IS NOT INITIAL
*      OR so_tplkz[] IS NOT INITIAL
*      OR so_tplma[] IS NOT INITIAL
*      OR so_owrol[] IS NOT INITIAL
*      OR so_owner[] IS NOT INITIAL
*      OR so_ownsh[] IS NOT INITIAL
*      OR so_garea[] IS NOT INITIAL
*      OR so_eqfnr[] IS NOT INITIAL
*      OR so_grup1[] IS NOT INITIAL
*      OR so_grup2[] IS NOT INITIAL
*      OR so_grup3[] IS NOT INITIAL
*      OR so_stort[] IS NOT INITIAL
*      OR so_bukrs[] IS NOT INITIAL
*      OR so_iwerk[] IS NOT INITIAL
*      OR so_ernam[] IS NOT INITIAL
*      OR so_erdat[] IS NOT INITIAL
*      OR so_aenam[] IS NOT INITIAL
*      OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iloa_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iloa_db>-tplnr.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
**    IF ls_iflot_where-where_tab IS NOT INITIAL.
**      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iloa_db>-tplnr.
**      IF sy-subrc <> 0 .
**        lv_subrc = 1.
**      ENDIF.
**    ENDIF.
*
*    IF ls_adrc_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_adrc_db TRANSPORTING NO FIELDS WITH KEY addrnumber = <lwa_iloa_db>-adrnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
**    IF ls_iflotx_where-where_tab IS NOT INITIAL.
**      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iloa_db>-tplnr.
**      IF sy-subrc <> 0 .
**        lv_subrc = 1.
**      ENDIF.
**    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iloa_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iloa_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iloa_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_iloa_db>-tplnr.
*      <lwa_flot>-adrnr   =  <lwa_iloa_db>-adrnr.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_iflotx_db ASSIGNING <lwa_iflotx_db>.
*    IF    so_strno[] IS NOT INITIAL
*       OR so_pltxt[] IS NOT INITIAL
*       OR so_fltyp[] IS NOT INITIAL
*       OR so_tplvl[] IS NOT INITIAL
*       OR so_pspnr[] IS NOT INITIAL
*       OR so_tplkz[] IS NOT INITIAL
*       OR so_tplma[] IS NOT INITIAL
*       OR so_owrol[] IS NOT INITIAL
*       OR so_owner[] IS NOT INITIAL
*       OR so_ownsh[] IS NOT INITIAL
*       OR so_garea[] IS NOT INITIAL
*       OR so_eqfnr[] IS NOT INITIAL
*       OR so_grup1[] IS NOT INITIAL
*       OR so_grup2[] IS NOT INITIAL
*       OR so_grup3[] IS NOT INITIAL
*       OR so_stort[] IS NOT INITIAL
*       OR so_bukrs[] IS NOT INITIAL
*       OR so_iwerk[] IS NOT INITIAL
*       OR so_ernam[] IS NOT INITIAL
*       OR so_erdat[] IS NOT INITIAL
*       OR so_aenam[] IS NOT INITIAL
*       OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*    IF ls_iflot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_iflotx_db>-tplnr.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_iflotx_db>-tplnr.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_flatg_db ASSIGNING <lwa_flatg_db>.
*    IF   so_strno[] IS NOT INITIAL
*      OR so_pltxt[] IS NOT INITIAL
*      OR so_fltyp[] IS NOT INITIAL
*      OR so_tplvl[] IS NOT INITIAL
*      OR so_pspnr[] IS NOT INITIAL
*      OR so_tplkz[] IS NOT INITIAL
*      OR so_tplma[] IS NOT INITIAL
*      OR so_owrol[] IS NOT INITIAL
*      OR so_owner[] IS NOT INITIAL
*      OR so_ownsh[] IS NOT INITIAL
*      OR so_garea[] IS NOT INITIAL
*      OR so_eqfnr[] IS NOT INITIAL
*      OR so_grup1[] IS NOT INITIAL
*      OR so_grup2[] IS NOT INITIAL
*      OR so_grup3[] IS NOT INITIAL
*      OR so_stort[] IS NOT INITIAL
*      OR so_bukrs[] IS NOT INITIAL
*      OR so_iwerk[] IS NOT INITIAL
*      OR so_ernam[] IS NOT INITIAL
*      OR so_erdat[] IS NOT INITIAL
*      OR so_aenam[] IS NOT INITIAL
*      OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflotx_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatg_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_flatg_db>-tplnr_fl.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_flatv_db ASSIGNING <lwa_flatv_db>.
*    IF    so_strno[] IS NOT INITIAL
*       OR so_pltxt[] IS NOT INITIAL
*       OR so_fltyp[] IS NOT INITIAL
*       OR so_tplvl[] IS NOT INITIAL
*       OR so_pspnr[] IS NOT INITIAL
*       OR so_tplkz[] IS NOT INITIAL
*       OR so_tplma[] IS NOT INITIAL
*       OR so_owrol[] IS NOT INITIAL
*       OR so_owner[] IS NOT INITIAL
*       OR so_ownsh[] IS NOT INITIAL
*       OR so_garea[] IS NOT INITIAL
*       OR so_eqfnr[] IS NOT INITIAL
*       OR so_grup1[] IS NOT INITIAL
*       OR so_grup2[] IS NOT INITIAL
*       OR so_grup3[] IS NOT INITIAL
*       OR so_stort[] IS NOT INITIAL
*       OR so_bukrs[] IS NOT INITIAL
*       OR so_iwerk[] IS NOT INITIAL
*       OR so_ernam[] IS NOT INITIAL
*       OR so_erdat[] IS NOT INITIAL
*       OR so_aenam[] IS NOT INITIAL
*       OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflotx_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flown_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flown_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flatv_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_flatv_db>-tplnr_fl.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  LOOP AT lt_flown_db ASSIGNING <lwa_flown_db>.
*    IF   so_strno[] IS NOT INITIAL
*      OR so_pltxt[] IS NOT INITIAL
*      OR so_fltyp[] IS NOT INITIAL
*      OR so_tplvl[] IS NOT INITIAL
*      OR so_pspnr[] IS NOT INITIAL
*      OR so_tplkz[] IS NOT INITIAL
*      OR so_tplma[] IS NOT INITIAL
*      OR so_owrol[] IS NOT INITIAL
*      OR so_owner[] IS NOT INITIAL
*      OR so_ownsh[] IS NOT INITIAL
*      OR so_garea[] IS NOT INITIAL
*      OR so_eqfnr[] IS NOT INITIAL
*      OR so_grup1[] IS NOT INITIAL
*      OR so_grup2[] IS NOT INITIAL
*      OR so_grup3[] IS NOT INITIAL
*      OR so_stort[] IS NOT INITIAL
*      OR so_bukrs[] IS NOT INITIAL
*      OR so_iwerk[] IS NOT INITIAL
*      OR so_ernam[] IS NOT INITIAL
*      OR so_erdat[] IS NOT INITIAL
*      OR so_aenam[] IS NOT INITIAL
*      OR so_aedat[] IS NOT INITIAL.
*
*      READ TABLE lt_flot_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flot_hdb TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0.
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflot_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflot_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iloa_where-where_tab IS NOT INITIAL
*       OR so_swerk[] IS NOT INITIAL .
*      READ TABLE lt_iloa_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_iflotx_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_iflotx_db TRANSPORTING NO FIELDS WITH KEY tplnr = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatg_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatg_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF ls_flatv_where-where_tab IS NOT INITIAL.
*      READ TABLE lt_flatv_db TRANSPORTING NO FIELDS WITH KEY tplnr_fl = <lwa_flown_db>-tplnr_fl.
*      IF sy-subrc <> 0 .
*        lv_subrc = 1.
*      ENDIF.
*    ENDIF.
*
*    IF lv_subrc = 0.
*      APPEND INITIAL LINE TO lt_flot ASSIGNING <lwa_flot>.
*      <lwa_flot>-tplnr_fl = <lwa_flown_db>-tplnr_fl.
*    ENDIF.
*    CLEAR: lv_subrc.
*  ENDLOOP.
*
*  SORT lt_flot BY tplnr_fl.
*  DELETE ADJACENT DUPLICATES FROM lt_flot.
*
*  CHECK lt_flot IS NOT INITIAL.
*
*  IF ls_flot_fields IS NOT INITIAL.
*    SELECT (ls_flot_fields-fields)
*      FROM /agri/glflot
*      INTO TABLE gt_flot
*      FOR ALL ENTRIES IN lt_flot
*      WHERE tplnr_fl = lt_flot-tplnr_fl.
*  ENDIF.
*
*  LOOP AT gt_flot INTO /agri/glflot.
*
*    MOVE-CORRESPONDING /agri/glflot TO lwa_search_hdr.
*    APPEND lwa_search_hdr TO gt_search_hdr.
*    CLEAR lwa_search_hdr.
*
*  ENDLOOP.

ENDFORM.
*}   INSERT
