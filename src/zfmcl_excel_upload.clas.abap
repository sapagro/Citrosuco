class ZFMCL_EXCEL_UPLOAD definition
  public
  final
  create public .

public section.

  methods STRUCTURE_BUILD
    importing
      !I_STRUCTURE type TABNAME
      !I_SHEET type NUMC2
    exporting
      !ET_DD03L type THRPAD_ERD_DD03L
    changing
      !CT_TABLE type /AGRI/T_EXCEL_SHEET .
protected section.
private section.
ENDCLASS.



CLASS ZFMCL_EXCEL_UPLOAD IMPLEMENTATION.


  METHOD STRUCTURE_BUILD.

    DATA: lwa_dd03l TYPE dd03l.
    FIELD-SYMBOLS: <lwa_data> TYPE /agri/s_excel_sheet.

    SELECT * INTO TABLE et_dd03l              "#EC CI_ALL_FIELDS_NEEDED
      FROM dd03l
      WHERE tabname = i_structure.

    SORT et_dd03l BY position.

    LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = i_sheet.
      READ TABLE et_dd03l INTO lwa_dd03l
        WITH KEY position = <lwa_data>-column BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_data>-fieldname = lwa_dd03l-fieldname.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
