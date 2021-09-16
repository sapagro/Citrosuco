class ZCL_ABS_ODATA_MAINTAIN definition
  public
  final
  create public .

public section.

  constants C_MOBL_OBJID type ZABS_DEL_OBJID value 'MOBL' ##NO_TEXT.
  constants C_MOBL_VAL type ZABS_DEL_K1VAL value 'MOBL' ##NO_TEXT.
  constants C_CTRY type ZABS_DEL_K2VAL value 'BR' ##NO_TEXT.
  constants C_INSP type ZABS_DEL_K3VAL value 'INP_VAL' ##NO_TEXT.
  constants C_MDM_NTP type ZABS_DEL_K3VAL value 'MDM_NTYP' ##NO_TEXT.
  constants C_TANK_UOM type ZABS_DEL_K3VAL value 'TANK_UOM' ##NO_TEXT.
  constants C_MEAS_NUMD type ZABS_DEL_K3VAL value 'MEAS_NUD' ##NO_TEXT.
  constants C_BAGS_UOM type ZABS_DEL_K3VAL value 'BAGS_UOM' ##NO_TEXT.
  constants C_TOKEN_SKIP type ZABS_DEL_K3VAL value 'SKIP' ##NO_TEXT.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_ODATA_MAINTAIN IMPLEMENTATION.
ENDCLASS.
