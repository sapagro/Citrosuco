FUNCTION zabs_fm_update_wghbridge.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_PRHDR) TYPE  /AGRI/S_FMPRHDR
*"     VALUE(IT_PRITM) TYPE  /AGRI/T_FMPRITM
*"----------------------------------------------------------------------
  IF is_prhdr-zzmobile = 'N'.
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE @DATA(lt_ilp_btch)
       FOR ALL ENTRIES IN @it_pritm
     WHERE in_licplate = @is_prhdr-lic_plate
       AND charg       = @it_pritm-zzhbatch
       AND loevm       = @space.
    IF sy-subrc = 0.
      SORT lt_ilp_btch BY charg.
    ENDIF.

*-- Wave 3 changes
  ELSEIF is_prhdr-zzmobile = 'Q'.
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE lt_ilp_btch
       FOR ALL ENTRIES IN it_pritm
     WHERE in_licplate = it_pritm-zzsrebo2
       AND charg       = it_pritm-zzhbatch
       AND loevm       = space.
    IF sy-subrc = 0.
      SORT lt_ilp_btch BY charg.
    ENDIF.

  ENDIF.
*-- Wave 3 changes

  LOOP AT lt_ilp_btch ASSIGNING FIELD-SYMBOL(<fs_ilp_btch>).
    READ TABLE it_pritm INTO DATA(ls_pritm)
                         WITH KEY zzhbatch = <fs_ilp_btch>-charg
                    BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_ilp_btch>-loevm = 'X'.
      <fs_ilp_btch>-prnum = is_prhdr-prnum.
      <fs_ilp_btch>-gjahr = is_prhdr-gjahr.
      <fs_ilp_btch>-pritm = ls_pritm-pritm.
      <fs_ilp_btch>-aenam = sy-uname.
      <fs_ilp_btch>-aedat = sy-datum.
      <fs_ilp_btch>-aezet = sy-uzeit.
    ENDIF.
  ENDLOOP.

  IF lt_ilp_btch IS NOT INITIAL.
    UPDATE zabs_ilp_btch FROM TABLE lt_ilp_btch.
  ENDIF.

ENDFUNCTION.
