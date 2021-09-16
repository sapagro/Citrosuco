************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_WEIGHBRIDGE_TOP                            *
* Tcode          : ZABS_BALANCA                                        *
* Created By     : Helio Kababe                                        *
* Requested by   : Ricardo Genovez                                     *
* Created on     : 09.03.2020                                          *
* TR             : C4DK909167                                          *
* Version        : 001                                                 *
* Description    : Weighbridge data declaration and Selection Screen   *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Include          ZABS_REP_WEIGHBRIDGE_TOP
*&---------------------------------------------------------------------*

*--Table declarations
TABLES: /agri/fmprhdr,
        /agri/fmpritm.

*--Table type declarations
TYPES: tty_weighbridge TYPE STANDARD TABLE OF zabs_str_weighbridge.

*--Global table declarations
DATA: gt_weighbridge TYPE tty_weighbridge,
      gt_fieldcat    TYPE lvc_t_fcat.

*--Global structure declarations
DATA: gs_layout TYPE lvc_s_layo.

*--Objects declaration
DATA: go_container        TYPE scrfname VALUE 'CONTAINER',
      go_custom_container TYPE REF TO cl_gui_custom_container,
      go_grid             TYPE REF TO cl_gui_alv_grid.

*--Varaibles declaration
DATA: ok_code TYPE sy-ucomm.

*&--------------------------------------------------------------------*
*& SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS: s_prnum  FOR /agri/fmprhdr-prnum, "Número de Ticket
                s_fldty  FOR /agri/fmprhdr-fldty, "Tipo de Produto
                s_werks  FOR /agri/fmprhdr-werks OBLIGATORY, "Centro
                s_status FOR /agri/fmprhdr-status, "Status
                s_budat  FOR /agri/fmprhdr-budat OBLIGATORY, "Data de Lançamento
                s_plate  FOR /agri/fmprhdr-lic_plate, "Placa CM
                s_arrend FOR /agri/fmprhdr-zarrend, "Imóvel arrendado
                s_tplnr  FOR /agri/fmpritm-tplnr, "Terreno
                s_lider  FOR /agri/fmpritm-zzldlifnr, "Líder da Colheita
                s_lote   FOR /agri/fmpritm-charg, "Lote
                s_rebo1  FOR /agri/fmpritm-zzsrebo1, "Placa Semi-Reboque
                s_lotec  FOR /agri/fmpritm-zzhbatch, "Lote Colheita
                s_viagem FOR /agri/fmprhdr-viagem_original.
PARAMETERS: p_tipo TYPE /agri/fmprhdr-tipo_viagem,
            p_merc TYPE /agri/fmprhdr-fruta_mercado.
SELECTION-SCREEN END OF BLOCK b1.
