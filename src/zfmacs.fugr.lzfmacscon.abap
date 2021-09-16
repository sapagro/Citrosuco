*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLSCON
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

CONSTANTS: BEGIN OF c_tabname,
             header TYPE tabname VALUE '/AGRI/GLFLOT',
             iflot  TYPE tabname VALUE 'IFLOT',
             adrc   TYPE tabname VALUE 'ADRC',
             iloa   TYPE tabname VALUE 'ILOA',
             iflotx TYPE tabname VALUE 'IFLOTX',
             flatg  TYPE tabname VALUE '/AGRI/GLFLATG',
             flatv  TYPE tabname VALUE '/AGRI/GLFLATV',
             flown  TYPE tabname VALUE '/AGRI/GLFLOWN',
           END OF c_tabname.

*CONSTANTS: BEGIN OF c_version,
*             change_docs TYPE trdir-type VALUE '007',
*
*           END OF c_version.
*
*****Change Docs Object
*CONSTANTS: BEGIN OF c_object,
*             change_docs TYPE cdhdr-objectclas VALUE '/AGRI/IPBB',
*           END OF c_object.

CONSTANTS: BEGIN OF c_selname,
             page_size    TYPE stexti VALUE 'P_PGSZ',
             max_hits     TYPE stexti VALUE 'P_MXHIT',
             read_all_itm TYPE stexti VALUE 'P_ALLIT',
             deleted_docs TYPE stexti VALUE 'P_SHDLD',
           END OF c_selname.
*}   INSERT
