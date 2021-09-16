***INCLUDE LSVCMCOD.
*----------------------------------------------------------------------*
* Globale Variable und interne Tabellen zur Verarbeitung der           *
* verschiedenen Objekte in der Viewclusterpflege,
** sowie Zugriffsroutinen auf diese Tabellen.
*
*----------------------------------------------------------------------*
*
TYPE-POOLS: vclty, svorg.
CLASS cl_abap_char_utilities DEFINITION LOAD.

* Um in den User-Exits zur Viewclusterpflege auf die Daten der am
* Cluster beteiligten Objekte zugreifen zu könne, muß dieses Include
* in das Hauptprogramm, das die User-Exit-Routinen enthält,
* inludiert werden.
*
* Über die Zugriffsroutine "VCL_TABLE_ACCESS_FOR_OBJ" wird der
* Zugriff auf die Verwaltungs- und Datentabellen für ein am
* Viewcluster beteiligtes Objekt ermöglicht:
* Die Routine setzt die globalen Feldsymbole <VCL_NAMTAB>,
* <VCL_DBA_SELLIST>,<VCL_DPL_SELLIST>, <VCL_HEADER>, <VCL_TOTAL>
* und <VCL_EXTRACT>. Diese Feldsymbole verweisen
* auf die entsprechenden internen Tabellen OHNE Kopfzeile.
* Nach Aufruf der Routine sollte der Ausgabeparameter ERROR_FLAG
* überprüft werden:
* Hat dieser den Wert 'X', so wurde das als Parameter spezifizierte
* Objekt nicht gefunden; die Feldsymbole in diesem Fall nicht gesetzt.

* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* ! Auf die in diesem Include deklarierten internen Tabellen          !
* ! (mit Ausnahme von VCL_STRUC_TAB und VCL_SUBSET_TAB) darf NICHT    !
* ! direkt zugegriffen werden, nur über diese Zugriffsroutine,        !
* ! da Datenstrukturänderungen für die Verwaltung der internen        !
* ! Tabellen bei Erweiterung der ABAP/4 Typkonzepts geplant sind.     !
* ! Die Zugriffsroutine stellt eine von den aktuellen Datenstrukturen !
* ! unabhängige Schnittstelle dar.                                    !
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*----------------------------------------------------------------------*

* Typdeklarationen für die Kontrollblocktabellen HEADER und NAMTAB,
* für die Selektionsbedingungen ..._SELLIST und für die
* Datentabellen TOTAL und EXTRACT

TYPES: vcl_flag_type(1) TYPE c.

* Zentrale Konstantendefinitionen (analog Viewpflege)
**********************************
CONSTANTS: vcl_update(1)           TYPE c VALUE 'U',
           vcl_show(1)             TYPE c VALUE 'S',
           vcl_transport(1)        TYPE c VALUE 'T',
           vcl_geloescht(1)        TYPE c VALUE 'D',
           vcl_update_geloescht(1) TYPE c VALUE 'Y',
           vcl_neuer_geloescht(1)  TYPE c VALUE 'X',
           vcl_neuer_eintrag(1)    TYPE c VALUE 'N',
           vcl_uebergehen(1)       TYPE c VALUE '*',  "Mark. n. beachten
           vcl_mark(1)             TYPE c VALUE 'M'.  "Markierungskennz.

* Typen für die DB-Zeilen (für Total und Extract) + VIMFLAGTAB
CONSTANTS: vcl_ultra_short TYPE i VALUE 32,
           vcl_very_short  TYPE i VALUE 48,
           vcl_short       TYPE i VALUE 64,
           vcl_middle      TYPE i VALUE 128,
           vcl_long        TYPE i VALUE 256,
           vcl_very_long   TYPE i VALUE 512,
           vcl_ultra_long  TYPE i VALUE 4096.
*
TYPES: BEGIN OF vcl_line_ul,
         line(vcl_ultra_long) TYPE c,
       END OF vcl_line_ul,
       BEGIN OF vcl_line_vl,
         line(vcl_very_long) TYPE c,
       END OF vcl_line_vl,
       BEGIN OF vcl_line_l,
         line(vcl_long) TYPE c,
       END OF vcl_line_l,
       BEGIN OF vcl_line_m,
         line(vcl_middle) TYPE c,
       END OF vcl_line_m,
       BEGIN OF vcl_line_s,
         line(vcl_short) TYPE c,
       END OF vcl_line_s,
       BEGIN OF vcl_line_vs,
         line(vcl_very_short) TYPE c,
       END OF vcl_line_vs,
       BEGIN OF vcl_line_us,
         line(vcl_ultra_short) TYPE c,
       END OF vcl_line_us.

TYPES: vcl_tab_ul TYPE vcl_line_ul OCCURS 0,
       vcl_tab_vl TYPE vcl_line_vl OCCURS 0,
       vcl_tab_l  TYPE vcl_line_l OCCURS 0,
       vcl_tab_m  TYPE vcl_line_m OCCURS 0,
       vcl_tab_s  TYPE vcl_line_s OCCURS 0,
       vcl_tab_vs TYPE vcl_line_vs OCCURS 0,
       vcl_tab_us TYPE vcl_line_us OCCURS 0.

* Verwaltungsinfo der am Cluster beteiligten Objekte
TYPES: vcl_namtab_type  LIKE vimnamtab OCCURS 10,
       vcl_sellist_type LIKE vimsellist OCCURS 10,
       vcl_header_type  LIKE vimdesc OCCURS 1.   " immer EINE Zeile

TYPES: BEGIN OF vcl_namtab_record,
         namtab_1  TYPE vcl_namtab_type,
         namtab_2  TYPE vcl_namtab_type,
         namtab_3  TYPE vcl_namtab_type,
         namtab_4  TYPE vcl_namtab_type,
         namtab_5  TYPE vcl_namtab_type,
         namtab_6  TYPE vcl_namtab_type,
         namtab_7  TYPE vcl_namtab_type,
         namtab_8  TYPE vcl_namtab_type,
         namtab_9  TYPE vcl_namtab_type,
         namtab_10 TYPE vcl_namtab_type,
         namtab_11 TYPE vcl_namtab_type,
         namtab_12 TYPE vcl_namtab_type,
         namtab_13 TYPE vcl_namtab_type,
         namtab_14 TYPE vcl_namtab_type,
         namtab_15 TYPE vcl_namtab_type,
         namtab_16 TYPE vcl_namtab_type,
         namtab_17 TYPE vcl_namtab_type,
         namtab_18 TYPE vcl_namtab_type,
         namtab_19 TYPE vcl_namtab_type,
         namtab_20 TYPE vcl_namtab_type,
         namtab_21 TYPE vcl_namtab_type,
         namtab_22 TYPE vcl_namtab_type,
         namtab_23 TYPE vcl_namtab_type,
         namtab_24 TYPE vcl_namtab_type,
         namtab_25 TYPE vcl_namtab_type,
         namtab_26 TYPE vcl_namtab_type,
         namtab_27 TYPE vcl_namtab_type,
         namtab_28 TYPE vcl_namtab_type,
         namtab_29 TYPE vcl_namtab_type,
         namtab_30 TYPE vcl_namtab_type,
         namtab_31 TYPE vcl_namtab_type,
         namtab_32 TYPE vcl_namtab_type,
         namtab_33 TYPE vcl_namtab_type,
         namtab_34 TYPE vcl_namtab_type,
         namtab_35 TYPE vcl_namtab_type,
         namtab_36 TYPE vcl_namtab_type,
         namtab_37 TYPE vcl_namtab_type,
         namtab_38 TYPE vcl_namtab_type,
         namtab_39 TYPE vcl_namtab_type,
         namtab_40 TYPE vcl_namtab_type,
         namtab_41 TYPE vcl_namtab_type,
         namtab_42 TYPE vcl_namtab_type,
         namtab_43 TYPE vcl_namtab_type,
         namtab_44 TYPE vcl_namtab_type,
         namtab_45 TYPE vcl_namtab_type,
         namtab_46 TYPE vcl_namtab_type,
         namtab_47 TYPE vcl_namtab_type,
         namtab_48 TYPE vcl_namtab_type,
         namtab_49 TYPE vcl_namtab_type,
         namtab_50 TYPE vcl_namtab_type,
       END OF vcl_namtab_record.

TYPES: BEGIN OF vcl_sellist_record,
         sellist_1  TYPE vcl_sellist_type,
         sellist_2  TYPE vcl_sellist_type,
         sellist_3  TYPE vcl_sellist_type,
         sellist_4  TYPE vcl_sellist_type,
         sellist_5  TYPE vcl_sellist_type,
         sellist_6  TYPE vcl_sellist_type,
         sellist_7  TYPE vcl_sellist_type,
         sellist_8  TYPE vcl_sellist_type,
         sellist_9  TYPE vcl_sellist_type,
         sellist_10 TYPE vcl_sellist_type,
         sellist_11 TYPE vcl_sellist_type,
         sellist_12 TYPE vcl_sellist_type,
         sellist_13 TYPE vcl_sellist_type,
         sellist_14 TYPE vcl_sellist_type,
         sellist_15 TYPE vcl_sellist_type,
         sellist_16 TYPE vcl_sellist_type,
         sellist_17 TYPE vcl_sellist_type,
         sellist_18 TYPE vcl_sellist_type,
         sellist_19 TYPE vcl_sellist_type,
         sellist_20 TYPE vcl_sellist_type,
         sellist_21 TYPE vcl_sellist_type,
         sellist_22 TYPE vcl_sellist_type,
         sellist_23 TYPE vcl_sellist_type,
         sellist_24 TYPE vcl_sellist_type,
         sellist_25 TYPE vcl_sellist_type,
         sellist_26 TYPE vcl_sellist_type,
         sellist_27 TYPE vcl_sellist_type,
         sellist_28 TYPE vcl_sellist_type,
         sellist_29 TYPE vcl_sellist_type,
         sellist_30 TYPE vcl_sellist_type,
         sellist_31 TYPE vcl_sellist_type,
         sellist_32 TYPE vcl_sellist_type,
         sellist_33 TYPE vcl_sellist_type,
         sellist_34 TYPE vcl_sellist_type,
         sellist_35 TYPE vcl_sellist_type,
         sellist_36 TYPE vcl_sellist_type,
         sellist_37 TYPE vcl_sellist_type,
         sellist_38 TYPE vcl_sellist_type,
         sellist_39 TYPE vcl_sellist_type,
         sellist_40 TYPE vcl_sellist_type,
         sellist_41 TYPE vcl_sellist_type,
         sellist_42 TYPE vcl_sellist_type,
         sellist_43 TYPE vcl_sellist_type,
         sellist_44 TYPE vcl_sellist_type,
         sellist_45 TYPE vcl_sellist_type,
         sellist_46 TYPE vcl_sellist_type,
         sellist_47 TYPE vcl_sellist_type,
         sellist_48 TYPE vcl_sellist_type,
         sellist_49 TYPE vcl_sellist_type,
         sellist_50 TYPE vcl_sellist_type,
       END OF vcl_sellist_record.

TYPES: BEGIN OF vcl_header_record,
         header_1  TYPE vcl_header_type,
         header_2  TYPE vcl_header_type,
         header_3  TYPE vcl_header_type,
         header_4  TYPE vcl_header_type,
         header_5  TYPE vcl_header_type,
         header_6  TYPE vcl_header_type,
         header_7  TYPE vcl_header_type,
         header_8  TYPE vcl_header_type,
         header_9  TYPE vcl_header_type,
         header_10 TYPE vcl_header_type,
         header_11 TYPE vcl_header_type,
         header_12 TYPE vcl_header_type,
         header_13 TYPE vcl_header_type,
         header_14 TYPE vcl_header_type,
         header_15 TYPE vcl_header_type,
         header_16 TYPE vcl_header_type,
         header_17 TYPE vcl_header_type,
         header_18 TYPE vcl_header_type,
         header_19 TYPE vcl_header_type,
         header_20 TYPE vcl_header_type,
         header_21 TYPE vcl_header_type,
         header_22 TYPE vcl_header_type,
         header_23 TYPE vcl_header_type,
         header_24 TYPE vcl_header_type,
         header_25 TYPE vcl_header_type,
         header_26 TYPE vcl_header_type,
         header_27 TYPE vcl_header_type,
         header_28 TYPE vcl_header_type,
         header_29 TYPE vcl_header_type,
         header_30 TYPE vcl_header_type,
         header_31 TYPE vcl_header_type,
         header_32 TYPE vcl_header_type,
         header_33 TYPE vcl_header_type,
         header_34 TYPE vcl_header_type,
         header_35 TYPE vcl_header_type,
         header_36 TYPE vcl_header_type,
         header_37 TYPE vcl_header_type,
         header_38 TYPE vcl_header_type,
         header_39 TYPE vcl_header_type,
         header_40 TYPE vcl_header_type,
         header_41 TYPE vcl_header_type,
         header_42 TYPE vcl_header_type,
         header_43 TYPE vcl_header_type,
         header_44 TYPE vcl_header_type,
         header_45 TYPE vcl_header_type,
         header_46 TYPE vcl_header_type,
         header_47 TYPE vcl_header_type,
         header_48 TYPE vcl_header_type,
         header_49 TYPE vcl_header_type,
         header_50 TYPE vcl_header_type,
       END OF vcl_header_record.

TYPES: vcl_table_name(17) TYPE c.
TYPES: vcl_table_name_array TYPE vcl_table_name OCCURS 50,
       BEGIN OF vcl_oc_type,
         viewname TYPE tabname,
         oc_inst  TYPE REF TO cl_viewfields_org_crit,
       END OF vcl_oc_type.

************************************************************************
* Globale Daten und internen Tabellen für am Cluster beteiligte Objekte

* nur relevant für Import/Compare:
CONSTANTS: vcl_import_mode(1)  TYPE c VALUE 'I',
           vcl_compare_mode(1) TYPE c VALUE 'C'.

DATA: BEGIN OF COMMON PART vcl_common_part,

*     nur relevant für Import:
        vcl_special_mode(1) TYPE c,     " I/C/' ' -> Import/Compare/Pflege
        vcl_testrun(1)      TYPE c,     " X ->Testimp ohne Save:DO NOT USE
        vcl_bcset_import(1) TYPE c,      " X ->Profilimport : DO NOT USE
*
        vcl_stop(1)         TYPE c,      " Stop der auszuf. Aktion
        vcl_function        LIKE sy-ucomm,       " Funktion der Viewpflege
        vcl_action(1)       TYPE c,      " SHOW/UPDATE/TRANSPORT
        vcl_enq_action(1)   TYPE c,      " E/D -> enqueue/dequeue
        vcl_akt_view        LIKE vclstruc-object,    "aktueller View
        vcl_last_view       LIKE vclstruc-object,    "zuletzt beh. View
        vcl_next_view       LIKE vclstruc-object,    "nächter View
        vcl_last_cursor_ix  LIKE sy-tabix,     "Cursor-Pos. aus Viewpflege
        vcl_corr_number     LIKE e070-trkorr,        " Korrekturnummer
        vcl_align1          TYPE f,
        vcl_object_area     TYPE vcl_line_ul,
        vcl_reduction_area  TYPE vcl_line_ul.

* Zähler für Löschen, Kopieren und Zurückholen von Tabelleneinträgen
DATA: vcl_total_dep_count  TYPE i,      " Gesamtzahl abh. Einträge
      vcl_total_depx_count TYPE i,    "  "  ",für die Op nicht durchgef.
      vcl_total_sup_count  TYPE i.      " Gesamtzahl übergeordn. Einträge

* interne Tabelle zur Viewcluster-Strukturbeschreibung
DATA: BEGIN OF vcl_struc_tab OCCURS 25.
    INCLUDE STRUCTURE vclstruc.
DATA: objecttext      LIKE v_vclstruc-objecttext,
      old_suppress(1) TYPE c,
      screenind       LIKE vclstruc-objpos,                 "obsolete
      fcode(4)        TYPE c,                               "obsolete
      upd_requ(1)     TYPE c,
      enqueue(1)      TYPE c,
      navi_mark       TYPE c,
      readonly(1)     TYPE c,
      ro_message(1)   TYPE c,  "'X'->Meldung für Readonly schon gesendet
      get_range(1)    TYPE c,          "Einschränkung des Datenbereichs
      isread(1)       TYPE c,          "Daten zu View sind eingelesen
      f4_for_comp(1)  TYPE c,         "nur COMPARE-Modus,
      " X' -> F4 für Subset-Eingabe schon prozessiert
      maint_object    LIKE vclstruc-object. "Pflegeview, falls OBJECT
"eine Variante ist
DATA: END OF vcl_struc_tab.

* interne Tabelle zur Beschreibung der Subsetbeziehungen der Clusterstr.
DATA: BEGIN OF vcl_subset_tab OCCURS 25.
    INCLUDE STRUCTURE v_vclstdep.
DATA: rootfield   TYPE viewfield,        "entspr. Feldname im Rootobj
      ofield_pos  LIKE vimnamtab-position, "Pos. von objfield in object
      ofield_len  LIKE vimnamtab-flength,  "Länge
      ofield_ityp LIKE vimnamtab-inttype,                 "ABAP-Typ
      ostruc      TYPE tabname,                "structure ofield comes from
      pfield_pos  LIKE vimnamtab-position, "Pos. von predfield in pred
      pfield_len  LIKE vimnamtab-flength,  "Länge
      pstruc      TYPE tabname,                "structure pfield comes from
      END OF vcl_subset_tab.

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Auf folgende Datendeklarationen nicht direkt zugreifen!!!
DATA: vcl_total_ul_1 TYPE vcl_tab_ul,
      vcl_total_vl_1 TYPE vcl_tab_vl,
      vcl_total_l_1  TYPE vcl_tab_l,
      vcl_total_m_1  TYPE vcl_tab_m,
      vcl_total_s_1  TYPE vcl_tab_s,
      vcl_total_vs_1 TYPE vcl_tab_vs,
      vcl_total_us_1 TYPE vcl_tab_us.
DATA: vcl_total_ul_2 TYPE vcl_tab_ul,
      vcl_total_vl_2 TYPE vcl_tab_vl,
      vcl_total_l_2  TYPE vcl_tab_l,
      vcl_total_m_2  TYPE vcl_tab_m,
      vcl_total_s_2  TYPE vcl_tab_s,
      vcl_total_vs_2 TYPE vcl_tab_vs,
      vcl_total_us_2 TYPE vcl_tab_us.
DATA: vcl_total_ul_3 TYPE vcl_tab_ul,
      vcl_total_vl_3 TYPE vcl_tab_vl,
      vcl_total_l_3  TYPE vcl_tab_l,
      vcl_total_m_3  TYPE vcl_tab_m,
      vcl_total_s_3  TYPE vcl_tab_s,
      vcl_total_vs_3 TYPE vcl_tab_vs,
      vcl_total_us_3 TYPE vcl_tab_us.
DATA: vcl_total_ul_4 TYPE vcl_tab_ul,
      vcl_total_vl_4 TYPE vcl_tab_vl,
      vcl_total_l_4  TYPE vcl_tab_l,
      vcl_total_m_4  TYPE vcl_tab_m,
      vcl_total_s_4  TYPE vcl_tab_s,
      vcl_total_vs_4 TYPE vcl_tab_vs,
      vcl_total_us_4 TYPE vcl_tab_us.
DATA: vcl_total_ul_5 TYPE vcl_tab_ul,
      vcl_total_vl_5 TYPE vcl_tab_vl,
      vcl_total_l_5  TYPE vcl_tab_l,
      vcl_total_m_5  TYPE vcl_tab_m,
      vcl_total_s_5  TYPE vcl_tab_s,
      vcl_total_vs_5 TYPE vcl_tab_vs,
      vcl_total_us_5 TYPE vcl_tab_us.
DATA: vcl_total_ul_6 TYPE vcl_tab_ul,
      vcl_total_vl_6 TYPE vcl_tab_vl,
      vcl_total_l_6  TYPE vcl_tab_l,
      vcl_total_m_6  TYPE vcl_tab_m,
      vcl_total_s_6  TYPE vcl_tab_s,
      vcl_total_vs_6 TYPE vcl_tab_vs,
      vcl_total_us_6 TYPE vcl_tab_us.
DATA: vcl_total_ul_7 TYPE vcl_tab_ul,
      vcl_total_vl_7 TYPE vcl_tab_vl,
      vcl_total_l_7  TYPE vcl_tab_l,
      vcl_total_m_7  TYPE vcl_tab_m,
      vcl_total_s_7  TYPE vcl_tab_s,
      vcl_total_vs_7 TYPE vcl_tab_vs,
      vcl_total_us_7 TYPE vcl_tab_us.
DATA: vcl_total_ul_8 TYPE vcl_tab_ul,
      vcl_total_vl_8 TYPE vcl_tab_vl,
      vcl_total_l_8  TYPE vcl_tab_l,
      vcl_total_m_8  TYPE vcl_tab_m,
      vcl_total_s_8  TYPE vcl_tab_s,
      vcl_total_vs_8 TYPE vcl_tab_vs,
      vcl_total_us_8 TYPE vcl_tab_us.
DATA: vcl_total_ul_9 TYPE vcl_tab_ul,
      vcl_total_vl_9 TYPE vcl_tab_vl,
      vcl_total_l_9  TYPE vcl_tab_l,
      vcl_total_m_9  TYPE vcl_tab_m,
      vcl_total_s_9  TYPE vcl_tab_s,
      vcl_total_vs_9 TYPE vcl_tab_vs,
      vcl_total_us_9 TYPE vcl_tab_us.
DATA: vcl_total_ul_10 TYPE vcl_tab_ul,
      vcl_total_vl_10 TYPE vcl_tab_vl,
      vcl_total_l_10  TYPE vcl_tab_l,
      vcl_total_m_10  TYPE vcl_tab_m,
      vcl_total_s_10  TYPE vcl_tab_s,
      vcl_total_vs_10 TYPE vcl_tab_vs,
      vcl_total_us_10 TYPE vcl_tab_us.
DATA: vcl_total_ul_11 TYPE vcl_tab_ul,
      vcl_total_vl_11 TYPE vcl_tab_vl,
      vcl_total_l_11  TYPE vcl_tab_l,
      vcl_total_m_11  TYPE vcl_tab_m,
      vcl_total_s_11  TYPE vcl_tab_s,
      vcl_total_vs_11 TYPE vcl_tab_vs,
      vcl_total_us_11 TYPE vcl_tab_us.
DATA: vcl_total_ul_12 TYPE vcl_tab_ul,
      vcl_total_vl_12 TYPE vcl_tab_vl,
      vcl_total_l_12  TYPE vcl_tab_l,
      vcl_total_m_12  TYPE vcl_tab_m,
      vcl_total_s_12  TYPE vcl_tab_s,
      vcl_total_vs_12 TYPE vcl_tab_vs,
      vcl_total_us_12 TYPE vcl_tab_us.
DATA: vcl_total_ul_13 TYPE vcl_tab_ul,
      vcl_total_vl_13 TYPE vcl_tab_vl,
      vcl_total_l_13  TYPE vcl_tab_l,
      vcl_total_m_13  TYPE vcl_tab_m,
      vcl_total_s_13  TYPE vcl_tab_s,
      vcl_total_vs_13 TYPE vcl_tab_vs,
      vcl_total_us_13 TYPE vcl_tab_us.
DATA: vcl_total_ul_14 TYPE vcl_tab_ul,
      vcl_total_vl_14 TYPE vcl_tab_vl,
      vcl_total_l_14  TYPE vcl_tab_l,
      vcl_total_m_14  TYPE vcl_tab_m,
      vcl_total_s_14  TYPE vcl_tab_s,
      vcl_total_vs_14 TYPE vcl_tab_vs,
      vcl_total_us_14 TYPE vcl_tab_us.
DATA: vcl_total_ul_15 TYPE vcl_tab_ul,
      vcl_total_vl_15 TYPE vcl_tab_vl,
      vcl_total_l_15  TYPE vcl_tab_l,
      vcl_total_m_15  TYPE vcl_tab_m,
      vcl_total_s_15  TYPE vcl_tab_s,
      vcl_total_vs_15 TYPE vcl_tab_vs,
      vcl_total_us_15 TYPE vcl_tab_us.
DATA: vcl_total_ul_16 TYPE vcl_tab_ul,
      vcl_total_vl_16 TYPE vcl_tab_vl,
      vcl_total_l_16  TYPE vcl_tab_l,
      vcl_total_m_16  TYPE vcl_tab_m,
      vcl_total_s_16  TYPE vcl_tab_s,
      vcl_total_vs_16 TYPE vcl_tab_vs,
      vcl_total_us_16 TYPE vcl_tab_us.
DATA: vcl_total_ul_17 TYPE vcl_tab_ul,
      vcl_total_vl_17 TYPE vcl_tab_vl,
      vcl_total_l_17  TYPE vcl_tab_l,
      vcl_total_m_17  TYPE vcl_tab_m,
      vcl_total_s_17  TYPE vcl_tab_s,
      vcl_total_vs_17 TYPE vcl_tab_vs,
      vcl_total_us_17 TYPE vcl_tab_us.
DATA: vcl_total_ul_18 TYPE vcl_tab_ul,
      vcl_total_vl_18 TYPE vcl_tab_vl,
      vcl_total_l_18  TYPE vcl_tab_l,
      vcl_total_m_18  TYPE vcl_tab_m,
      vcl_total_s_18  TYPE vcl_tab_s,
      vcl_total_vs_18 TYPE vcl_tab_vs,
      vcl_total_us_18 TYPE vcl_tab_us.
DATA: vcl_total_ul_19 TYPE vcl_tab_ul,
      vcl_total_vl_19 TYPE vcl_tab_vl,
      vcl_total_l_19  TYPE vcl_tab_l,
      vcl_total_m_19  TYPE vcl_tab_m,
      vcl_total_s_19  TYPE vcl_tab_s,
      vcl_total_vs_19 TYPE vcl_tab_vs,
      vcl_total_us_19 TYPE vcl_tab_us.
DATA: vcl_total_ul_20 TYPE vcl_tab_ul,
      vcl_total_vl_20 TYPE vcl_tab_vl,
      vcl_total_l_20  TYPE vcl_tab_l,
      vcl_total_m_20  TYPE vcl_tab_m,
      vcl_total_s_20  TYPE vcl_tab_s,
      vcl_total_vs_20 TYPE vcl_tab_vs,
      vcl_total_us_20 TYPE vcl_tab_us.
DATA: vcl_total_ul_21 TYPE vcl_tab_ul,
      vcl_total_vl_21 TYPE vcl_tab_vl,
      vcl_total_l_21  TYPE vcl_tab_l,
      vcl_total_m_21  TYPE vcl_tab_m,
      vcl_total_s_21  TYPE vcl_tab_s,
      vcl_total_vs_21 TYPE vcl_tab_vs,
      vcl_total_us_21 TYPE vcl_tab_us.
DATA: vcl_total_ul_22 TYPE vcl_tab_ul,
      vcl_total_vl_22 TYPE vcl_tab_vl,
      vcl_total_l_22  TYPE vcl_tab_l,
      vcl_total_m_22  TYPE vcl_tab_m,
      vcl_total_s_22  TYPE vcl_tab_s,
      vcl_total_vs_22 TYPE vcl_tab_vs,
      vcl_total_us_22 TYPE vcl_tab_us.
DATA: vcl_total_ul_23 TYPE vcl_tab_ul,
      vcl_total_vl_23 TYPE vcl_tab_vl,
      vcl_total_l_23  TYPE vcl_tab_l,
      vcl_total_m_23  TYPE vcl_tab_m,
      vcl_total_s_23  TYPE vcl_tab_s,
      vcl_total_vs_23 TYPE vcl_tab_vs,
      vcl_total_us_23 TYPE vcl_tab_us.
DATA: vcl_total_ul_24 TYPE vcl_tab_ul,
      vcl_total_vl_24 TYPE vcl_tab_vl,
      vcl_total_l_24  TYPE vcl_tab_l,
      vcl_total_m_24  TYPE vcl_tab_m,
      vcl_total_s_24  TYPE vcl_tab_s,
      vcl_total_vs_24 TYPE vcl_tab_vs,
      vcl_total_us_24 TYPE vcl_tab_us.
DATA: vcl_total_ul_25 TYPE vcl_tab_ul,
      vcl_total_vl_25 TYPE vcl_tab_vl,
      vcl_total_l_25  TYPE vcl_tab_l,
      vcl_total_m_25  TYPE vcl_tab_m,
      vcl_total_s_25  TYPE vcl_tab_s,
      vcl_total_vs_25 TYPE vcl_tab_vs,
      vcl_total_us_25 TYPE vcl_tab_us.
DATA: vcl_total_ul_26 TYPE vcl_tab_ul,
      vcl_total_vl_26 TYPE vcl_tab_vl,
      vcl_total_l_26  TYPE vcl_tab_l,
      vcl_total_m_26  TYPE vcl_tab_m,
      vcl_total_s_26  TYPE vcl_tab_s,
      vcl_total_vs_26 TYPE vcl_tab_vs,
      vcl_total_us_26 TYPE vcl_tab_us.
DATA: vcl_total_ul_27 TYPE vcl_tab_ul,
      vcl_total_vl_27 TYPE vcl_tab_vl,
      vcl_total_l_27  TYPE vcl_tab_l,
      vcl_total_m_27  TYPE vcl_tab_m,
      vcl_total_s_27  TYPE vcl_tab_s,
      vcl_total_vs_27 TYPE vcl_tab_vs,
      vcl_total_us_27 TYPE vcl_tab_us.
DATA: vcl_total_ul_28 TYPE vcl_tab_ul,
      vcl_total_vl_28 TYPE vcl_tab_vl,
      vcl_total_l_28  TYPE vcl_tab_l,
      vcl_total_m_28  TYPE vcl_tab_m,
      vcl_total_s_28  TYPE vcl_tab_s,
      vcl_total_vs_28 TYPE vcl_tab_vs,
      vcl_total_us_28 TYPE vcl_tab_us.
DATA: vcl_total_ul_29 TYPE vcl_tab_ul,
      vcl_total_vl_29 TYPE vcl_tab_vl,
      vcl_total_l_29  TYPE vcl_tab_l,
      vcl_total_m_29  TYPE vcl_tab_m,
      vcl_total_s_29  TYPE vcl_tab_s,
      vcl_total_vs_29 TYPE vcl_tab_vs,
      vcl_total_us_29 TYPE vcl_tab_us.
DATA: vcl_total_ul_30 TYPE vcl_tab_ul,
      vcl_total_vl_30 TYPE vcl_tab_vl,
      vcl_total_l_30  TYPE vcl_tab_l,
      vcl_total_m_30  TYPE vcl_tab_m,
      vcl_total_s_30  TYPE vcl_tab_s,
      vcl_total_vs_30 TYPE vcl_tab_vs,
      vcl_total_us_30 TYPE vcl_tab_us.
DATA: vcl_total_ul_31 TYPE vcl_tab_ul,
      vcl_total_vl_31 TYPE vcl_tab_vl,
      vcl_total_l_31  TYPE vcl_tab_l,
      vcl_total_m_31  TYPE vcl_tab_m,
      vcl_total_s_31  TYPE vcl_tab_s,
      vcl_total_vs_31 TYPE vcl_tab_vs,
      vcl_total_us_31 TYPE vcl_tab_us.
DATA: vcl_total_ul_32 TYPE vcl_tab_ul,
      vcl_total_vl_32 TYPE vcl_tab_vl,
      vcl_total_l_32  TYPE vcl_tab_l,
      vcl_total_m_32  TYPE vcl_tab_m,
      vcl_total_s_32  TYPE vcl_tab_s,
      vcl_total_vs_32 TYPE vcl_tab_vs,
      vcl_total_us_32 TYPE vcl_tab_us.
DATA: vcl_total_ul_33 TYPE vcl_tab_ul,
      vcl_total_vl_33 TYPE vcl_tab_vl,
      vcl_total_l_33  TYPE vcl_tab_l,
      vcl_total_m_33  TYPE vcl_tab_m,
      vcl_total_s_33  TYPE vcl_tab_s,
      vcl_total_vs_33 TYPE vcl_tab_vs,
      vcl_total_us_33 TYPE vcl_tab_us.
DATA: vcl_total_ul_34 TYPE vcl_tab_ul,
      vcl_total_vl_34 TYPE vcl_tab_vl,
      vcl_total_l_34  TYPE vcl_tab_l,
      vcl_total_m_34  TYPE vcl_tab_m,
      vcl_total_s_34  TYPE vcl_tab_s,
      vcl_total_vs_34 TYPE vcl_tab_vs,
      vcl_total_us_34 TYPE vcl_tab_us.
DATA: vcl_total_ul_35 TYPE vcl_tab_ul,
      vcl_total_vl_35 TYPE vcl_tab_vl,
      vcl_total_l_35  TYPE vcl_tab_l,
      vcl_total_m_35  TYPE vcl_tab_m,
      vcl_total_s_35  TYPE vcl_tab_s,
      vcl_total_vs_35 TYPE vcl_tab_vs,
      vcl_total_us_35 TYPE vcl_tab_us.
DATA: vcl_total_ul_36 TYPE vcl_tab_ul,
      vcl_total_vl_36 TYPE vcl_tab_vl,
      vcl_total_l_36  TYPE vcl_tab_l,
      vcl_total_m_36  TYPE vcl_tab_m,
      vcl_total_s_36  TYPE vcl_tab_s,
      vcl_total_vs_36 TYPE vcl_tab_vs,
      vcl_total_us_36 TYPE vcl_tab_us.
DATA: vcl_total_ul_37 TYPE vcl_tab_ul,
      vcl_total_vl_37 TYPE vcl_tab_vl,
      vcl_total_l_37  TYPE vcl_tab_l,
      vcl_total_m_37  TYPE vcl_tab_m,
      vcl_total_s_37  TYPE vcl_tab_s,
      vcl_total_vs_37 TYPE vcl_tab_vs,
      vcl_total_us_37 TYPE vcl_tab_us.
DATA: vcl_total_ul_38 TYPE vcl_tab_ul,
      vcl_total_vl_38 TYPE vcl_tab_vl,
      vcl_total_l_38  TYPE vcl_tab_l,
      vcl_total_m_38  TYPE vcl_tab_m,
      vcl_total_s_38  TYPE vcl_tab_s,
      vcl_total_vs_38 TYPE vcl_tab_vs,
      vcl_total_us_38 TYPE vcl_tab_us.
DATA: vcl_total_ul_39 TYPE vcl_tab_ul,
      vcl_total_vl_39 TYPE vcl_tab_vl,
      vcl_total_l_39  TYPE vcl_tab_l,
      vcl_total_m_39  TYPE vcl_tab_m,
      vcl_total_s_39  TYPE vcl_tab_s,
      vcl_total_vs_39 TYPE vcl_tab_vs,
      vcl_total_us_39 TYPE vcl_tab_us.
DATA: vcl_total_ul_40 TYPE vcl_tab_ul,
      vcl_total_vl_40 TYPE vcl_tab_vl,
      vcl_total_l_40  TYPE vcl_tab_l,
      vcl_total_m_40  TYPE vcl_tab_m,
      vcl_total_s_40  TYPE vcl_tab_s,
      vcl_total_vs_40 TYPE vcl_tab_vs,
      vcl_total_us_40 TYPE vcl_tab_us.
DATA: vcl_total_ul_41 TYPE vcl_tab_ul,
      vcl_total_vl_41 TYPE vcl_tab_vl,
      vcl_total_l_41  TYPE vcl_tab_l,
      vcl_total_m_41  TYPE vcl_tab_m,
      vcl_total_s_41  TYPE vcl_tab_s,
      vcl_total_vs_41 TYPE vcl_tab_vs,
      vcl_total_us_41 TYPE vcl_tab_us.
DATA: vcl_total_ul_42 TYPE vcl_tab_ul,
      vcl_total_vl_42 TYPE vcl_tab_vl,
      vcl_total_l_42  TYPE vcl_tab_l,
      vcl_total_m_42  TYPE vcl_tab_m,
      vcl_total_s_42  TYPE vcl_tab_s,
      vcl_total_vs_42 TYPE vcl_tab_vs,
      vcl_total_us_42 TYPE vcl_tab_us.
DATA: vcl_total_ul_43 TYPE vcl_tab_ul,
      vcl_total_vl_43 TYPE vcl_tab_vl,
      vcl_total_l_43  TYPE vcl_tab_l,
      vcl_total_m_43  TYPE vcl_tab_m,
      vcl_total_s_43  TYPE vcl_tab_s,
      vcl_total_vs_43 TYPE vcl_tab_vs,
      vcl_total_us_43 TYPE vcl_tab_us.
DATA: vcl_total_ul_44 TYPE vcl_tab_ul,
      vcl_total_vl_44 TYPE vcl_tab_vl,
      vcl_total_l_44  TYPE vcl_tab_l,
      vcl_total_m_44  TYPE vcl_tab_m,
      vcl_total_s_44  TYPE vcl_tab_s,
      vcl_total_vs_44 TYPE vcl_tab_vs,
      vcl_total_us_44 TYPE vcl_tab_us.
DATA: vcl_total_ul_45 TYPE vcl_tab_ul,
      vcl_total_vl_45 TYPE vcl_tab_vl,
      vcl_total_l_45  TYPE vcl_tab_l,
      vcl_total_m_45  TYPE vcl_tab_m,
      vcl_total_s_45  TYPE vcl_tab_s,
      vcl_total_vs_45 TYPE vcl_tab_vs,
      vcl_total_us_45 TYPE vcl_tab_us.
DATA: vcl_total_ul_46 TYPE vcl_tab_ul,
      vcl_total_vl_46 TYPE vcl_tab_vl,
      vcl_total_l_46  TYPE vcl_tab_l,
      vcl_total_m_46  TYPE vcl_tab_m,
      vcl_total_s_46  TYPE vcl_tab_s,
      vcl_total_vs_46 TYPE vcl_tab_vs,
      vcl_total_us_46 TYPE vcl_tab_us.
DATA: vcl_total_ul_47 TYPE vcl_tab_ul,
      vcl_total_vl_47 TYPE vcl_tab_vl,
      vcl_total_l_47  TYPE vcl_tab_l,
      vcl_total_m_47  TYPE vcl_tab_m,
      vcl_total_s_47  TYPE vcl_tab_s,
      vcl_total_vs_47 TYPE vcl_tab_vs,
      vcl_total_us_47 TYPE vcl_tab_us.
DATA: vcl_total_ul_48 TYPE vcl_tab_ul,
      vcl_total_vl_48 TYPE vcl_tab_vl,
      vcl_total_l_48  TYPE vcl_tab_l,
      vcl_total_m_48  TYPE vcl_tab_m,
      vcl_total_s_48  TYPE vcl_tab_s,
      vcl_total_vs_48 TYPE vcl_tab_vs,
      vcl_total_us_48 TYPE vcl_tab_us.
DATA: vcl_total_ul_49 TYPE vcl_tab_ul,
      vcl_total_vl_49 TYPE vcl_tab_vl,
      vcl_total_l_49  TYPE vcl_tab_l,
      vcl_total_m_49  TYPE vcl_tab_m,
      vcl_total_s_49  TYPE vcl_tab_s,
      vcl_total_vs_49 TYPE vcl_tab_vs,
      vcl_total_us_49 TYPE vcl_tab_us.
DATA: vcl_total_ul_50 TYPE vcl_tab_ul,
      vcl_total_vl_50 TYPE vcl_tab_vl,
      vcl_total_l_50  TYPE vcl_tab_l,
      vcl_total_m_50  TYPE vcl_tab_m,
      vcl_total_s_50  TYPE vcl_tab_s,
      vcl_total_vs_50 TYPE vcl_tab_vs,
      vcl_total_us_50 TYPE vcl_tab_us.


DATA: vcl_extract_ul_1 TYPE vcl_tab_ul,
      vcl_extract_vl_1 TYPE vcl_tab_vl,
      vcl_extract_l_1  TYPE vcl_tab_l,
      vcl_extract_m_1  TYPE vcl_tab_m,
      vcl_extract_s_1  TYPE vcl_tab_s,
      vcl_extract_vs_1 TYPE vcl_tab_vs,
      vcl_extract_us_1 TYPE vcl_tab_us.
DATA: vcl_extract_ul_2 TYPE vcl_tab_ul,
      vcl_extract_vl_2 TYPE vcl_tab_vl,
      vcl_extract_l_2  TYPE vcl_tab_l,
      vcl_extract_m_2  TYPE vcl_tab_m,
      vcl_extract_s_2  TYPE vcl_tab_s,
      vcl_extract_vs_2 TYPE vcl_tab_vs,
      vcl_extract_us_2 TYPE vcl_tab_us.
DATA: vcl_extract_ul_3 TYPE vcl_tab_ul,
      vcl_extract_vl_3 TYPE vcl_tab_vl,
      vcl_extract_l_3  TYPE vcl_tab_l,
      vcl_extract_m_3  TYPE vcl_tab_m,
      vcl_extract_s_3  TYPE vcl_tab_s,
      vcl_extract_vs_3 TYPE vcl_tab_vs,
      vcl_extract_us_3 TYPE vcl_tab_us.
DATA: vcl_extract_ul_4 TYPE vcl_tab_ul,
      vcl_extract_vl_4 TYPE vcl_tab_vl,
      vcl_extract_l_4  TYPE vcl_tab_l,
      vcl_extract_m_4  TYPE vcl_tab_m,
      vcl_extract_s_4  TYPE vcl_tab_s,
      vcl_extract_vs_4 TYPE vcl_tab_vs,
      vcl_extract_us_4 TYPE vcl_tab_us.
DATA: vcl_extract_ul_5 TYPE vcl_tab_ul,
      vcl_extract_vl_5 TYPE vcl_tab_vl,
      vcl_extract_l_5  TYPE vcl_tab_l,
      vcl_extract_m_5  TYPE vcl_tab_m,
      vcl_extract_s_5  TYPE vcl_tab_s,
      vcl_extract_vs_5 TYPE vcl_tab_vs,
      vcl_extract_us_5 TYPE vcl_tab_us.
DATA: vcl_extract_ul_6 TYPE vcl_tab_ul,
      vcl_extract_vl_6 TYPE vcl_tab_vl,
      vcl_extract_l_6  TYPE vcl_tab_l,
      vcl_extract_m_6  TYPE vcl_tab_m,
      vcl_extract_s_6  TYPE vcl_tab_s,
      vcl_extract_vs_6 TYPE vcl_tab_vs,
      vcl_extract_us_6 TYPE vcl_tab_us.
DATA: vcl_extract_ul_7 TYPE vcl_tab_ul,
      vcl_extract_vl_7 TYPE vcl_tab_vl,
      vcl_extract_l_7  TYPE vcl_tab_l,
      vcl_extract_m_7  TYPE vcl_tab_m,
      vcl_extract_s_7  TYPE vcl_tab_s,
      vcl_extract_vs_7 TYPE vcl_tab_vs,
      vcl_extract_us_7 TYPE vcl_tab_us.
DATA: vcl_extract_ul_8 TYPE vcl_tab_ul,
      vcl_extract_vl_8 TYPE vcl_tab_vl,
      vcl_extract_l_8  TYPE vcl_tab_l,
      vcl_extract_m_8  TYPE vcl_tab_m,
      vcl_extract_s_8  TYPE vcl_tab_s,
      vcl_extract_vs_8 TYPE vcl_tab_vs,
      vcl_extract_us_8 TYPE vcl_tab_us.
DATA: vcl_extract_ul_9 TYPE vcl_tab_ul,
      vcl_extract_vl_9 TYPE vcl_tab_vl,
      vcl_extract_l_9  TYPE vcl_tab_l,
      vcl_extract_m_9  TYPE vcl_tab_m,
      vcl_extract_s_9  TYPE vcl_tab_s,
      vcl_extract_vs_9 TYPE vcl_tab_vs,
      vcl_extract_us_9 TYPE vcl_tab_us.
DATA: vcl_extract_ul_10 TYPE vcl_tab_ul,
      vcl_extract_vl_10 TYPE vcl_tab_vl,
      vcl_extract_l_10  TYPE vcl_tab_l,
      vcl_extract_m_10  TYPE vcl_tab_m,
      vcl_extract_s_10  TYPE vcl_tab_s,
      vcl_extract_vs_10 TYPE vcl_tab_vs,
      vcl_extract_us_10 TYPE vcl_tab_us.
DATA: vcl_extract_ul_11 TYPE vcl_tab_ul,
      vcl_extract_vl_11 TYPE vcl_tab_vl,
      vcl_extract_l_11  TYPE vcl_tab_l,
      vcl_extract_m_11  TYPE vcl_tab_m,
      vcl_extract_s_11  TYPE vcl_tab_s,
      vcl_extract_vs_11 TYPE vcl_tab_vs,
      vcl_extract_us_11 TYPE vcl_tab_us.
DATA: vcl_extract_ul_12 TYPE vcl_tab_ul,
      vcl_extract_vl_12 TYPE vcl_tab_vl,
      vcl_extract_l_12  TYPE vcl_tab_l,
      vcl_extract_m_12  TYPE vcl_tab_m,
      vcl_extract_s_12  TYPE vcl_tab_s,
      vcl_extract_vs_12 TYPE vcl_tab_vs,
      vcl_extract_us_12 TYPE vcl_tab_us.
DATA: vcl_extract_ul_13 TYPE vcl_tab_ul,
      vcl_extract_vl_13 TYPE vcl_tab_vl,
      vcl_extract_l_13  TYPE vcl_tab_l,
      vcl_extract_m_13  TYPE vcl_tab_m,
      vcl_extract_s_13  TYPE vcl_tab_s,
      vcl_extract_vs_13 TYPE vcl_tab_vs,
      vcl_extract_us_13 TYPE vcl_tab_us.
DATA: vcl_extract_ul_14 TYPE vcl_tab_ul,
      vcl_extract_vl_14 TYPE vcl_tab_vl,
      vcl_extract_l_14  TYPE vcl_tab_l,
      vcl_extract_m_14  TYPE vcl_tab_m,
      vcl_extract_s_14  TYPE vcl_tab_s,
      vcl_extract_vs_14 TYPE vcl_tab_vs,
      vcl_extract_us_14 TYPE vcl_tab_us.
DATA: vcl_extract_ul_15 TYPE vcl_tab_ul,
      vcl_extract_vl_15 TYPE vcl_tab_vl,
      vcl_extract_l_15  TYPE vcl_tab_l,
      vcl_extract_m_15  TYPE vcl_tab_m,
      vcl_extract_s_15  TYPE vcl_tab_s,
      vcl_extract_vs_15 TYPE vcl_tab_vs,
      vcl_extract_us_15 TYPE vcl_tab_us.
DATA: vcl_extract_ul_16 TYPE vcl_tab_ul,
      vcl_extract_vl_16 TYPE vcl_tab_vl,
      vcl_extract_l_16  TYPE vcl_tab_l,
      vcl_extract_m_16  TYPE vcl_tab_m,
      vcl_extract_s_16  TYPE vcl_tab_s,
      vcl_extract_vs_16 TYPE vcl_tab_vs,
      vcl_extract_us_16 TYPE vcl_tab_us.
DATA: vcl_extract_ul_17 TYPE vcl_tab_ul,
      vcl_extract_vl_17 TYPE vcl_tab_vl,
      vcl_extract_l_17  TYPE vcl_tab_l,
      vcl_extract_m_17  TYPE vcl_tab_m,
      vcl_extract_s_17  TYPE vcl_tab_s,
      vcl_extract_vs_17 TYPE vcl_tab_vs,
      vcl_extract_us_17 TYPE vcl_tab_us.
DATA: vcl_extract_ul_18 TYPE vcl_tab_ul,
      vcl_extract_vl_18 TYPE vcl_tab_vl,
      vcl_extract_l_18  TYPE vcl_tab_l,
      vcl_extract_m_18  TYPE vcl_tab_m,
      vcl_extract_s_18  TYPE vcl_tab_s,
      vcl_extract_vs_18 TYPE vcl_tab_vs,
      vcl_extract_us_18 TYPE vcl_tab_us.
DATA: vcl_extract_ul_19 TYPE vcl_tab_ul,
      vcl_extract_vl_19 TYPE vcl_tab_vl,
      vcl_extract_l_19  TYPE vcl_tab_l,
      vcl_extract_m_19  TYPE vcl_tab_m,
      vcl_extract_s_19  TYPE vcl_tab_s,
      vcl_extract_vs_19 TYPE vcl_tab_vs,
      vcl_extract_us_19 TYPE vcl_tab_us.
DATA: vcl_extract_ul_20 TYPE vcl_tab_ul,
      vcl_extract_vl_20 TYPE vcl_tab_vl,
      vcl_extract_l_20  TYPE vcl_tab_l,
      vcl_extract_m_20  TYPE vcl_tab_m,
      vcl_extract_s_20  TYPE vcl_tab_s,
      vcl_extract_vs_20 TYPE vcl_tab_vs,
      vcl_extract_us_20 TYPE vcl_tab_us.
DATA: vcl_extract_ul_21 TYPE vcl_tab_ul,
      vcl_extract_vl_21 TYPE vcl_tab_vl,
      vcl_extract_l_21  TYPE vcl_tab_l,
      vcl_extract_m_21  TYPE vcl_tab_m,
      vcl_extract_s_21  TYPE vcl_tab_s,
      vcl_extract_vs_21 TYPE vcl_tab_vs,
      vcl_extract_us_21 TYPE vcl_tab_us.
DATA: vcl_extract_ul_22 TYPE vcl_tab_ul,
      vcl_extract_vl_22 TYPE vcl_tab_vl,
      vcl_extract_l_22  TYPE vcl_tab_l,
      vcl_extract_m_22  TYPE vcl_tab_m,
      vcl_extract_s_22  TYPE vcl_tab_s,
      vcl_extract_vs_22 TYPE vcl_tab_vs,
      vcl_extract_us_22 TYPE vcl_tab_us.
DATA: vcl_extract_ul_23 TYPE vcl_tab_ul,
      vcl_extract_vl_23 TYPE vcl_tab_vl,
      vcl_extract_l_23  TYPE vcl_tab_l,
      vcl_extract_m_23  TYPE vcl_tab_m,
      vcl_extract_s_23  TYPE vcl_tab_s,
      vcl_extract_vs_23 TYPE vcl_tab_vs,
      vcl_extract_us_23 TYPE vcl_tab_us.
DATA: vcl_extract_ul_24 TYPE vcl_tab_ul,
      vcl_extract_vl_24 TYPE vcl_tab_vl,
      vcl_extract_l_24  TYPE vcl_tab_l,
      vcl_extract_m_24  TYPE vcl_tab_m,
      vcl_extract_s_24  TYPE vcl_tab_s,
      vcl_extract_vs_24 TYPE vcl_tab_vs,
      vcl_extract_us_24 TYPE vcl_tab_us.
DATA: vcl_extract_ul_25 TYPE vcl_tab_ul,
      vcl_extract_vl_25 TYPE vcl_tab_vl,
      vcl_extract_l_25  TYPE vcl_tab_l,
      vcl_extract_m_25  TYPE vcl_tab_m,
      vcl_extract_s_25  TYPE vcl_tab_s,
      vcl_extract_vs_25 TYPE vcl_tab_vs,
      vcl_extract_us_25 TYPE vcl_tab_us.
DATA: vcl_extract_ul_26 TYPE vcl_tab_ul,
      vcl_extract_vl_26 TYPE vcl_tab_vl,
      vcl_extract_l_26  TYPE vcl_tab_l,
      vcl_extract_m_26  TYPE vcl_tab_m,
      vcl_extract_s_26  TYPE vcl_tab_s,
      vcl_extract_vs_26 TYPE vcl_tab_vs,
      vcl_extract_us_26 TYPE vcl_tab_us.
DATA: vcl_extract_ul_27 TYPE vcl_tab_ul,
      vcl_extract_vl_27 TYPE vcl_tab_vl,
      vcl_extract_l_27  TYPE vcl_tab_l,
      vcl_extract_m_27  TYPE vcl_tab_m,
      vcl_extract_s_27  TYPE vcl_tab_s,
      vcl_extract_vs_27 TYPE vcl_tab_vs,
      vcl_extract_us_27 TYPE vcl_tab_us.
DATA: vcl_extract_ul_28 TYPE vcl_tab_ul,
      vcl_extract_vl_28 TYPE vcl_tab_vl,
      vcl_extract_l_28  TYPE vcl_tab_l,
      vcl_extract_m_28  TYPE vcl_tab_m,
      vcl_extract_s_28  TYPE vcl_tab_s,
      vcl_extract_vs_28 TYPE vcl_tab_vs,
      vcl_extract_us_28 TYPE vcl_tab_us.
DATA: vcl_extract_ul_29 TYPE vcl_tab_ul,
      vcl_extract_vl_29 TYPE vcl_tab_vl,
      vcl_extract_l_29  TYPE vcl_tab_l,
      vcl_extract_m_29  TYPE vcl_tab_m,
      vcl_extract_s_29  TYPE vcl_tab_s,
      vcl_extract_vs_29 TYPE vcl_tab_vs,
      vcl_extract_us_29 TYPE vcl_tab_us.
DATA: vcl_extract_ul_30 TYPE vcl_tab_ul,
      vcl_extract_vl_30 TYPE vcl_tab_vl,
      vcl_extract_l_30  TYPE vcl_tab_l,
      vcl_extract_m_30  TYPE vcl_tab_m,
      vcl_extract_s_30  TYPE vcl_tab_s,
      vcl_extract_vs_30 TYPE vcl_tab_vs,
      vcl_extract_us_30 TYPE vcl_tab_us.
DATA: vcl_extract_ul_31 TYPE vcl_tab_ul,
      vcl_extract_vl_31 TYPE vcl_tab_vl,
      vcl_extract_l_31  TYPE vcl_tab_l,
      vcl_extract_m_31  TYPE vcl_tab_m,
      vcl_extract_s_31  TYPE vcl_tab_s,
      vcl_extract_vs_31 TYPE vcl_tab_vs,
      vcl_extract_us_31 TYPE vcl_tab_us.
DATA: vcl_extract_ul_32 TYPE vcl_tab_ul,
      vcl_extract_vl_32 TYPE vcl_tab_vl,
      vcl_extract_l_32  TYPE vcl_tab_l,
      vcl_extract_m_32  TYPE vcl_tab_m,
      vcl_extract_s_32  TYPE vcl_tab_s,
      vcl_extract_vs_32 TYPE vcl_tab_vs,
      vcl_extract_us_32 TYPE vcl_tab_us.
DATA: vcl_extract_ul_33 TYPE vcl_tab_ul,
      vcl_extract_vl_33 TYPE vcl_tab_vl,
      vcl_extract_l_33  TYPE vcl_tab_l,
      vcl_extract_m_33  TYPE vcl_tab_m,
      vcl_extract_s_33  TYPE vcl_tab_s,
      vcl_extract_vs_33 TYPE vcl_tab_vs,
      vcl_extract_us_33 TYPE vcl_tab_us.
DATA: vcl_extract_ul_34 TYPE vcl_tab_ul,
      vcl_extract_vl_34 TYPE vcl_tab_vl,
      vcl_extract_l_34  TYPE vcl_tab_l,
      vcl_extract_m_34  TYPE vcl_tab_m,
      vcl_extract_s_34  TYPE vcl_tab_s,
      vcl_extract_vs_34 TYPE vcl_tab_vs,
      vcl_extract_us_34 TYPE vcl_tab_us.
DATA: vcl_extract_ul_35 TYPE vcl_tab_ul,
      vcl_extract_vl_35 TYPE vcl_tab_vl,
      vcl_extract_l_35  TYPE vcl_tab_l,
      vcl_extract_m_35  TYPE vcl_tab_m,
      vcl_extract_s_35  TYPE vcl_tab_s,
      vcl_extract_vs_35 TYPE vcl_tab_vs,
      vcl_extract_us_35 TYPE vcl_tab_us.
DATA: vcl_extract_ul_36 TYPE vcl_tab_ul,
      vcl_extract_vl_36 TYPE vcl_tab_vl,
      vcl_extract_l_36  TYPE vcl_tab_l,
      vcl_extract_m_36  TYPE vcl_tab_m,
      vcl_extract_s_36  TYPE vcl_tab_s,
      vcl_extract_vs_36 TYPE vcl_tab_vs,
      vcl_extract_us_36 TYPE vcl_tab_us.
DATA: vcl_extract_ul_37 TYPE vcl_tab_ul,
      vcl_extract_vl_37 TYPE vcl_tab_vl,
      vcl_extract_l_37  TYPE vcl_tab_l,
      vcl_extract_m_37  TYPE vcl_tab_m,
      vcl_extract_s_37  TYPE vcl_tab_s,
      vcl_extract_vs_37 TYPE vcl_tab_vs,
      vcl_extract_us_37 TYPE vcl_tab_us.
DATA: vcl_extract_ul_38 TYPE vcl_tab_ul,
      vcl_extract_vl_38 TYPE vcl_tab_vl,
      vcl_extract_l_38  TYPE vcl_tab_l,
      vcl_extract_m_38  TYPE vcl_tab_m,
      vcl_extract_s_38  TYPE vcl_tab_s,
      vcl_extract_vs_38 TYPE vcl_tab_vs,
      vcl_extract_us_38 TYPE vcl_tab_us.
DATA: vcl_extract_ul_39 TYPE vcl_tab_ul,
      vcl_extract_vl_39 TYPE vcl_tab_vl,
      vcl_extract_l_39  TYPE vcl_tab_l,
      vcl_extract_m_39  TYPE vcl_tab_m,
      vcl_extract_s_39  TYPE vcl_tab_s,
      vcl_extract_vs_39 TYPE vcl_tab_vs,
      vcl_extract_us_39 TYPE vcl_tab_us.
DATA: vcl_extract_ul_40 TYPE vcl_tab_ul,
      vcl_extract_vl_40 TYPE vcl_tab_vl,
      vcl_extract_l_40  TYPE vcl_tab_l,
      vcl_extract_m_40  TYPE vcl_tab_m,
      vcl_extract_s_40  TYPE vcl_tab_s,
      vcl_extract_vs_40 TYPE vcl_tab_vs,
      vcl_extract_us_40 TYPE vcl_tab_us.
DATA: vcl_extract_ul_41 TYPE vcl_tab_ul,
      vcl_extract_vl_41 TYPE vcl_tab_vl,
      vcl_extract_l_41  TYPE vcl_tab_l,
      vcl_extract_m_41  TYPE vcl_tab_m,
      vcl_extract_s_41  TYPE vcl_tab_s,
      vcl_extract_vs_41 TYPE vcl_tab_vs,
      vcl_extract_us_41 TYPE vcl_tab_us.
DATA: vcl_extract_ul_42 TYPE vcl_tab_ul,
      vcl_extract_vl_42 TYPE vcl_tab_vl,
      vcl_extract_l_42  TYPE vcl_tab_l,
      vcl_extract_m_42  TYPE vcl_tab_m,
      vcl_extract_s_42  TYPE vcl_tab_s,
      vcl_extract_vs_42 TYPE vcl_tab_vs,
      vcl_extract_us_42 TYPE vcl_tab_us.
DATA: vcl_extract_ul_43 TYPE vcl_tab_ul,
      vcl_extract_vl_43 TYPE vcl_tab_vl,
      vcl_extract_l_43  TYPE vcl_tab_l,
      vcl_extract_m_43  TYPE vcl_tab_m,
      vcl_extract_s_43  TYPE vcl_tab_s,
      vcl_extract_vs_43 TYPE vcl_tab_vs,
      vcl_extract_us_43 TYPE vcl_tab_us.
DATA: vcl_extract_ul_44 TYPE vcl_tab_ul,
      vcl_extract_vl_44 TYPE vcl_tab_vl,
      vcl_extract_l_44  TYPE vcl_tab_l,
      vcl_extract_m_44  TYPE vcl_tab_m,
      vcl_extract_s_44  TYPE vcl_tab_s,
      vcl_extract_vs_44 TYPE vcl_tab_vs,
      vcl_extract_us_44 TYPE vcl_tab_us.
DATA: vcl_extract_ul_45 TYPE vcl_tab_ul,
      vcl_extract_vl_45 TYPE vcl_tab_vl,
      vcl_extract_l_45  TYPE vcl_tab_l,
      vcl_extract_m_45  TYPE vcl_tab_m,
      vcl_extract_s_45  TYPE vcl_tab_s,
      vcl_extract_vs_45 TYPE vcl_tab_vs,
      vcl_extract_us_45 TYPE vcl_tab_us.
DATA: vcl_extract_ul_46 TYPE vcl_tab_ul,
      vcl_extract_vl_46 TYPE vcl_tab_vl,
      vcl_extract_l_46  TYPE vcl_tab_l,
      vcl_extract_m_46  TYPE vcl_tab_m,
      vcl_extract_s_46  TYPE vcl_tab_s,
      vcl_extract_vs_46 TYPE vcl_tab_vs,
      vcl_extract_us_46 TYPE vcl_tab_us.
DATA: vcl_extract_ul_47 TYPE vcl_tab_ul,
      vcl_extract_vl_47 TYPE vcl_tab_vl,
      vcl_extract_l_47  TYPE vcl_tab_l,
      vcl_extract_m_47  TYPE vcl_tab_m,
      vcl_extract_s_47  TYPE vcl_tab_s,
      vcl_extract_vs_47 TYPE vcl_tab_vs,
      vcl_extract_us_47 TYPE vcl_tab_us.
DATA: vcl_extract_ul_48 TYPE vcl_tab_ul,
      vcl_extract_vl_48 TYPE vcl_tab_vl,
      vcl_extract_l_48  TYPE vcl_tab_l,
      vcl_extract_m_48  TYPE vcl_tab_m,
      vcl_extract_s_48  TYPE vcl_tab_s,
      vcl_extract_vs_48 TYPE vcl_tab_vs,
      vcl_extract_us_48 TYPE vcl_tab_us.
DATA: vcl_extract_ul_49 TYPE vcl_tab_ul,
      vcl_extract_vl_49 TYPE vcl_tab_vl,
      vcl_extract_l_49  TYPE vcl_tab_l,
      vcl_extract_m_49  TYPE vcl_tab_m,
      vcl_extract_s_49  TYPE vcl_tab_s,
      vcl_extract_vs_49 TYPE vcl_tab_vs,
      vcl_extract_us_49 TYPE vcl_tab_us.
DATA: vcl_extract_ul_50 TYPE vcl_tab_ul,
      vcl_extract_vl_50 TYPE vcl_tab_vl,
      vcl_extract_l_50  TYPE vcl_tab_l,
      vcl_extract_m_50  TYPE vcl_tab_m,
      vcl_extract_s_50  TYPE vcl_tab_s,
      vcl_extract_vs_50 TYPE vcl_tab_vs,
      vcl_extract_us_50 TYPE vcl_tab_us.

* Enthält die Namen der benutzten internen Tabellen.
* Diese Datenstruktur wird beim Einlesen der header-info gefüllt.
DATA: vcl_cluster_total   TYPE vcl_table_name_array,
      vcl_cluster_extract TYPE vcl_table_name_array.

DATA: vcl_cluster_namtab     TYPE vcl_namtab_record,
      vcl_cluster_dbasellist TYPE vcl_sellist_record, "initiale Rangetab
      vcl_cluster_dplsellist TYPE vcl_sellist_record,
      "akt. Rangetab (Navigation)
      vcl_cluster_header     TYPE vcl_header_record.

* Sobald ABAP "ASSIGNING <FS>" zum Zugriff auf Zeilen einer internen
* Tabelle zur Verfügung stellt, sollte statt der Records dynam.
* Arrays zur Verwaltung obiger Tabellen benutzt werden, z.B.
* TYPES  vcl_NAMTAB_ARRAY TYPE vcl_namtab_type OCCURS 10.
* DATA   vcl_CLUSTER_NAMTAB TYPE NAMTAB_ARRAY.
DATA: vcl_oc_tab TYPE SORTED TABLE OF vcl_oc_type
       WITH UNIQUE KEY viewname.
*Ende der Datendekl., auf die nicht direkt zugegriffen weden darf
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

DATA: END OF COMMON PART.
************************************************************************
* Variables for vcl_process_message at bcset import
DATA: vcm_actopts  TYPE scpractopt, "Activation options at BC-SET import
      vcm_bcset_id TYPE scpr_id.                "Name of BC-SET

* Globale Feldsymbole für den Zugriff auf die internen Tabellen für
* ein Clusterobjekt. Die Feldsymbole werden durch Aufruf der Routine
* VCL_SET_TABLE_ACCESS_FOR_OBJ gesetzt:
FIELD-SYMBOLS: <vcl_header>      TYPE vcl_header_type,
               <vcl_namtab>      TYPE vcl_namtab_type,
               <vcl_dba_sellist> TYPE vcl_sellist_type,
               <vcl_dpl_sellist> TYPE vcl_sellist_type,
               <vcl_total>       TYPE table,
               <vcl_extract>     TYPE table.

*---------------------------------------------------------------------*
*    FORM VCL_SET_TABLE_ACCESS_FOR_OBJ                                *
*---------------------------------------------------------------------*
*    OBJECT      (IN): View/Tabelle, für die die Feldsymbole          *
*                      gesetzt werden sollen                          *
*    ERROR_FLAG (OUT): ' ' -> Feldsymbole gesetzt                     *
*                      'X' -> OBJECT gehört nicht zum Cluster,        *
*                             Feldsymbole wurden nicht gesetzt        *
*---------------------------------------------------------------------*
*    Setzt die globalen Feldsymbole                                   *
*      <VCL_HEADER>, <VCL_NAMTAB>, <VCL_DBA_SELLIST>,                 *
*      <VCL_DPL_SELLIST>, <VCL_TOTAL> und <VCL_EXTRACT> auf           *
*      die entsprechenden internen Tabellen für das Objekt OBJECT     *
*---------------------------------------------------------------------*
FORM vcl_set_table_access_for_obj
          USING VALUE(object) LIKE vclstruc-object
          CHANGING error_flag TYPE vcl_flag_type.

  DATA: position    LIKE sy-tabix,
        structab_wa LIKE vcl_struc_tab,
        tabname     TYPE vcl_table_name.

  CLEAR error_flag.
  READ TABLE vcl_struc_tab WITH KEY object = object INTO structab_wa.
  IF sy-subrc <> 0.
    READ TABLE vcl_struc_tab WITH KEY maint_object = object
                             INTO structab_wa.
  ENDIF.
  IF sy-subrc = 0.
    position  = structab_wa-objpos.
    ASSIGN COMPONENT position
           OF STRUCTURE vcl_cluster_header TO <vcl_header>.
    ASSIGN COMPONENT position
           OF STRUCTURE vcl_cluster_namtab TO <vcl_namtab>.
    ASSIGN COMPONENT position
           OF STRUCTURE vcl_cluster_dbasellist TO <vcl_dba_sellist>.
    ASSIGN COMPONENT position
           OF STRUCTURE vcl_cluster_dplsellist TO <vcl_dpl_sellist>.
    READ TABLE vcl_cluster_total INDEX position INTO tabname.
    ASSIGN (tabname) TO <vcl_total>.
    READ TABLE vcl_cluster_extract INDEX position INTO tabname.
    ASSIGN (tabname) TO <vcl_extract>.
  ELSE.
    error_flag = 'X'.
  ENDIF.
ENDFORM.                               "VCL_SET_TABLE_ACCESS_FOR_OBJ


*---------------------------------------------------------------------*
*    FORM VCL_CONNECT_COMMON_PART                                     *
*---------------------------------------------------------------------*
*  Diese Routine wird automatisch von der Viewcluster-Pflege aus      *
*  aufgerufen, falls zum bearbeiteten Viewcluster User-Exits          *
*  definiert sind.                                                    *
*  Der Aufruf erfolgt als externer Perform im Subroutinen-Pool        *
*  der User-Exits, um diesen mit dem Viewcluster-Programm             *
*  zu verbinden und damit den COMMON Bereich in beiden Programmen     *
*  verfügbar zu machen.                                               *
*---------------------------------------------------------------------*
*  Routine nicht in User-Exits aufrufen !                             *
*---------------------------------------------------------------------*
FORM vcl_connect_common_part ##NEEDED.
ENDFORM.

* 20.1.1998, IMP
*---------------------------------------------------------------------*
*    FORM VCL_PROCESS_MESSAGE                                         *
*---------------------------------------------------------------------*
* Diese Routine ist im Import-Modus aufzurufen, um Meldungen in die   *
* Protokolldatei zu schreiben.                                        *
* Um nicht bei jeder Message eine entsprechende Abfrage programmieren *
* zu müssen, kann diese Routine immer aufgerufen werden;              *
* für den Dialog- und Batch-Modus können unterschiedl. Message-Typen  *
* angegeben werden. Im Dialog wird die Message 'normal' ausgegeben,   *
* im Batch erfolgt die Ausgabe in die Protokolldatei.                 *
* Meldungen vom Typ 'A' führen zum Abbruch                            *
*---------------------------------------------------------------------*
* VALUE(PM_ID)          ---> ID of message to send                    *
* VALUE(PM_ONLINE_TYPE) ---> message type used for online mode        *
* VALUE(PM_BATCH_TYPE)  ---> message type used for batch mode         *
* VALUE(PM_NBR)         ---> number of message to send                *
* VALUE(PM_V1)          ---> first mesage variable                    *
* VALUE(PM_V2)          ---> second mesage variable                   *
* VALUE(PM_V3)          ---> third mesage variable                    *
* VALUE(PM_V4)          ---> fourth mesage variable                   *
*---------------------------------------------------------------------*
FORM vcl_process_message USING VALUE(pm_id) LIKE sy-msgid
                         VALUE(pm_online_type) LIKE sy-msgty
                         VALUE(pm_batch_type) LIKE sy-msgty
                         VALUE(pm_nbr)  LIKE sy-msgno
                         VALUE(pm_v1) LIKE sy-msgv1
                         VALUE(pm_v2) LIKE sy-msgv2
                         VALUE(pm_v3) LIKE sy-msgv3
                         VALUE(pm_v4) LIKE sy-msgv4.
  LOCAL: vcl_struc_tab.
  DATA: i_type  LIKE sprot_u-severity, i_msgty LIKE sy-msgty.

  IF vcl_special_mode = vcl_import_mode. "no dialog - write protocol
    i_type = i_msgty = pm_batch_type.
    TRANSLATE i_type USING 'I S '.
    IF pm_batch_type = 'A'.
      "Raise Excpetion ERROR_MESSAGE (for 'A'-message) to stop
      MESSAGE ID pm_id TYPE i_msgty NUMBER pm_nbr
              WITH pm_v1 pm_v2 pm_v3 pm_v4.
    ELSE.   "PM_BATCH_TYPE = 'I' / 'S' / 'W' / 'E'.
      IF vcl_bcset_import = space.
        CALL FUNCTION 'LCT_MESSAGE'
          EXPORTING
            iv_msgid  = pm_id
            iv_msgty  = i_type
            iv_msgno  = pm_nbr
            iv_msgv1  = pm_v1
            iv_msgv2  = pm_v2
            iv_msgv3  = pm_v3
            iv_msgv4  = pm_v4
            iv_dialog = space.
      ELSE.
        READ TABLE vcl_struc_tab INDEX 1.
        CALL FUNCTION 'SCPR_PROT_DATA_WRITE'
          EXPORTING
            act_id     = vcm_actopts-act_id
            bcset_id   = vcm_bcset_id
            objectname = vcl_struc_tab-vclname
*           tablename  = object
*           tabletype  =
*           tablekey   =
            msgid      = pm_id
            msgty      = i_type
            msgno      = pm_nbr
            var1       = pm_v1
            var2       = pm_v2
            var3       = pm_v3
            var4       = pm_v4
            objecttype = 'C'.
      ENDIF.
    ENDIF.
  ELSE.                                "with dialog - use pm_online_type
    i_msgty = pm_online_type.
    MESSAGE ID pm_id TYPE i_msgty NUMBER pm_nbr
              WITH pm_v1 pm_v2 pm_v3 pm_v4.
  ENDIF.
ENDFORM.                               "vcl_process_message

FORM sum_fields.

ENDFORM.
