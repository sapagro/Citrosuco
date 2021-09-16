FUNCTION /AGRI/GLFL_KEY_STRUCTURE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_STRUCTURED_KEY) LIKE  /AGRI/TGL370S-EDITM
*"     VALUE(I_STRUCTURE_INDICATOR) LIKE  /AGRI/TGL370S-TPLKZ
*"     VALUE(I_CORRECT_SEPARATORS) LIKE  IREF-IIND DEFAULT SPACE
*"     VALUE(I_CORRECT_BLOCK_DIGITS) LIKE  IREF-IIND DEFAULT SPACE
*"  EXPORTING
*"     VALUE(E_CORRECTED_KEY) LIKE  /AGRI/TGL370S-EDITM
*"     VALUE(E_SEPARATOR_CORRECTED) LIKE  IREF-IIND
*"     VALUE(E_BLOCK_DIGIT_CORRECTED) LIKE  IREF-IIND
*"     VALUE(E_EDITMASK) LIKE  /AGRI/TGL370S-EDITM
*"     VALUE(E_HIERARCHY_LEVELS) LIKE  /AGRI/TGL370S-STUFM
*"  EXCEPTIONS
*"      EX_STR_INDICATOR_NOT_FOUND
*"      EX_KEY_TOO_LONG
*"      EX_FIRST_BLOCK_IS_INITIAL
*"      EX_NO_COMPLIANCE_WITH_EDITMASK
*"      EX_KEY_ENDS_WITH_SEPARATOR
*"----------------------------------------------------------------------
* New parameter with 4.5B: LABELING_SYSTEM                  "P45K078156

  DATA:
    ind_not_initial,                  "Indikator: STRING nicht initial
    calc_int                TYPE i,             "Rechenfeld
    l_block_digit_corrected           "lokales Übernahmefeld
      LIKE e_block_digit_corrected,
    l_corrected_key                   "lokales Übernahmefeld
      LIKE e_corrected_key,
    l_not_correct_seperator           "lokales Übernahmefeld
      LIKE i_correct_separators,
    offset_block1           TYPE i,             "Offset des letzten Zeichens von
    "Block 1 in T370S-STUFM; also 1
    "Zeichen weniger als Länge Block 1
    offset_string           TYPE i,             "Offset in STRING
    string                  LIKE i_structured_key,       "lokales Arbeitsfeld
    strlen_editm            TYPE i,             "gefüllte Länge der Editionsmaske
    strlen_string           TYPE i,             "gefüllte Länge der Strukturnummer
    t370s                   TYPE /agri/tgl370s.

  CONSTANTS:
*------ Konstanten -----------------------------------------------------
*----------------- Standardmaskierungszeichen in Editionsmaske ---------
    y_editm_a    VALUE 'A',           "alphabetisches Zeichen
    y_editm_n    VALUE 'N',           "numerisches Zeichen
    y_editm_x    VALUE 'X',           "alphanumerisches Zeichen
    y_editm_s    VALUE 'S',           "alphanumerisch + Sonderzeichen

    y_type_a(27) VALUE                "alphabetische Zeichen
                   ' ABCDEFGHIJKLMNOPQRSTUVWXYZ',
    y_type_n(11) VALUE                "numerische Zeichen
                   ' 0123456789',
    y_type_x(39) VALUE                "alphanumerisches Zeichen
                   ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-_',
    y_type_s(51) VALUE                "alphanumerisch + Sonderzeichen
                   ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-_&()+,./:;<=>'.

  CONSTANTS:
*----------------- Integer-Werte ---------------------------------------
    y_i_0 TYPE i   VALUE 0,            "Wert '0' (z.Zt. Initialwert)
    y_i_1 TYPE i   VALUE 1,            "Wert '1'
    y_i_2 TYPE i   VALUE 2.            "Wert '2'


  FIELD-SYMBOLS:
    <pos_editm>,                      "Zeichen in T370S-EDITM
    <pos_string>.                     "Zeichen in STRING


  PERFORM get_edit_mask USING i_structure_indicator
                     CHANGING t370s.
*  IF I_STRUCTURE_INDICATOR NE T370S-TPLKZ.
**                                      neues Strukturkennzeichen .......
*    SELECT SINGLE * FROM T370S
*           WHERE TPLKZ =  I_STRUCTURE_INDICATOR.
  IF t370s IS INITIAL.
    MESSAGE e029(/agri/glfl) WITH i_structure_indicator
            RAISING ex_str_indicator_not_found.
  ENDIF.
*  ENDIF.

  MOVE: t370s-editm TO e_editmask,
        t370s-stufm TO e_hierarchy_levels.
*                                      gefüllte Länge der Editionsmaske
  strlen_editm = strlen( t370s-editm ).
*                                      Länge von Block1 in STUFM .......
  IF t370s-stufm CA '1'.
    MOVE sy-fdpos TO offset_block1.
  ENDIF.


  CLEAR: e_separator_corrected,
         e_block_digit_corrected.
  MOVE i_structured_key TO string.
*                                      Vor User-Exit auf gefüllte Länge
*                                      und Trennzeichen prüfen .........
  strlen_string = strlen( string ).
  IF strlen_string GT strlen_editm.
*                                      gefüllte Stringlänge größer als
*                                      Editionsmaske ..................
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
    MESSAGE e047(/agri/glfl) "WITH strlen_editm
            RAISING ex_key_too_long. "#EC*
****
  ENDIF.

*                                      Vorprüfung auf Trennzeichen .....
  ASSIGN t370s-editm(1) TO <pos_editm>.
  ASSIGN string(1)      TO <pos_string>.
  DO strlen_string TIMES.
    IF <pos_editm> NE y_editm_a AND
       <pos_editm> NE y_editm_n AND
       <pos_editm> NE y_editm_x AND
       <pos_editm> NE y_editm_s.       "Sonderzeichen ab Rel. 4.70

*                                      Trennzeichen ....................
      calc_int = offset_string + y_i_1.
      IF calc_int NE strlen_string.
        IF <pos_string> NE <pos_editm>.
*                                      Trennzeichen stimmt nicht .......
          IF i_correct_separators IS INITIAL.
            MESSAGE e050(/agri/glfl) WITH calc_int
                    RAISING ex_no_compliance_with_editmask.
          ELSE.
*                                      Trennzeichen übernehmen .........
            MOVE <pos_editm> TO <pos_string>.
            MOVE 'X' TO e_separator_corrected.
          ENDIF.
        ENDIF.
      ELSE.
*                                      String endet mit Trennzeichen ...
        calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
        MESSAGE e053(/agri/glfl)" WITH calc_int
                RAISING ex_key_ends_with_separator. "#EC*
*****
      ENDIF.
    ENDIF.
    ADD y_i_1 TO offset_string.
    ASSIGN: <pos_editm>+y_i_1  TO <pos_editm>,
            <pos_string>+y_i_1 TO <pos_string>.
  ENDDO.

  IF NOT ( i_correct_block_digits    IS INITIAL ) AND
     NOT ( l_block_digit_corrected IS INITIAL ) AND
     NOT ( l_corrected_key         IS INITIAL ) AND
     l_corrected_key               NE string.
*                                      Strukturnummer geändert .........
    MOVE l_corrected_key TO string.
  ELSE.
    CLEAR l_block_digit_corrected.
  ENDIF.

* Prüfung ob nach Aufruf des Customer-Exit eine Trennzeichenkorrektur
* durchgeführt werden soll. Ist das Feld mit 'X' gefüllt, erfolgt eine
* Initialisierung des Feldes I_CORRECT_SEPARATORS, welches die Korrektur
* der Trennzeichen steuert. Durch Initialisieren des Feldes
* I_CORRECT_SEPARATORS erfolgt bei falscher Eingabe an der Stelle des
* Trennzeichens kein Überschreiben sondern eine Fehlermeldung.

  IF NOT l_not_correct_seperator IS INITIAL.
    CLEAR i_correct_separators.
  ENDIF.

*                                      Nach User-Exit komplette Prüfung
  strlen_string = strlen( string ).
  IF strlen_string GT strlen_editm.
*                                      gefüllte Stringlänge größer als
*                                      Editionsmaske ..................
    MESSAGE e047(/agri/fl) WITH strlen_editm  "#EC MG_MISSING
            RAISING ex_key_too_long.
  ENDIF.

*                                      STRING zeichenweise prüfen ......
  CLEAR: ind_not_initial,
         offset_string.
  ASSIGN t370s-editm(1) TO <pos_editm>.
  ASSIGN string(1)      TO <pos_string>.
  DO strlen_string TIMES.
    IF NOT ( <pos_string> IS INITIAL ).
*                                      Position nicht initial ..........
      MOVE 'X' TO ind_not_initial.
    ENDIF.
    IF offset_string = offset_block1 AND
       ind_not_initial IS INITIAL.
*                                      Ende von Block 1 erreicht und ...
*                                      noch kein Zeichen gefunden ......
      MESSAGE e054(/agri/glfl) RAISING ex_first_block_is_initial.
    ENDIF.
    CASE <pos_editm>.
      WHEN y_editm_a.
*                                      alphabetisches Zeichen? .........
        IF <pos_string> CN y_type_a.
          calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
          MESSAGE e048(/agri/glfl)" WITH calc_int
                  RAISING ex_no_compliance_with_editmask. "#EC*
*****
        ENDIF.
      WHEN y_editm_n.
*                                      numerisches Zeichen? ............
        IF <pos_string> CN y_type_n.
          calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
          MESSAGE e049(/agri/glfl) "WITH calc_int
                  RAISING ex_no_compliance_with_editmask. "#EC*
****
        ENDIF.
      WHEN y_editm_x.
*                                      alphanumerisches Zeichen? .......
        IF <pos_string> CN y_type_x.
          calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
          MESSAGE e048(/agri/glfl) "WITH calc_int
                  RAISING ex_no_compliance_with_editmask. "#EC*
****
        ENDIF.
*                                      Sonderzeichen?
      WHEN y_editm_s.
        IF <pos_string> CN y_type_s.
          calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
          MESSAGE e055(/agri/glfl) "WITH calc_int
                  RAISING ex_no_compliance_with_editmask. "#EC*
****
        ENDIF.

      WHEN OTHERS.
*                                      Trennzeichen ....................
        calc_int = offset_string + y_i_1.
        IF calc_int NE strlen_string.
          IF <pos_string> NE <pos_editm>.
*                                      Trennzeichen stimmt nicht .......
            IF i_correct_separators IS INITIAL.
              MESSAGE e050(/agri/glfl) WITH calc_int
                      RAISING ex_no_compliance_with_editmask.
            ELSE.
*                                      Trennzeichen übernehmen .........
              MOVE <pos_editm> TO <pos_string>.
              MOVE 'X' TO e_separator_corrected.
            ENDIF.
          ENDIF.
        ELSE.
*                                      String endet mit Trennzeichen ...
          calc_int = offset_string + y_i_1.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ
          MESSAGE e053(/agri/glfl) "WITH calc_int
                  RAISING ex_key_ends_with_separator. "#EC*
*****
        ENDIF.
    ENDCASE.
    ADD y_i_1 TO offset_string.
    ASSIGN: <pos_editm>+y_i_1  TO <pos_editm>,
            <pos_string>+y_i_1 TO <pos_string>.
  ENDDO.

*                                      Export-Parameter füllen .........
  MOVE string TO e_corrected_key.

ENDFUNCTION.
