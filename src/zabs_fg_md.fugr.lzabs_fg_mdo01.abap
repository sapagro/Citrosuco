*&---------------------------------------------------------------------*
*& Include          LZABS_FG_MDO01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  f_get_susbst
*&---------------------------------------------------------------------*
FORM f_get_susbst CHANGING p_stop_set TYPE xfeld.

  SELECT SINGLE * FROM zabs_tab_user
        INTO gs_deleb WHERE subst = sy-uname.
  IF sy-subrc IS NOT INITIAL
  OR ( sy-subrc IS INITIAL
 AND NOT sy-datum BETWEEN
         gs_deleb-datab
     AND gs_deleb-datbi ).
    p_stop_set = abap_true.
  ENDIF.

ENDFORM.                    "f_get_susbst
*&---------------------------------------------------------------------*
*&      Form  f_add_subst
*&---------------------------------------------------------------------*
FORM f_add_subst TABLES lt_receiver STRUCTURE somlreci1.

  IF gs_deleb IS INITIAL.
    SELECT SINGLE * FROM zabs_tab_user
          INTO gs_deleb WHERE aprov = sy-uname.
  ENDIF.

  CHECK gs_deleb IS NOT INITIAL.

  DATA: gs_receiver TYPE somlreci1,
        l_email     TYPE adr6-smtp_addr.

  SELECT SINGLE adr6~smtp_addr INTO l_email
            FROM adr6
              INNER JOIN usr21
                 ON usr21~addrnumber = adr6~addrnumber
                AND usr21~persnumber = adr6~persnumber
                    WHERE usr21~bname     = gs_deleb-subst
                      AND adr6~flgdefault = abap_true.
  CHECK sy-subrc IS INITIAL.

  TRANSLATE l_email TO LOWER CASE.
*****
  gs_receiver-receiver   = l_email.
  gs_receiver-rec_type   = 'U'.
  gs_receiver-notif_del  = abap_true.
  gs_receiver-notif_ndel = abap_true.
  APPEND gs_receiver TO lt_receiver.

  SORT lt_receiver BY receiver.
  DELETE ADJACENT DUPLICATES FROM lt_receiver COMPARING receiver.

ENDFORM.                    "f_add_subst
