FUNCTION zfmfp_unplanned_taskorder.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_CMNUM) TYPE  /AGRI/GLCMNUM
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_BASOQ) TYPE  /AGRI/GLBASOQ
*"     REFERENCE(I_STRNO) TYPE  /AGRI/GLSTRNO
*"     REFERENCE(I_VARIA) TYPE  /AGRI/GLVARIA
*"     REFERENCE(I_CPROS) TYPE  /AGRI/GLCPROS
*"     REFERENCE(I_DATE) TYPE  SYST_DATUM
*"     REFERENCE(I_VERID) TYPE  VERID
*"  EXPORTING
*"     REFERENCE(E_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA: t_list        TYPE table_abaplist,
        lt_memory_msg TYPE /agri/t_gprolog,
        t_ascii       TYPE STANDARD TABLE OF soli INITIAL SIZE 0,
        t_split       TYPE STANDARD TABLE OF so_text255 INITIAL SIZE 0,
        t_messages    TYPE zt_fmfp_bdcmsgcoll,
        r_strno       TYPE RANGE OF /agri/glstrno.

  INSERT INITIAL LINE INTO TABLE r_strno
    ASSIGNING FIELD-SYMBOL(<wa_stnro>).
  IF sy-subrc EQ 0.
    <wa_stnro> = 'IEQ'.
    <wa_stnro>-low = i_strno.

    SUBMIT zfmfp_unplanned_taskorder
            WITH p_werks  EQ i_werks
            WITH p_cmnum  EQ i_cmnum
            WITH p_matnr  EQ i_matnr
            WITH p_basoq  EQ i_basoq
            WITH so_strno IN r_strno[]
            WITH p_varia  EQ i_varia
            WITH p_cpros  EQ i_cpros
            WITH p_date   EQ i_date
            WITH p_verid  EQ i_verid
            EXPORTING LIST TO MEMORY AND RETURN.

      CALL FUNCTION 'ZFMFP_MEMORY_MESSAGES'
        IMPORTING
          et_messages = lt_memory_msg.

      LOOP AT lt_memory_msg INTO DATA(lwa_mem_msg).
        INSERT INITIAL LINE INTO TABLE e_return
          ASSIGNING FIELD-SYMBOL(<wa_return>).
        IF sy-subrc EQ 0.
          <wa_return>-id = lwa_mem_msg-msgid.
          <wa_return>-number = lwa_mem_msg-msgno.
          <wa_return>-type = lwa_mem_msg-msgty.
          <wa_return>-message_v1 = lwa_mem_msg-msgv1.
          <wa_return>-message_v2 = lwa_mem_msg-msgv2.
          <wa_return>-message_v3 = lwa_mem_msg-msgv3.
          <wa_return>-message_v4 = lwa_mem_msg-msgv4.
          MESSAGE ID <wa_return>-id TYPE <wa_return>-type
            NUMBER <wa_return>-number WITH <wa_return>-message_v1
            <wa_return>-message_v2 <wa_return>-message_v3
            <wa_return>-message_v4 INTO <wa_return>-message.
        ENDIF.
      ENDLOOP.

*.get the output list from memory
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = t_list
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.

    CALL FUNCTION 'LIST_TO_ASCI'
      TABLES
        listasci   = t_ascii
        listobject = t_list
      EXCEPTIONS
        OTHERS     = 0.

    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = t_list
      EXCEPTIONS
        OTHERS     = 0.

    LOOP AT t_ascii INTO DATA(wa_ascii).
      IF wa_ascii-line(5) NE 'DISP;'
      AND wa_ascii-line(7) NE 'NODISP;'.
        DELETE t_ascii INDEX sy-tabix.
      ELSE.
        REFRESH t_split.
        IF wa_ascii-line(7) EQ 'NODISP;'.
          INSERT INITIAL LINE INTO TABLE t_messages
             ASSIGNING FIELD-SYMBOL(<wa_message>).
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE e_return
              ASSIGNING <wa_return>.
            IF sy-subrc EQ 0.
              SPLIT wa_ascii-line AT ';' INTO TABLE t_split.
              DELETE t_split INDEX 1.
              LOOP AT t_split REFERENCE INTO DATA(r_split).
                CASE sy-tabix.
                  WHEN 1.
                    <wa_message>-msgid     = r_split->*.
                    <wa_return>-id         = r_split->*.
                  WHEN 2.
                    <wa_message>-msgtyp    = r_split->*.
                    <wa_return>-type       = r_split->*.
                  WHEN 3.
                    <wa_message>-msgnr     = r_split->*.
                    <wa_return>-number     = r_split->*.
                  WHEN 4.
                    <wa_message>-msgv1     = r_split->*.
                    <wa_return>-message_v1 = r_split->*.
                  WHEN 5.
                    <wa_message>-msgv2     = r_split->*.
                    <wa_return>-message_v2 = r_split->*.
                  WHEN 6.
                    <wa_message>-msgv3     = r_split->*.
                    <wa_return>-message_v3 = r_split->*.
                  WHEN 7.
                    <wa_message>-msgv4     = r_split->*.
                    <wa_return>-message_v4 = r_split->*.
                ENDCASE.
              ENDLOOP.
              MESSAGE ID <wa_return>-id TYPE <wa_return>-type
                NUMBER <wa_return>-number WITH <wa_return>-message_v1
                <wa_return>-message_v2 <wa_return>-message_v3
                <wa_return>-message_v4 INTO <wa_return>-message.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT e_return BY id number type message.
  DELETE ADJACENT DUPLICATES FROM e_return COMPARING ALL FIELDS.

ENDFUNCTION.
