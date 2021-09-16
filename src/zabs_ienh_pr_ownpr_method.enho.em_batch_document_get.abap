METHOD batch_document_get .

  SELECT SINGLE charg FROM mseg INTO (e_charg)
                       WHERE mblnr = i_mblnr
                    AND mjahr  =  i_mjahr.
ENDMETHOD.
