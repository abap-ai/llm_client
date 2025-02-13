CLASS zcl_llm_encryption DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    INTERFACES zif_llm_encryption .
  PROTECTED SECTION.
    CLASS-DATA subject    TYPE string.
    CLASS-DATA addrbook   TYPE ssfpab.
    CLASS-DATA auth_class TYPE REF TO zif_llm_auth.

    CONSTANTS application TYPE ssfappl VALUE 'ZLLMCT'.

    CONSTANTS bin_line    TYPE i       VALUE 255.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_encryption IMPLEMENTATION.
  METHOD zif_llm_encryption~decrypt.
    DATA total_bytes TYPE i.
    TYPES temp11 TYPE STANDARD TABLE OF ssfbin.
DATA ssfbin_tab TYPE temp11.
    DATA temp1 LIKE ssfbin_tab.
    DATA j TYPE i.
    DATA temp3 LIKE sy-index.
      DATA temp2 LIKE LINE OF temp1.
      DATA temp8 TYPE xstring.
    TYPES temp12 TYPE TABLE OF ssfinfo.
DATA recipients TYPE temp12.
    DATA temp4 LIKE recipients.
    DATA temp5 LIKE LINE OF temp4.
    TYPES temp13 TYPE STANDARD TABLE OF ssfbin.
DATA output TYPE temp13.
    DATA output_len TYPE i.
      DATA temp6 TYPE string.
      DATA temp9 TYPE REF TO zcx_llm_validation.
    DATA temp7 TYPE xstring.
    DATA temp10 TYPE xstring.
    DATA r LIKE temp7.
    DATA wa LIKE LINE OF output.
    DATA outdata LIKE temp10.
    IF encrypted IS INITIAL.
      RETURN.
    ENDIF.

    auth_class->check_decrypt( ).

    
    total_bytes = xstrlen( encrypted ).
    


    
    CLEAR temp1.
    
    j = 0.
    
    temp3 = sy-index.
    WHILE j < total_bytes.
      sy-index = temp3.
      
      
      IF total_bytes - j >= bin_line.
        temp8 = encrypted+j(bin_line).
      ELSE.
        temp8 = encrypted+j.
      ENDIF.
      temp2-bindata = temp8.
      INSERT temp2 INTO TABLE temp1.
      j = j + bin_line.
    ENDWHILE.
    ssfbin_tab = temp1. " Remaining bytes

    

    
    CLEAR temp4.
    
    temp5-id = subject.
    temp5-profile = addrbook.
    INSERT temp5 INTO TABLE temp4.
    recipients = temp4.

    

    

    CALL FUNCTION 'SSF_KRN_DEVELOPE'
      EXPORTING  ostr_enveloped_data_l        = total_bytes
      IMPORTING  ostr_output_data_l           = output_len
      TABLES     ostr_enveloped_data          = ssfbin_tab
                 recipient                    = recipients
                 ostr_output_data             = output
      EXCEPTIONS ssf_krn_error                = 1
                 ssf_krn_noop                 = 2
                 ssf_krn_nomemory             = 3                " Main Memory Not Sufficient
                 ssf_krn_opinv                = 4
                 ssf_krn_nossflib             = 5
                 ssf_krn_recipient_error      = 6                " Error Occurred During Reading of Recipient Line
                 ssf_krn_input_data_error     = 7
                 ssf_krn_invalid_par          = 8                " An SSF Parameter is Erroneous
                 ssf_krn_invalid_parlen       = 9                " Length of an SSF Parameter is Erroneous
                 ssf_fb_input_parameter_error = 10               " Function Module Parameter Error
                 OTHERS                       = 11.
    IF sy-subrc <> 0.
      
      temp6 = sy-subrc.
      
      CREATE OBJECT temp9 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>decryption_failed attr1 = temp6.
      RAISE EXCEPTION temp9.
    ENDIF.

    
    CLEAR temp7.
    
    
    r = temp7.
    
    LOOP AT output INTO wa.
      r = r && wa-bindata.
    ENDLOOP.
    temp10 = r.
    
    outdata = temp10.

    result = cl_binary_convert=>xstring_utf8_to_string( outdata ).
    result = result(output_len).
  ENDMETHOD.

  METHOD zif_llm_encryption~encrypt.
    DATA input_len TYPE i.
    TYPES temp16 TYPE STANDARD TABLE OF ssfbin.
DATA input_data TYPE temp16.
    DATA temp8 LIKE input_data.
    DATA j TYPE i.
    DATA temp10 LIKE sy-index.
      DATA temp9 LIKE LINE OF temp8.
      DATA temp14 TYPE xstring.
    TYPES temp17 TYPE TABLE OF ssfinfo.
DATA recipients TYPE temp17.
    DATA temp11 LIKE recipients.
    DATA temp12 LIKE LINE OF temp11.
    TYPES temp18 TYPE STANDARD TABLE OF ssfbin.
DATA enveloped_data TYPE temp18.
      DATA temp13 TYPE string.
      DATA temp15 TYPE REF TO zcx_llm_validation.
    FIELD-SYMBOLS <fs_data> LIKE LINE OF enveloped_data.
    IF unencrypted IS INITIAL.
      RETURN.
    ENDIF.

    auth_class->check_encrypt( ).

    DATA(to_encrypt) = cl_binary_convert=>string_to_xstring_utf8( unencrypted ).
    
    input_len = xstrlen( to_encrypt ).
    


    
    CLEAR temp8.
    
    j = 0.
    
    temp10 = sy-index.
    WHILE j < input_len.
      sy-index = temp10.
      
      
      IF input_len - j >= bin_line.
        temp14 = to_encrypt+j(bin_line).
      ELSE.
        temp14 = to_encrypt+j.
      ENDIF.
      temp9-bindata = temp14.
      INSERT temp9 INTO TABLE temp8.
      j = j + bin_line.
    ENDWHILE.
    input_data = temp8. " Remaining bytes

    

    
    CLEAR temp11.
    
    temp12-id = subject.
    INSERT temp12 INTO TABLE temp11.
    recipients = temp11.

    


    CALL FUNCTION 'SSF_KRN_ENVELOPE'
      EXPORTING  ostr_input_data_l            = input_len
                 str_pab                      = addrbook
                 str_pab_password             = ''
      TABLES     ostr_input_data              = input_data
                 recipient_list               = recipients
                 ostr_enveloped_data          = enveloped_data
      EXCEPTIONS ssf_krn_error                = 1
                 ssf_krn_noop                 = 2
                 ssf_krn_nomemory             = 3                " Main Memory Not Sufficient
                 ssf_krn_opinv                = 4
                 ssf_krn_nossflib             = 5
                 ssf_krn_recipient_list_error = 6                " Error Occurred During Reading of Recipient Line
                 ssf_krn_input_data_error     = 7
                 ssf_krn_invalid_par          = 8                " An SSF Parameter is Erroneous
                 ssf_krn_invalid_parlen       = 9                " Length of an SSF Parameter is Erroneous
                 ssf_fb_input_parameter_error = 10               " Function Module Parameter Error
                 OTHERS                       = 11.
    IF sy-subrc <> 0.
      
      temp13 = sy-subrc.
      
      CREATE OBJECT temp15 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>encryption_failed attr1 = temp13.
      RAISE EXCEPTION temp15.
    ENDIF.

    
    LOOP AT enveloped_data ASSIGNING <fs_data>.
      CONCATENATE result <fs_data>-bindata INTO result IN BYTE MODE.
    ENDLOOP.
  ENDMETHOD.

  METHOD class_constructor.
    DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
    DATA profile TYPE localfile.
      DATA temp14 TYPE string.
      DATA temp17 TYPE REF TO zcx_llm_validation.
    DATA certificate TYPE xstring.
      DATA temp15 TYPE string.
      DATA temp18 TYPE REF TO zcx_llm_validation.
      DATA temp16 TYPE string.
      DATA temp19 TYPE REF TO zcx_llm_validation.
    CALL BADI llm_badi->get_authorization_impl
      RECEIVING result = auth_class.

    

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING  applic        = application
      IMPORTING  profile       = profile
      EXCEPTIONS pse_not_found = 1
                 OTHERS        = 2.
    IF sy-subrc <> 0 OR profile IS INITIAL.
      
      temp14 = sy-subrc.
      
      CREATE OBJECT temp17 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>encryption_setup attr1 = 'Filename' attr2 = temp14.
      RAISE EXCEPTION temp17 ##NO_TEXT.
    ENDIF.

    
    addrbook = profile.

    CALL FUNCTION 'SSFC_GET_CERTIFICATE'
      EXPORTING  profile               = addrbook
      IMPORTING  certificate           = certificate
      EXCEPTIONS ssf_krn_error         = 1
                 ssf_krn_nomemory      = 2                " Main Memory Not Sufficient
                 ssf_krn_nossflib      = 3
                 ssf_krn_invalid_par   = 4                " An SSF Parameter is Erroneous
                 ssf_krn_nocertificate = 5
                 OTHERS                = 6.
    IF sy-subrc <> 0 OR certificate IS INITIAL.
      
      temp15 = sy-subrc.
      
      CREATE OBJECT temp18 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>encryption_setup attr1 = 'GetCert' attr2 = temp15.
      RAISE EXCEPTION temp18.
    ENDIF.

    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING  certificate         = certificate
      IMPORTING  subject             = subject
      EXCEPTIONS ssf_krn_error       = 1
                 ssf_krn_nomemory    = 2                " Main Memory Not Sufficient
                 ssf_krn_nossflib    = 3
                 ssf_krn_invalid_par = 4                " An SSF Parameter is Erroneous
                 OTHERS              = 5.
    IF sy-subrc <> 0 OR subject IS INITIAL.
      
      temp16 = sy-subrc.
      
      CREATE OBJECT temp19 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>encryption_setup attr1 = 'ParseCert' attr2 = temp16.
      RAISE EXCEPTION temp19.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_encryption~sign.
    TYPES temp26 TYPE STANDARD TABLE OF ssfbin WITH DEFAULT KEY.
DATA ssfbin_tab TYPE temp26.
    TYPES temp27 TYPE STANDARD TABLE OF ssfbin WITH DEFAULT KEY.
DATA signed_bin TYPE temp27.
    DATA bin_line   TYPE i VALUE 255.

    DATA total_bytes TYPE i.
    DATA temp17 LIKE ssfbin_tab.
    DATA j TYPE i.
    DATA temp19 LIKE sy-index.
      DATA temp18 LIKE LINE OF temp17.
      DATA temp24 TYPE xstring.
    DATA output_length TYPE ssflen.
    TYPES temp28 TYPE STANDARD TABLE OF ssfinfo.
DATA signers TYPE temp28.
    DATA profile TYPE localfile.
      DATA temp20 TYPE REF TO zcx_llm_validation.
    DATA temp21 LIKE LINE OF signers.
      DATA temp22 TYPE REF TO zcx_llm_validation.
    DATA temp23 TYPE xstring.
    DATA temp25 TYPE xstring.
    DATA r LIKE temp23.
    DATA wa LIKE LINE OF signed_bin.
    DATA signature LIKE temp25.
    total_bytes = xstrlen( xstring_to_sign ).

    
    CLEAR temp17.
    
    j = 0.
    
    temp19 = sy-index.
    WHILE j < total_bytes.
      sy-index = temp19.
      
      
      IF total_bytes - j >= bin_line.
        temp24 = xstring_to_sign+j(bin_line).
      ELSE.
        temp24 = xstring_to_sign+j.
      ENDIF.
      temp18-bindata = temp24.
      INSERT temp18 INTO TABLE temp17.
      j = j + bin_line.
    ENDWHILE.
    ssfbin_tab = temp17.

    
    

    

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING  applic        = ssf_application
      IMPORTING  profile       = profile
      EXCEPTIONS pse_not_found = 1
                 OTHERS        = 2.
    IF sy-subrc <> 0 OR profile IS INITIAL.
      
      CREATE OBJECT temp20 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>sign_issue attr1 = |SSF Application { ssf_application } invalid|.
      RAISE EXCEPTION temp20 ##NO_TEXT.
    ENDIF.

    
    CLEAR temp21.
    temp21-id = '<implicit>'.
    temp21-result = 28.
    temp21-profile = profile.
    APPEND temp21 TO signers.
    CALL FUNCTION 'SSF_KRN_SIGN'
      EXPORTING  str_format                   = 'PKCS1-V1.5'
                 b_inc_certs                  = abap_false
                 b_detached                   = abap_false
                 b_inenc                      = abap_false
                 ostr_input_data_l            = total_bytes
                 str_hashalg                  = 'SHA256'
      IMPORTING  ostr_signed_data_l           = output_length
      TABLES     ostr_input_data              = ssfbin_tab
                 signer                       = signers
                 ostr_signed_data             = signed_bin
      EXCEPTIONS ssf_krn_error                = 1
                 ssf_krn_noop                 = 2
                 ssf_krn_nomemory             = 3
                 ssf_krn_opinv                = 4
                 ssf_krn_nossflib             = 5
                 ssf_krn_signer_list_error    = 6
                 ssf_krn_input_data_error     = 7
                 ssf_krn_invalid_par          = 8
                 ssf_krn_invalid_parlen       = 9
                 ssf_fb_input_parameter_error = 10.

    IF sy-subrc <> 0.
      
      CREATE OBJECT temp22 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>sign_issue attr1 = |SSF_KRN_SIGN failed with sy-subrc = { sy-subrc }|.
      RAISE EXCEPTION temp22 ##NO_TEXT.
    ENDIF.

    
    CLEAR temp23.
    
    
    r = temp23.
    
    LOOP AT signed_bin INTO wa.
      r = r && wa-bindata.
    ENDLOOP.
    temp25 = r.
    
    signature = temp25.
    result = signature(output_length).
  ENDMETHOD.

ENDCLASS.
