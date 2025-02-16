"! <p class="shorttext synchronized" lang="en">AWS Signing Implementation</p>
CLASS zcl_llm_client_aws_sigv4 DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF auth_credentials,
             access_key TYPE string,
             secret_key TYPE string,
             region     TYPE string,
             service    TYPE string,
           END OF auth_credentials.

    METHODS constructor
      IMPORTING credentials TYPE auth_credentials.

    METHODS get_auth_header
      IMPORTING request_method TYPE string
                request_uri    TYPE string
                query_params   TYPE string OPTIONAL
                !headers       TYPE tihttpnvp
                payload        TYPE string
                request_time   TYPE timestamp
      RETURNING VALUE(result)  TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS algorithm TYPE string VALUE 'AWS4-HMAC-SHA256'.

    DATA auth_data TYPE auth_credentials.

    METHODS get_canonical_request
      IMPORTING request_method TYPE string
                request_uri    TYPE string
                query_params   TYPE string
                !headers       TYPE tihttpnvp
                payload        TYPE string
      RETURNING VALUE(result)  TYPE string.

    METHODS get_string_to_sign
      IMPORTING canonical_request TYPE string
                request_time      TYPE timestamp
      RETURNING VALUE(result)     TYPE string.

    METHODS get_signing_key
      IMPORTING request_time  TYPE timestamp
      RETURNING VALUE(result) TYPE xstring.

    METHODS get_credential_scope
      IMPORTING request_time  TYPE timestamp
      RETURNING VALUE(result) TYPE string.

    METHODS get_canonical_headers
      IMPORTING !headers      TYPE tihttpnvp
      RETURNING VALUE(result) TYPE string.

    METHODS get_signed_headers
      IMPORTING !headers      TYPE tihttpnvp
      RETURNING VALUE(result) TYPE string.

    METHODS hash_sha256
      IMPORTING !input        TYPE string
      RETURNING VALUE(result) TYPE string.

    METHODS sign_hmac256
      IMPORTING !key          TYPE xstring
                !input        TYPE string
      RETURNING VALUE(result) TYPE xstring.

    METHODS convert_time_stamp_to_date
      IMPORTING request_time  TYPE timestamp
      RETURNING VALUE(result) TYPE d.
ENDCLASS.

CLASS zcl_llm_client_aws_sigv4 IMPLEMENTATION.
  METHOD constructor.
    me->auth_data = credentials.
  ENDMETHOD.

  METHOD get_auth_header.
    DATA canonical_request TYPE string.
    DATA string_to_sign TYPE string.
    DATA signing_key TYPE xstring.
    DATA signature TYPE xstring.
    DATA temp TYPE string.
    canonical_request = get_canonical_request( request_method = request_method
                                                     request_uri    = request_uri
                                                     query_params   = query_params
                                                     headers        = headers
                                                     payload        = payload ).

    
    string_to_sign = get_string_to_sign( canonical_request = canonical_request
                                               request_time      = request_time ).

    
    signing_key = get_signing_key( request_time ).

    
    signature = sign_hmac256( key   = signing_key
                                    input = string_to_sign ).
    
    temp = |{ signature }|.
    result = |{ algorithm } Credential={ auth_data-access_key }/{ get_credential_scope( request_time ) }, |
          && |SignedHeaders={ get_signed_headers( headers ) }, Signature={ to_lower( temp ) }| ##NO_TEXT.
  ENDMETHOD.

  METHOD get_canonical_request.
    DATA canonical_uri TYPE string.

    DATA temp1 TYPE string.
    IF request_uri IS INITIAL.
      temp1 = '/'.
    ELSE.
      temp1 = request_uri.
    ENDIF.
    canonical_uri = temp1.

    result = |{ request_method }{ cl_abap_char_utilities=>newline }| &&
            |{ canonical_uri }{ cl_abap_char_utilities=>newline }| &&
            |{ query_params }{ cl_abap_char_utilities=>newline }| &&
            |{ get_canonical_headers( headers ) }{ cl_abap_char_utilities=>newline }| &&
            |{ get_signed_headers( headers ) }{ cl_abap_char_utilities=>newline }| &&
            |{ hash_sha256( payload ) }|.
  ENDMETHOD.

  METHOD get_string_to_sign.
    DATA helper TYPE string.
    DATA temp2 TYPE string.
    DATA timestamp TYPE string.
    DATA credential_scope TYPE string.
    temp2 = request_time.
    helper = temp2.
    
    timestamp = |{ helper(8) }T{ helper+8(6) }Z|.

    
    credential_scope = get_credential_scope( request_time ).

    result = |{ algorithm }{ cl_abap_char_utilities=>newline }| &&
             |{ timestamp }{ cl_abap_char_utilities=>newline }| &&
             |{ credential_scope }{ cl_abap_char_utilities=>newline }| &&
             |{ hash_sha256( canonical_request ) }|.
  ENDMETHOD.

  METHOD get_signing_key.
    DATA date TYPE d.
    DATA date_str TYPE string.
    DATA k_date TYPE xstring.
    DATA k_region TYPE xstring.
    DATA k_service TYPE xstring.
    date = convert_time_stamp_to_date( request_time ).
    
    date_str = date.
    
    k_date = sign_hmac256( key   = cl_binary_convert=>string_to_xstring_utf8( |AWS4{ auth_data-secret_key }| )
                                 input = date_str ).

    
    k_region = sign_hmac256( key   = k_date
                                   input = auth_data-region ).

    
    k_service = sign_hmac256( key   = k_region
                                    input = auth_data-service ).

    result = sign_hmac256( key   = k_service
                           input = 'aws4_request' ).
  ENDMETHOD.

  METHOD get_credential_scope.
    DATA date TYPE d.
    DATA date_str TYPE string.
    date = convert_time_stamp_to_date( request_time ).
    
    date_str = |{ date }|.

    result = |{ date_str }/| &&
             |{ auth_data-region }/| &&
             |{ auth_data-service }/| &&
             |aws4_request|.
  ENDMETHOD.

  METHOD get_canonical_headers.
    DATA sorted_headers TYPE tihttpnvp.
    FIELD-SYMBOLS <header> LIKE LINE OF sorted_headers.

    sorted_headers = headers.
    SORT sorted_headers BY name ASCENDING.

    
    LOOP AT sorted_headers ASSIGNING <header>.
      result = |{ result }{ to_lower( <header>-name ) }:{ condense( <header>-value ) }{ cl_abap_char_utilities=>newline }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_signed_headers.
    DATA sorted_headers TYPE tihttpnvp.
    FIELD-SYMBOLS <header> LIKE LINE OF sorted_headers.

    sorted_headers = headers.
    SORT sorted_headers BY name ASCENDING.

    
    LOOP AT sorted_headers ASSIGNING <header>.
      IF result IS NOT INITIAL.
        result = |{ result };|.
      ENDIF.
      result = |{ result }{ to_lower( <header>-name ) }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD hash_sha256.
        DATA hash TYPE string.
    TRY.
        
        cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_algorithm  = 'SHA256'
                                                                   if_data       = input
                                                         IMPORTING ef_hashstring = hash ).

        result = to_lower( hash ).
      CATCH cx_abap_message_digest.
    ENDTRY.
  ENDMETHOD.

  METHOD sign_hmac256.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_char( EXPORTING if_algorithm   = 'SHA256'
                                                         if_key         = key
                                                         if_data        = input
                                               IMPORTING ef_hmacxstring = result ).
      CATCH cx_abap_message_digest.
    ENDTRY.
  ENDMETHOD.

  METHOD convert_time_stamp_to_date.
    DATA date TYPE d.

    CONVERT TIME STAMP request_time TIME ZONE sy-zonlo
            INTO DATE date.

    result = date.
  ENDMETHOD.
ENDCLASS.
