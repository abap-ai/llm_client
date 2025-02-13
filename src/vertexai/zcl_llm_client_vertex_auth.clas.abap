"! <p class="shorttext synchronized" lang="en">Vertex Authentication Implementation</p>
CLASS zcl_llm_client_vertex_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Get Vertex AI JWT token</p>
    "! Checks shared memory for an existing token, if none exists create a new one.
    "! @parameter provider | <p class="shorttext synchronized">LLM Provider Entry</p>
    "! @parameter result   | <p class="shorttext synchronized">JWT Token</p>
    "! @raising zcx_llm_http_error | <p class="shorttext synchronized" lang="en">HTTP Error</p>
    "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en">Authorization Error</p>
    METHODS get_token IMPORTING provider      TYPE zllm_providers
                      RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token
                      RAISING   zcx_llm_http_error zcx_llm_authorization.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Get a token from shared memory if exists</p>
    "!
    "! @parameter provider_name | <p class="shorttext synchronized">Provider Name</p>
    "! @parameter result        | <p class="shorttext synchronized">Token</p>
    METHODS get_memory_token IMPORTING provider_name TYPE zllm_provider_name
                             RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token.

    "! <p class="shorttext synchronized">Check if the token is still valid</p>
    "!
    "! @parameter token  | <p class="shorttext synchronized">Token</p>
    "! @parameter result | <p class="shorttext synchronized">Validation Result</p>
    METHODS is_valid IMPORTING token         TYPE zcl_llm_client_vertexai_sr=>token
                     RETURNING VALUE(result) TYPE sap_bool.

    "! <p class="shorttext synchronized">Get a new token</p>
    "!
    "! @parameter provider              | <p class="shorttext synchronized">Provider Details</p>
    "! @parameter result                | <p class="shorttext synchronized">Token</p>
    "! @raising   zcx_llm_http_error    | <p class="shorttext synchronized">HTTP Error</p>
    "! @raising   zcx_llm_authorization | <p class="shorttext synchronized">Authorization Error</p>
    METHODS new_token IMPORTING provider      TYPE zllm_providers
                      RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token
                      RAISING   zcx_llm_http_error zcx_llm_authorization.

    "! <p class="shorttext synchronized">Save a new token to shared memory</p>
    "!
    "! @parameter token              | <p class="shorttext synchronized">Token</p>
    "! @raising   zcx_llm_http_error | <p class="shorttext synchronized">HTTP Error</p>
    METHODS save_token IMPORTING token TYPE zcl_llm_client_vertexai_sr=>token
                       RAISING   zcx_llm_http_error.

ENDCLASS.

CLASS zcl_llm_client_vertex_auth IMPLEMENTATION.
  METHOD get_token.
    DATA token TYPE zcl_llm_client_vertexai_sr=>token.
    token = get_memory_token( provider-provider_name ).
    IF token IS NOT INITIAL AND is_valid( token ) = abap_true.
      result = token.
      RETURN.
    ENDIF.
    token = new_token( provider ).
    save_token( token ).
    result = token.
  ENDMETHOD.

  METHOD get_memory_token.
        DATA area TYPE REF TO ZCL_LLM_CLIENT_VERTEXAI_S_AREA.
        DATA temp1 TYPE zcl_llm_client_vertexai_sr=>token.
    TRY.
        
        area = zcl_llm_client_vertexai_s_area=>attach_for_read( ).
        result = area->root->get_token( provider_name ).
        area->detach( ).
      CATCH cx_shm_inconsistent
            cx_shm_no_active_version
            cx_shm_read_lock_active
            cx_shm_exclusive_lock_active
            cx_shm_parameter_error
            cx_shm_change_lock_active.
        " we just set this as no token
        
        CLEAR temp1.
        result = temp1.
    ENDTRY.
  ENDMETHOD.

  METHOD is_valid.
    DATA current_timestamp TYPE timestamp.
    DATA valid TYPE i.
    DATA temp1 TYPE xsdboolean.

    GET TIME STAMP FIELD current_timestamp.
    
    valid = cl_abap_tstmp=>subtract( tstmp1 = token-valid_until
                                           tstmp2 = current_timestamp ).
    
    temp1 = boolc( valid > 60 ).
    result = temp1.
  ENDMETHOD.

  METHOD new_token.
    DATA auth_config TYPE string.
      DATA temp2 TYPE REF TO zcx_llm_http_error.
    DATA ssf_application TYPE string.
    DATA username TYPE string.
      DATA temp3 TYPE REF TO zcx_llm_http_error.
DATA BEGIN OF payload_header.
DATA alg TYPE string.
DATA typ TYPE string.
DATA END OF payload_header.
DATA BEGIN OF payload_body.
DATA iss TYPE string.
DATA aud TYPE string.
DATA scope TYPE string.
DATA iat TYPE i.
DATA exp TYPE i.
DATA END OF payload_body.
    DATA timestamp TYPE p LENGTH 8 DECIMALS 0.
    DATA date TYPE d.
    DATA time TYPE t.
    DATA unix_time TYPE string.
    DATA json_header TYPE string.
    DATA json_body TYPE string.
    DATA header_encoded TYPE string.
    DATA body_encoded TYPE string.
    DATA payload_encoded TYPE string.
        DATA temp4 TYPE ssfappl.
        DATA error TYPE REF TO zcx_llm_validation.
        DATA temp5 TYPE REF TO zcx_llm_http_error.
    DATA signature_base64 TYPE string.
    DATA client TYPE REF TO if_http_client.
      DATA temp6 TYPE REF TO zcx_llm_http_error.
      DATA temp7 TYPE REF TO zcx_llm_http_error.
      DATA temp8 TYPE REF TO zcx_llm_http_error.
    DATA response TYPE string.
DATA BEGIN OF oauth_response.
DATA access_token TYPE string.
DATA expires_in TYPE i.
DATA END OF oauth_response.
      DATA temp9 TYPE REF TO zcx_llm_http_error.

    IF provider-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_config = enc_class->decrypt( provider-auth_encrypted ).
    ELSE.
      
      CREATE OBJECT temp2 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Missing google vertex configuration'.
      RAISE EXCEPTION temp2 ##NO_TEXT.
    ENDIF.

    
    
    SPLIT auth_config AT ';' INTO ssf_application username.
    IF ssf_application IS INITIAL OR username IS INITIAL.
      
      CREATE OBJECT temp3 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Missing google vertex details it should be ssf_application;serviceaccountemail'.
      RAISE EXCEPTION temp3 ##NO_TEXT.
    ENDIF.

    
    

    payload_header-alg = 'RS256'.
    payload_header-typ = 'JWT'.

    payload_body-iss   = username.
    payload_body-scope = 'https://www.googleapis.com/auth/cloud-platform'.
    payload_body-aud   = 'https://oauth2.googleapis.com/token'.

    
    GET TIME STAMP FIELD timestamp.
    
    
    CONVERT TIME STAMP timestamp TIME ZONE 'UTC' INTO DATE date TIME time.
    
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date      = date
                                                              iv_time      = time
                                                              iv_msec      = 0
                                                    IMPORTING ev_timestamp = unix_time ).

    payload_body-iat = substring( val = unix_time
                                  off = 0
                                  len = strlen( unix_time ) - 3 ).
    payload_body-exp = payload_body-iat + 3600.

    " Create the JSON strings inside the concatenation using string templates
    
    json_header = zcl_llm_common=>to_json( payload_header ).
    
    json_body = zcl_llm_common=>to_json( payload_body ).
    
    header_encoded = cl_http_utility=>encode_base64( json_header ).
    
    body_encoded = cl_http_utility=>encode_base64( json_body ).

    " Concatenate with '.'
    
    payload_encoded = |{ header_encoded }.{ body_encoded }|.
    payload_encoded = replace( val  = payload_encoded
                               sub  = '='
                               with = ``
                               occ  = 0 ).
    payload_encoded = replace( val  = payload_encoded
                               sub  = '+'
                               with = '-'
                               occ  = 0 ).
    payload_encoded = replace( val  = payload_encoded
                               sub  = '/'
                               with = '_'
                               occ  = 0 ).

    DATA(bin_payload) = cl_binary_convert=>string_to_xstring_utf8( payload_encoded ).

    TRY.
        
        temp4 = ssf_application.
        DATA(signature) = enc_class->sign( ssf_application = temp4
                                           xstring_to_sign = bin_payload ).
        
      CATCH zcx_llm_validation INTO error. " Validation error
        
        CREATE OBJECT temp5 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Error during signing' previous = error.
        RAISE EXCEPTION temp5 ##NO_TEXT.
    ENDTRY.

    " Base64 encode the binary signature
    
    signature_base64 = cl_http_utility=>encode_x_base64( unencoded = signature ).
    " URL-safe replacements after standard Base64 encoding:
    signature_base64 = replace( val  = signature_base64
                                sub  = '='
                                with = ``
                                occ  = 0 ).
    signature_base64 = replace( val  = signature_base64
                                sub  = '+'
                                with = '-'
                                occ  = 0 ).
    signature_base64 = replace( val  = signature_base64
                                sub  = '/'
                                with = '_'
                                occ  = 0 ).

    " Append signature separated by '.' to the binary string
    result-content  = |{ payload_encoded }.{ signature_base64 }|.

    result-provider = provider-provider_name.

    " Use the signed jwt to authenticate and get the auth token
    

    cl_http_client=>create_by_destination( EXPORTING  destination              = provider-auth_rfc_destination
                                           IMPORTING  client                   = client
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5
                                                      OTHERS                   = 6 ).

    IF sy-subrc <> 0.
      
      CREATE OBJECT temp6 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Destination error, check setup'.
      RAISE EXCEPTION temp6 ##NO_TEXT.
    ENDIF.

    client->request->set_formfield_encoding( formfield_encoding = if_http_entity=>co_formfield_encoding_encoded ).
    client->request->set_form_field( name  = 'grant_type'
                                     value = 'urn:ietf:params:oauth:grant-type:jwt-bearer' ) ##NO_TEXT.
    client->request->set_form_field( name  = 'assertion'
                                     value = result-content ).

    client->request->set_method( if_http_request=>co_request_method_post ).

    client->send( EXCEPTIONS http_communication_failure = 1                  " Communication Error
                             http_invalid_state         = 2                  " Invalid state
                             http_processing_failed     = 3                  " Error When Processing Method
                             http_invalid_timeout       = 4                  " Invalid Time Entry
                             OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      
      CREATE OBJECT temp7 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Cannot send auth message'.
      RAISE EXCEPTION temp7 ##NO_TEXT.
    ENDIF.

    client->receive( EXCEPTIONS http_communication_failure = 1                " Communication Error
                                http_invalid_state         = 2                " Invalid state
                                http_processing_failed     = 3                " Error When Processing Method
                                OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      
      CREATE OBJECT temp8 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Auth communication error'.
      RAISE EXCEPTION temp8 ##NO_TEXT.
    ENDIF.

    
    response = client->response->get_cdata( ).

    

    zcl_llm_common=>from_json( EXPORTING json = response
                               CHANGING  data = oauth_response ).
    result-valid_until = timestamp.
    result-valid_until = cl_abap_tstmp=>add_to_short( tstmp = result-valid_until
                                                      secs  = oauth_response-expires_in ).
    result-content     = oauth_response-access_token.

    IF result-content IS INITIAL.
      
      CREATE OBJECT temp9 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = |No auth token returned{ response }|.
      RAISE EXCEPTION temp9 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD save_token.
        DATA area TYPE REF TO ZCL_LLM_CLIENT_VERTEXAI_S_AREA.
        DATA temp10 TYPE REF TO zcx_llm_http_error.
    TRY.
        zcl_llm_client_vertexai_s_area=>build( ).
        
        area = zcl_llm_client_vertexai_s_area=>attach_for_update( ).
        area->root->set_token( token ).
        area->detach_commit( ).
      CATCH cx_shm_inconsistent
            cx_shm_no_active_version
            cx_shm_exclusive_lock_active
            cx_shm_version_limit_exceeded
            cx_shm_change_lock_active
            cx_shm_parameter_error
            cx_shm_pending_lock_removed.
        
        CREATE OBJECT temp10 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_auth_processing attr1 = 'Unable to save token to shared memory'.
        RAISE EXCEPTION temp10 ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
