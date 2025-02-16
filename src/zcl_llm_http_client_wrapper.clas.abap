"! <p class="shorttext synchronized" lang="en">HTTP Client Wrapper</p>
CLASS zcl_llm_http_client_wrapper DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_llm_http_client_wrapper.

    ALIASES get_client   FOR zif_llm_http_client_wrapper~get_client.
    ALIASES set_header   FOR zif_llm_http_client_wrapper~set_header.
    ALIASES set_url      FOR zif_llm_http_client_wrapper~set_url.
    ALIASES communicate  FOR zif_llm_http_client_wrapper~communicate.
    ALIASES close_client FOR zif_llm_http_client_wrapper~close_client.

    METHODS constructor
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RAISING   zcx_llm_validation.

  PROTECTED SECTION.
    DATA client_config   TYPE zllm_clnt_config.
    DATA provider_config TYPE zllm_providers.
    DATA url             TYPE string.
    DATA client          TYPE REF TO if_http_client.
    DATA call_logger     TYPE REF TO zif_llm_call_logger.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_http_client_wrapper IMPLEMENTATION.
  METHOD constructor.
      DATA temp1 TYPE string.
      DATA temp2 TYPE string.
      DATA temp3 TYPE REF TO zcx_llm_validation.
    me->client_config   = client_config.
    me->provider_config = provider_config.

    DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
    CALL BADI llm_badi->get_call_logger_impl
      RECEIVING result = call_logger.

    cl_http_client=>create_by_destination( EXPORTING  destination              = provider_config-rfc_destination
                                           IMPORTING  client                   = client
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5
                                                      OTHERS                   = 6 ).
    IF sy-subrc <> 0.
      
      CASE sy-subrc.
        WHEN 1.
          temp1 = `Argument Not Found`.
        WHEN 2.
          temp1 = `Destination Not Found`.
        WHEN 3.
          temp1 = `Destination No Authority`.
        WHEN 4.
          temp1 = `Plugin Not Active`.
        WHEN 5.
          temp1 = `Internal Error`.
        WHEN OTHERS.
          temp1 = `Others`.
      ENDCASE.
      
      temp2 = provider_config-rfc_destination.
      
      CREATE OBJECT temp3 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>http_destination_error attr1 = temp2 attr2 = temp1.
      RAISE EXCEPTION temp3 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_client.
    CREATE OBJECT client TYPE zcl_llm_http_client_wrapper EXPORTING client_config = client_config provider_config = provider_config.
  ENDMETHOD.

  METHOD set_header.
    client->request->set_header_field( name  = name
                                       value = value ).
  ENDMETHOD.

  METHOD set_url.
    me->url = url.
  ENDMETHOD.

  METHOD close_client.
    client->close( EXCEPTIONS http_invalid_state = 0
                              OTHERS             = 1 ).
  ENDMETHOD.

  METHOD communicate.
    DATA timestamp TYPE timestamp.
    DATA temp2 TYPE zllm_call_log.
      DATA headers TYPE tihttpnvp.
      DATA error_code TYPE i.
      DATA error_message TYPE string.
        DATA temp3 TYPE string.
        DATA temp6 TYPE REF TO zcx_llm_http_error.
        DATA temp4 TYPE string.
        DATA temp7 TYPE REF TO zcx_llm_http_error.
      DATA temp5 TYPE string.
      DATA temp8 TYPE REF TO zcx_llm_http_error.
    client->request->set_method( if_http_request=>co_request_method_post ).
    client->request->set_header_field( name  = 'Content-Type'
                                       value = 'application/json' ) ##NO_TEXT.

    IF url IS NOT INITIAL.
      cl_http_utility=>set_request_uri( request = client->request
                                        uri     = url ).
    ENDIF.

    client->request->set_cdata( request ).

    client->send( EXCEPTIONS http_communication_failure = 1
                             http_invalid_state         = 2
                             http_invalid_timeout       = 3
                             http_processing_failed     = 4
                             OTHERS                     = 5 ).
    IF sy-subrc = 0.
      client->receive( EXCEPTIONS http_communication_failure = 1
                                  http_invalid_state         = 2
                                  http_processing_failed     = 4
                                  OTHERS                     = 5 ).
    ENDIF.

    response-http_response = client->response.
    client->response->get_status( IMPORTING code = response-code ).
    response-response = client->response->get_cdata( ).

    
    GET TIME STAMP FIELD timestamp.
    
    CLEAR temp2.
    temp2-id = session_id.
    temp2-msg = msg.
    temp2-timestamp = timestamp.
    temp2-request = request.
    temp2-response = response-response.
    temp2-uname = sy-uname.
    call_logger->add( temp2 ).

    " Need to reset the request as otherwise the next call in this session will overwrite the
    " path prefix defined in SM59.
    IF url IS NOT INITIAL.
      " Need to save all non-SAP internal headers (SAP internal ones start with ~).
      " This ensures we keep e.g. Authorization or API-Key headers
      
      client->request->get_header_fields( CHANGING fields = headers ).
      client->refresh_request( ).
      DELETE headers WHERE name CP '~*'.
      client->request->set_header_fields( headers ).
    ENDIF.

    IF sy-subrc = 0.
      IF response-code >= 300.                            "#EC CI_MAGIC
        client->get_last_error( IMPORTING message = response-message ).
        IF response-message IS INITIAL.
          response-message = response-response.
        ENDIF.
      ENDIF.
    ELSE.
      
      
      client->get_last_error( IMPORTING code    = error_code
                                        message = error_message ).
      IF sy-subrc = 1.
        
        temp3 = error_code.
        
        CREATE OBJECT temp6 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_communication_failure attr1 = temp3 attr2 = error_message.
        RAISE EXCEPTION temp6.
      ENDIF.
      IF sy-subrc = 4.
        
        temp4 = error_code.
        
        CREATE OBJECT temp7 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_processing_failed attr1 = temp4 attr2 = error_message.
        RAISE EXCEPTION temp7.
      ENDIF.
      
      temp5 = error_code.
      
      CREATE OBJECT temp8 TYPE zcx_llm_http_error EXPORTING textid = zcx_llm_http_error=>http_others attr1 = temp5 attr2 = error_message.
      RAISE EXCEPTION temp8.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_http_client_wrapper~set_parmeter.
    client->request->set_form_field( name  = name
                                     value = value ).
  ENDMETHOD.

  METHOD zif_llm_http_client_wrapper~get_req_headers.
    client->request->get_header_fields( CHANGING fields = result ).
  ENDMETHOD.

ENDCLASS.
