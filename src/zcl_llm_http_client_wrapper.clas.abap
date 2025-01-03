"! <p class="shorttext synchronized" lang="en">HTTP Client Wrapper</p>
CLASS zcl_llm_http_client_wrapper DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_llm_http_client_wrapper.
    ALIASES:
      get_client       FOR zif_llm_http_client_wrapper~get_client,
      set_header       FOR zif_llm_http_client_wrapper~set_header,
      set_url          FOR zif_llm_http_client_wrapper~set_url,
      communicate      FOR zif_llm_http_client_wrapper~communicate,
      close_client     FOR zif_llm_http_client_wrapper~close_client.

    METHODS:
      constructor
        IMPORTING
                  config TYPE zllm_clnt_config
        RAISING   zcx_llm_validation.

  PROTECTED SECTION.
    DATA:
      config TYPE zllm_clnt_config,
      url    TYPE string,
      client TYPE REF TO if_http_client.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_http_client_wrapper IMPLEMENTATION.

  METHOD constructor.
    me->config = config.
    cl_http_client=>create_by_destination(
     EXPORTING
       destination = config-rfc_destination
     IMPORTING
       client = client
     EXCEPTIONS
       argument_not_found = 1
       destination_not_found = 2
       destination_no_authority = 3
       plugin_not_active = 4
       internal_error = 5
       OTHERS = 6 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>http_destination_error
          attr1  = CONV #( config-rfc_destination )
          attr2  = SWITCH string( sy-subrc
                                  WHEN 1 THEN `Argument Not Found`
                                  WHEN 2 THEN `Destination Not Found`
                                  WHEN 3 THEN `Destination No Authority`
                                  WHEN 4 THEN `Plugin Not Active`
                                  WHEN 5 THEN `Internal Error`
                                  ELSE `Others` ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_client.
    client = NEW zcl_llm_http_client_wrapper( config = config ).
  ENDMETHOD.

  METHOD set_header.
    client->request->set_header_field(
      name  = name
      value = value ).
  ENDMETHOD.

  METHOD set_url.
    me->url = url.
  ENDMETHOD.

  METHOD close_client.
    "Exceptions are intentionally ignored, at that point in time we just try to remove the client.
    client->close( EXCEPTIONS http_invalid_state = 0 OTHERS = 0 ).
  ENDMETHOD.

  METHOD communicate.
    client->request->set_method( if_http_request=>co_request_method_post ).
    client->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ) ##NO_TEXT.

    IF url IS NOT INITIAL.
      cl_http_utility=>set_request_uri(
        request = client->request
        uri     = me->url ).
    ENDIF.

    client->request->set_cdata( request ).

    client->send(
        EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state = 2
        http_invalid_timeout = 3
        http_processing_failed = 4
        OTHERS = 5 ).
    IF sy-subrc = 0.
      client->receive(
        EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state = 2
        http_processing_failed = 4
        OTHERS = 5 ).
    ENDIF.

    response-http_response = client->response.
    client->response->get_status( IMPORTING code = response-code ).

    "Need to reset the request as otherwise the next call in this session will overwrite the
    "path prefix defined in SM59.
    IF url IS NOT INITIAL.
      "Need to save all non-SAP internal headers (SAP internal ones start with ~).
      "This ensures we keep e.g. Authorization or API-Key headers
      DATA headers TYPE tihttpnvp.
      client->request->get_header_fields( CHANGING fields = headers ).
      client->refresh_request( ).
      DELETE headers WHERE name CP '~*'.
      client->request->set_header_fields( fields = headers ).
    ENDIF.


    IF sy-subrc = 0.
      response-response = client->response->get_cdata( ).
      IF response-code >= 300.
        client->get_last_error( IMPORTING message = response-message ).
        IF response-message IS INITIAL.
          response-message = response-response.
        ENDIF.
      ENDIF.
    ELSE.
      client->get_last_error( IMPORTING code = DATA(error_code) message = DATA(error_message) ).
      IF sy-subrc = 1.
        RAISE EXCEPTION TYPE zcx_llm_http_error
          EXPORTING
            textid = zcx_llm_http_error=>http_communication_failure
            attr1  = CONV #( error_code )
            attr2  = error_message.
      ENDIF.
      IF sy-subrc = 4.
        RAISE EXCEPTION TYPE zcx_llm_http_error
          EXPORTING
            textid = zcx_llm_http_error=>http_processing_failed
            attr1  = CONV #( error_code )
            attr2  = error_message.
      ENDIF.
      RAISE EXCEPTION TYPE zcx_llm_http_error
        EXPORTING
          textid = zcx_llm_http_error=>http_others
          attr1  = CONV #( error_code )
          attr2  = error_message.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

