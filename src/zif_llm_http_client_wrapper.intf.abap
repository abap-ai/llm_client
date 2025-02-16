"! <p class="shorttext synchronized" lang="en">HTTP client wrapper</p>
INTERFACE zif_llm_http_client_wrapper
PUBLIC.
  TYPES: BEGIN OF response,
           code          TYPE sysubrc,
           message       TYPE string,
           response      TYPE string,
           http_response TYPE REF TO if_http_response,
         END OF response.


  "! <p class="shorttext synchronized"></p>
  "!
  "! @parameter client_config      | <p class="shorttext synchronized"></p>
  "! @parameter provider_config    | <p class="shorttext synchronized"></p>
  "! @parameter client             | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_validation | <p class="shorttext synchronized"></p>
  CLASS-METHODS get_client
    IMPORTING client_config   TYPE zllm_clnt_config
              provider_config TYPE zllm_providers
    RETURNING VALUE(client)   TYPE REF TO zif_llm_http_client_wrapper
    RAISING   zcx_llm_validation.

  "! <p class="shorttext synchronized"></p>
  "! Set a header value. Setting the same multiple time overwrites the header.
  "! @parameter name               | <p class="shorttext synchronized"></p>
  "! @parameter value              | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_validation | <p class="shorttext synchronized"></p>
  METHODS set_header
    IMPORTING !name  TYPE string
              !value TYPE string
    RAISING   zcx_llm_validation.

  "! <p class="shorttext synchronized"></p>
  "! Set the url to be called. The SM59 destination path will be prepended.
  "! @parameter url                | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_validation | <p class="shorttext synchronized"></p>
  METHODS set_url
    IMPORTING url TYPE string
    RAISING   zcx_llm_validation.

  "! <p class="shorttext synchronized"></p>
  "! Call the endpoint.
  "! @parameter request            | <p class="shorttext synchronized"></p>
  "! @parameter session_id         | <p class="shorttext synchronized"></p>
  "! @parameter msg                | <p class="shorttext synchronized"></p>
  "! @parameter response           | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_http_error | <p class="shorttext synchronized"></p>
  METHODS communicate
    IMPORTING !request        TYPE string
              session_id      TYPE zllm_session_id
              msg             TYPE i
    RETURNING VALUE(response) TYPE response
    RAISING   zcx_llm_http_error.

  "! <p class="shorttext synchronized">Close the client</p>
  "!
  METHODS close_client.

  "! <p class="shorttext synchronized">Set a parameter</p>
  "!
  "! @parameter name  | <p class="shorttext synchronized">Parameter Name</p>
  "! @parameter value | <p class="shorttext synchronized">Parameter Value</p>
  METHODS set_parmeter IMPORTING !name  TYPE string
                                 !value TYPE string.

  "! <p class="shorttext synchronized">Get all current request headers</p>
  "!
  "! @parameter result | <p class="shorttext synchronized">Request Headers</p>
  METHODS get_req_headers RETURNING VALUE(result) TYPE tihttpnvp.
ENDINTERFACE.
