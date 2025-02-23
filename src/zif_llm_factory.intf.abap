"! <p class="shorttext synchronized" lang="en">LLM Factory</p>
INTERFACE zif_llm_factory
  PUBLIC.

  "! <p class="shorttext synchronized">Get an LLM Client</p>
  "! Returns an instance of the llm client for the given model.
  "! @parameter model                 | <p class="shorttext synchronized">Model Name</p>
  "! @parameter response              | <p class="shorttext synchronized">Client</p>
  "! @raising   zcx_llm_validation    | <p class="shorttext synchronized">Validation Error - dynamic check</p>
  "! @raising   zcx_llm_authorization | <p class="shorttext synchronized">Authorization error</p>
  CLASS-METHODS get_client
    IMPORTING model           TYPE zllm_model
    RETURNING VALUE(response) TYPE REF TO zif_llm_client
    RAISING   zcx_llm_validation zcx_llm_authorization.


  "! <p class="shorttext synchronized">Deprecated - Get an internal client</p>
  "! This is only used internally, avoid using it in future directly.
  "! Might be changed or even removed in future without notice.
  "! @parameter model                 | <p class="shorttext synchronized">Model Name</p>
  "! @parameter response              | <p class="shorttext synchronized">Client</p>
  "! @raising   zcx_llm_validation    | <p class="shorttext synchronized">Validation Error - dynamic check</p>
  "! @raising   zcx_llm_authorization | <p class="shorttext synchronized">Authorization error</p>
  CLASS-METHODS get_client_int
    IMPORTING model           TYPE zllm_model
    RETURNING VALUE(response) TYPE REF TO zif_llm_client_int
    RAISING   zcx_llm_validation zcx_llm_authorization.
ENDINTERFACE.
