"! <p class="shorttext synchronized" lang="en">Internal Agent Interface</p>
INTERFACE zif_llm_agent_internal
  PUBLIC.

  "! <p class="shorttext synchronized">Processes LLM response</p>
  "! @parameter response            | <p class="shorttext synchronized">LLM response</p>
  "! @parameter result              | <p class="shorttext synchronized">Processing result</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Processing error</p>
  METHODS process_response
    IMPORTING !response     TYPE zllm_response
    RETURNING VALUE(result) TYPE zllm_response
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Executes tool call</p>
  "! @parameter tool_call           | <p class="shorttext synchronized">Tool call details</p>
  "! @parameter result              | <p class="shorttext synchronized">Tool execution result</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Tool execution error</p>
  METHODS execute_tool
    IMPORTING tool_call     TYPE zif_llm_tool=>tool_details
    RETURNING VALUE(result) TYPE zif_llm_tool=>tool_result
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Adds entry to memory</p>
  "! @parameter entry | <p class="shorttext synchronized">Memory entry</p>
  METHODS add_to_memory
    IMPORTING !entry TYPE zif_llm_agent=>memory_entry.

  "! <p class="shorttext synchronized">Checks if agent can proceed</p>
  "! @parameter result | <p class="shorttext synchronized">True if can proceed</p>
  METHODS can_proceed
    RETURNING VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized">Prepares next iteration</p>
  "! @raising zcx_llm_agent_error | <p class="shorttext synchronized">Preparation error</p>
  METHODS prepare_next_iteration
    RAISING zcx_llm_agent_error.

ENDINTERFACE.
