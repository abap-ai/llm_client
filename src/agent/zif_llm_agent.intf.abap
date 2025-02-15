"! <p class="shorttext synchronized" lang="en">Agent Interface</p>
INTERFACE zif_llm_agent
  PUBLIC.
  TYPES:
    "! Agent status
    BEGIN OF status,
      is_running TYPE abap_bool,
      is_done    TYPE abap_bool,
      has_error  TYPE abap_bool,
      message    TYPE string,
    END OF status.
  TYPES:
    "! Memory entry structure
    BEGIN OF memory_entry,
      timestamp TYPE timestamp,
      msg       TYPE zllm_msg,
    END OF memory_entry,
    "! Memory entries collection
    memory_entries TYPE STANDARD TABLE OF memory_entry WITH DEFAULT KEY.

  CONSTANTS:
    BEGIN OF memory_types,
      message     TYPE string VALUE 'MESSAGE',
      tool_call   TYPE string VALUE 'TOOL_CALL',
      tool_result TYPE string VALUE 'TOOL_RESULT',
    END OF memory_types.

  "! <p class="shorttext synchronized">Executes the agent's main task</p>
  "! @parameter prompt              | <p class="shorttext synchronized">Initial prompt</p>
  "! @parameter result              | <p class="shorttext synchronized">Execution result</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Agent execution error</p>
  METHODS execute
    IMPORTING prompt        TYPE string OPTIONAL
    RETURNING VALUE(result) TYPE zllm_response
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Gets current agent status</p>
  "! @parameter result | <p class="shorttext synchronized">Current status</p>
  METHODS get_status
    RETURNING VALUE(result) TYPE status.

  "! <p class="shorttext synchronized">Sets the agent's context</p>
  "! @parameter messages | <p class="shorttext synchronized">Context messages</p>
  METHODS set_context
    IMPORTING !messages TYPE zllm_msgs.

  "! <p class="shorttext synchronized">Gets the agent's context</p>
  "! @parameter result | <p class="shorttext synchronized">Current context</p>
  METHODS get_context
    RETURNING VALUE(result) TYPE zllm_msgs
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Gets agent memory</p>
  "! @parameter result | <p class="shorttext synchronized">Memory entries</p>
  METHODS get_memory
    RETURNING VALUE(result) TYPE memory_entries.

  "! <p class="shorttext synchronized">Add a tool to the agent</p>
  "! @parameter tool                | <p class="shorttext synchronized">Tool to add</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Tool error</p>
  METHODS add_tool
    IMPORTING tool TYPE REF TO zif_llm_tool
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Add multiple tools</p>
  "! @parameter tools               | <p class="shorttext synchronized">Tools to add</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Tool error</p>
  METHODS add_tools
    IMPORTING tools TYPE zllm_tools
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Get configured tools</p>
  "! @parameter result | <p class="shorttext synchronized">Available tools</p>
  METHODS get_tools
    RETURNING VALUE(result) TYPE zllm_tools.

  "! <p class="shorttext synchronized">Get options reference</p>
  "!
  "! @parameter result | <p class="shorttext synchronized">Option reference</p>
  METHODS get_options
    RETURNING VALUE(result) TYPE REF TO zif_llm_options.

ENDINTERFACE.
