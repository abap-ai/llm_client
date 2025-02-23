"! <p class="shorttext synchronized" lang="en">LLM Client Interface</p>
INTERFACE zif_llm_client
  PUBLIC.
  CONSTANTS role_user            TYPE zllm_role VALUE `user`.
  CONSTANTS role_system          TYPE zllm_role VALUE `system`.
  CONSTANTS role_assistant       TYPE zllm_role VALUE 'assistant'.
  CONSTANTS role_tool            TYPE zllm_role VALUE `tool`.

  CONSTANTS tool_choice_none     TYPE string    VALUE `none`.
  CONSTANTS tool_choice_auto     TYPE string    VALUE `auto`.
  CONSTANTS tool_choice_required TYPE string    VALUE `required`.

  "! <p class="shorttext synchronized">Call the chat endpoint</p>
  "! With default settings auto-retries failed calls up to 3 times and adds
  "! the llm response to the chat history.
  "! @parameter user_message       | <p class="shorttext synchronized">User Message - Optional</p>
  "! @parameter temperature        | <p class="shorttext synchronized">Temperature</p>
  "! @parameter auto_add_response  | <p class="shorttext synchronized">Add llm response to chat history</p>
  "! @parameter auto_retries       | <p class="shorttext synchronized">Max. # Retries, set 0 to disable</p>
  "! @parameter response           | <p class="shorttext synchronized">LLM respopnse</p>
  "! @raising   zcx_llm_validation | <p class="shorttext synchronized">Validation error</p>
  METHODS execute IMPORTING user_message      TYPE string     OPTIONAL
                            temperature       TYPE decfloat16 OPTIONAL
                            auto_add_response TYPE sap_bool   DEFAULT abap_true
                            auto_retries      TYPE i          DEFAULT 3
                  RETURNING VALUE(response)   TYPE zllm_response
                  RAISING   zcx_llm_validation.

  "! <p class="shorttext synchronized">Set system message</p>
  "! Existing one will be overwritten.
  "! @parameter message           | <p class="shorttext synchronized">System Message</p>
  METHODS set_system_message IMPORTING !message TYPE string.

  "! <p class="shorttext synchronized">Add a message to the message list</p>
  "!
  "! @parameter message | <p class="shorttext synchronized">Message</p>
  METHODS add_message IMPORTING !message TYPE zllm_msg.
  "! <p class="shorttext synchronized">Add multiple messages to the message list</p>
  "!
  "! @parameter messages | <p class="shorttext synchronized">Messages</p>
  METHODS add_messages IMPORTING !messages TYPE zllm_msgs.
  "! <p class="shorttext synchronized">Remove all messages</p>
  "!
  METHODS clear_messages.

  "! <p class="shorttext synchronized">Get current messages</p>
  "!
  "! @parameter response | <p class="shorttext synchronized">Messages</p>
  METHODS get_messages RETURNING VALUE(response) TYPE zllm_msgs.

  "! <p class="shorttext synchronized">Add a tool</p>
  "! Tools allow the llm to request additional details.
  "! Default mode auto is usually interpreted as tool calls are optional.
  "! Tool support and call quality depends on provider and model.
  "! @parameter tool        | <p class="shorttext synchronized">Tool</p>
  "! @parameter tool_choice | <p class="shorttext synchronized">Tool Choice configuration</p>
  METHODS add_tool IMPORTING tool        TYPE REF TO zif_llm_tool
                             tool_choice TYPE string DEFAULT `auto`.

  "! <p class="shorttext synchronized">Add multiple tools</p>
  "! Tools allow the llm to request additional details.
  "! Default mode auto is usually interpreted as tool calls are optional.
  "! Tool support and call quality depends on provider and model.
  "! @parameter tools       | <p class="shorttext synchronized">Tools</p>
  "! @parameter tool_choice | <p class="shorttext synchronized">Tool Choice configuration</p>
  METHODS add_tools IMPORTING tools       TYPE zllm_tools
                              tool_choice TYPE string DEFAULT `auto`.

  "! <p class="shorttext synchronized">Return all defined tools</p>
  "!
  "! @parameter result | <p class="shorttext synchronized">Tools</p>
  METHODS get_tools RETURNING VALUE(result) TYPE zllm_tools.

  "! <p class="shorttext synchronized">Set tool choice</p>
  "! Use constants of the interface.
  "! @parameter tool_choice | <p class="shorttext synchronized">Tool Choice</p>
  METHODS set_tool_choice IMPORTING tool_choice TYPE string.

  "! <p class="shorttext synchronized">Add a tool result</p>
  "! Special case to map the tool result, usually consider execute_tool(s).
  "! @parameter tool | <p class="shorttext synchronized"></p>
  METHODS add_tool_result
    IMPORTING tool TYPE REF TO zif_llm_tool.

  "! <p class="shorttext synchronized">Execute a specific tool</p>
  "! Executes a specific tool call and adds the result to the message history
  "! @parameter tool_Call_id       | <p class="shorttext synchronized">Tool Call ID</p>
  "! @raising   zcx_llm_validation | <p class="shorttext synchronized">Validation error</p>
  METHODS execute_tool IMPORTING tool_call_id TYPE string
                       RAISING   zcx_llm_validation.

  "! <p class="shorttext synchronized">Execute all requested tools</p>
  "!
  METHODS execute_tools.

  "! <p class="shorttext synchronized">Delete all tools</p>
  "! Note that some providers require tool definitions to properly process
  "! history tool calls during a conversation. Use with caution.
  METHODS clear_tools.

  "! <p class="shorttext synchronized">Set structured output details</p>
  "! Only few providers support structured output while writing this.
  "! Some providers support only json mode, in those cases the json definition
  "! and the request to output the result as json is added to the last user message.
  "! @parameter data_desc             | <p class="shorttext synchronized">Data reference</p>
  "! @parameter descriptions          | <p class="shorttext synchronized">Descriptions for the LLM</p>
  "! @parameter use_structured_output | <p class="shorttext synchronized">Enable structured output (default)</p>
  METHODS set_structured_output
    IMPORTING data_desc             TYPE REF TO cl_abap_datadescr
              descriptions          TYPE zif_llm_so=>def_descriptions
              use_structured_output TYPE sap_bool DEFAULT abap_true.

  "! <p class="shorttext synchronized">Enable/Disable structured output</p>
  "!
  "! @parameter active | <p class="shorttext synchronized">Active?</p>
  METHODS set_structured_output_active IMPORTING active TYPE sap_bool.

  "! <p class="shorttext synchronized">Add llm result to the message history</p>
  "! Use this only if you disabled auto-adding responses and need to control it manually.
  "! @parameter choice | <p class="shorttext synchronized">LLM Choice (Response)</p>
  METHODS add_choice
    IMPORTING choice TYPE zllm_choice.

  "! <p class="shorttext synchronized">Define Options</p>
  "! Get options implementation, use it to set options like
  "! temperature, top_k, etc. Support depends on provider and model.
  "! Unsupported options will be ignored except set manually.
  "! @parameter result | <p class="shorttext synchronized">Options Reference</p>
  METHODS options RETURNING VALUE(result) TYPE REF TO zif_llm_options.
ENDINTERFACE.
