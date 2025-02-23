CLASS zcl_llm_chat_request DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_chat_request .

    ALIASES add_choice FOR zif_llm_chat_request~add_choice.
    ALIASES add_tool_result FOR zif_llm_chat_request~add_tool_result.
    ALIASES add_message FOR zif_llm_chat_request~add_message.
    ALIASES add_messages FOR zif_llm_chat_request~add_messages.
    ALIASES add_tool FOR zif_llm_chat_request~add_tool.
    ALIASES add_tools FOR zif_llm_chat_request~add_tools.
    ALIASES clear_messages FOR zif_llm_chat_request~clear_messages.
    ALIASES clear_tools FOR zif_llm_chat_request~clear_tools.
    ALIASES get_messages FOR  zif_llm_chat_request~get_messages.
    ALIASES get_tools FOR zif_llm_chat_request~get_tools.
    ALIASES set_structured_output FOR zif_llm_chat_request~set_structured_output.
    ALIASES set_structured_output_active FOR zif_llm_chat_request~set_structured_output_active.
    ALIASES set_tool_choice FOR zif_llm_chat_request~set_tool_choice.
    ALIASES get_internal_request FOR zif_llm_chat_request~get_internal_request.
    ALIASES options FOR zif_llm_chat_request~options.
    ALIASES add_tool_choices FOR zif_llm_chat_request~add_tool_choices.

    METHODS constructor IMPORTING request TYPE zllm_request.
  PROTECTED SECTION.
    DATA request TYPE zllm_request.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_chat_request IMPLEMENTATION.

  METHOD add_choice.
    APPEND choice-message TO request-messages.
  ENDMETHOD.

  METHOD add_tool_result.
    DATA(tool_result) = tool->get_result( ).
    APPEND VALUE #( role         = zif_llm_client_int=>role_tool
                    tool_call_id = tool_result-tool_call_id
                    content      = zcl_llm_common=>to_json( data = tool_result-data compress = abap_false )
                    name         = tool_result-name ) TO request-messages.
  ENDMETHOD.

  METHOD add_messages.
    APPEND LINES OF messages TO request-messages.
  ENDMETHOD.

  METHOD add_message.
    APPEND message TO request-messages.
  ENDMETHOD.

  METHOD add_tool.
    APPEND tool TO request-tools.
    request-tool_choice = tool_choice.
  ENDMETHOD.

  METHOD add_tools.
    APPEND LINES OF tools TO request-tools.
    request-tool_choice = tool_choice.
  ENDMETHOD.

  METHOD clear_messages.
    CLEAR request-messages.
  ENDMETHOD.

  METHOD clear_tools.
    CLEAR request-tools.
  ENDMETHOD.

  METHOD get_messages.
    result = request-messages.
  ENDMETHOD.

  METHOD get_tools.
    result = request-tools.
  ENDMETHOD.

  METHOD set_structured_output.
    request-structured_output->set_schema( data_desc = data_desc description = descriptions ).
    IF use_structured_output = abap_true.
      request-use_structured_output = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_structured_output_active.
    request-use_structured_output = active.
  ENDMETHOD.

  METHOD set_tool_choice.
    request-tool_choice = tool_choice.
  ENDMETHOD.

  METHOD get_internal_request.
    result = request.
  ENDMETHOD.

  METHOD options.
    result = request-options.
  ENDMETHOD.

  METHOD constructor.
    me->request = request.
  ENDMETHOD.

  METHOD add_tool_choices.
    DATA msg TYPE zllm_msg.
    msg-role = zif_llm_client_int=>role_assistant.
    APPEND LINES OF choices TO msg-tool_calls.
    APPEND msg TO request-messages.
  ENDMETHOD.

ENDCLASS.
