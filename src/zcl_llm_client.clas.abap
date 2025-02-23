"! <p class="shorttext synchronized" lang="en">LLM Client Master</p>
"! This class is currently a wrapper around the old internal interfaces.
"! Use the internal interfaces only if really required. Those might change
"! without notice.
CLASS zcl_llm_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE GLOBAL FRIENDS zcl_llm_factory.

  PUBLIC SECTION.
    INTERFACES zif_llm_client.

    "! <p class="shorttext synchronized" lang="en">Internal constructor</p>
    "! Get an instance via ZCL_LLM_FACTORY only!
    "! @parameter int_client | <p class="shorttext synchronized" lang="en">Internal Client reference</p>
    METHODS constructor IMPORTING int_client TYPE REF TO zif_llm_client_int.

  PRIVATE SECTION.
    DATA client TYPE REF TO zif_llm_client_int.
    DATA request TYPE REF TO zif_llm_chat_request.
    DATA last_response TYPE zllm_response.
ENDCLASS.

CLASS zcl_llm_client IMPLEMENTATION.
  METHOD zif_llm_client~add_choice.
    request->add_choice( choice ).
  ENDMETHOD.

  METHOD zif_llm_client~add_message.
    request->add_message( message ).
  ENDMETHOD.

  METHOD zif_llm_client~add_messages.
    request->add_messages( messages ).
  ENDMETHOD.

  METHOD zif_llm_client~add_tool.
    request->add_tool( tool = tool tool_choice = tool_choice ).
  ENDMETHOD.

  METHOD zif_llm_client~add_tools.
    request->add_tools( tools = tools tool_choice = tool_choice ).
  ENDMETHOD.

  METHOD zif_llm_client~add_tool_result.
    request->add_tool_result( tool ).
  ENDMETHOD.

  METHOD zif_llm_client~clear_messages.
    request->clear_messages( ).
  ENDMETHOD.

  METHOD zif_llm_client~clear_tools.
    request->clear_tools( ).
  ENDMETHOD.

  METHOD zif_llm_client~execute.
      DATA temp1 TYPE zllm_msg.
    DATA retries TYPE i.
    DATA remaining_retries LIKE retries.
    IF user_message IS NOT INITIAL.
      
      CLEAR temp1.
      temp1-role = zif_llm_client=>role_user.
      temp1-content = user_message.
      request->add_message( temp1 ).
    ENDIF.

    IF temperature IS SUPPLIED.
      request->options( )->set_temperature( temperature ).
    ENDIF.

    " Auto-retry in case of errors.
    
    retries = auto_retries + 1.
    
    remaining_retries = retries.

    DO retries TIMES.
      remaining_retries = remaining_retries - 1.
      response = client->chat( request ).
      IF response-success = abap_false AND remaining_retries > 0.
        " 429/529 are temporary rate limits/overload scenarios.
        " Add a short wait before retrying
        IF response-error-http_code = 429 OR response-error-http_code = 529.
          WAIT UP TO 10 SECONDS.
        ENDIF.
        CLEAR response.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF auto_add_response = abap_true AND response-success = abap_true.
      " We either add tool responses or message responses.
      " Currently a combination of models returning text content and function calls is
      " not supported, we ignore the text content in that case. Usually this is anyhow
      " just an argumentation or "think" part of the model leading to the function call.
      IF lines( response-choice-tool_calls ) > 0.
        request->add_tool_choices( response-choice-tool_calls ).
      ELSE.
        request->add_choice( response-choice ).
      ENDIF.
    ENDIF.

    " Save last response e.g. to execute tools more easily
    last_response = response.
  ENDMETHOD.

  METHOD zif_llm_client~execute_tool.
    FIELD-SYMBOLS <tool_call> TYPE zllm_tool_call.
      DATA temp2 TYPE REF TO zcx_llm_validation.
    DATA tools TYPE zllm_tools.
    DATA found LIKE abap_false.
    FIELD-SYMBOLS <tool> LIKE LINE OF tools.
      DATA temp3 TYPE REF TO zcx_llm_validation.
    READ TABLE last_response-choice-tool_calls WITH KEY id = tool_call_id ASSIGNING <tool_call>.
    IF sy-subrc <> 0.
      
      CREATE OBJECT temp2 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>tool_call_not_found attr1 = tool_call_id.
      RAISE EXCEPTION temp2.
    ENDIF.

    
    tools = request->get_tools( ).
    
    found = abap_false.
    
    LOOP AT tools ASSIGNING <tool>.
      IF <tool>->get_tool_details( )-name = <tool_call>-function-name.
        found = abap_true.
        <tool>->execute( data         = <tool_call>-function-arguments
                         tool_call_id = tool_call_id ).
        request->add_tool_result( <tool> ).
        EXIT.
      ENDIF.
    ENDLOOP.
    IF found = abap_false.
      
      CREATE OBJECT temp3 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>tool_not_found attr1 = <tool_call>-function-name.
      RAISE EXCEPTION temp3.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_client~execute_tools.
    DATA tools TYPE zllm_tools.
    FIELD-SYMBOLS <tool> LIKE LINE OF tools.
      DATA name TYPE zif_llm_tool=>tool_details-name.
      FIELD-SYMBOLS <tool_call> LIKE LINE OF last_response-choice-tool_calls.
    tools = request->get_tools( ).
    
    LOOP AT tools ASSIGNING <tool>.
      
      name = <tool>->get_tool_details( )-name.
      
      LOOP AT last_response-choice-tool_calls ASSIGNING <tool_call> WHERE function-name = name.
        <tool>->execute( data         = <tool_call>-function-arguments
                         tool_call_id = <tool_call>-id ).
        request->add_tool_result( <tool> ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_client~get_tools.
    result = request->get_tools( ).
  ENDMETHOD.

  METHOD zif_llm_client~options.
    result = request->options( ).
  ENDMETHOD.

  METHOD zif_llm_client~set_structured_output.
    request->set_structured_output( data_desc             = data_desc
                                    descriptions          = descriptions
                                    use_structured_output = use_structured_output ).
  ENDMETHOD.

  METHOD zif_llm_client~set_structured_output_active.
    request->set_structured_output_active( active ).
  ENDMETHOD.

  METHOD zif_llm_client~set_system_message.
    " Replace existing system message if it exists.
    " We only allow one system message and it should be the first.
    DATA old_messages TYPE zllm_msgs.
    DATA new_messages TYPE zllm_msgs.
    DATA temp4 TYPE zllm_msg.
    old_messages = request->get_messages( ).
    DELETE old_messages WHERE role = zif_llm_client=>role_system.

    " An empty system message leads to no system message
    IF strlen( message ) = 0.
      IF lines( old_messages ) > 0.
        request->add_messages( old_messages ).
      ENDIF.
      RETURN.
    ENDIF.

    
    
    CLEAR temp4.
    temp4-role = zif_llm_client=>role_system.
    temp4-content = message.
    APPEND temp4 TO new_messages.
    APPEND LINES OF old_messages TO new_messages.
    request->add_messages( new_messages ).
  ENDMETHOD.

  METHOD zif_llm_client~set_tool_choice.
    request->set_tool_choice( tool_choice ).
  ENDMETHOD.

  METHOD constructor.
    client = int_client.
    request = client->new_request( ).
  ENDMETHOD.

  METHOD zif_llm_client~get_messages.
    response = request->get_messages( ).
  ENDMETHOD.

ENDCLASS.
