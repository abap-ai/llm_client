CLASS zcl_llm_agent_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_llm_agent.
    INTERFACES zif_llm_agent_internal.

    METHODS constructor
      IMPORTING !client TYPE REF TO zif_llm_client
                tools   TYPE zllm_tools
      RAISING   zcx_llm_agent_error.

    ALIASES execute     FOR zif_llm_agent~execute.
    ALIASES get_context FOR zif_llm_agent~get_context.
    ALIASES get_memory  FOR zif_llm_agent~get_memory.
    ALIASES get_status  FOR zif_llm_agent~get_status.
    ALIASES set_context FOR zif_llm_agent~set_context.
    ALIASES add_tool    FOR zif_llm_agent~add_tool.
    ALIASES add_tools   FOR zif_llm_agent~add_tools.
    ALIASES get_tools   FOR zif_llm_agent~get_tools.
    ALIASES get_options FOR zif_llm_agent~get_options.

  PROTECTED SECTION.
    DATA client         TYPE REF TO zif_llm_client.
    DATA chat_request   TYPE REF TO zif_llm_chat_request.
    DATA memory         TYPE zif_llm_agent=>memory_entries.
    DATA status         TYPE zif_llm_agent=>status.
    DATA max_iterations TYPE i VALUE 5.
    DATA tools          TYPE zllm_tools.

    METHODS initialize ABSTRACT
      RAISING zcx_llm_agent_error.

    METHODS create_chat_request
      RETURNING VALUE(result) TYPE REF TO zif_llm_chat_request.

    METHODS prepare_messages
      RETURNING VALUE(result) TYPE zllm_msgs
      RAISING   zcx_llm_agent_error.

    METHODS process_structured_output
      IMPORTING !response     TYPE zllm_response
      RETURNING VALUE(result) TYPE zllm_response
      RAISING   zcx_llm_agent_error ##CALLED.

    METHODS process_tool_calls
      IMPORTING !response     TYPE zllm_response
      RETURNING VALUE(result) TYPE zllm_response
      RAISING   zcx_llm_agent_error.

    METHODS add_to_memory_internal
      IMPORTING !memory TYPE zif_llm_agent=>memory_entry.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_agent_base IMPLEMENTATION.
  METHOD constructor.
    me->client = client.
    me->tools  = tools.
    CLEAR status.
    status-is_running = abap_false.
    status-is_done = abap_false.
    status-has_error = abap_false.
  ENDMETHOD.

  METHOD zif_llm_agent~execute.
          DATA temp1 TYPE zif_llm_agent=>memory_entry.
        DATA messages TYPE zllm_msgs.
          DATA temp2 TYPE scx_t100key.
          DATA temp4 TYPE REF TO zcx_llm_agent_error.
    DATA iteration TYPE i.
        DATA temp3 TYPE scx_t100key.
        DATA temp5 TYPE REF TO zcx_llm_agent_error.
    status-is_running = abap_true.
    status-is_done    = abap_false.

    TRY.
        chat_request = create_chat_request( ).

        IF prompt IS NOT INITIAL.
          
          CLEAR temp1.
          temp1-msg-role = client->role_user.
          temp1-msg-content = prompt.
          add_to_memory_internal( temp1 ).
        ENDIF.

        " Prepare messages from memory
        
        messages = prepare_messages( ).
        chat_request->add_messages( messages ).
        chat_request->options( )->set_temperature( '0.1' ).

        " Execute chat request
        result = client->chat( chat_request ).

        IF result-success = abap_false.
          
          CLEAR temp2.
          temp2-msgid = 'ZLLM_AGENT'.
          temp2-msgno = '001'.
          temp2-attr1 = result-error-error_text.
          
          CREATE OBJECT temp4 TYPE zcx_llm_agent_error EXPORTING textid = temp2.
          RAISE EXCEPTION temp4.
        ENDIF.

        " Process response
        result = zif_llm_agent_internal~process_response( result ).

        IF lines( result-choice-tool_calls ) = 0.
          status-is_done = abap_true.
        ENDIF.

      CLEANUP.
        status-is_running = abap_false.
    ENDTRY.

    
    iteration = 1.
    WHILE status-is_done = abap_false.
      " Next execution
      result = client->chat( chat_request ).
      IF result-success = abap_false.
        
        CLEAR temp3.
        temp3-msgid = 'ZLLM_AGENT'.
        temp3-msgno = '001'.
        temp3-attr1 = result-error-error_text.
        
        CREATE OBJECT temp5 TYPE zcx_llm_agent_error EXPORTING textid = temp3.
        RAISE EXCEPTION temp5.
      ENDIF.

      " Process response
      result = zif_llm_agent_internal~process_response( result ).

      IF lines( result-choice-tool_calls ) = 0 OR iteration >= max_iterations.
        status-is_done = abap_true.
        IF iteration >= max_iterations.
          result-success = abap_false.
          result-error-error_text = `Max iterations reached` ##NO_TEXT.
          result-choice-message-content = result-error-error_text.
        ENDIF.
      ELSE.
        iteration = iteration + 1.
      ENDIF.

    ENDWHILE.
  ENDMETHOD.

  METHOD prepare_messages.
    FIELD-SYMBOLS <entry> LIKE LINE OF memory.
      DATA temp4 TYPE zllm_msg.
    LOOP AT memory ASSIGNING <entry>.
      
      CLEAR temp4.
      temp4-role = <entry>-msg-role.
      temp4-content = <entry>-msg-content.
      temp4-name = <entry>-msg-name.
      temp4-tool_call_id = <entry>-msg-tool_call_id.
      temp4-tool_calls = <entry>-msg-tool_calls.
      APPEND temp4 TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_agent~get_context.
    result = prepare_messages( ).
  ENDMETHOD.

  METHOD zif_llm_agent~get_memory.
    result = memory.
  ENDMETHOD.

  METHOD zif_llm_agent~get_status.
    result = status.
  ENDMETHOD.

  METHOD zif_llm_agent~set_context.
    FIELD-SYMBOLS <message> LIKE LINE OF messages.
      DATA temp5 TYPE zif_llm_agent=>memory_entry.
    CLEAR memory.
    
    LOOP AT messages ASSIGNING <message>.
      
      CLEAR temp5.
      temp5-msg = <message>.
      add_to_memory_internal( temp5 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_agent_internal~add_to_memory.
    INSERT entry INTO TABLE memory.
  ENDMETHOD.

  METHOD add_to_memory_internal.
    zif_llm_agent_internal~add_to_memory( memory ).
  ENDMETHOD.

  METHOD zif_llm_agent_internal~can_proceed.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( lines( memory ) < max_iterations AND status-has_error = abap_false ).
    result = temp1.
  ENDMETHOD.

  METHOD zif_llm_agent_internal~prepare_next_iteration.
    " To be implemented by concrete classes if needed
  ENDMETHOD.

  METHOD process_structured_output.
    result = response.
  ENDMETHOD.

  METHOD zif_llm_agent~add_tool.
    INSERT tool INTO TABLE tools.
  ENDMETHOD.

  METHOD zif_llm_agent~add_tools.
    APPEND LINES OF tools TO me->tools.
  ENDMETHOD.

  METHOD zif_llm_agent~get_tools.
    result = tools.
  ENDMETHOD.

  METHOD create_chat_request.
    result = client->new_request( ).

    " Add configured tools to request
    IF tools IS NOT INITIAL.
      result->add_tools( tools       = tools
                         tool_choice = zif_llm_chat_request=>tool_choice_auto ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_agent_internal~process_response.
    DATA temp6 TYPE zif_llm_agent=>memory_entry.
    result = response.

    " Process any tool calls in the response
    IF response-choice-tool_calls IS NOT INITIAL.
      result = process_tool_calls( response ).
    ENDIF.

    " Add assistant's response to memory
    
    CLEAR temp6.
    temp6-msg-role = client->role_assistant.
    temp6-msg-content = response-choice-message-content.
    temp6-msg-tool_calls = response-choice-message-tool_calls.
    temp6-msg-tool_call_id = response-choice-message-tool_call_id.
    add_to_memory_internal( temp6 ).
  ENDMETHOD.

  METHOD process_tool_calls.
    FIELD-SYMBOLS <tool_call> LIKE LINE OF response-choice-tool_calls.
          DATA matching_tool TYPE REF TO zif_llm_tool.
          FIELD-SYMBOLS <configured_tool> LIKE LINE OF tools.
            DATA temp7 TYPE scx_t100key.
            DATA temp6 TYPE REF TO zcx_llm_agent_error.
          FIELD-SYMBOLS <tool_args> TYPE any.
            DATA temp8 TYPE REF TO data.
          DATA temp9 TYPE zif_llm_agent=>memory_entry.
          DATA error TYPE REF TO zcx_llm_agent_error.
          DATA temp10 TYPE zif_llm_agent=>memory_entry.
    result = response.

    
    LOOP AT response-choice-tool_calls ASSIGNING <tool_call>.
      " Add tool calls to request for context
      chat_request->add_tool_choices( response-choice-tool_calls ).
      TRY.
          " Find matching tool
          
          
          LOOP AT tools ASSIGNING <configured_tool>.
            IF <configured_tool>->get_tool_details( )-name = <tool_call>-function-name.
              matching_tool = <configured_tool>.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF matching_tool IS INITIAL.
            
            CLEAR temp7.
            temp7-msgid = 'ZLLM_AGENT'.
            temp7-msgno = '002'.
            temp7-attr1 = <tool_call>-function-name.
            
            CREATE OBJECT temp6 TYPE zcx_llm_agent_error EXPORTING textid = temp7.
            RAISE EXCEPTION temp6.
          ENDIF.

          " Execute the tool with the provided arguments
          
          ASSIGN <tool_call>-function-arguments->* TO <tool_args>.
          IF sy-subrc = 0.
            
GET REFERENCE OF <tool_args> INTO temp8.
matching_tool->execute( data         = temp8
                                    tool_call_id = <tool_call>-id ).
          ENDIF.

          
          CLEAR temp9.
          temp9-msg-role = zif_llm_client=>role_tool.
          temp9-msg-tool_call_id = <tool_call>-id.
          temp9-msg-content = zcl_llm_common=>to_json( data = matching_tool->get_result( ) ).
          add_to_memory_internal( temp9 ).

          " Add tool result to chat request
          " TOOD - do we need this? Probably we should change the whole logic to avoid the memory and instead use chats
          chat_request->add_tool_result( matching_tool ).

          
        CATCH zcx_llm_agent_error INTO error.
          " Log error but continue with other tools
          
          CLEAR temp10.
          temp10-msg-role = client->role_tool.
          temp10-msg-content = |Error executing { <tool_call>-function-name }: { error->get_text( ) }|.
          temp10-msg-name = <tool_call>-function-name.
          add_to_memory_internal(
              temp10 ) ##NO_TEXT.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_agent_internal~execute_tool.
    " Find matching tool
    DATA matching_tool TYPE REF TO zif_llm_tool.

    FIELD-SYMBOLS <configured_tool> LIKE LINE OF tools.
      DATA temp11 TYPE scx_t100key.
      DATA temp7 TYPE REF TO zcx_llm_agent_error.
        DATA ref_data TYPE REF TO data.
        DATA error TYPE REF TO cx_root.
        DATA temp12 TYPE scx_t100key.
        DATA temp8 TYPE REF TO zcx_llm_agent_error.
    LOOP AT tools ASSIGNING <configured_tool>.
      IF <configured_tool>->get_tool_details( )-name = tool_call-name.
        matching_tool = <configured_tool>.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF matching_tool IS INITIAL.
      
      CLEAR temp11.
      temp11-msgid = 'ZLLM_AGENT'.
      temp11-msgno = '002'.
      temp11-attr1 = tool_call-name.
      
      CREATE OBJECT temp7 TYPE zcx_llm_agent_error EXPORTING textid = temp11.
      RAISE EXCEPTION temp7.
    ENDIF.

    " Execute the tool
    TRY.
        " Create the data reference for the tool call
        
        CREATE DATA ref_data TYPE HANDLE tool_call-parameters-data_desc.

        " Execute tool with the prepared data reference
        CLEAR result.
        result-name = tool_call-name.
        result-data = matching_tool->execute( data = ref_data
tool_call_id = tool_call-name )-data.
        
      CATCH cx_root INTO error.
        
        CLEAR temp12.
        temp12-msgid = 'ZLLM_AGENT'.
        temp12-msgno = '003'.
        temp12-attr1 = error->get_text( ).
        
        CREATE OBJECT temp8 TYPE zcx_llm_agent_error EXPORTING textid = temp12 previous = error.
        RAISE EXCEPTION temp8.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_llm_agent~get_options.
    result = chat_request->options( ).
  ENDMETHOD.

ENDCLASS.
