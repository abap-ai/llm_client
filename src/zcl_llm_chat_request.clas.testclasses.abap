CLASS ltcl_tool_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_llm_tool.

    METHODS:
      set_result IMPORTING result TYPE zif_llm_tool=>tool_result,
      set_tool_details IMPORTING details TYPE zif_llm_tool=>tool_details.

  PRIVATE SECTION.
    DATA:
      result  TYPE zif_llm_tool=>tool_result,
      details TYPE zif_llm_tool=>tool_details.
ENDCLASS.

CLASS ltcl_chat_request DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      chat_request TYPE REF TO zcl_llm_chat_request,
      request      TYPE zllm_request,
      tool_double  TYPE REF TO ltcl_tool_double.

    METHODS:
      setup,
      create_test_tool_result RETURNING VALUE(result) TYPE zif_llm_tool=>tool_result,
      create_test_tool_details RETURNING VALUE(result) TYPE zif_llm_tool=>tool_details,
      test_add_message FOR TESTING,
      test_add_messages FOR TESTING,
      test_clear_messages FOR TESTING,
      test_add_tool FOR TESTING,
      test_add_tools FOR TESTING,
      test_add_tool_result FOR TESTING,
      test_clear_tools FOR TESTING,
      test_set_tool_choice FOR TESTING,
      test_set_structured_active FOR TESTING,
      test_add_tool_choices FOR TESTING,
      test_get_internal_request FOR TESTING.

ENDCLASS.

CLASS ltcl_chat_request IMPLEMENTATION.

  METHOD setup.
    chat_request = NEW #( request ).
    tool_double = NEW #( ).
  ENDMETHOD.

  METHOD create_test_tool_result.
    result = VALUE #(
      tool_call_id = 'test_call_1'
      name = 'test_tool'
      data = NEW string( 'test data' ) ).
  ENDMETHOD.

  METHOD create_test_tool_details.
    DATA(string_descr) = CAST cl_abap_datadescr(
      cl_abap_typedescr=>describe_by_data( VALUE string( ) ) ).

    result = VALUE zif_llm_tool=>tool_details(
      name = `test_tool`
      description = 'Test Tool Description'
      type = zif_llm_tool=>type_function
      parameters = VALUE #(
        data_desc = string_descr
        descriptions = VALUE #( ( fieldname = 'param1' description = 'Test parameter' ) )
      ) ).
  ENDMETHOD.

  METHOD test_add_message.
    DATA(message) = VALUE zllm_msg(
      role = zif_llm_client_int=>role_user
      content = 'Test message' ).

    chat_request->add_message( message ).

    DATA(messages) = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = message
      act = messages[ 1 ] ).
  ENDMETHOD.

  METHOD test_add_messages.
    DATA(messages) = VALUE zllm_msgs( (
      role = zif_llm_client_int=>role_user
      content = 'Message 1' ) (
      role = zif_llm_client_int=>role_assistant
      content = 'Message 2' ) ).

    chat_request->add_messages( messages ).

    DATA(result_messages) = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result_messages ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = messages
      act = result_messages ).
  ENDMETHOD.

  METHOD test_clear_messages.
    DATA(message) = VALUE zllm_msg(
      content = 'Test message' ).
    chat_request->add_message( message ).

    chat_request->clear_messages( ).

    DATA(messages) = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_initial( messages ).
  ENDMETHOD.

  METHOD test_add_tool.
    DATA(test_details) = create_test_tool_details( ).
    tool_double->set_tool_details( test_details ).

    chat_request->add_tool(
      tool = tool_double
      tool_choice = zif_llm_chat_request=>tool_choice_auto ).

    DATA(tools) = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( tools ) ).
  ENDMETHOD.

  METHOD test_add_tool_result.
    DATA(test_result) = create_test_tool_result( ).
    tool_double->set_result( test_result ).

    chat_request->add_tool_result( tool_double ).

    DATA(messages) = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_client_int=>role_tool
      act = messages[ 1 ]-role ).
    cl_abap_unit_assert=>assert_equals(
      exp = test_result-tool_call_id
      act = messages[ 1 ]-tool_call_id ).
  ENDMETHOD.

  METHOD test_add_tools.
    DATA tools TYPE zllm_tools.
    APPEND NEW ltcl_tool_double( ) TO tools.
    APPEND NEW ltcl_tool_double( ) TO tools.

    chat_request->add_tools(
      tools = tools
      tool_choice = zif_llm_chat_request=>tool_choice_auto ).

    DATA(result_tools) = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result_tools ) ).
  ENDMETHOD.

  METHOD test_clear_tools.
    DATA(tool) = NEW ltcl_tool_double( ).
    chat_request->add_tool( tool = tool ).

    chat_request->clear_tools( ).

    DATA(tools) = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_initial( tools ).
  ENDMETHOD.

  METHOD test_set_tool_choice.
    chat_request->set_tool_choice( zif_llm_chat_request=>tool_choice_auto ).

    DATA(internal_request) = chat_request->get_internal_request( ).
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_chat_request=>tool_choice_auto
      act = internal_request-tool_choice ).
  ENDMETHOD.

  METHOD test_set_structured_active.
    chat_request->set_structured_output_active( abap_true ).

    DATA(internal_request) = chat_request->get_internal_request( ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = internal_request-use_structured_output ).
  ENDMETHOD.

  METHOD test_add_tool_choices.
    DATA(choices) = VALUE zllm_tool_calls( (
      id = '1'
      type = 'function'
      function = VALUE #( name = 'test_function' ) ) ).

    chat_request->add_tool_choices( choices ).

    DATA(messages) = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_client_int=>role_assistant
      act = messages[ 1 ]-role ).
  ENDMETHOD.

  METHOD test_get_internal_request.
    DATA(internal_request) = chat_request->get_internal_request( ).
    cl_abap_unit_assert=>assert_equals(
      exp = request
      act = internal_request ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_tool_double IMPLEMENTATION.

  METHOD zif_llm_tool~get_result.
    result = me->result.
  ENDMETHOD.

  METHOD zif_llm_tool~get_tool_details.
    result = me->details.
  ENDMETHOD.



  METHOD set_result.
    me->result = result.
  ENDMETHOD.

  METHOD set_tool_details.
    me->details = details.
  ENDMETHOD.

  METHOD zif_llm_tool~execute.
    "do nothing
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

ENDCLASS.
