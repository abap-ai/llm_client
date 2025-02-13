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
    CREATE OBJECT chat_request EXPORTING REQUEST = request.
    CREATE OBJECT tool_double.
  ENDMETHOD.

  METHOD create_test_tool_result.
    CLEAR result.
    result-tool_call_id = 'test_call_1'.
    result-name = 'test_tool'.
    CREATE OBJECT result-data TYPE string ClassDefinitionNotFound ERROR.
  ENDMETHOD.

  METHOD create_test_tool_details.
    DATA temp2 TYPE string.
    CLEAR temp2.
    DATA temp1 TYPE REF TO cl_abap_datadescr.
    temp1 ?= cl_abap_typedescr=>describe_by_data( temp2 ).
    DATA(string_descr) = temp1.

    DATA temp3 TYPE zif_llm_tool=>tool_details.
    CLEAR temp3.
    temp3-name = `test_tool`.
    temp3-description = 'Test Tool Description'.
    temp3-type = zif_llm_tool=>type_function.
    CLEAR temp3-parameters.
    temp3-parameters-data_desc = string_descr.
    temp3-parameters-descriptions = VALUE #( ( fieldname = 'param1' description = 'Test parameter' ) ).
    result = temp3.
  ENDMETHOD.

  METHOD test_add_message.
    DATA temp4 TYPE zllm_msg.
    CLEAR temp4.
    temp4-role = zif_llm_client=>role_user.
    temp4-content = 'Test message'.
    DATA message LIKE temp4.
    message = temp4.

    chat_request->add_message( message ).

    DATA messages TYPE zllm_msgs.
    messages = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    DATA temp5 LIKE LINE OF messages.
    DATA temp6 LIKE sy-tabix.
    temp6 = sy-tabix.
    READ TABLE messages INDEX 1 INTO temp5.
    sy-tabix = temp6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      exp = message
      act = temp5 ).
  ENDMETHOD.

  METHOD test_add_messages.
    DATA temp7 TYPE zllm_msgs.
    CLEAR temp7.
    DATA temp8 LIKE LINE OF temp7.
    temp8-role = zif_llm_client=>role_user.
    temp8-content = 'Message 1'.
    INSERT temp8 INTO TABLE temp7.
    temp8-role = zif_llm_client=>role_assistant.
    temp8-content = 'Message 2'.
    INSERT temp8 INTO TABLE temp7.
    DATA messages LIKE temp7.
    messages = temp7.

    chat_request->add_messages( messages ).

    DATA result_messages TYPE zllm_msgs.
    result_messages = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result_messages ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = messages
      act = result_messages ).
  ENDMETHOD.

  METHOD test_clear_messages.
    DATA temp9 TYPE zllm_msg.
    CLEAR temp9.
    temp9-content = 'Test message'.
    DATA message LIKE temp9.
    message = temp9.
    chat_request->add_message( message ).

    chat_request->clear_messages( ).

    DATA messages TYPE zllm_msgs.
    messages = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_initial( messages ).
  ENDMETHOD.

  METHOD test_add_tool.
    DATA test_details TYPE zif_llm_tool=>tool_details.
    test_details = create_test_tool_details( ).
    tool_double->set_tool_details( test_details ).

    chat_request->add_tool(
      tool = tool_double
      tool_choice = zif_llm_chat_request=>tool_choice_auto ).

    DATA tools TYPE zllm_tools.
    tools = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( tools ) ).
  ENDMETHOD.

  METHOD test_add_tool_result.
    DATA test_result TYPE zif_llm_tool=>tool_result.
    test_result = create_test_tool_result( ).
    tool_double->set_result( test_result ).

    chat_request->add_tool_result( tool_double ).

    DATA messages TYPE zllm_msgs.
    messages = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    DATA temp10 LIKE LINE OF messages.
    DATA temp11 LIKE sy-tabix.
    temp11 = sy-tabix.
    READ TABLE messages INDEX 1 INTO temp10.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_client=>role_tool
      act = temp10-role ).
    DATA temp12 LIKE LINE OF messages.
    DATA temp13 LIKE sy-tabix.
    temp13 = sy-tabix.
    READ TABLE messages INDEX 1 INTO temp12.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      exp = test_result-tool_call_id
      act = temp12-tool_call_id ).
  ENDMETHOD.

  METHOD test_add_tools.
    DATA tools TYPE zllm_tools.
    DATA temp14 TYPE REF TO ltcl_tool_double.
    CREATE OBJECT temp14 TYPE ltcl_tool_double.
    APPEND temp14 TO tools.
    DATA temp15 TYPE REF TO ltcl_tool_double.
    CREATE OBJECT temp15 TYPE ltcl_tool_double.
    APPEND temp15 TO tools.

    chat_request->add_tools(
      tools = tools
      tool_choice = zif_llm_chat_request=>tool_choice_auto ).

    DATA result_tools TYPE zllm_tools.
    result_tools = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result_tools ) ).
  ENDMETHOD.

  METHOD test_clear_tools.
    DATA tool TYPE REF TO ltcl_tool_double.
    CREATE OBJECT tool TYPE ltcl_tool_double.
    chat_request->add_tool( tool = tool ).

    chat_request->clear_tools( ).

    DATA tools TYPE zllm_tools.
    tools = chat_request->get_tools( ).
    cl_abap_unit_assert=>assert_initial( tools ).
  ENDMETHOD.

  METHOD test_set_tool_choice.
    chat_request->set_tool_choice( zif_llm_chat_request=>tool_choice_auto ).

    DATA internal_request TYPE zllm_request.
    internal_request = chat_request->get_internal_request( ).
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_chat_request=>tool_choice_auto
      act = internal_request-tool_choice ).
  ENDMETHOD.

  METHOD test_set_structured_active.
    chat_request->set_structured_output_active( abap_true ).

    DATA internal_request TYPE zllm_request.
    internal_request = chat_request->get_internal_request( ).
    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = internal_request-use_structured_output ).
  ENDMETHOD.

  METHOD test_add_tool_choices.
    DATA temp16 TYPE zllm_tool_calls.
    CLEAR temp16.
    DATA temp17 LIKE LINE OF temp16.
    temp17-id = '1'.
    temp17-type = 'function'.
    CLEAR temp17-function.
    temp17-function-name = 'test_function'.
    INSERT temp17 INTO TABLE temp16.
    DATA choices LIKE temp16.
    choices = temp16.

    chat_request->add_tool_choices( choices ).

    DATA messages TYPE zllm_msgs.
    messages = chat_request->get_messages( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( messages ) ).
    DATA temp18 LIKE LINE OF messages.
    DATA temp19 LIKE sy-tabix.
    temp19 = sy-tabix.
    READ TABLE messages INDEX 1 INTO temp18.
    sy-tabix = temp19.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      exp = zif_llm_client=>role_assistant
      act = temp18-role ).
  ENDMETHOD.

  METHOD test_get_internal_request.
    DATA internal_request TYPE zllm_request.
    internal_request = chat_request->get_internal_request( ).
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
