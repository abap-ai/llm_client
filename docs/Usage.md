# How to use the LLM Client

Have a look at [llm_client_test](https://github.com/abap-ai/llm_client_tests) repository for an implemented example.

## Test Report

Use report ZLLM_CHAT_EXAMPLE to try the client. At the moment a very simple implementation without history. Select the model, enter a user message and hit send to get a response.

## Simple Call

You can find an implementation example in zcl_llm_tests_main method simple_call.

```abap
    " Get client via the factory
    TRY.
        DATA(client) = zcl_llm_factory=>get_client( model ).
      CATCH zcx_llm_authorization INTO DATA(error).
        "Handle the error
    ENDTRY.

    " Send a chat message
    DATA(response) = client->execute( user_message = `What makes the ABAP programming language special?` ) ##NO_TEXT.

    "Check response status and handle errors
    IF response-success = abap_false.
      "Handle the error, e.g.:
      "APPEND |Error: return code { response-error-http_code } message { response-error-error_text }| TO result-out.
      "RETURN.
    ENDIF.

    "Use the output
    APPEND response-choice-message-content TO result-out.
    ...
```

## Structured Output

You can find an implementation example in zcl_llm_tests_main method so_simple.\
In general structured output is at the time of writing this still quite limited but promising and expected to be way more reliable in the upcoming model generations. Make it as simple as possible for the model and validate the output. Low temperature values can help.
Important limitations:

- Top-level data structure MUST be an ABAP structure. Elements or tables are not supported on top-level (but on lower levels).
- Only the following data types are supported:
  - DECFLOAT16 & DECFLOAT32
  - INT4 (type i) & INT8
  - STRING
  - SAP_BOOL\
    This is an intentional limitation as most structured output implementations don't support further limitations. This serves as a reminder that you should do a proper validation before using the data and e.g. mapping it to small internal data types.
Hints:
- Use simple and easy to understand field names
- Provide detailed descriptions for all fields as a hint to the LLM
- Chain of Thought Reasoning and often in general better outcomes can be achieved by adding as a first field a "reasoning" or similar field that is just for the LLM to "think".

## Tools

The same limitations and hints as in Structured Output apply for tools. For tool-choice you can use 'auto', 'none', 'required' and 'toolname'. If you use 'toolname' it will be converted in a mandatory call of that specific tool. This can be used to force structured output in models that don't support it.\
Depending on the provider and model tool responses are currently quite different with some models beeing able to call multiple tools and others only beeing able to use one. This is expected to improve with the upcoming model generations.\

### ZCL_LLM_TOOL_ECHO

This is a simple tool that supports defining everything during runtime so that you can make a tool call without having to implement a tool. For an example on how to use it see zcl_llm_tests_main method func_call_echo.

### Custom Tools - Interface IF_LLM_TOOL

This interface is mandatory for all tools.
