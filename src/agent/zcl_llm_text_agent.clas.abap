CLASS zcl_llm_text_agent DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_agent_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS:
      constructor
        IMPORTING
          model TYPE zllm_model DEFAULT 'llama3.2'
          tools TYPE zllm_tools OPTIONAL
        RAISING
          zcx_llm_agent_error.

    METHODS zif_llm_agent~execute REDEFINITION.

  PROTECTED SECTION.
    METHODS: initialize REDEFINITION.
  PRIVATE SECTION.

    " A default value should not exceed 132 characters, therefore we set it in constructor
    CLASS-DATA system_prompt TYPE string.

ENDCLASS.

CLASS zcl_llm_text_agent IMPLEMENTATION.

  METHOD constructor.
        DATA client TYPE REF TO zif_llm_client_int.
    TRY.
        
        client = zcl_llm_factory=>get_client_int( model ).
      CATCH zcx_llm_authorization.
        " Currently no action
    ENDTRY.
    super->constructor( client = client tools = tools ).
    initialize( ).
  ENDMETHOD.

  METHOD zif_llm_agent~execute.
    " Execute with input text as prompt
    result = super->execute( prompt ).
  ENDMETHOD.

  METHOD initialize.
    " Add system prompt to memory
    DATA temp1 TYPE zif_llm_agent=>memory_entry.
    CLEAR temp1.
    temp1-msg-role = client->role_system.
    temp1-msg-content = system_prompt.
    add_to_memory_internal( temp1 ).
  ENDMETHOD.

  METHOD class_constructor.
    system_prompt =
        |You are a helpful expert assistant that happily solves the given task. |
        && |Your tone is business professional, concise and precise. |
        && |If tools are available, use them when appropriate to gather information needed for your response.|
        ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
