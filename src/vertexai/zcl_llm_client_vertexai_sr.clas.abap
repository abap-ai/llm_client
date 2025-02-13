"! <p class="shorttext synchronized" lang="en">Memory Root for VertexAi</p>
CLASS zcl_llm_client_vertexai_sr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  SHARED MEMORY ENABLED .

  PUBLIC SECTION.
    INTERFACES if_shm_build_instance.

    TYPES:
      BEGIN OF token,
        valid_until TYPE timestamp,
        content     TYPE string,
        provider    TYPE zllm_provider_name,
      END OF token.

    "! <p class="shorttext synchronized" lang="en">Set a new auth token</p>
    "!
    "! @parameter jwt_token | <p class="shorttext synchronized" lang="en">Token</p>
    METHODS set_token
      IMPORTING jwt_token TYPE token.

    "! <p class="shorttext synchronized" lang="en">Retrieve an auth token</p>
    "!
    "! @parameter provider | <p class="shorttext synchronized" lang="en">Provider Name</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Token</p>
    METHODS get_token IMPORTING provider      TYPE zllm_provider_name
                      RETURNING VALUE(result) TYPE token.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA jwt_tokens TYPE HASHED TABLE OF token WITH UNIQUE KEY provider.
ENDCLASS.



CLASS zcl_llm_client_vertexai_sr IMPLEMENTATION.
  METHOD get_token.
        DATA temp1 LIKE LINE OF jwt_tokens.
        DATA temp2 LIKE sy-tabix.
        DATA temp3 TYPE zcl_llm_client_vertexai_sr=>token.
    TRY.
        
        
        temp2 = sy-tabix.
        READ TABLE jwt_tokens WITH KEY provider = provider INTO temp1.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        result = temp1.
      CATCH cx_sy_itab_line_not_found.
        " If it does not exist we return an empty entry
        
        CLEAR temp3.
        result = temp3.
    ENDTRY.
  ENDMETHOD.

  METHOD if_shm_build_instance~build.
    DATA area  TYPE REF TO zcl_llm_client_vertexai_s_area.
    DATA root  TYPE REF TO zcl_llm_client_vertexai_sr.
        DATA excep TYPE REF TO cx_shm_error.
        DATA temp4 TYPE REF TO cx_shm_build_failed.

    TRY.
        area = zcl_llm_client_vertexai_s_area=>attach_for_write( ).
        
      CATCH cx_shm_error INTO excep.
        
        CREATE OBJECT temp4 TYPE cx_shm_build_failed EXPORTING previous = excep.
        RAISE EXCEPTION temp4.
    ENDTRY.

    CREATE OBJECT root AREA HANDLE area.
    area->set_root( root ).
    area->detach_commit( ).
  ENDMETHOD.

  METHOD set_token.
    DATA temp5 LIKE sy-subrc.
    READ TABLE jwt_tokens WITH KEY provider = jwt_token-provider TRANSPORTING NO FIELDS.
    temp5 = sy-subrc.
    IF temp5 = 0.
      MODIFY TABLE jwt_tokens FROM jwt_token.
    ELSE.
      INSERT jwt_token INTO TABLE jwt_tokens.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
