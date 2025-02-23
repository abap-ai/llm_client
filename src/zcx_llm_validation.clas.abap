class ZCX_LLM_VALIDATION definition
  public
  inheriting from CX_DYNAMIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of UNSUPPORTED_TYPE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNSUPPORTED_TYPE .
  constants:
    begin of VALUE_OUT_OF_RANGE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of VALUE_OUT_OF_RANGE .
  constants:
    begin of MODEL_DOES_NOT_EXIST,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MODEL_DOES_NOT_EXIST .
  constants:
    begin of CLIENT_MODEL_NOT_FOUND,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CLIENT_MODEL_NOT_FOUND .
  constants:
    begin of HTTP_DESTINATION_ERROR,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HTTP_DESTINATION_ERROR .
  constants:
    begin of ENCRYPTION_SETUP,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENCRYPTION_SETUP .
  constants:
    begin of ENCRYPTION_FAILED,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENCRYPTION_FAILED .
  constants:
    begin of DECRYPTION_FAILED,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DECRYPTION_FAILED .
  constants:
    begin of PROVIDER_DOES_NOT_EXIST,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROVIDER_DOES_NOT_EXIST .
  constants:
    begin of SIGN_ISSUE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '044',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SIGN_ISSUE .
  constants:
    begin of TOOL_CALL_NOT_FOUND,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '055',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TOOL_CALL_NOT_FOUND .
  constants:
    begin of TOOL_NOT_FOUND,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '045',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TOOL_NOT_FOUND .
  data MV_ATTR1 type STRING .
  data MV_ATTR2 type STRING .
  data MV_ATTR3 type STRING .
  data MV_ATTR4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !ATTR1 type STRING optional
      !ATTR2 type STRING optional
      !ATTR3 type STRING optional
      !ATTR4 type STRING optional
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_LLM_VALIDATION IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).
    mv_attr1 = attr1.
    mv_attr2 = attr2.
    mv_attr3 = attr3.
    mv_attr4 = attr4.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
