"! <p class="shorttext synchronized" lang="en">Structured Output</p>
INTERFACE zif_llm_client_json_schema
  PUBLIC .
  TYPES value_type TYPE string.
CONSTANTS: BEGIN OF undefined,
             string TYPE value_type VALUE '1',
             number TYPE value_type VALUE '2',
             boolean TYPE value_type VALUE '3',
             integer TYPE value_type VALUE '4',
           END OF undefined.

  TYPES: BEGIN OF property_def,
           name        TYPE string,
           type        TYPE value_type,
           description TYPE string,
           allow_null  TYPE abap_bool,
         END OF property_def,
         properties_def TYPE HASHED TABLE OF property_def WITH UNIQUE KEY name.

  TYPES: BEGIN OF object_def,
           name        TYPE string,
           description TYPE string,
           properties  TYPE properties_def,
         END OF object_def.

  TYPES: BEGIN OF array_def,
           name        TYPE string,
           description TYPE string,
           properties  TYPE properties_def,
         END OF array_def.

  CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Create a new instance.
    "! @parameter title | <p class="shorttext synchronized" lang="en">JSON Schema Title</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">JSON Schema</p>
    new_format IMPORTING title TYPE string RETURNING VALUE(result) TYPE REF TO zif_llm_client_json_schema.
  METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Add one property (only one level supported).
    "! @parameter property | <p class="shorttext synchronized" lang="en">Property to add</p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    add_property IMPORTING property TYPE property_def RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Add an array of a specific object. Only one level supported.
    "! @parameter array | <p class="shorttext synchronized" lang="en">Array to add</p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    add_array IMPORTING array TYPE array_def RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Add an object (only one level supported).
    "! @parameter object | <p class="shorttext synchronized" lang="en">Object with parameters to add</p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    add_object IMPORTING object TYPE object_def RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Generate the final schema.
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    generate_schema RETURNING VALUE(result) TYPE string.

ENDINTERFACE.
