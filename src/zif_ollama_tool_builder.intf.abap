INTERFACE zif_ollama_tool_builder
  PUBLIC.

  " Constants for argument types
  CONSTANTS:
    BEGIN OF mc_argument_types,
      string  TYPE string VALUE 'string',  " Represents a string type
      number  TYPE string VALUE 'number',  " Represents a numeric type
      boolean TYPE string VALUE 'boolean', " Represents a boolean type
      object  TYPE string VALUE 'object',  " Represents an object type
      array   TYPE string VALUE 'array',   " Represents an array type
    END OF mc_argument_types.

  TYPES ty_enum TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  " Types for parameter specifications
  TYPES:
    BEGIN OF ty_parameter_spec,
      parameter   TYPE string,  " Name of the parameter
      description TYPE string,  " Description of the parameter
      enum        TYPE ty_enum, " Valid value list
    END OF ty_parameter_spec.

  TYPES ty_parameter_specs TYPE STANDARD TABLE OF ty_parameter_spec WITH EMPTY KEY.

  " Types for method specifications
  TYPES:
    BEGIN OF ty_tool_spec,
      class       TYPE string,             " Name of the method
      description TYPE string,             " Description of the method
      parameters  TYPE ty_parameter_specs, " List of parameters
    END OF ty_tool_spec.


  METHODS to_request
    IMPORTING is_spec           TYPE ty_tool_spec
    RETURNING VALUE(rs_request) TYPE zif_ollama_chat=>ty_request_tool
    RAISING   zcx_ollama_message.

  METHODS to_abap
    IMPORTING is_response        TYPE zif_ollama_chat=>ty_response_tool_call
    RETURNING VALUE(ro_instance) TYPE REF TO object
    RAISING   zcx_ollama_message.
ENDINTERFACE.
