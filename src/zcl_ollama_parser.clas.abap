CLASS zcl_ollama_parser DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Returns a singleton instance of the parser.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zif_ollama_parser.

    INTERFACES zif_ollama_parser.

  PRIVATE SECTION.
    CONSTANTS mc_options_fieldname TYPE string VALUE 'OPTIONS'.

    CLASS-DATA instance TYPE REF TO zcl_ollama_parser.
ENDCLASS.


CLASS zcl_ollama_parser IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW zcl_ollama_parser( ).
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.

  METHOD zif_ollama_parser~deserialize.
    /ui2/cl_json=>deserialize( EXPORTING json        = iv_data
                                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
                               CHANGING  data        = data ).
  ENDMETHOD.

  METHOD zif_ollama_parser~serialize.
    rv_data = /ui2/cl_json=>serialize( data        = data
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                       compress    = abap_true ).
  ENDMETHOD.
ENDCLASS.
