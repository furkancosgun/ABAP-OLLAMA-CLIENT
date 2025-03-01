"! Implements the zif_ollama_parser interface to handle serialization and deserialization.
CLASS zcl_ollama_parser DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Returns a singleton instance of the parser.
    "!
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zif_ollama_parser.

    INTERFACES zif_ollama_parser.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zcl_ollama_parser.

    "! Transforms the 'options' field into a flat structure for serialization.
    "! @parameter is_data | The input data containing the 'options' field.
    "! @parameter ro_data | The transformed data structure.
    METHODS transform_options_to_structure
      IMPORTING is_data        TYPE any
      RETURNING VALUE(ro_data) TYPE REF TO data.
ENDCLASS.


CLASS zcl_ollama_parser IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW zcl_ollama_parser( ).
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.

  METHOD zif_ollama_parser~deserialize.
    " Deserialize JSON string into ABAP data
    /ui2/cl_json=>deserialize( EXPORTING json        = iv_data
                                         pretty_name = /ui2/cl_json=>pretty_mode-low_case
                               CHANGING  data        = data ).
  ENDMETHOD.

  METHOD zif_ollama_parser~serialize.
    " Serialize ABAP data into JSON string
    IF options = abap_true.
      rv_data = /ui2/cl_json=>serialize( data        = transform_options_to_structure( is_data = data )
                                         pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ELSE.
      rv_data = /ui2/cl_json=>serialize( data        = data
                                         pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ENDIF.
  ENDMETHOD.

  METHOD transform_options_to_structure.
    FIELD-SYMBOLS <fs_options> TYPE zif_ollama_common=>ty_options.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA lr_type       TYPE REF TO cl_abap_structdescr.

    " Assign the 'options' field from the input data
    ASSIGN COMPONENT 'OPTIONS' OF STRUCTURE is_data TO <fs_options>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get the components of the input structure
    lt_components = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( is_data )
    )->get_components( ).

    " Remove the 'options' component from the structure
    DELETE lt_components WHERE name = 'OPTIONS'.

    " Add each option as a new component to the structure
    LOOP AT <fs_options> ASSIGNING FIELD-SYMBOL(<fs_option>).
      INSERT VALUE #( name = <fs_option>-key
                      type = CAST #( cl_abap_typedescr=>describe_by_data_ref( <fs_option>-value ) ) )
             INTO TABLE lt_components.
    ENDLOOP.

    " Create a new structure type with the updated components
    lr_type = cl_abap_structdescr=>create( p_components = lt_components ).
    CREATE DATA ro_data TYPE HANDLE lr_type.

    " Assign the new structure and copy data from the input
    ASSIGN ro_data->* TO FIELD-SYMBOL(<fs_data>).
    MOVE-CORRESPONDING is_data TO <fs_data>.

    " Populate the new structure with option values
    LOOP AT <fs_options> ASSIGNING <fs_option>.
      ASSIGN COMPONENT <fs_option>-key OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN <fs_option>-value->* TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      <fs_field> = <fs_value>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
