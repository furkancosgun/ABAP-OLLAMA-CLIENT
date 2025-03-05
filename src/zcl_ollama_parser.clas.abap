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
    CONSTANTS mc_options_fieldname TYPE string VALUE 'OPTIONS'.

    CLASS-DATA instance TYPE REF TO zcl_ollama_parser.

    "! Transforms the 'options' field into a structured format.
    "!
    "! @parameter ro_data |
    METHODS transform_options_to_structure
      IMPORTING is_data        TYPE any
      RETURNING VALUE(ro_data) TYPE REF TO data.

    "! Extracts the 'options' field from the input data.
    METHODS extract_options
      IMPORTING is_data           TYPE any
      RETURNING VALUE(rt_options) TYPE zif_ollama_common=>ty_options.

    "! Builds the component table for the 'options' field.
    "!
    "! @parameter rt_components |
    METHODS build_options_components
      IMPORTING it_options           TYPE zif_ollama_common=>ty_options
      RETURNING VALUE(rt_components) TYPE cl_abap_structdescr=>component_table.

    "! Copies non-options fields to the transformed structure.
    "!
    "! @parameter cs_transformed_data |
    METHODS copy_non_options_fields
      IMPORTING is_data             TYPE any
      CHANGING  cs_transformed_data TYPE any.

    "! Maps the options fields to the transformed structure.
    "!
    "! @parameter cs_transformed_data |
    METHODS map_options_fields
      IMPORTING it_options          TYPE zif_ollama_common=>ty_options
      CHANGING  cs_transformed_data TYPE any.
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
    DATA(lr_transformed_data) = transform_options_to_structure( is_data = data ).
    rv_data = /ui2/cl_json=>serialize( data        = lr_transformed_data
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD transform_options_to_structure.
    DATA(lt_options) = extract_options( is_data ).
    IF lt_options IS INITIAL.
      CREATE DATA ro_data LIKE is_data.
      ASSIGN ro_data->* TO FIELD-SYMBOL(<fs_data>).
      <fs_data> = is_data.
      RETURN.
    ENDIF.

    DATA(lt_components) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( is_data )
    )->get_components( ).

    DATA(lt_options_components) = build_options_components( lt_options ).
    IF lt_options_components IS NOT INITIAL.
      lt_components[ name = mc_options_fieldname ]-type = cl_abap_structdescr=>create(
                                                              p_components = lt_options_components ).
    ELSE.
      DELETE lt_components WHERE name = mc_options_fieldname.
    ENDIF.

    DATA(lr_type) = cl_abap_structdescr=>create( p_components = lt_components ).
    CREATE DATA ro_data TYPE HANDLE lr_type.
    ASSIGN ro_data->* TO <fs_data>.

    copy_non_options_fields( EXPORTING is_data             = is_data
                             CHANGING  cs_transformed_data = <fs_data> ).

    map_options_fields( EXPORTING it_options          = lt_options
                        CHANGING  cs_transformed_data = <fs_data> ).
  ENDMETHOD.

  METHOD extract_options.
    ASSIGN COMPONENT mc_options_fieldname OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_options>).
    IF sy-subrc = 0.
      rt_options = <fs_options>.
    ENDIF.
  ENDMETHOD.

  METHOD build_options_components.
    LOOP AT it_options ASSIGNING FIELD-SYMBOL(<fs_option>).
      INSERT VALUE #( name = <fs_option>-key
                      type = CAST #( cl_abap_typedescr=>describe_by_data_ref( <fs_option>-value ) ) )
             INTO TABLE rt_components.
    ENDLOOP.
  ENDMETHOD.

  METHOD copy_non_options_fields.
    DATA(lt_components) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( is_data )
    )->get_components( ).

    LOOP AT lt_components INTO DATA(ls_component) WHERE name <> mc_options_fieldname.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_input>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE cs_transformed_data TO FIELD-SYMBOL(<fs_output>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      <fs_output> = <fs_input>.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_options_fields.
    LOOP AT it_options ASSIGNING FIELD-SYMBOL(<fs_option>).
      ASSIGN COMPONENT |{ mc_options_fieldname }-{ <fs_option>-key }| OF STRUCTURE cs_transformed_data TO FIELD-SYMBOL(<fs_field>).
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
