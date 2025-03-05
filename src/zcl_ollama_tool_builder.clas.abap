CLASS zcl_ollama_tool_builder DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ollama_tool_builder.

    ALIASES to_request FOR zif_ollama_tool_builder~to_request.
    ALIASES to_abap    FOR zif_ollama_tool_builder~to_abap.

    CONSTANTS mc_method_name TYPE abap_methname VALUE 'CONSTRUCTOR'.

    TYPES:
      BEGIN OF ty_parameters_element,
        type        TYPE string,
        description TYPE string,
        enum        TYPE zif_ollama_tool_builder=>ty_enum,
        optional    TYPE abap_bool,
      END OF ty_parameters_element.

    METHODS get_method_parameters
      IMPORTING io_clasdescr        TYPE REF TO cl_abap_classdescr
                io_methdescr        TYPE REF TO abap_methdescr
      RETURNING VALUE(rt_component) TYPE cl_abap_structdescr=>component_table
      RAISING   zcx_ollama_message.

    METHODS set_parameter_details
      IMPORTING is_spec       TYPE zif_ollama_tool_builder=>ty_tool_spec
                io_clasdescr  TYPE REF TO cl_abap_classdescr
                io_methdescr  TYPE REF TO abap_methdescr
      CHANGING  cs_properties TYPE any.
ENDCLASS.


CLASS zcl_ollama_tool_builder IMPLEMENTATION.
  METHOD get_method_parameters.
    DATA lo_parmdescr TYPE REF TO abap_parmdescr.

    DATA(lo_paramtype) = cl_abap_typedescr=>describe_by_data( VALUE ty_parameters_element( ) ).

    LOOP AT io_methdescr->parameters REFERENCE INTO lo_parmdescr WHERE parm_kind = cl_abap_objectdescr=>importing.
      DATA(lo_typedescr) = io_clasdescr->get_method_parameter_type( p_method_name    = mc_method_name
                                                                    p_parameter_name = lo_parmdescr->name ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_elem.
          APPEND VALUE #( name = lo_parmdescr->name
                          type = CAST #( lo_paramtype ) ) TO rt_component.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_ollama_message
            EXPORTING
              message = |Unsupported type ({ lo_typedescr->absolute_name })!|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_parameter_details.
    DATA lo_parmdescr TYPE REF TO abap_parmdescr.

    FIELD-SYMBOLS <fs_field> TYPE ty_parameters_element.

    LOOP AT io_methdescr->parameters REFERENCE INTO lo_parmdescr WHERE parm_kind = cl_abap_objectdescr=>importing.
      ASSIGN COMPONENT lo_parmdescr->name OF STRUCTURE cs_properties TO <fs_field>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA(lo_typedescr) = io_clasdescr->get_method_parameter_type( p_method_name    = mc_method_name
                                                                    p_parameter_name = lo_parmdescr->name ).
      CASE lo_parmdescr->type_kind.
        WHEN cl_abap_objectdescr=>typekind_int
          OR cl_abap_objectdescr=>typekind_int1
          OR cl_abap_objectdescr=>typekind_int2
          OR cl_abap_objectdescr=>typekind_int8
          OR cl_abap_objectdescr=>typekind_packed
          OR cl_abap_objectdescr=>typekind_float
          OR cl_abap_objectdescr=>typekind_decfloat16
          OR cl_abap_objectdescr=>typekind_decfloat34.
          <fs_field>-type = zif_ollama_tool_builder=>mc_argument_types-number.
        WHEN OTHERS.
          IF lo_typedescr->absolute_name CS 'ABAP_BOOL'.
            <fs_field>-type = zif_ollama_tool_builder=>mc_argument_types-boolean.
          ELSE.
            <fs_field>-type = zif_ollama_tool_builder=>mc_argument_types-string.
          ENDIF.
      ENDCASE.

      READ TABLE is_spec-parameters WITH KEY parameter = lo_parmdescr->name INTO DATA(ls_param).
      IF sy-subrc = 0.
        <fs_field>-description = ls_param-description.
        <fs_field>-enum        = ls_param-enum.
      ENDIF.
      <fs_field>-optional = lo_parmdescr->is_optional.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ollama_tool_builder~to_abap.
    DATA lo_strucdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_parameters TYPE abap_parmbind_tab.

    ASSIGN is_response-function-arguments->* TO FIELD-SYMBOL(<fs_parameters>).
    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( <fs_parameters> ).

    LOOP AT lo_strucdescr->get_components( ) ASSIGNING FIELD-SYMBOL(<fs_component>).
      ASSIGN COMPONENT <fs_component>-name OF STRUCTURE <fs_parameters> TO FIELD-SYMBOL(<fs_val>).
      IF sy-subrc = 0.
        INSERT VALUE #( name  = <fs_component>-name
                        kind  = cl_abap_objectdescr=>exporting
                        value = <fs_val> ) INTO TABLE lt_parameters.
      ENDIF.
    ENDLOOP.

    TRY.
        CREATE OBJECT ro_instance TYPE (is_response-function-name)
               PARAMETER-TABLE lt_parameters.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_ollama_message
          EXPORTING
            previous = lx_root
            message  = lx_root->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ollama_tool_builder~to_request.
    DATA lo_clasdescr TYPE REF TO cl_abap_classdescr.
    DATA lo_methdescr TYPE REF TO abap_methdescr.
    DATA lt_component TYPE cl_abap_structdescr=>component_table.
    DATA lo_strudescr TYPE REF TO cl_abap_structdescr.

    DATA(ls_spec) = VALUE zif_ollama_tool_builder=>ty_tool_spec(
                              class       = to_upper( is_spec-class )
                              description = is_spec-description
                              parameters  = VALUE #( FOR p IN is_spec-parameters
                                                     ( parameter   = to_upper( p-parameter )
                                                       description = p-description ) ) ).

    rs_request-function-name        = ls_spec-class.
    rs_request-function-description = is_spec-description.
    rs_request-function-parameters-type = zif_ollama_tool_builder=>mc_argument_types-object.
    TRY.
        lo_clasdescr ?= cl_abap_classdescr=>describe_by_name( ls_spec-class ).
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_ollama_message
          EXPORTING
            message = 'Class not found!'.
    ENDTRY.

    READ TABLE lo_clasdescr->methods WITH KEY name = mc_method_name REFERENCE INTO lo_methdescr.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ollama_message
        EXPORTING
          message = |Method not found ({ mc_method_name })!|.
    ENDIF.

    lt_component = get_method_parameters( io_clasdescr = lo_clasdescr
                                          io_methdescr = lo_methdescr ).
    IF lt_component IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ollama_message
        EXPORTING
          message = |Method can't be resolved!|.
    ENDIF.

    lo_strudescr ?= cl_abap_structdescr=>create( lt_component ).

    TRY.
        CREATE DATA rs_request-function-parameters-properties TYPE HANDLE lo_strudescr.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE zcx_ollama_message
          EXPORTING
            previous = lx_root
            message  = lx_root->get_text( ).
    ENDTRY.
    ASSIGN rs_request-function-parameters-properties->* TO FIELD-SYMBOL(<fs_properties>).

    set_parameter_details( EXPORTING is_spec       = ls_spec
                                     io_clasdescr  = lo_clasdescr
                                     io_methdescr  = lo_methdescr
                           CHANGING  cs_properties = <fs_properties> ).
  ENDMETHOD.
ENDCLASS.
