*&---------------------------------------------------------------------*
*& Report ZABAP_P_OLLAMA_DEMO004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_p_ollama_demo004.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE title.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) cm_model.
    PARAMETERS p_model TYPE char30.
    SELECTION-SCREEN PUSHBUTTON 50(15) bt_exec USER-COMMAND exec.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

INTERFACE zif_ollama_tool_result.
  METHODS get_result
    RETURNING VALUE(result) TYPE string.
ENDINTERFACE.


CLASS lcl_get_weather DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_location TYPE string ##NEEDED.

    INTERFACES zif_ollama_tool_result.

  PRIVATE SECTION.
    DATA temperature TYPE i.
ENDCLASS.


CLASS lcl_get_weather IMPLEMENTATION.
  METHOD constructor.
    temperature = cl_abap_random=>create( )->packedinrange( min = '-10'
                                                            max = '40' ).
  ENDMETHOD.

  METHOD zif_ollama_tool_result~get_result.
    result = |\{"temperature":{ temperature }\}|.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      RAISING cx_sql_exception.

    TYPES: BEGIN OF ty_available_models,
             name TYPE char50,
           END OF ty_available_models.
    TYPES tt_available_models TYPE STANDARD TABLE OF ty_available_models WITH EMPTY KEY.

    METHODS build_response
      IMPORTING iv_prompt          TYPE string
      RETURNING VALUE(rv_response) TYPE string
      RAISING   zcx_ollama_message.

    METHODS get_available_models
      RETURNING VALUE(rt_models) TYPE tt_available_models
      RAISING   zcx_ollama_message.

  PRIVATE SECTION.
    METHODS get_system_prompt
      RETURNING VALUE(rv_prompt) TYPE string.

    DATA mo_client       TYPE REF TO zif_ollama_client.
    DATA mo_tool_builder TYPE REF TO zif_ollama_tool_builder.
    DATA mt_tools        TYPE zif_ollama_chat=>ty_tools_request.
    DATA mt_messages     TYPE STANDARD TABLE OF zif_ollama_chat=>ty_chat_message WITH EMPTY KEY.
ENDCLASS.


CLASS lcl_model IMPLEMENTATION.
  METHOD constructor.
    mo_client = zcl_ollama_client=>create( ).
    mo_tool_builder = NEW zcl_ollama_tool_builder( ).

    APPEND mo_tool_builder->to_request( is_spec = VALUE #(
                                            class       = |\\PROGRAM={ sy-repid }\\CLASS=LCL_GET_WEATHER|
                                            description = 'Get weather'
                                            parameters  = VALUE #(
                                                (  parameter = 'IV_LOCATION' description = 'Location name'  ) ) ) ) TO mt_tools.

    mt_messages = VALUE #( ( role    = zif_ollama_chat=>mc_chat_message_roles-system
                             content = get_system_prompt( ) ) ).
  ENDMETHOD.

  METHOD get_system_prompt.
    DATA lt_prompt TYPE string_table.

    DEFINE add_to_prompt.
      APPEND &1 TO lt_prompt.
    END-OF-DEFINITION.

    add_to_prompt 'You are a helpful assistant with tool calling capabilities.'.
    add_to_prompt 'When you receive a tool call response, use the output to format an answer to the original user question.'.

    rv_prompt = concat_lines_of( table = lt_prompt
                                 sep   = cl_abap_char_utilities=>newline ).
  ENDMETHOD.

  METHOD build_response.
    DATA lv_tool_processed TYPE abap_bool.

    APPEND VALUE #( role    = zif_ollama_chat=>mc_chat_message_roles-user
                    content = iv_prompt ) TO mt_messages.

    DATA(ls_request) = mo_client->chat( is_request = VALUE #( model    = p_model
                                                              messages = mt_messages
                                                              tools    = mt_tools ) ).
    LOOP AT ls_request-message-tool_calls INTO DATA(tool).
      IF line_exists( mt_tools[ function-name = tool-function-name ] ).
        DATA(lo_tool) = CAST zif_ollama_tool_result( mo_tool_builder->to_abap( tool ) ).
        APPEND VALUE #( role    = zif_ollama_chat=>mc_chat_message_roles-tool
                        content = lo_tool->get_result( ) ) TO mt_messages.
        lv_tool_processed = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_tool_processed = abap_true.
      ls_request = mo_client->chat( is_request = VALUE #( model    = p_model
                                                          messages = mt_messages
                                                          tools    = mt_tools ) ).
    ENDIF.

    rv_response = ls_request-message-content.
  ENDMETHOD.

  METHOD get_available_models.
    DATA response TYPE zif_ollama_model_manager=>ty_model_list_response.

    response = mo_client->get_available_models( ).
    rt_models = VALUE #( FOR ls_model IN response-models
                         ( name  = ls_model-name ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_view DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_prompt
      RETURNING VALUE(rv_prompt) TYPE string.

    METHODS set_response
      IMPORTING iv_response TYPE string.

    METHODS get_response
      RETURNING VALUE(rv_response) TYPE string.

    METHODS select_model
      IMPORTING it_models TYPE lcl_model=>tt_available_models.

  PRIVATE SECTION.
    DATA mo_docking  TYPE REF TO cl_gui_docking_container.
    DATA mo_spliter  TYPE REF TO cl_gui_splitter_container.
    DATA mo_prompt   TYPE REF TO cl_gui_textedit.
    DATA mo_response TYPE REF TO cl_gui_textedit.
ENDCLASS.


CLASS lcl_view IMPLEMENTATION.
  METHOD constructor.
    DATA lt_exclude TYPE TABLE OF syucomm.

    lt_exclude = VALUE #( ( 'ONLI' ) ( 'SJOB' ) ( 'PRIN' ) ).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING p_status  = sy-pfkey
                p_program = sy-repid
      TABLES    p_exclude = lt_exclude.
    mo_docking = NEW cl_gui_docking_container( repid = sy-repid
                                               dynnr = sy-dynnr
                                               side  = cl_gui_docking_container=>dock_at_bottom
                                               ratio = 90 ).
    mo_spliter = NEW cl_gui_splitter_container( parent  = mo_docking
                                                rows    = 2
                                                columns = 1 ).
    mo_prompt = NEW cl_gui_textedit( parent        = mo_spliter->get_container( row    = 1
                                                                                column = 1 )
                                     wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).
    mo_response = NEW cl_gui_textedit( parent        = mo_spliter->get_container( row    = 2
                                                                                  column = 1 )
                                       wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).
    mo_response->set_readonly_mode( ).
  ENDMETHOD.

  METHOD get_prompt.
    DATA lt_prompt TYPE STANDARD TABLE OF char255.

    mo_prompt->get_text_as_r3table( IMPORTING table = lt_prompt ).

    rv_prompt = concat_lines_of( lt_prompt ).
  ENDMETHOD.

  METHOD set_response.
    DATA lt_response TYPE STANDARD TABLE OF char255.
    DATA lv_response TYPE string.

    lv_response = iv_response.

    DO.
      IF strlen( lv_response ) > 255.
        APPEND lv_response(255) TO lt_response.
        lv_response = lv_response+255.
      ELSE.
        APPEND lv_response TO lt_response.
        EXIT.
      ENDIF.
    ENDDO.

    mo_response->set_text_as_r3table( table = lt_response ).
  ENDMETHOD.

  METHOD get_response.
    DATA lt_response TYPE STANDARD TABLE OF char255.

    mo_response->get_text_as_r3table( IMPORTING table = lt_response ).

    rv_response = concat_lines_of( lt_response ).
  ENDMETHOD.

  METHOD select_model.
    DATA lt_return TYPE TABLE OF ddshretval.
    DATA lt_fields TYPE STANDARD TABLE OF dfies.
    DATA ls_fields LIKE LINE OF lt_fields.
    FIELD-SYMBOLS <fs_field> TYPE dfies.

    ls_fields = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'CHAR50' ) )->get_ddic_field( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_field>.
    MOVE-CORRESPONDING ls_fields TO <fs_field>.
    <fs_field>-fieldname = 'NAME'.
    <fs_field>-reptext   = 'Model Name'.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield   = 'NAME'
                 value_org  = 'S'
      TABLES     value_tab  = it_models
                 field_tab  = lt_fields
                 return_tab = lt_return
      EXCEPTIONS OTHERS     = 1.
    IF sy-subrc = 0.
      READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
      p_model = ls_return-fieldval.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS init.
    METHODS run.

    METHODS on_event
      IMPORTING iv_event TYPE clike.

  PRIVATE SECTION.
    DATA mo_model TYPE REF TO lcl_model.
    DATA mo_view  TYPE REF TO lcl_view.
ENDCLASS.


CLASS lcl_application IMPLEMENTATION.
  METHOD init.
    cm_model = 'Ollama Model'.
    bt_exec  = 'Execute'.
    title = 'Selection Criteria'.
    TRY.
        mo_model = NEW lcl_model( ).
        mo_view = NEW lcl_view( ).
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD on_event.
    TRY.
        CASE iv_event.
          WHEN 'EXEC'.
            mo_view->set_response( iv_response = mo_model->build_response( iv_prompt = mo_view->get_prompt( ) ) ).
          WHEN 'P_MODEL'.
            mo_view->select_model( mo_model->get_available_models( ) ).
        ENDCASE.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD run.
  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  DATA(app) = NEW lcl_application( ).

INITIALIZATION.
  app->init( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_model.
  app->on_event( iv_event = 'P_MODEL' ).

AT SELECTION-SCREEN.
  app->on_event( sy-ucomm ).

START-OF-SELECTION.
  app->run( ).
