*&---------------------------------------------------------------------*
*& Report zabap_p_ollama_demo003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_p_ollama_demo003.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE title.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) cm_model.
    PARAMETERS p_model TYPE char30.
    SELECTION-SCREEN PUSHBUTTON 50(15) bt_build USER-COMMAND build.
    SELECTION-SCREEN PUSHBUTTON 65(15) bt_exec USER-COMMAND exec.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      RAISING cx_sql_exception.

    METHODS build_query
      IMPORTING iv_prompt          TYPE string
      RETURNING VALUE(rv_response) TYPE string
      RAISING   zcx_ollama_message.

    METHODS execute_query
      IMPORTING iv_query         TYPE string
      RETURNING VALUE(ro_result) TYPE REF TO data
      RAISING   cx_sql_exception.

    TYPES: BEGIN OF ty_available_models,
             name TYPE char50,
           END OF ty_available_models.
    TYPES tt_available_models TYPE STANDARD TABLE OF ty_available_models WITH EMPTY KEY.

    METHODS get_available_models
      RETURNING VALUE(rt_models) TYPE tt_available_models
      RAISING   zcx_ollama_message.

  PRIVATE SECTION.
    METHODS get_system_prompt
      RETURNING VALUE(rv_prompt) TYPE string.

    DATA mo_connection TYPE REF TO cl_sql_connection.
    DATA mo_client     TYPE REF TO zif_ollama_client.
    DATA mo_format     TYPE REF TO data.
    DATA mt_messages   TYPE STANDARD TABLE OF zif_ollama_chat=>ty_chat_message WITH EMPTY KEY.
ENDCLASS.


CLASS lcl_model IMPLEMENTATION.
  METHOD constructor.
    mo_connection = cl_sql_connection=>get_connection( con_name = cl_sql_connection=>c_default_connection ).
    mo_client = zcl_ollama_client=>create( ).
    mt_messages = VALUE #( ( role    = zif_ollama_chat=>mc_chat_message_roles-system
                             content = get_system_prompt( ) ) ).
    mo_format = /ui2/cl_json=>generate( json        = |\{| &&
                                                        |"type": "object",| &&
                                                        |"properties": \{| &&
                                                            |"query": \{| &&
                                                                |"type": "string"| &&
                                                            |\}| &&
                                                        |\},| &&
                                                        |"required": ["query"]| &&
                                                      |\}|
                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

  METHOD build_query.
    TYPES: BEGIN OF ty_query,
             query TYPE string,
           END OF ty_query.
    DATA ls_query TYPE ty_query.

    APPEND VALUE #( role    = zif_ollama_chat=>mc_chat_message_roles-user
                    content = iv_prompt ) TO mt_messages.

    DATA(ls_response) = mo_client->chat( is_request = VALUE #( model    = p_model
                                                               messages = mt_messages
                                                               format   = mo_format ) ).
    APPEND VALUE #( role    = zif_ollama_chat=>mc_chat_message_roles-assistant
                    content = ls_response-message-content ) TO mt_messages.

    /ui2/cl_json=>deserialize( EXPORTING json = ls_response-message-content
                               CHANGING  data = ls_query ).
    rv_response = ls_query-query.
  ENDMETHOD.

  METHOD execute_query.
    DATA(lo_query) = mo_connection->create_statement( )->execute_query( statement = iv_query ).

    DATA(lt_metadata) = lo_query->get_metadata( ).
    IF lt_metadata IS INITIAL.
      lo_query->close( ).
      RETURN.
    ENDIF.

    LOOP AT lt_metadata ASSIGNING FIELD-SYMBOL(<fs_metadata>).
      IF <fs_metadata> IS INITIAL.
        <fs_metadata>-column_name = |COL{ sy-tabix }|.
      ENDIF.
      CASE <fs_metadata>-data_type.
        WHEN cl_sql_result_set=>c_md_type_p.
          IF <fs_metadata>-length > cl_abap_elemdescr=>type_p_max_length.
            <fs_metadata>-length = cl_abap_elemdescr=>type_p_max_length.
          ENDIF.
        WHEN cl_sql_result_set=>c_md_type_c.
          IF <fs_metadata>-length > cl_abap_elemdescr=>type_c_max_length.
            <fs_metadata>-length = cl_abap_elemdescr=>type_c_max_length.
          ENDIF.
        WHEN cl_sql_result_set=>c_md_type_x.
          IF <fs_metadata>-length > cl_abap_elemdescr=>type_x_max_length.
            <fs_metadata>-length = cl_abap_elemdescr=>type_x_max_length.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    DATA(lo_tabletype) = cl_abap_tabledescr=>create( p_line_type = CAST cl_abap_structdescr(
                                                        cl_abap_structdescr=>describe_by_data_ref(
                                                            p_data_ref = lo_query->get_struct_ref(
                                                                             md_tab = lt_metadata ) ) ) ).

    CREATE DATA ro_result TYPE HANDLE lo_tabletype.
    lo_query->set_param_table( ro_result ).
    lo_query->next_package( ).
    lo_query->close( ).
  ENDMETHOD.

  METHOD get_system_prompt.
    DATA lt_prompt TYPE string_table.

    DEFINE add_to_prompt.
      APPEND &1 TO lt_prompt.
    END-OF-DEFINITION.

    add_to_prompt 'You are tasked with constructing SQL queries to extract meaningful data from a given context.'.
    add_to_prompt 'Before you begin writing the query, clearly understand the objective of the data retrieval.'.
    add_to_prompt 'Identify the tables and columns relevant to your task.'.
    add_to_prompt ''.
    add_to_prompt ''.
    add_to_prompt '# Steps'.
    add_to_prompt '1. **Understand the Objective**: Clarify what data you need to retrieve and why. Understand the requirements or questions that the data will answer.'.
    add_to_prompt '2. **Identify Relevant Tables**: Look at the database schema and identify which tables contain the data needed.'.
    add_to_prompt '3. **Determine Necessary Columns**: Pinpoint the specific columns that hold the information you need.'.
    add_to_prompt '4. **Formulate SQL Clauses**: Use appropriate clauses to construct your query. Ensure robustness by incorporating conditions and filters using the WHERE clause, and join tables if necessary.'.
    add_to_prompt '5. **Optimize for Performance**: Limit your query to essential columns and use indexing wisely if possible to enhance performance.'.
    add_to_prompt '6. **Validate and Test Your Query**: After writing, ensure the query yields the expected results and performs efficiently by running tests.'.
    add_to_prompt ''.
    add_to_prompt ''.
    add_to_prompt '# Output Format'.
    add_to_prompt 'The SQL query should be presented in a code block. Make sure to explain any complex parts of your query in comments above the SQL code if necessary.'.
    add_to_prompt ''.
    add_to_prompt ''.
    add_to_prompt '# Example'.
    add_to_prompt '```sql'.
    add_to_prompt '-- Retrieve customer names and order dates for all orders placed after January 1, 2020'.
    add_to_prompt 'SELECT Customers.CustomerName, Orders.OrderDate'.
    add_to_prompt 'FROM Customers'.
    add_to_prompt 'JOIN Orders ON Customers.CustomerID = Orders.CustomerID'.
    add_to_prompt 'WHERE Orders.OrderDate > ''2020-01-01'''.
    add_to_prompt 'ORDER BY Orders.OrderDate DESC;'.
    add_to_prompt '```'.

    rv_prompt = concat_lines_of( table = lt_prompt
                                 sep   = cl_abap_char_utilities=>newline ).
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

    METHODS set_query
      IMPORTING iv_query TYPE string.

    METHODS get_query
      RETURNING VALUE(rv_query) TYPE string.

    METHODS display_result
      IMPORTING io_result TYPE REF TO data
      RAISING   cx_salv_msg
                cx_salv_no_new_data_allowed.

    METHODS select_model
      IMPORTING it_models TYPE lcl_model=>tt_available_models.

  PRIVATE SECTION.
    DATA mo_docking  TYPE REF TO cl_gui_docking_container.
    DATA mo_spliter  TYPE REF TO cl_gui_splitter_container.
    DATA mo_prompt   TYPE REF TO cl_gui_textedit.
    DATA mo_response TYPE REF TO cl_gui_textedit.
    DATA mo_preview  TYPE REF TO cl_salv_table.
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
                                                rows    = 3
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

  METHOD set_query.
    DATA lt_response TYPE STANDARD TABLE OF char255.
    DATA lv_query    TYPE string.

    lv_query = iv_query.

    DO.
      IF strlen( lv_query ) > 255.
        APPEND lv_query(255) TO lt_response.
        lv_query = lv_query+255.
      ELSE.
        APPEND lv_query TO lt_response.
        EXIT.
      ENDIF.
    ENDDO.

    mo_response->set_text_as_r3table( table = lt_response ).
  ENDMETHOD.

  METHOD get_query.
    DATA lt_response TYPE STANDARD TABLE OF char255.

    mo_response->get_text_as_r3table( IMPORTING table = lt_response ).

    rv_query = concat_lines_of( lt_response ).
  ENDMETHOD.

  METHOD display_result.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    IF io_result IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN io_result->* TO <fs_table>.

    IF mo_preview IS NOT BOUND.
      cl_salv_table=>factory( EXPORTING r_container  = mo_spliter->get_container( row    = 3
                                                                                  column = 1 )
                              IMPORTING r_salv_table = mo_preview
                              CHANGING  t_table      = <fs_table> ).
      mo_preview->get_functions( )->set_all( ).
      mo_preview->get_columns( )->set_optimize( ).
    ELSE.
      mo_preview->set_data( CHANGING t_table = <fs_table> ).
    ENDIF.

    LOOP AT mo_preview->get_columns( )->get( ) INTO DATA(column).
      column-r_column->set_long_text( CONV #( column-columnname ) ).
    ENDLOOP.

    mo_preview->display( ).
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
    bt_build = 'Build Query'.
    bt_exec  = 'Execute Query'.
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
          WHEN 'BUILD'.
            mo_view->set_query( iv_query = mo_model->build_query( iv_prompt = mo_view->get_prompt( ) ) ).
          WHEN 'EXEC'.
            mo_view->display_result( io_result = mo_model->execute_query( iv_query = mo_view->get_query( ) ) ).
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
