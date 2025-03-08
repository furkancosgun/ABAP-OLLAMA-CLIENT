*&---------------------------------------------------------------------*
*& Report zabap_p_ollama_demo002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_p_ollama_demo002.
TABLES snap.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
  PARAMETERS p_datum TYPE snap-datum OBLIGATORY DEFAULT sy-datum.
  PARAMETERS p_model TYPE string OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    TYPES  BEGIN OF ty_list.
             INCLUDE TYPE rsdumpinfo.
    TYPES:   color TYPE lvc_t_scol,
           END OF ty_list.

    TYPES tt_list TYPE STANDARD TABLE OF ty_list WITH EMPTY KEY.

    DATA mt_list TYPE tt_list.
    DATA ms_list LIKE LINE OF mt_list.

    TYPES: BEGIN OF ty_available_models,
             name TYPE char50,
           END OF ty_available_models.
    TYPES tt_available_models TYPE STANDARD TABLE OF ty_available_models WITH EMPTY KEY.

    METHODS constructor
      IMPORTING io_client TYPE REF TO zif_ollama_client.

    METHODS fetch
      IMPORTING iv_date TYPE datum.

    METHODS get_available_models
      RETURNING VALUE(rt_models) TYPE tt_available_models
      RAISING   zcx_ollama_message.

    METHODS select_available_model
      CHANGING cv_model TYPE clike.

    METHODS analyze_dump
      IMPORTING is_list TYPE ty_list.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_json_error,
        type        TYPE string,
        message     TYPE string,
        program     TYPE string,
        line_number TYPE i,
        statement   TYPE string,
      END OF ty_json_error.

    TYPES:
      BEGIN OF ty_json_analysis,
        description TYPE string,
      END OF ty_json_analysis.

    TYPES:
      BEGIN OF ty_json_source_code,
        before_fix TYPE string_table,
        after_fix  TYPE string_table,
      END OF ty_json_source_code.

    TYPES:
      BEGIN OF ty_json_data,
        error       TYPE ty_json_error,
        analysis    TYPE ty_json_analysis,
        source_code TYPE ty_json_source_code,
      END OF ty_json_data.

    METHODS get_dump_section
      IMPORTING iv_errid          TYPE snapt-errid
                iv_section        TYPE snapt-ttype
      RETURNING VALUE(rt_section) TYPE string_table.

    DATA mo_client           TYPE REF TO zif_ollama_client.
    DATA mt_available_models TYPE tt_available_models.

ENDCLASS.


CLASS lcl_model IMPLEMENTATION.
  METHOD constructor.
    mo_client = io_client.
  ENDMETHOD.

  METHOD fetch.
    DATA lt_infotab TYPE rsdumptab.

    CALL FUNCTION 'RS_ST22_GET_DUMPS'
      EXPORTING  p_day     = iv_date
      IMPORTING  p_infotab = lt_infotab
      EXCEPTIONS OTHERS    = 1.

    MOVE-CORRESPONDING lt_infotab TO mt_list.

    DATA(red_color) = VALUE lvc_s_colo( col = 6 ).
    DATA(orange_color) = VALUE lvc_s_colo( col = 7 ).

    LOOP AT mt_list ASSIGNING FIELD-SYMBOL(<fs_list>).
      <fs_list>-color = VALUE #( ( fname = 'SYHOST'      color = red_color )
                                 ( fname = 'SYUSER'      color = red_color )
                                 ( fname = 'PROGRAMNAME' color = orange_color )
                                 ( fname = 'INCLUDENAME' color = orange_color ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD analyze_dump.
    DATA lt_ft     TYPE rsdump_ft_it.
    DATA lt_source TYPE string_table.
    DATA lv_json   TYPE string.
    DATA ls_json   TYPE ty_json_data.

    SELECT SINGLE * FROM snap
      WHERE datum = @is_list-sydate
        AND uzeit = @is_list-sytime
        AND ahost = @is_list-syhost
        AND uname = @is_list-syuser
        AND mandt = @sy-mandt
      INTO @DATA(ls_snap).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_ST22_GET_FT'
      EXPORTING datum = ls_snap-datum
                uzeit = ls_snap-uzeit
                uname = ls_snap-uname
                ahost = ls_snap-ahost
                modno = ls_snap-modno
                mandt = ls_snap-mandt
      IMPORTING ft    = lt_ft.

    READ REPORT is_list-includename INTO lt_source.

    ls_json-error-type    = concat_lines_of( table = get_dump_section( iv_errid   = is_list-dumpid
                                                                       iv_section = 'K' ) ).

    ls_json-error-message = concat_lines_of( table = get_dump_section( iv_errid   = is_list-dumpid
                                                                       iv_section = 'U' ) ).

    ls_json-error-program = is_list-includename.

    LOOP AT lt_source INTO DATA(lv_source) FROM is_list-linenumber - 5 TO is_list-linenumber + 5.
      IF sy-tabix = is_list-linenumber.
        ls_json-error-statement   = lv_source.
        ls_json-error-line_number = lines( ls_json-source_code-before_fix ) + 1.
      ENDIF.
      APPEND lv_source TO ls_json-source_code-before_fix.
    ENDLOOP.

    ls_json-analysis-description = concat_lines_of( table = get_dump_section( iv_errid   = is_list-dumpid
                                                                              iv_section = 'U' ) ).

    lv_json = /ui2/cl_json=>serialize( data        = ls_json
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                       compress    = abap_true ).

    LOOP AT lt_ft INTO DATA(ls_ft).
      REPLACE ALL OCCURRENCES OF |&{ ls_ft-id }| IN lv_json WITH ls_ft-value.
    ENDLOOP.

    DATA(lv_system_prompt) =
      |**Prompt:**| &&
      |You are an experienced ABAP developer. Your task is to analyze the provided ABAP dump, identify the root cause of the error, and provide a solution.| &&
      |**Instructions:**| &&
      |1. Carefully review the ABAP dump details provided in the JSON schema.| &&
      |2. Identify the exact location and cause of the error in the source code.| &&
      |3. Provide a detailed explanation of the issue in the `analysis` section.| &&
      |4. Correct the source code to resolve the error and include the fixed code in the `after_fix` section.| &&
      |5. Ensure the corrected code follows ABAP best practices and is free of syntax or logical errors.| &&
      |6. Return the completed JSON schema with all sections filled out.|. " &&

    DATA(lr_format) = /ui2/cl_json=>generate(
                          json        = |\{| &&
                                                          |"type": "object",| &&
                                                          |"properties": \{| &&
                                                              |"source_code": \{| &&
                                                                  |"type": "object",| &&
                                                                  |"properties": \{| &&
                                                                      |"before_fix": \{| &&
                                                                          |"type": "array",| &&
                                                                          |"items": \{ "type": "string" \}| &&
                                                                      |\},| &&
                                                                      |"after_fix": \{| &&
                                                                          |"type": "array",| &&
                                                                          |"items": \{ "type": "string" \}| &&
                                                                      |\}| &&
                                                                  |\},| &&
                                                                  |"required": [ "before_fix", "after_fix" ]| &&
                                                              |\}| &&
                                                          |\},| &&
                                                          |"required": [ "error", "analysis", "source_code" ]| &&
                                                     |\}|
                          pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    TRY.
        DATA(ls_chat_response) = mo_client->chat(
            is_request = VALUE #(
                model    = p_model
                messages = VALUE #( ( role = zif_ollama_chat=>mc_chat_message_roles-system content = lv_system_prompt )
                                    ( role = zif_ollama_chat=>mc_chat_message_roles-user content = lv_json ) )
                format   = lr_format ) ).

        cl_demo_output=>display_json( ls_chat_response-message-content ).
      CATCH zcx_ollama_message INTO DATA(lx_ollama).
        MESSAGE lx_ollama->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_available_models.
    DATA response TYPE zif_ollama_model_manager=>ty_model_list_response.

    IF mt_available_models IS NOT INITIAL.
      rt_models = mt_available_models.
      RETURN.
    ENDIF.

    response = mo_client->get_available_models( ).
    mt_available_models = VALUE #( FOR ls_model IN response-models
                                   ( name               = ls_model-name ) ).

    rt_models = mt_available_models.
  ENDMETHOD.

  METHOD select_available_model.
    DATA lt_return TYPE TABLE OF ddshretval.
    DATA lt_fields TYPE STANDARD TABLE OF dfies.
    DATA ls_fields LIKE LINE OF lt_fields.
    FIELD-SYMBOLS <fs_field> TYPE dfies.

    TRY.
        DATA(lt_models) = get_available_models( ).
      CATCH zcx_ollama_message.
        RETURN.
    ENDTRY.

    ls_fields = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'CHAR50' ) )->get_ddic_field( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_field>.
    MOVE-CORRESPONDING ls_fields TO <fs_field>.
    <fs_field>-fieldname = 'NAME'.
    <fs_field>-reptext   = 'Model Name'.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield   = 'NAME'
                 value_org  = 'S'
      TABLES     value_tab  = lt_models
                 field_tab  = lt_fields
                 return_tab = lt_return
      EXCEPTIONS OTHERS     = 1.

    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    cv_model = ls_return-fieldval.
  ENDMETHOD.

  METHOD get_dump_section.
    SELECT * FROM snapt
      WHERE langu = 'E'
        AND errid = @iv_errid
        AND ttype = @iv_section
      INTO TABLE @DATA(lt_snapt).

    LOOP AT lt_snapt INTO DATA(ls_snapt).
      FIND '&INCLUDE' IN ls_snapt-tline.
      IF sy-subrc = 0.
        REPLACE '&INCLUDE' IN ls_snapt-tline WITH ''.
        CONDENSE ls_snapt-tline.
        rt_section = get_dump_section( iv_errid   = CONV #( ls_snapt-tline )
                                       iv_section = iv_section ).
      ELSE.
        APPEND ls_snapt-tline TO rt_section.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_view DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_model TYPE REF TO lcl_model.

    METHODS display.

  PRIVATE SECTION.
    DATA mo_model TYPE REF TO lcl_model.

    METHODS set_column_options
      IMPORTING io_columns TYPE REF TO cl_salv_columns_table.

    METHODS on_hotspot_click FOR EVENT link_click OF cl_salv_events_table IMPORTING !row !column.
    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table  IMPORTING !row !column.
ENDCLASS.


CLASS lcl_view IMPLEMENTATION.
  METHOD constructor.
    mo_model = io_model.
  ENDMETHOD.

  METHOD display.
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv)
                            CHANGING  t_table      = mo_model->mt_list ).

    lo_salv->get_columns( )->set_optimize( ).
    lo_salv->get_functions( )->set_all( ).
    lo_salv->get_columns( )->set_color_column( 'COLOR' ).
    set_column_options( io_columns = lo_salv->get_columns( ) ).
    SET HANDLER me->on_hotspot_click FOR lo_salv->get_event( ).
    SET HANDLER me->on_double_click FOR lo_salv->get_event( ).

    lo_salv->display( ).
  ENDMETHOD.

  METHOD set_column_options.
    TYPES: BEGIN OF ty_column_options,
             columnname TYPE lvc_fname,
             text       TYPE scrtext_l,
             celltype   TYPE salv_de_celltype,
           END OF ty_column_options.
    DATA lt_column_options TYPE TABLE OF ty_column_options.

    lt_column_options = VALUE #(
        ( columnname = 'SYDATE'      text = 'System Date' )
        ( columnname = 'SYTIME'      text = 'System Time' )
        ( columnname = 'SYHOST'      text = 'Application Server' )
        ( columnname = 'SYUSER'      text = 'User Name' )
        ( columnname = 'DUMPID'      text = 'Name of Runtime Error' )
        ( columnname = 'PROGRAMNAME' text = 'ABAP Program Name' celltype = if_salv_c_cell_type=>hotspot )
        ( columnname = 'INCLUDENAME' text = 'ABAP Include Name' celltype = if_salv_c_cell_type=>hotspot )
        ( columnname = 'LINENUMBER'  text = 'Line of ABAP Program' celltype = if_salv_c_cell_type=>hotspot ) ).

    LOOP AT lt_column_options ASSIGNING FIELD-SYMBOL(<fs_column_option>).
      DATA(lo_column) = CAST cl_salv_column_table( io_columns->get_column( <fs_column_option>-columnname ) ).
      lo_column->set_long_text( <fs_column_option>-text ).
      lo_column->set_medium_text( '' ).
      lo_column->set_short_text( '' ).
      IF <fs_column_option>-celltype IS NOT INITIAL.
        lo_column->set_cell_type( <fs_column_option>-celltype ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_hotspot_click.
    READ TABLE mo_model->mt_list INDEX row INTO mo_model->ms_list.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CASE column.
      WHEN 'PROGRAMNAME' OR 'INCLUDENAME' OR 'LINENUMBER'.
        CALL FUNCTION 'EDITOR_PROGRAM'
          EXPORTING  appid   = 'PG'
                     display = abap_true
                     program = mo_model->ms_list-includename
                     line    = mo_model->ms_list-linenumber
                     topline = mo_model->ms_list-linenumber
          EXCEPTIONS OTHERS  = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    READ TABLE mo_model->mt_list INDEX row INTO mo_model->ms_list.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    mo_model->analyze_dump( mo_model->ms_list ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS init.

    METHODS on_f4
      IMPORTING !for     TYPE clike
      CHANGING  cv_value TYPE any.

    METHODS run.

  PRIVATE SECTION.
    DATA mo_client TYPE REF TO zif_ollama_client.
    DATA mo_model  TYPE REF TO lcl_model.
    DATA mo_view   TYPE REF TO lcl_view.
    DATA mt_list   TYPE lcl_model=>tt_list.
ENDCLASS.


CLASS lcl_controller IMPLEMENTATION.
  METHOD constructor.
    mo_client = zcl_ollama_client=>create( ).
    mo_model = NEW lcl_model( io_client = mo_client ).
    mo_view = NEW lcl_view( io_model = mo_model ).
  ENDMETHOD.

  METHOD init.
    %_p_datum_%_app_%-text = 'System Date'.
    %_p_model_%_app_%-text = 'Ollama Model'.
    t01 = 'Selection Criteria'.
    sy-title = 'ABAP & Ollama Dump Fixer'.
  ENDMETHOD.

  METHOD on_f4.
    CASE for.
      WHEN 'P_MODEL'.
        mo_model->select_available_model( CHANGING cv_model = cv_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD run.
    mo_model->fetch( iv_date = p_datum ).
    mo_view->display( ).
  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  DATA(controller) = NEW lcl_controller( ).

INITIALIZATION.
  controller->init( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_model.
  controller->on_f4( EXPORTING for      = 'P_MODEL'
                     CHANGING  cv_value = p_model ).

START-OF-SELECTION.
  controller->run( ).
