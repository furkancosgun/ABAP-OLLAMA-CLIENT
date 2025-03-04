*&---------------------------------------------------------------------*
*& Report zabap_p_ollama_demo02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_p_ollama_demo001.

PARAMETERS p_model TYPE char25 OBLIGATORY.
PARAMETERS p_promp TYPE string OBLIGATORY.


CLASS lcl_ollama_demo_ui DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS display_model_selection
      CHANGING cv_model TYPE char25.

    METHODS display_prompt_response
      IMPORTING iv_model  TYPE char25
                iv_prompt TYPE string.

  PRIVATE SECTION.
    DATA mo_client TYPE REF TO zif_ollama_client.
ENDCLASS.


CLASS lcl_ollama_demo_ui IMPLEMENTATION.
  METHOD constructor.
    mo_client = zcl_ollama_client=>create( ).
  ENDMETHOD.

  METHOD display_model_selection.
    TYPES: BEGIN OF ty_models,
             name  TYPE char25,
             model TYPE char25,
           END OF ty_models.
    DATA lt_models TYPE TABLE OF ty_models.
    DATA lt_return TYPE TABLE OF ddshretval.

    TRY.
        DATA(ls_response) = mo_client->get_available_models( ).
        lt_models = VALUE #( FOR ls_model IN ls_response-models
                             ( name = ls_model-name model = ls_model-model ) ).

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield    = 'NAME'
            value_org   = 'S'
            dynpprog    = sy-repid
            dynpnr      = sy-dynnr
            dynprofield = 'P_MODEL'
          TABLES
            value_tab   = lt_models
            return_tab  = lt_return
          EXCEPTIONS
            OTHERS      = 1.

        READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
        cv_model = ls_return-fieldval.

      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_prompt_response.
    TRY.
        DATA(ls_response) = mo_client->generate( is_request = VALUE #( model  = iv_model
                                                                       prompt = iv_prompt ) ).

        cl_demo_output=>display_html( |<b>Prompt:</b><br>{ iv_prompt }| &&
                                      |<br><br>| &&
                                      |<b>Response:</b><br>{ ls_response-response }| ).

      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  %_p_model_%_app_%-text = 'Ollama Model'.
  %_p_promp_%_app_%-text = 'Prompt'.
  DATA(ui) = NEW lcl_ollama_demo_ui( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_model.
  ui->display_model_selection( CHANGING cv_model = p_model ).

START-OF-SELECTION.
  ui->display_prompt_response( iv_model  = p_model
                               iv_prompt = p_promp ).
