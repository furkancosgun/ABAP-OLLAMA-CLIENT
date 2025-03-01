*&---------------------------------------------------------------------*
*& Report zabap_p_ollama_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_p_ollama_demo.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS get_available_models.
    CLASS-METHODS get_model_details.
    CLASS-METHODS embed.
    CLASS-METHODS generate.
    CLASS-METHODS chat.

    CLASS-DATA mo_client TYPE REF TO zif_ollama_client.

  PRIVATE SECTION.
    CONSTANTS mc_model TYPE string VALUE 'mistral:latest'.

    CLASS-METHODS display_as_json
      IMPORTING !data       TYPE any
      RETURNING VALUE(json) TYPE string.
ENDCLASS.


CLASS lcl_application IMPLEMENTATION.
  METHOD class_constructor.
    mo_client = zcl_ollama_client=>create( ).
  ENDMETHOD.

  METHOD get_available_models.
    TRY.
        DATA(response) = mo_client->get_available_models( ).
        display_as_json( response ).
      CATCH zcx_ollama_message INTO DATA(lx_message).
        WRITE lx_message->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_model_details.
    TRY.
        DATA(response) = mo_client->get_model_details( VALUE #( model = mc_model ) ).
        display_as_json( response ).
      CATCH zcx_ollama_message INTO DATA(lx_message).
        WRITE lx_message->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD embed.
    TRY.
        DATA(response) = mo_client->embed( VALUE #( model = mc_model
                                                    input = 'Hello World!' ) ).
        display_as_json( response ).
      CATCH zcx_ollama_message INTO DATA(lx_message).
        WRITE lx_message->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD generate.
    TRY.
        DATA(response) = mo_client->generate( VALUE #( model  = mc_model
                                                       prompt = 'What is the square of 10?' ) ).
        display_as_json( response ).
      CATCH zcx_ollama_message INTO DATA(lx_message).
        WRITE lx_message->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD chat.
    TRY.
        DATA(response) = mo_client->chat( VALUE #( model    = mc_model
                                                   messages = VALUE #(
                                                       ( role    = zif_ollama_chat=>mc_chat_message_roles-user
                                                         content = 'Hello' )

                                                       ( role    = zif_ollama_chat=>mc_chat_message_roles-system
                                                         content = 'Hello! How can I assist you today?' )

                                                       ( role    = zif_ollama_chat=>mc_chat_message_roles-user
                                                         content = 'What is the sum of 5 and 3?' )

                                                       ( role    = zif_ollama_chat=>mc_chat_message_roles-system
                                                         content = '8' )

                                                       ( role    = zif_ollama_chat=>mc_chat_message_roles-user
                                                         content = 'What is the square of 10?' ) ) ) ).
        display_as_json( response ).
      CATCH zcx_ollama_message INTO DATA(lx_message).
        WRITE lx_message->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD display_as_json.
    cl_demo_output=>display_json( zcl_ollama_parser=>get_instance( )->serialize( data = data ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_application=>get_available_models( ).
  lcl_application=>get_model_details( ).
  lcl_application=>embed( ).
  lcl_application=>generate( ).
  lcl_application=>chat( ).
