# ABAP Ollama Client

This project provides an ABAP client for interacting with the Ollama API. Ollama is a platform for running and managing large language models (LLMs). This ABAP package allows you to perform operations such as text generation, chat, embedding creation, and model management using the Ollama API.

## Features

- **Model Management:** List available models and retrieve model details.
- **Text Generation:** Generate text using a specific model.
- **Chat:** Engage in a chat with the model and receive dynamic responses.
- **Embedding Creation:** Generate embedding vectors for texts.

## Installation

1. **ABAP System:** This package runs on an SAP ABAP system. Your system must have ABAP 7.4 or higher installed.
2. **Ollama API:** An Ollama server must be set up and running to access the Ollama API.
3. **Package Import:** Download this package from GitHub and import it into your ABAP system.

## Usage

### 1. Creating an Ollama Client

The Ollama client is created using the `zcl_ollama_client=>create()` method. By default, it uses the address `http://localhost:11434`.

```abap
DATA(lo_ollama_client) = zcl_ollama_client=>create( ).
```

### 2. Model Management

#### Listing Available Models

```abap
DATA(ls_available_models_response) = lo_ollama_client->get_available_models( ).
LOOP  AT ls_available_models_response-models INTO DATA(model).
	...
ENDLOOP.
```

#### Retrieving Model Details

```abap
DATA(ls_model_detail) = lo_ollama_client->get_model_details( VALUE #( model = 'mistral:latest' ) ).
```

### 3. Text Generation

```abap
DATA(ls_generate_response) = lo_ollama_client->generate(
  VALUE #( model  = 'mistral:latest'
           prompt = 'What is the square of 10?' )
).
WRITE ls_generate_response-response.
```

### 4. Chat

```abap
DATA(ls_chat_response) = lo_ollama_client->chat(
  VALUE #( model    = 'mistral:latest'
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
               content = 'What is the square of 10?' ) ) )
).
WRITE ls_chat_response-message-content.
```

### 5. Embedding Creation

```abap
DATA(ls_embed_response) = lo_ollama_client->embed(
  VALUE #( model = 'mistral:latest'
           input = 'Hello World!' )
).
```

## Example Program

Below is an example ABAP program that uses this package to interact with the Ollama API:

```abap
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
```

## Contributing

This project is open-source, and we welcome your contributions. To contribute:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature/AmazingFeature`).
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4. Push to the branch (`git push origin feature/AmazingFeature`).
5. Open a Pull Request.

## License

This project is licensed under the MIT License. See the `LICENSE` file for more details.

