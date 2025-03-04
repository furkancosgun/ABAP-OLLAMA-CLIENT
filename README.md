# ABAP Ollama Client: Seamless Integration with Large Language Models

Welcome to the **ABAP Ollama Client** project! This repository provides a powerful ABAP client for interacting with the **Ollama API**, enabling you to leverage large language models (LLMs) directly within your SAP ABAP environment. Whether you're generating text, engaging in dynamic conversations, or creating embeddings, this package simplifies the integration of cutting-edge AI capabilities into your ABAP applications.

---

## Key Features

The ABAP Ollama Client offers a wide range of functionalities to enhance your applications:

- **Model Management:**  
  - List all available models on the Ollama server.  
  - Retrieve detailed information about specific models.  

- **Text Generation:**  
  - Generate high-quality text responses using your preferred LLM.  

- **Interactive Chat:**  
  - Engage in dynamic, multi-turn conversations with the model.  

- **Embedding Creation:**  
  - Generate embedding vectors for text inputs, enabling advanced NLP tasks.  

- **Tool Integration:**  
  - Dynamically call user-defined ABAP classes as tools within chat interactions, extending the model's capabilities.  

---

## Installation Guide

To get started with the ABAP Ollama Client, follow these steps:

1. **ABAP System Requirements:**  
   Ensure your SAP system runs **ABAP 7.4 or higher**.  

2. **Ollama Server Setup:**  
   Set up and run an Ollama server to access the Ollama API.  

3. **Import the Package:**  
   Download the package from GitHub and import it into your ABAP system.  

---

## Getting Started

### 1. Create an Ollama Client Instance

To begin, instantiate the Ollama client. By default, it connects to `http://localhost:11434`.

```abap
DATA(lo_ollama_client) = zcl_ollama_client=>create( ).
```

### 2. Manage Models

#### List Available Models

Retrieve a list of all models available on the Ollama server.

```abap
DATA(ls_available_models_response) = lo_ollama_client->get_available_models( ).
LOOP AT ls_available_models_response-models INTO DATA(model).
  " Process each model
ENDLOOP.
```

#### Retrieve Model Details

Fetch detailed information about a specific model.

```abap
DATA(ls_model_detail) = lo_ollama_client->get_model_details( VALUE #( model = 'mistral:latest' ) ).
```

### 3. Generate Text

Generate text using a specified model and prompt.

```abap
DATA(ls_generate_response) = lo_ollama_client->generate(
  VALUE #( model  = 'mistral:latest'
           prompt = 'What is the square of 10?' )
).
WRITE ls_generate_response-response.
```

### 4. Engage in a Chat

Converse with the model by sending a series of messages.

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

### 5. Create Embeddings

Generate embedding vectors for text inputs.

```abap
DATA(ls_embed_response) = lo_ollama_client->embed(
  VALUE #( model = 'mistral:latest'
           input = 'Hello World!' )
).
```

### 6. Integrate Custom Tools in Chat

Extend the model's functionality by integrating custom ABAP classes as tools. Below is an example of a math operation tool:

```abap
CLASS lcl_ollama_user_tool DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_number1  TYPE i
                iv_operator TYPE string
                iv_number2  TYPE i.

    DATA number1  TYPE i.
    DATA operator TYPE string.
    DATA number2  TYPE i.
    DATA result   TYPE i.
ENDCLASS.

CLASS lcl_ollama_user_tool IMPLEMENTATION.
  METHOD constructor.
    number1 = iv_number1.
    operator = iv_operator.
    number2 = iv_number2.

    CASE operator.
      WHEN '+' OR 'ADD'.
        result = number1 + number2.
      WHEN '-' OR 'SUBTRACT'.
        result = number1 - number2.
      WHEN '*' OR 'MULTIPLY'.
        result = number1 * number2.
      WHEN '/' OR 'DIVIDE'.
        IF number2 <> 0.
          result = number1 / number2.
        ELSE.
          WRITE 'Error: Division by zero.'.
        ENDIF.
      WHEN OTHERS.
        WRITE 'Invalid operator'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_ollama_client) = zcl_ollama_client=>create( ).

  DATA lo_tool_builder TYPE REF TO zif_ollama_tool_builder.
  DATA lv_classname    TYPE string.

  lv_classname = |\\PROGRAM={ sy-repid }\\CLASS=LCL_OLLAMA_USER_TOOL|.

  lo_tool_builder = NEW zcl_ollama_tool_builder( ).

  TRY.
      DATA(response) = lo_ollama_client->chat(
          VALUE #(
              model    = 'mistral:latest'
              messages = VALUE #( ( role    = zif_ollama_chat=>mc_chat_message_roles-user
                                    content = 'Perform an operation using the tool' ) )
              tools    = VALUE #(
                  ( lo_tool_builder->to_request(
                        is_spec = VALUE #(
                            class       = lv_classname
                            description = 'Math operations'
                            parameters  = VALUE #( ( parameter = 'iv_number1' description = 'First number' )
                                                   ( parameter = 'iv_number2' description = 'Second number' )
                                                   ( parameter = 'iv_operator' description = 'Operator' ) ) ) ) ) ) ) ).

      LOOP AT response-message-tool_calls INTO DATA(ls_tool).
        IF ls_tool-function-name = lv_classname.
          DATA(lo_user_tool) = CAST lcl_ollama_user_tool( lo_tool_builder->to_abap( is_response = ls_tool ) ).
          WRITE / |Tool executed with params: { lo_user_tool->number1 } { lo_user_tool->operator } { lo_user_tool->number2 } = { lo_user_tool->result }|.
        ENDIF.
      ENDLOOP.
    CATCH zcx_ollama_message INTO DATA(lx_message).
      WRITE lx_message->get_text( ).
  ENDTRY.
```

---

## How to Contribute

We welcome contributions from the community! To contribute:

1. Fork the repository.  
2. Create a new branch (`git checkout -b feature/AmazingFeature`).  
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).  
4. Push to the branch (`git push origin feature/AmazingFeature`).  
5. Open a Pull Request.  

---

## License

This project is licensed under the **MIT License**. For more details, see the [LICENSE](LICENSE) file.
