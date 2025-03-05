"! Provides methods to generate embeddings using Ollama models.
INTERFACE zif_ollama_embed
  PUBLIC.


  " Represents an embedding as an array of floating-point numbers.
  TYPES ty_embedding  TYPE STANDARD TABLE OF f WITH EMPTY KEY.
  " Represents a table of embeddings.
  TYPES tt_embeddings TYPE STANDARD TABLE OF ty_embedding WITH EMPTY KEY.
  TYPES:
    " Represents a request for generating embeddings.
    BEGIN OF ty_embed_request,
      model   TYPE string,      " Model name
      input   TYPE string,      " Input text
      options TYPE REF TO data, " Additional options
      stream  TYPE abap_bool,   " Stream
    END OF ty_embed_request.
  TYPES:
    " Represents a response containing embeddings.
    BEGIN OF ty_embed_response,
      model             TYPE string,        " Model name
      embeddings        TYPE tt_embeddings, " Generated embeddings
      total_duration    TYPE i,             " Total duration in milliseconds
      load_duration     TYPE i,             " Load duration in milliseconds
      prompt_eval_count TYPE i,             " Prompt evaluation count
    END OF ty_embed_response.

  METHODS embed
    IMPORTING VALUE(is_request)  TYPE ty_embed_request
    RETURNING VALUE(rs_response) TYPE ty_embed_response
    RAISING   zcx_ollama_message.
ENDINTERFACE.
