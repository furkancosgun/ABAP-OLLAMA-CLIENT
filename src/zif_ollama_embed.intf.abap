"! Provides methods to generate embeddings using Ollama models.
interface ZIF_OLLAMA_EMBED
  public .


  types:
  " Represents an embedding as an array of floating-point numbers.
    ty_embedding  TYPE STANDARD TABLE OF f WITH EMPTY KEY .
  types:
  " Represents a table of embeddings.
    tt_embeddings TYPE STANDARD TABLE OF ty_embedding WITH EMPTY KEY .
  types:
  " Represents a request for generating embeddings.
    BEGIN OF ty_embed_request,
           model   TYPE string,                        " Model name
           input   TYPE string,                        " Input text
           options TYPE zif_ollama_common=>ty_options, " Additional options
         END OF ty_embed_request .
  types:
  " Represents a response containing embeddings.
    BEGIN OF ty_embed_response,
           model             TYPE string,        " Model name
           embeddings        TYPE tt_embeddings, " Generated embeddings
           total_duration    TYPE i,             " Total duration in milliseconds
           load_duration     TYPE i,             " Load duration in milliseconds
           prompt_eval_count TYPE i,             " Prompt evaluation count
         END OF ty_embed_response .

  methods EMBED
    importing
      value(IS_REQUEST) type TY_EMBED_REQUEST
    returning
      value(RS_RESPONSE) type TY_EMBED_RESPONSE
    raising
      ZCX_OLLAMA_MESSAGE .
endinterface.
