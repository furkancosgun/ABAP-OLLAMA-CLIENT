"! Provides methods to generate text using Ollama models.
INTERFACE zif_ollama_generate
  PUBLIC.


  TYPES:
    " Represents a request for text generation.
    BEGIN OF ty_generate_request,
      model   TYPE string,      " Model name
      prompt  TYPE string,      " Input prompt
      options TYPE REF TO data, " Additional options
      stream  TYPE abap_bool,   " Stream
      format  TYPE REF TO data, " Format
    END OF ty_generate_request.
  TYPES:
    " Represents a response containing generated text.
    BEGIN OF ty_generate_response,
      model                TYPE string,     " Model name
      created_at           TYPE timestampl, " Creation timestamp
      response             TYPE string,     " Generated text
      done                 TYPE abap_bool,  " Completion status
      done_reason          TYPE string,     " Reason for completion
      context              TYPE STANDARD TABLE OF i WITH EMPTY KEY,               " Context data
      total_duration       TYPE i,          " Total duration in milliseconds
      load_duration        TYPE i,          " Load duration in milliseconds
      prompt_eval_duration TYPE i,          " Prompt evaluation duration
      prompt_eval_count    TYPE i,          " Prompt evaluation count
      eval_duration        TYPE i,          " Evaluation duration
      eval_count           TYPE i,          " Evaluation count
    END OF ty_generate_response.

  METHODS generate
    IMPORTING VALUE(is_request)  TYPE ty_generate_request
    RETURNING VALUE(rs_response) TYPE ty_generate_response
    RAISING   zcx_ollama_message.
ENDINTERFACE.
