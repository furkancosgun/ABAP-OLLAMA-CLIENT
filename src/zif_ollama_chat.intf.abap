"! Provides methods to interact with Ollama models in a chat-based manner.
interface ZIF_OLLAMA_CHAT
  public .


  types:
  " Represents a chat message with a role and content.
    BEGIN OF ty_chat_message,
           role    TYPE string, " Message role (system, user, assistant)
           content TYPE string, " Message content
         END OF ty_chat_message .
  types:
  " Represents a request for chat-based text generation.
    BEGIN OF ty_chat_request,
           model    TYPE string,                        " Model name
           messages TYPE STANDARD TABLE OF ty_chat_message WITH EMPTY KEY,                                  " List of messages
           options  TYPE zif_ollama_common=>ty_options, " Additional options
         END OF ty_chat_request .
  types:
  " Represents a response containing chat-based generated text.
    BEGIN OF ty_chat_response,
           model                TYPE string,          " Model name
           created_at           TYPE timestampl,      " Creation timestamp
           message              TYPE ty_chat_message, " Generated message
           done                 TYPE abap_bool,       " Completion status
           total_duration       TYPE i,               " Total duration in milliseconds
           load_duration        TYPE i,               " Load duration in milliseconds
           prompt_eval_duration TYPE i,               " Prompt evaluation duration
           prompt_eval_count    TYPE i,               " Prompt evaluation count
           eval_duration        TYPE i,               " Evaluation duration
           eval_count           TYPE i,               " Evaluation count
         END OF ty_chat_response .

  constants:
  " Defines the roles for chat messages.
    BEGIN OF mc_chat_message_roles,
               system    TYPE string VALUE 'system',    " System role
               user      TYPE string VALUE 'user',      " User role
               assistant TYPE string VALUE 'assistant', " Assistant role
             END OF mc_chat_message_roles .

  methods CHAT
    importing
      value(IS_REQUEST) type TY_CHAT_REQUEST
    returning
      value(RS_RESPONSE) type TY_CHAT_RESPONSE
    raising
      ZCX_OLLAMA_MESSAGE .
endinterface.
