"! Provides methods to interact with Ollama models in a chat-based manner.
INTERFACE zif_ollama_chat
  PUBLIC.

  " Types for request parameters
  TYPES:
    BEGIN OF ty_request_parameters,
      type       TYPE string,      " Type of the parameter
      properties TYPE REF TO data, " Reference to the parameter properties
    END OF ty_request_parameters.

  " Types for tool function requests
  TYPES:
    BEGIN OF ty_request_tool_function,
      name        TYPE string,                " Name of the function
      description TYPE string,                " Description of the function
      parameters  TYPE ty_request_parameters, " Parameters of the function
    END OF ty_request_tool_function.

  " Types for tool requests
  TYPES:
    BEGIN OF ty_request_tool,
      function TYPE ty_request_tool_function, " Function details
    END OF ty_request_tool.

  " Table type for multiple tool requests
  TYPES ty_tools_request TYPE STANDARD TABLE OF ty_request_tool WITH EMPTY KEY.

  " Types for function responses
  TYPES:
    BEGIN OF ty_response_function,
      name      TYPE string,      " Name of the function
      arguments TYPE REF TO data, " Reference to the function arguments
    END OF ty_response_function.

  " Types for tool call responses
  TYPES:
    BEGIN OF ty_response_tool_call,
      function TYPE ty_response_function, " Function response details
    END OF ty_response_tool_call.

  " Table type for multiple tool call responses
  TYPES ty_tool_calls_response TYPE STANDARD TABLE OF ty_response_tool_call WITH EMPTY KEY.

  TYPES:
    " Represents a chat message with a role and content.
    BEGIN OF ty_chat_message,
      role    TYPE string, " Message role (system, user, assistant)
      content TYPE string, " Message content
    END OF ty_chat_message.
  TYPES:
    " Represents a chat message with a role and content.
    BEGIN OF ty_chat_message_response,
      role       TYPE string,                 " Message role (system, user, assistant)
      content    TYPE string,                 " Message content
      tool_calls TYPE ty_tool_calls_response, " Tools
    END OF ty_chat_message_response.

  TYPES:
    " Represents a request for chat-based text generation.
    BEGIN OF ty_chat_request,
      model    TYPE string,                        " Model name
      messages TYPE STANDARD TABLE OF ty_chat_message WITH EMPTY KEY,
      tools    TYPE ty_tools_request,              " Tools                               " List of messages
      options  TYPE zif_ollama_common=>ty_options, " Additional options
    END OF ty_chat_request.
  TYPES:
    " Represents a response containing chat-based generated text.
    BEGIN OF ty_chat_response,
      model                TYPE string,                   " Model name
      created_at           TYPE timestampl,               " Creation timestamp
      message              TYPE ty_chat_message_response, " Generated message
      done                 TYPE abap_bool,                " Completion status
      total_duration       TYPE i,                        " Total duration in milliseconds
      load_duration        TYPE i,                        " Load duration in milliseconds
      prompt_eval_duration TYPE i,                        " Prompt evaluation duration
      prompt_eval_count    TYPE i,                        " Prompt evaluation count
      eval_duration        TYPE i,                        " Evaluation duration
      eval_count           TYPE i,                        " Evaluation count
    END OF ty_chat_response.

  CONSTANTS:
    " Defines the roles for chat messages.
    BEGIN OF mc_chat_message_roles,
      system    TYPE string VALUE 'system',    " System role
      user      TYPE string VALUE 'user',      " User role
      assistant TYPE string VALUE 'assistant', " Assistant role
    END OF mc_chat_message_roles.

  METHODS chat
    IMPORTING VALUE(is_request)  TYPE ty_chat_request
    RETURNING VALUE(rs_response) TYPE ty_chat_response
    RAISING   zcx_ollama_message.
ENDINTERFACE.
