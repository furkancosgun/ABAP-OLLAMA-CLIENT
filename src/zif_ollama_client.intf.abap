"! Main interface for the Ollama client, combining all functionalities.
INTERFACE zif_ollama_client
  PUBLIC.

  INTERFACES zif_ollama_connection.
  INTERFACES zif_ollama_model_manager.
  INTERFACES zif_ollama_embed.
  INTERFACES zif_ollama_generate.
  INTERFACES zif_ollama_chat.

  ALIASES check_connection     FOR zif_ollama_connection~check_connection.
  ALIASES get_available_models FOR zif_ollama_model_manager~get_available_models.
  ALIASES get_model_details    FOR zif_ollama_model_manager~get_model_details.
  ALIASES embed                FOR zif_ollama_embed~embed.
  ALIASES generate             FOR zif_ollama_generate~generate.
  ALIASES chat                 FOR zif_ollama_chat~chat.


  CONSTANTS: BEGIN OF mc_endpoints,
               chat     TYPE string VALUE '/api/chat',
               embed    TYPE string VALUE '/api/embed',
               generate TYPE string VALUE '/api/generate',
               tags     TYPE string VALUE '/api/tags',
               show     TYPE string VALUE '/api/show',
             END OF mc_endpoints.
  CONSTANTS: BEGIN OF mc_options,
               stream TYPE string VALUE 'stream',
             END OF mc_options.
ENDINTERFACE.
