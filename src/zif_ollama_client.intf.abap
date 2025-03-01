"! Main interface for the Ollama client, combining all functionalities.
INTERFACE zif_ollama_client
  PUBLIC.
  INTERFACES zif_ollama_connection.
  INTERFACES zif_ollama_model_management.
  INTERFACES zif_ollama_embedding.
  INTERFACES zif_ollama_generation.
  INTERFACES zif_ollama_chat.


  ALIASES check_connection     FOR zif_ollama_connection~check_connection.
  ALIASES get_available_models FOR zif_ollama_model_management~get_available_models.
  ALIASES get_model_details    FOR zif_ollama_model_management~get_model_details.
  ALIASES embed                FOR zif_ollama_embedding~embed.
  ALIASES generate             FOR zif_ollama_generation~generate.
  ALIASES chat                 FOR zif_ollama_chat~chat.
ENDINTERFACE.
