"! Provides methods to check the connection to the Ollama server.
INTERFACE zif_ollama_connection
  PUBLIC.
  METHODS check_connection
    RETURNING VALUE(rv_available) TYPE abap_bool
    RAISING   zcx_ollama_message.
ENDINTERFACE.
