"! This interface defines common types, structures, and constants for the Ollama package.
"! It is used to standardize request and response structures for API interactions.
INTERFACE zif_ollama_common
  PUBLIC.
  CONSTANTS mc_default_host TYPE string VALUE 'http://localhost:11434' ##NO_TEXT.
  CONSTANTS mc_default_timo TYPE i      VALUE 300 ##NO_TEXT.
ENDINTERFACE.
