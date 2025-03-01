"! This interface defines common types, structures, and constants for the Ollama package.
"! It is used to standardize request and response structures for API interactions.
interface ZIF_OLLAMA_COMMON
  public .


  types:
  " Represents a key-value pair for generic options.
    BEGIN OF ty_option,
           key   TYPE string,      " Option key
           value TYPE REF TO data, " Option value (generic)
         END OF ty_option .
  types:
  " A hashed table of key-value pairs with unique key 'key'.
    ty_options TYPE HASHED TABLE OF ty_option WITH UNIQUE KEY key .

  constants MC_DEFAULT_HOST type STRING value 'http://localhost:11434' ##NO_TEXT.
  constants MC_DEFAULT_TIMO type I value 300 ##NO_TEXT.
endinterface.
