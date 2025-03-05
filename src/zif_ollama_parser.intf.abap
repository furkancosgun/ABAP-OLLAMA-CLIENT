"! Provides methods to serialize and deserialize data for communication with the Ollama API.
INTERFACE zif_ollama_parser
  PUBLIC.


  "! Serializes ABAP data into a JSON string.
  "! @parameter data    | The ABAP data to be serialized.
  "! @parameter rv_data | The resulting JSON string.
  "! @parameter options | If true, transforms the 'options' field into a flat structure.
  METHODS serialize
    IMPORTING !data          TYPE any
    RETURNING VALUE(rv_data) TYPE string.

  "! Deserializes a JSON string into ABAP data.
  "! @parameter iv_data | The JSON string to be deserialized.
  "! @parameter data    | The ABAP data structure to populate.
  METHODS deserialize
    IMPORTING VALUE(iv_data) TYPE string
    CHANGING  !data          TYPE any.
ENDINTERFACE.
