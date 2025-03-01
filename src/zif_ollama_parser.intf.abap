"! Provides methods to serialize and deserialize data for communication with the Ollama API.
interface ZIF_OLLAMA_PARSER
  public .


  "! Serializes ABAP data into a JSON string.
  "! @parameter data | The ABAP data to be serialized.
  "! @parameter options | If true, transforms the 'options' field into a flat structure.
  "! @parameter rv_data | The resulting JSON string.
  methods SERIALIZE
    importing
      !DATA type ANY
      !OPTIONS type ABAP_BOOL optional
    returning
      value(RV_DATA) type STRING .
  "! Deserializes a JSON string into ABAP data.
  "! @parameter iv_data | The JSON string to be deserialized.
  "! @parameter data | The ABAP data structure to populate.
  methods DESERIALIZE
    importing
      value(IV_DATA) type STRING
    changing
      !DATA type ANY .
endinterface.
