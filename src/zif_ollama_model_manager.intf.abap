"! Provides methods to manage and retrieve information about models.
interface ZIF_OLLAMA_MODEL_MANAGER
  public .


  types:
  " Represents detailed information about a model.
    BEGIN OF ty_model_item_detail_response,
           parent_model       TYPE string, " Parent model name
           format             TYPE string, " Model format
           family             TYPE string, " Model family
           families           TYPE STANDARD TABLE OF string WITH EMPTY KEY,           " List of families
           parameter_size     TYPE string, " Parameter size
           quantization_level TYPE string, " Quantization level
         END OF ty_model_item_detail_response .
  types:
  " Represents an item in the list of available models.
    BEGIN OF ty_model_list_item_response,
           name       TYPE string,                        " Model name
           model      TYPE string,                        " Model identifier
           size       TYPE i,                             " Model size in bytes
           digest     TYPE string,                        " Model digest
           details    TYPE ty_model_item_detail_response, " Model details
           expires_at TYPE timestampl,                    " Expiration timestamp
           size_vram  TYPE i,                             " VRAM size required
         END OF ty_model_list_item_response .
  types:
  " Represents a response containing a list of available models.
    BEGIN OF ty_model_list_response,
           models TYPE STANDARD TABLE OF ty_model_list_item_response WITH EMPTY KEY, " List of models
         END OF ty_model_list_response .
  types:
  " Represents a request to retrieve details of a specific model.
    BEGIN OF ty_model_detail_request,
           model TYPE string, " Model name
         END OF ty_model_detail_request .
  types:
  " Represents detailed information about a specific model, including additional metadata.
    BEGIN OF ty_model_detail_response,
           license     TYPE string,                        " Model license
           modelfile   TYPE string,                        " Model file content
           parameters  TYPE string,                        " Model parameters
           template    TYPE string,                        " Model template
           details     TYPE ty_model_item_detail_response, " Model details
           modified_at TYPE timestampl,                    " Last modified timestamp
         END OF ty_model_detail_response .

  " Retrieves a list of available models.
  methods GET_AVAILABLE_MODELS
    returning
      value(RS_RESPONSE) type TY_MODEL_LIST_RESPONSE
    raising
      ZCX_OLLAMA_MESSAGE .
  " Retrieves detailed information about a specific model.
  methods GET_MODEL_DETAILS
    importing
      !IS_REQUEST type TY_MODEL_DETAIL_REQUEST
    returning
      value(RS_RESPONSE) type TY_MODEL_DETAIL_RESPONSE
    raising
      ZCX_OLLAMA_MESSAGE .
endinterface.
