"! Provides methods to perform HTTP requests for communication with the Ollama API.
INTERFACE zif_ollama_http
  PUBLIC.


  types:
  " Represents a key-value pair for HTTP headers.
    BEGIN OF ty_header,
           name  TYPE string, " Header name
           value TYPE string, " Header value
         END OF ty_header .
  types:
  " A hashed table of HTTP headers with unique key 'name'.
    ty_headers TYPE HASHED TABLE OF ty_header WITH UNIQUE KEY name .

  "! Performs an HTTP GET request.
  "! @parameter iv_url             | The URL to send the request to.
  "! @parameter it_headers         | Optional HTTP headers.
  "! @parameter rv_body            | The response body as a string.
  "! @raising   zcx_ollama_message | If an error occurs during the request.
  methods GET
    importing
      !IV_URL type STRING
      !IT_HEADERS type TY_HEADERS optional
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_OLLAMA_MESSAGE .
  "! Performs an HTTP POST request.
  "! @parameter iv_url             | The URL to send the request to.
  "! @parameter it_headers         | Optional HTTP headers.
  "! @parameter iv_data            | The request body as a string.
  "! @parameter rv_body            | The response body as a string.
  "! @raising   zcx_ollama_message | If an error occurs during the request.
  methods POST
    importing
      !IV_URL type STRING
      !IT_HEADERS type TY_HEADERS optional
      !IV_DATA type STRING
    returning
      value(RV_BODY) type STRING
    raising
      ZCX_OLLAMA_MESSAGE .
endinterface.
