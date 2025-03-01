"! Implements the zif_ollama_http interface to handle HTTP requests.
CLASS zcl_ollama_http_remote DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ollama_http.

    "! Constructor for the HTTP client.
    "! @parameter iv_timeout | The timeout for HTTP requests in seconds.
    METHODS constructor
      IMPORTING iv_timeout TYPE i.

  PRIVATE SECTION.
    DATA timeout TYPE i.

    "! Sends an HTTP request.
    "! @parameter iv_url             | The URL to send the request to.
    "! @parameter iv_method          | The HTTP method (GET, POST, etc.).
    "! @parameter it_headers         | Optional HTTP headers.
    "! @parameter iv_data            | The request body as a string.
    "! @parameter rv_body            | The response body as a string.
    "! @raising   zcx_ollama_message | If an error occurs during the request.
    METHODS send_request
      IMPORTING iv_url         TYPE string
                iv_method      TYPE string
                it_headers     TYPE zif_ollama_http=>ty_headers
                iv_data        TYPE string OPTIONAL
      RETURNING VALUE(rv_body) TYPE string
      RAISING   zcx_ollama_message.

    "! Handles HTTP errors by raising an exception.
    "! @parameter iv_status_code     | The HTTP status code.
    "! @parameter iv_status_text     | The HTTP status text.
    "! @raising   zcx_ollama_message | The exception to raise.
    METHODS handle_http_error
      IMPORTING iv_status_code TYPE i
                iv_status_text TYPE string
      RAISING   zcx_ollama_message.

    "! Prepares HTTP headers for the request.
    "! @parameter it_headers | The headers to prepare.
    "! @parameter rt_headers | The prepared headers.
    METHODS prepare_headers
      IMPORTING it_headers        TYPE zif_ollama_http=>ty_headers
      RETURNING VALUE(rt_headers) TYPE tihttpnvp.
ENDCLASS.


CLASS zcl_ollama_http_remote IMPLEMENTATION.
  METHOD constructor.
    timeout = iv_timeout.
  ENDMETHOD.

  METHOD handle_http_error.
    RAISE EXCEPTION TYPE zcx_ollama_message
      EXPORTING message = |HTTP Error: { iv_status_code } - { iv_status_text }|.
  ENDMETHOD.

  METHOD prepare_headers.
    rt_headers = VALUE #( FOR header IN it_headers
                          ( name  = header-name
                            value = header-value ) ).
  ENDMETHOD.

  METHOD send_request.
    DATA lv_status_code TYPE i.
    DATA lv_status_text TYPE string.

    cl_http_client=>create_by_url( EXPORTING  url    = iv_url
                                   IMPORTING  client = DATA(lo_client)
                                   EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      handle_http_error( iv_status_code = 999
                         iv_status_text = 'Failed to create HTTP client' ).
    ENDIF.

    lo_client->propertytype_logon_popup   = lo_client->co_disabled.
    lo_client->propertytype_accept_cookie = lo_client->co_enabled.
    lo_client->request->set_method( iv_method ).
    lo_client->request->set_header_fields( fields = prepare_headers( it_headers ) ).

    IF iv_data IS NOT INITIAL.
      lo_client->request->set_cdata( iv_data ).
    ENDIF.

    lo_client->send( EXPORTING  timeout = timeout
                     EXCEPTIONS OTHERS  = 1 ).
    IF sy-subrc <> 0.
      lo_client->get_last_error( IMPORTING code    = lv_status_code
                                           message = lv_status_text ).
      handle_http_error( iv_status_code = lv_status_code
                         iv_status_text = lv_status_text ).
    ENDIF.

    lo_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      lo_client->get_last_error( IMPORTING code    = lv_status_code
                                           message = lv_status_text ).
      handle_http_error( iv_status_code = lv_status_code
                         iv_status_text = lv_status_text ).
    ENDIF.

    rv_body = lo_client->response->get_cdata( ).
  ENDMETHOD.

  METHOD zif_ollama_http~get.
    rv_body = send_request( iv_url     = iv_url
                            iv_method  = 'GET'
                            it_headers = it_headers ).
  ENDMETHOD.

  METHOD zif_ollama_http~post.
    rv_body = send_request( iv_url     = iv_url
                            iv_method  = 'POST'
                            it_headers = it_headers
                            iv_data    = iv_data ).
  ENDMETHOD.
ENDCLASS.
