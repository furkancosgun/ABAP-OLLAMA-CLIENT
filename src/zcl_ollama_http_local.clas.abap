"! Implements the zif_ollama_http interface to handle HTTP requests.
CLASS zcl_ollama_http_local DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ollama_http.

    "! Constructor for the HTTP client.
    "! @parameter iv_timeout | The timeout for HTTP requests in seconds.
    METHODS constructor
      IMPORTING iv_timeout TYPE i.

  PRIVATE SECTION.
    TYPES ty_payload TYPE STANDARD TABLE OF char1024 WITH EMPTY KEY.

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

ENDCLASS.


CLASS zcl_ollama_http_local IMPLEMENTATION.
  METHOD constructor.
    timeout = iv_timeout.
  ENDMETHOD.

  METHOD handle_http_error.
    RAISE EXCEPTION TYPE zcx_ollama_message
      EXPORTING message = |HTTP Error: { iv_status_code } - { iv_status_text }|.
  ENDMETHOD.

  METHOD send_request.
    DATA lv_funcname         TYPE funcname.
    DATA lt_request_headers  TYPE ty_payload.
    DATA lt_request_body     TYPE ty_payload.
    DATA lt_response_body    TYPE ty_payload.
    DATA lt_response_headers TYPE ty_payload.
    DATA lv_http_code        TYPE char3.
    DATA lv_http_text        TYPE char1024.

    lv_funcname = |HTTP_{ iv_method }|.

    lt_request_headers = VALUE #( FOR header IN it_headers
                                  ( |{ header-name }: { header-value }| ) ).

    IF iv_data IS NOT INITIAL.
      SPLIT iv_data
            AT cl_abap_char_utilities=>newline
            INTO TABLE lt_request_body.
    ENDIF.

    CALL FUNCTION lv_funcname
      EXPORTING  absolute_uri               = CONV char1024( iv_url )
                 rfc_destination            = 'SAPHTTP'
                 timeout                    = timeout
                 request_entity_body_length = strlen( iv_data )
                 blankstocrlf               = abap_true
      IMPORTING  status_code                = lv_http_code
                 status_text                = lv_http_text
      TABLES     request_entity_body        = lt_request_body
                 response_entity_body       = lt_response_body
                 response_headers           = lt_response_headers
                 request_headers            = lt_request_headers
      EXCEPTIONS OTHERS                     = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              INTO lv_http_text
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      handle_http_error( iv_status_code = '999'
                         iv_status_text = CONV #( lv_http_text ) ).
    ENDIF.
    IF lv_http_code >= 400.
      handle_http_error( iv_status_code = CONV #( lv_http_code )
                         iv_status_text = CONV #( COND #( WHEN lt_response_body IS NOT INITIAL
                                                          THEN concat_lines_of( lt_response_body )
                                                          ELSE lv_http_text ) ) ).
    ENDIF.

    rv_body = concat_lines_of( table = lt_response_body ).
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
