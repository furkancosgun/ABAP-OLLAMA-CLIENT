"! @class zcl_ollama_client
"! Implements the zif_ollama_client interface to interact with the Ollama API.
"! This class handles HTTP requests, serialization, and deserialization of data.
CLASS zcl_ollama_client DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_ollama_client.

    "! Creates an instance of the Ollama client.
    "! @parameter iv_host     | The host URL of the Ollama API.
    "! @parameter iv_locl     | If true, uses local HTTP calls.
    "! @parameter iv_timo     | The timeout for HTTP requests in seconds.
    "! @parameter it_head     | Optional HTTP headers.
    "! @parameter ro_instance | The instance of the Ollama client.
    CLASS-METHODS create
      IMPORTING iv_host            TYPE string                      DEFAULT zif_ollama_common=>mc_default_host
                iv_locl            TYPE abap_bool                   DEFAULT abap_true
                iv_timo            TYPE i                           DEFAULT zif_ollama_common=>mc_default_timo
                it_head            TYPE zif_ollama_http=>ty_headers OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO zif_ollama_client.

  PRIVATE SECTION.
    "! Constructor for the Ollama client.
    "! @parameter iv_host | The host URL of the Ollama API.
    "! @parameter iv_locl | If true, uses local HTTP calls.
    "! @parameter iv_timo | The timeout for HTTP requests in seconds.
    "! @parameter it_head | Optional HTTP headers.
    METHODS constructor
      IMPORTING iv_host TYPE string
                iv_locl TYPE abap_bool
                iv_timo TYPE i
                it_head TYPE zif_ollama_http=>ty_headers.

    DATA host   TYPE string.
    DATA head   TYPE zif_ollama_http=>ty_headers.
    DATA http   TYPE REF TO zif_ollama_http.
    DATA parser TYPE REF TO zif_ollama_parser.
ENDCLASS.


CLASS zcl_ollama_client IMPLEMENTATION.
  METHOD constructor.
    host = iv_host.
    IF host CP '*/'.
      host = substring( val = host
                        len = strlen( host ) - 1 ).
    ENDIF.
    head = VALUE #( value = 'application/json'
                    ( name   = 'Accept' )
                    ( name   = 'Content-type' ) ).
    LOOP AT it_head ASSIGNING FIELD-SYMBOL(<fs_head>).
      IF line_exists( head[ name = <fs_head>-name ] ).
        CONTINUE.
      ENDIF.
      INSERT <fs_head> INTO TABLE head.
    ENDLOOP.

    IF iv_locl = abap_true.
      http = NEW zcl_ollama_http_local( iv_timeout = iv_timo ).
    ELSE.
      http = NEW zcl_ollama_http_remote( iv_timeout = iv_timo ).
    ENDIF.

    parser = zcl_ollama_parser=>get_instance( ).
  ENDMETHOD.

  METHOD create.
    ro_instance = NEW zcl_ollama_client( iv_host = iv_host
                                         it_head = it_head
                                         iv_locl = iv_locl
                                         iv_timo = iv_timo ).
  ENDMETHOD.

  METHOD zif_ollama_chat~chat.
    IF NOT line_exists( is_request-options[ key = 'stream' ] ).
      INSERT VALUE #( key   = 'stream'
                      value = REF #( abap_false ) ) INTO TABLE is_request-options.
    ENDIF.
    DATA(request) = parser->serialize( data    = is_request
                                       options = abap_true ).
    DATA(response) = http->post( iv_url     = |{ host }/api/chat|
                                 it_headers = head
                                 iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_connection~check_connection.
    TRY.
        me->zif_ollama_model_management~get_available_models( ).
        rv_available = abap_true.
      CATCH zcx_ollama_message.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ollama_embedding~embed.
    IF NOT line_exists( is_request-options[ key = 'stream' ] ).
      INSERT VALUE #( key   = 'stream'
                      value = REF #( abap_false ) ) INTO TABLE is_request-options.
    ENDIF.
    DATA(request) = parser->serialize( data    = is_request
                                       options = abap_true ).
    DATA(response) = http->post( iv_url     = |{ host }/api/embed|
                                 it_headers = head
                                 iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_generation~generate.
    IF NOT line_exists( is_request-options[ key = 'stream' ] ).
      INSERT VALUE #( key   = 'stream'
                      value = REF #( abap_false ) ) INTO TABLE is_request-options.
    ENDIF.

    DATA(request) = parser->serialize( data    = is_request
                                       options = abap_true ).
    DATA(response) = http->post( iv_url     = |{ host }/api/generate|
                                 it_headers = head
                                 iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_model_management~get_available_models.
    DATA(response) = http->get( iv_url     = |{ host }/api/tags|
                                it_headers = head ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_model_management~get_model_details.
    DATA(request) = parser->serialize( data = is_request ).
    DATA(response) = http->post( iv_url     = |{ host }/api/show|
                                 it_headers = head
                                 iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.
ENDCLASS.
