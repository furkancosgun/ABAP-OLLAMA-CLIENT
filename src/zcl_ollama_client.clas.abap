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

  PROTECTED SECTION.
    "! Constructor for the Ollama client.
    "! @parameter iv_host | The host URL of the Ollama API.
    "! @parameter iv_locl | If true, uses local HTTP calls.
    "! @parameter iv_timo | The timeout for HTTP requests in seconds.
    "! @parameter it_head | Optional HTTP headers.
    METHODS constructor
      IMPORTING iv_host TYPE string                      DEFAULT zif_ollama_common=>mc_default_host
                iv_locl TYPE abap_bool                   DEFAULT abap_true
                iv_timo TYPE i                           DEFAULT zif_ollama_common=>mc_default_timo
                it_head TYPE zif_ollama_http=>ty_headers OPTIONAL.

    DATA host   TYPE string.
    DATA head   TYPE zif_ollama_http=>ty_headers.
    DATA http   TYPE REF TO zif_ollama_http.
    DATA parser TYPE REF TO zif_ollama_parser.
ENDCLASS.


CLASS zcl_ollama_client IMPLEMENTATION.
  METHOD create.
    ro_instance = NEW zcl_ollama_client( iv_host = iv_host
                                         it_head = it_head
                                         iv_locl = iv_locl
                                         iv_timo = iv_timo ).
  ENDMETHOD.

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

  METHOD zif_ollama_connection~check_connection.
    TRY.
        http->get( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-tags }|
                   it_headers = head ).
        rv_available = abap_true.
      CATCH zcx_ollama_message.
        rv_available = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ollama_chat~chat.
    DATA request  TYPE string.
    DATA response TYPE string.

    is_request-stream = '-'.

    request = parser->serialize( is_request ).
    response = http->post( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-chat }|
                           it_headers = head
                           iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_embed~embed.
    DATA request  TYPE string.
    DATA response TYPE string.

    is_request-stream = '-'.

    request = parser->serialize( is_request ).

    response = http->post( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-embed }|
                           it_headers = head
                           iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_generate~generate.
    DATA request  TYPE string.
    DATA response TYPE string.

    is_request-stream = '-'.

    request = parser->serialize( is_request ).

    response = http->post( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-generate }|
                           it_headers = head
                           iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_model_manager~get_available_models.
    DATA response TYPE string.

    response = http->get( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-tags }|
                          it_headers = head ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.

  METHOD zif_ollama_model_manager~get_model_details.
    DATA request  TYPE string.
    DATA response TYPE string.

    request = parser->serialize( data = is_request ).
    response = http->post( iv_url     = |{ host }{ zif_ollama_client=>mc_endpoints-show }|
                           it_headers = head
                           iv_data    = request ).
    parser->deserialize( EXPORTING iv_data = response
                         CHANGING  data    = rs_response ).
  ENDMETHOD.
ENDCLASS.
