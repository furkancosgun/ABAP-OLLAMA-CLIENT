CLASS zcx_ollama_message DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                !message  TYPE string.

    METHODS get_text REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA message TYPE string.
ENDCLASS.


CLASS zcx_ollama_message IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->message = message.
  ENDMETHOD.

  METHOD get_text.
    result = me->message.
  ENDMETHOD.
ENDCLASS.
