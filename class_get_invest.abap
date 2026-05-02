CLASS zcl_get_invest DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~get
      REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_get_invest IMPLEMENTATION.

  METHOD if_rest_resource~get.

    DATA: lt_data    TYPE ztt_it_invest,
          lv_ziprper TYPE char5.

    lv_ziprper = mo_request->get_uri_query_parameter(
      iv_name = 'ZIPRPER'
    ).

    TRY.

        CALL FUNCTION 'ZBPC_GET_INVEST'
          EXPORTING
            iv_ziprper = lv_ziprper
          IMPORTING
            et_data    = lt_data.

        IF sy-subrc <> 0.
          mo_response->set_status(
            cl_rest_status_code=>gc_client_error_bad_request
          ).
          RETURN.
        ENDIF.

        DATA(lv_json) = /ui2/cl_json=>serialize(
          data        = lt_data
          pretty_name = /ui2/cl_json=>pretty_mode-low_case
        ).

        mo_response->create_entity( )->set_string_data(
          iv_data = lv_json
        ).

        mo_response->get_entity( )->set_content_type(
          'application/json'
        ).

      CATCH cx_root.
        mo_response->set_status(
          cl_rest_status_code=>gc_client_error_bad_request
        ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.