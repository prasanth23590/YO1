CLASS zcl_a5_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: lty_nr_object(10) TYPE c,
           lty_nr_number(20) TYPE n,
           lty_nr_error(1)   TYPE c.

    CONSTANTS: gc_Overhead   TYPE lty_nr_object VALUE 'ZNRA5_YP03',
               gc_Investment TYPE lty_nr_object VALUE 'ZNRA5_YP02',
               gc_Revenue    TYPE lty_nr_object VALUE 'ZNRA5_YP05'.

    CLASS-METHODS: factory RETURNING VALUE(ro_instance) TYPE REF TO zcl_a5_utility.

    METHODS: get_number IMPORTING im_object TYPE lty_nr_object
                        EXPORTING ex_number TYPE lty_nr_number
                        RAISING   cx_root,
      create_NR_intervals IMPORTING im_object       TYPE lty_nr_object
                          RETURNING VALUE(rv_error) TYPE lty_nr_error
                          RAISING   cx_root,
      delete_nr_interval IMPORTING im_object TYPE lty_nr_object
                         RAISING   cx_root,
      reset_nr_interval IMPORTING im_object TYPE lty_nr_object
                        RAISING   cx_root.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: lc_fprjID(7)       TYPE c VALUE '0000001',
               lc_tprjID(7)       TYPE c VALUE '9999999',
               lc_nr_interval(2)  TYPE c VALUE '01',
               lc_insert(1)       TYPE c VALUE 'I',
               lc_update(1)       TYPE c VALUE 'U',
               lc_delete(1)       TYPE c VALUE 'D',
               lc_subobject_prjid TYPE cl_numberrange_intervals=>nr_subobject VALUE ''.

    CLASS-DATA: lo_instance TYPE REF TO zcl_a5_utility.

    METHODS: read_nr_object IMPORTING im_object       TYPE lty_nr_object
                            RETURNING VALUE(rv_exist) TYPE char1
                            RAISING   cx_nr_object_not_found
                                      cx_number_ranges.
ENDCLASS.



CLASS zcl_a5_utility IMPLEMENTATION.
  METHOD: factory.
    IF lo_instance IS NOT BOUND.
      lo_instance = NEW zcl_a5_utility(  ).
    ENDIF.
    ro_instance = lo_instance.
  ENDMETHOD.

  METHOD: read_nr_object.
    cl_numberrange_objects=>read(
      EXPORTING
        language        = sy-langu
        object          = im_object
      IMPORTING
        interval_exists = rv_exist
    ).
  ENDMETHOD.

  METHOD: get_number.
*---- Read Number Range Object
    IF me->read_nr_object( im_object = im_object ) = abap_true.
      TRY.
*---- Read Number Range Object
          CALL METHOD cl_numberrange_intervals=>read
            EXPORTING
              object       = im_object
              nr_range_nr1 = lc_nr_interval
              subobject    = me->lc_subobject_prjid
            IMPORTING
              interval     = DATA(lt_interval).
        CATCH cx_root INTO DATA(lo_obj_nfound).
          IF lo_obj_nfound IS BOUND.
            DATA(lv_error) = me->create_NR_intervals( im_object ).
          ENDIF.
      ENDTRY.

      IF lt_interval IS NOT INITIAL OR lv_error IS INITIAL.
*---- GET Number Range Objects
        CALL METHOD cl_numberrange_runtime=>number_get
          EXPORTING
            nr_range_nr = lc_nr_interval
            object      = im_object
          IMPORTING
            number      = ex_number
            returncode  = DATA(lv_rcode).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD: create_NR_intervals.
    TYPES: ltt_interval TYPE STANDARD TABLE OF cl_numberrange_intervals=>nr_nriv_line WITH EMPTY KEY.

    DATA(lt_interval) = VALUE ltt_interval( ( nrrangenr  = me->lc_nr_interval
                                              fromnumber = me->lc_fprjID
                                              tonumber   = me->lc_tprjid
                                              procind    = me->lc_insert ) ).
*---- Create Number Range Object
    CALL METHOD cl_numberrange_intervals=>create
      EXPORTING
        interval  = lt_interval
        object    = im_object
        subobject = me->lc_subobject_prjid
      IMPORTING
        error     = DATA(lv_error).
  ENDMETHOD.

  METHOD: delete_nr_interval.
    CALL METHOD cl_numberrange_intervals=>read
      EXPORTING
        object       = im_object
        nr_range_nr1 = lc_nr_interval
        subobject    = lc_subobject_prjid
      IMPORTING
        interval     = DATA(lt_interval).

    IF lt_interval IS NOT INITIAL.
      lt_interval[ 1 ]-procind = me->lc_delete.
      CLEAR:lt_interval[ 1 ]-nrlevel.
      CALL METHOD cl_numberrange_intervals=>delete
        EXPORTING
          interval  = lt_interval
          object    = im_object
          subobject = me->lc_subobject_prjid
        IMPORTING
          error     = DATA(lv_error)
          error_inf = DATA(ls_error)
          error_iv  = DATA(lt_error_iv)
          warning   = DATA(lv_warning).
    ENDIF.

  ENDMETHOD.

  METHOD: reset_nr_interval.
    CALL METHOD cl_numberrange_intervals=>read
      EXPORTING
        object       = im_object
        nr_range_nr1 = lc_nr_interval
        subobject    = lc_subobject_prjid
      IMPORTING
        interval     = DATA(lt_interval).

    IF lt_interval IS NOT INITIAL.
      lt_interval[ 1 ]-procind = me->lc_update.
      lt_interval[ 1 ]-nrlevel = 1.
      CALL METHOD cl_numberrange_intervals=>update
        EXPORTING
          interval  = lt_interval
          object    = im_object
          subobject = me->lc_subobject_prjid
        IMPORTING
          error     = DATA(lv_error)
          error_inf = DATA(ls_error)
          error_iv  = DATA(lt_error_iv)
          warning   = DATA(lv_warning).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
