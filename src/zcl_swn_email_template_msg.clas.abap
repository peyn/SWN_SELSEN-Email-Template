class ZCL_SWN_EMAIL_TEMPLATE_MSG definition
  public
  inheriting from CL_SWN_MESSAGE_STD
  final
  create public .

public section.

  methods IF_SWN_MESSAGE~BUILD
    redefinition .
  methods IF_SWN_MESSAGE~GET_SUBJECT
    redefinition .
protected section.

  methods ENTERED
    redefinition .
  methods LEAVING
    redefinition .
PRIVATE SECTION.

  TYPES:
    BEGIN OF ygs_email_template,
      obj_name TYPE sobj_name,
    END OF ygs_email_template .
  TYPES:
    ygt_email_templates TYPE SORTED TABLE OF ygs_email_template WITH UNIQUE KEY obj_name .
  TYPES:
    BEGIN OF ygs_task,
      wi_id    TYPE sww_wiid,
      task     TYPE sww_task,
      top_task TYPE sww_top_task,
    END OF ygs_task .
  TYPES:
    ygt_tasks TYPE SORTED TABLE OF ygs_task WITH UNIQUE KEY wi_id .

  CLASS-DATA at_email_templates TYPE ygt_email_templates .
  DATA at_tasks TYPE ygt_tasks .

  METHODS load_email_templates .
  METHODS load_tasks .
ENDCLASS.



CLASS ZCL_SWN_EMAIL_TEMPLATE_MSG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SWN_EMAIL_TEMPLATE_MSG->ENTERED
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_METHOD                       TYPE        C (default ='unknown')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD entered.
    m_log->entered( i_class  = 'zcl_swn_email_template_msg' i_method = i_method ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SWN_EMAIL_TEMPLATE_MSG->IF_SWN_MESSAGE~BUILD
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] NOT_IMPLEMENTED
* | [EXC!] BUILD_MESSAGE_FAILED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_swn_message~build.
    DATA:
      lv_obsolete_count TYPE i.

    entered( 'build' ).

    load_tasks( ).
    load_email_templates( ).

    DATA(lt_notifications) = m_notifications.
    CLEAR m_notifications.

    LOOP AT lt_notifications INTO DATA(lo_notification).
      DATA(lv_tabix) = sy-tabix.

      " note 1043630: do not send obsolete notifications
      IF lo_notification->get_is_valid( ) = abap_false.
        lv_obsolete_count = lv_obsolete_count + 1.
        DELETE lt_notifications INDEX lv_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE at_tasks REFERENCE INTO DATA(ld_task)
        WITH TABLE KEY wi_id = lo_notification->app_key.
      IF sy-subrc <> 0.
        DELETE lt_notifications INDEX lv_tabix.
        CONTINUE.
      ENDIF.

      DATA(lv_template_found) = abap_false.
      DO 2 TIMES.
        CASE sy-index.
          WHEN 1.
            CHECK ld_task->task IS NOT INITIAL.
            DATA(lv_template_name) = |YY1_{ substring( val = ld_task->task off = 2 ) }_CRT_ALL|.
          WHEN 2.
            CHECK ld_task->top_task IS NOT INITIAL.
            lv_template_name = |YY1_{ substring( val = ld_task->top_task off = 2 ) }_CRT_ALL|.
        ENDCASE.

        READ TABLE at_email_templates REFERENCE INTO DATA(ld_email_template)
          BINARY SEARCH
          WITH KEY obj_name = lv_template_name.
        CHECK sy-subrc = 0.

        TRY.
            DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = |{ lv_template_name }| ).
            DATA(lt_cds_key) = VALUE if_smtg_email_template=>ty_gt_data_key( ( name = 'WORKFLOWTASKINTERNALID' value = lo_notification->app_key ) ).
			
            lo_email_api->render(
              EXPORTING
                iv_language = 'E'
                it_data_key = lt_cds_key
              IMPORTING
                ev_subject = m_subject
                ev_body_html = body ).

            lv_template_found = abap_true.

            " leave the DO .. ENDDO
            EXIT.
          CATCH cx_smtg_email_common.

            " in case of an exception,
            " try with the next template
            CONTINUE.
        ENDTRY.
      ENDDO.

      CHECK lv_template_found = abap_false.
      RAISE build_message_failed.
    ENDLOOP.

    IF lv_obsolete_count > 0.
      MESSAGE s157(swn) WITH lv_obsolete_count INTO DATA(lv_message).
      m_log->add_message( ).
    ENDIF.

    m_notifications[] = lt_notifications[].

    IF m_notifications[] IS INITIAL.
      RAISE build_message_failed.
    ENDIF.

    leaving( 'build' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SWN_EMAIL_TEMPLATE_MSG->IF_SWN_MESSAGE~GET_SUBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_SUBJECT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_swn_message~get_subject.
    entered( 'get_subject' ).

    IF m_subject IS NOT INITIAL.
      r_subject = m_subject.
      leaving( 'get_subject' ).
      RETURN.
    ENDIF.

    r_subject = super->if_swn_message~get_subject( ).

    leaving( 'get_subject' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SWN_EMAIL_TEMPLATE_MSG->LEAVING
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_METHOD                       TYPE        C (default ='unknown')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD leaving.
    m_log->leaving( i_class  = 'zcl_swn_email_template_msg' i_method = i_method ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SWN_EMAIL_TEMPLATE_MSG->LOAD_EMAIL_TEMPLATES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_email_templates.
    entered( 'load_email_templates' ).

    IF at_email_templates[] IS NOT INITIAL.
      leaving( 'load_email_templates' ).
      RETURN.
    ENDIF.

    SELECT DISTINCT obj_name
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = 'SMTG'
        AND obj_name LIKE 'YY1_%'
        AND delflag = @abap_false
      ORDER BY obj_name
      INTO TABLE @at_email_templates.

    leaving( 'load_email_templates' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SWN_EMAIL_TEMPLATE_MSG->LOAD_TASKS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_tasks.
    DATA:
      ls_task TYPE ygs_task.

    entered( 'load_tasks' ).

    IF at_tasks[] IS NOT INITIAL.
      leaving( 'load_tasks' ).
      RETURN.
    ENDIF.

    LOOP AT m_notifications INTO DATA(lo_notification).
      ls_task-wi_id = lo_notification->app_key.
      INSERT ls_task INTO TABLE at_tasks.
    ENDLOOP.

    " do only if there are any notifications to be processed
    IF sy-subrc <> 0.
      leaving( 'load_tasks' ).
      RETURN.
    ENDIF.

    " get top level tasks (workflows)
    SELECT wi_id, wi_rh_task, top_task
      FROM swwwihead
      FOR ALL ENTRIES IN @at_tasks
      WHERE wi_id = @at_tasks-wi_id
      INTO TABLE @at_tasks.

    leaving( 'load_tasks' ).
  ENDMETHOD.
ENDCLASS.