FUNCTION zbpc_get_invest.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IV_ZIPRPER) TYPE  CHAR5
*"  EXPORTING
*"     VALUE(ET_DATA) TYPE  ZTT_IT_INVEST
*"----------------------------------------------------------------------

  DATA: lt_partner    TYPE ztt_d0126,
        ls_partner    TYPE zts_d0126,
        lv_sum_rate   TYPE p LENGTH 16 DECIMALS 7.

  DATA: lt_d096       TYPE ztt_d096,
        ls_d096       TYPE zts_d096,
        lv_days_diff  TYPE i,
        lv_deviation  TYPE p LENGTH 8 DECIMALS 2.

  DATA: lt_risk    TYPE ztt_d006,
        ls_risk    TYPE zts_d006.

  DATA: lt_kpi     TYPE ztt_d036,
        ls_kpi     TYPE zts_d036.

  "------------------------------------------------------------
  " Вычисляем период -1 квартал от iv_ziprper
  "------------------------------------------------------------
  DATA: lv_prev_per TYPE char5,
        lv_year     TYPE i,
        lv_qtr      TYPE i.

  lv_year = iv_ziprper(4).
  lv_qtr  = iv_ziprper+4(1).

  IF lv_qtr = 1.
    lv_year = lv_year - 1.
    lv_qtr  = 4.
  ELSE.
    lv_qtr = lv_qtr - 1.
  ENDIF.
  lv_prev_per = |{ lv_year }{ lv_qtr }|.

  "------------------------------------------------------------
  " Промежуточный тип — все поля et_data + dateto для фильтра
  "------------------------------------------------------------
  TYPES: BEGIN OF ty_tmp,
           /bic/ziprper        TYPE /bic/azip_d016-/bic/ziprper,
           /bic/zportfcom      TYPE /bic/azip_d016-/bic/zportfcom,
           /bic/zportfcomtxt   TYPE /bic/tzportfcom-txtlg,
           /bic/ziproject      TYPE /bic/azip_d016-/bic/ziproject,
           /bic/ziprojecttxt   TYPE /bic/tziproject-txtlg,
           /bic/zipinicpr      TYPE /bic/mziproject-/bic/zipinicpr,
           /bic/zipinicprtxt   TYPE /bic/tzipinicpr-txtlg,
           /bic/zipjustif      TYPE /bic/mziproject-/bic/zipjustif,
           /bic/zipprresp      TYPE /bic/mziproject-/bic/zipprresp,
           /bic/zipprresptxt   TYPE /bic/tzsubsid-txtlg,
           /bic/zipprcond      TYPE /bic/mziproject-/bic/zipprcond,
           /bic/zipprexec      TYPE /bic/mziproject-/bic/zipprexec,
           /bic/ziprimpst      TYPE /bic/mziproject-/bic/ziprimpst,
           /bic/ziprimpsttxt   TYPE /bic/tziprimpst-txtsh,
           /bic/zipprkind      TYPE /bic/mziproject-/bic/zipprkind,
           /bic/zipprkindtxt   TYPE /bic/tzipprkind-txtlg,
           /bic/ziprskind      TYPE /bic/mziproject-/bic/ziprskind,
           /bic/ziprskindtxt   TYPE /bic/tziprskind-txtlg,
           /bic/zipsocind      TYPE /bic/mziproject-/bic/zipsocind,
           /bic/zipgvprog      TYPE /bic/mziproject-/bic/zipgvprog,
           /bic/zipgvprogtxt   TYPE /bic/tzipgvprog-txtlg,
           /bic/ziprimplc      TYPE /bic/mziproject-/bic/ziprimplc,
           /bic/ziprimplctxt   TYPE /bic/tziprimplc-txtmd,
           /bic/ziplace        TYPE /bic/mziproject-/bic/ziplace,
           /bic/zipconcst      TYPE /bic/mziproject-/bic/zipconcst,
           /bic/zipconcsttxt   TYPE /bic/tzipconcst-txtmd,
           /bic/ziprdecrs      TYPE /bic/mziproject-/bic/ziprdecrs,
           /bic/ziprojtyp      TYPE /bic/mziproject-/bic/ziprojtyp,
           /bic/ziprojtyptxt   TYPE /bic/tziprojtyp-txtmd,
           /bic/zipconsmd      TYPE /bic/mziproject-/bic/zipconsmd,
           /bic/zipconsmdtxt   TYPE /bic/tzipconsmd-txtmd,
           /bic/ziprstprs      TYPE /bic/mziproject-/bic/ziprstprs,
           /bic/ziprstprstxt   TYPE /bic/tziprstprs-txtsh,
           /bic/ziprtargt      TYPE /bic/mziproject-/bic/ziprtargt,
           /bic/ziprinflu      TYPE /bic/mziproject-/bic/ziprinflu,
           /bic/ziprodtru      TYPE /bic/mziproject-/bic/ziprodtru,
           currency            TYPE /bic/mziproject-currency,
           /bic/zipingeog      TYPE /bic/mziproject-/bic/zipingeog,
           /bic/zipingeogtxt   TYPE /bic/tzipingeog-txtmd,
           /bic/zipcap         TYPE /bic/mziproject-/bic/zipcap,
           dateto              TYPE /bic/mziproject-dateto,
         END OF ty_tmp.

  DATA: lt_tmp TYPE STANDARD TABLE OF ty_tmp,
        ls_tmp LIKE LINE OF lt_tmp,
        ls_et  LIKE LINE OF et_data.

  CLEAR et_data.

  "------------------------------------------------------------
  " 1. SELECT основных данных проекта
  "------------------------------------------------------------
    SELECT DISTINCT
        d016~/bic/ziprper        AS /bic/ziprper,
        d016~/bic/zportfcom      AS /bic/zportfcom,
        t_portfcom~txtlg         AS /bic/zportfcomtxt,
        d016~/bic/ziproject      AS /bic/ziproject,
        t_iproject~txtlg         AS /bic/ziprojecttxt,
        mproj~/bic/zipinicpr     AS /bic/zipinicpr,
        t_ipinicpr~txtlg         AS /bic/zipinicprtxt,
        mproj~/bic/zipjustif     AS /bic/zipjustif,
        mproj~/bic/zipprresp     AS /bic/zipprresp,
        t_pprresp~txtlg          AS /bic/zipprresptxt,
        mproj~/bic/zipprcond     AS /bic/zipprcond,
        mproj~/bic/zipprexec     AS /bic/zipprexec,
        mproj~/bic/ziprimpst     AS /bic/ziprimpst,
        t_rimpst~txtsh           AS /bic/ziprimpsttxt,
        mproj~/bic/zipprkind     AS /bic/zipprkind,
        t_pprkind~txtlg          AS /bic/zipprkindtxt,
        mproj~/bic/ziprskind     AS /bic/ziprskind,
        t_prskind~txtlg          AS /bic/ziprskindtxt,
        mproj~/bic/zipsocind     AS /bic/zipsocind,
        mproj~/bic/zipgvprog     AS /bic/zipgvprog,
        t_gvprog~txtlg           AS /bic/zipgvprogtxt,
        mproj~/bic/ziprimplc     AS /bic/ziprimplc,
        t_rimplc~txtmd           AS /bic/ziprimplctxt,
        mproj~/bic/ziplace       AS /bic/ziplace,
        mproj~/bic/zipconcst     AS /bic/zipconcst,
        t_concst~txtmd           AS /bic/zipconcsttxt,
        mproj~/bic/ziprdecrs     AS /bic/ziprdecrs,
        mproj~/bic/ziprojtyp     AS /bic/ziprojtyp,
        t_rojtyp~txtmd           AS /bic/ziprojtyptxt,
        mproj~/bic/zipconsmd     AS /bic/zipconsmd,
        t_consmd~txtmd           AS /bic/zipconsmdtxt,
        mproj~/bic/ziprstprs     AS /bic/ziprstprs,
        t_rstprs~txtsh           AS /bic/ziprstprstxt,
        mproj~/bic/ziprtargt     AS /bic/ziprtargt,
        mproj~/bic/ziprinflu     AS /bic/ziprinflu,
        mproj~/bic/ziprodtru     AS /bic/ziprodtru,
        mproj~currency           AS currency,
        mproj~/bic/zipingeog     AS /bic/zipingeog,
        t_ingeog~txtmd           AS /bic/zipingeogtxt,
        mproj~/bic/zipcap        AS /bic/zipcap
      FROM /bic/azip_d016 AS d016


      INNER JOIN /bic/mziproject AS mproj
        ON mproj~/bic/ziproject = d016~/bic/ziproject
       AND mproj~dateto         = '99991231'

      " ← Текстовые таблицы (все LEFT JOIN с langu='R')
      LEFT JOIN /bic/tzportfcom  AS t_portfcom
        ON t_portfcom~/bic/zportfcom = d016~/bic/zportfcom
       AND t_portfcom~langu = 'R'
      LEFT JOIN /bic/tziproject  AS t_iproject
        ON t_iproject~/bic/ziproject = d016~/bic/ziproject
       AND t_iproject~langu = 'R'
      LEFT JOIN /bic/tzipinicpr  AS t_ipinicpr
        ON t_ipinicpr~/bic/zipinicpr = mproj~/bic/zipinicpr
       AND t_ipinicpr~langu = 'R'
      LEFT JOIN /bic/tzsubsid    AS t_pprresp
        ON t_pprresp~/bic/zsubsid = mproj~/bic/zipprresp
       AND t_pprresp~langu = 'R'
      LEFT JOIN /bic/tziprimpst  AS t_rimpst
        ON t_rimpst~/bic/ziprimpst = mproj~/bic/ziprimpst
       AND t_rimpst~langu = 'R'
      LEFT JOIN /bic/tzipprkind  AS t_pprkind
        ON t_pprkind~/bic/zipprkind = mproj~/bic/zipprkind
       AND t_pprkind~langu = 'R'
      LEFT JOIN /bic/tziprskind  AS t_prskind
        ON t_prskind~/bic/ziprskind = mproj~/bic/ziprskind
       AND t_prskind~langu = 'R'
      LEFT JOIN /bic/tzipgvprog  AS t_gvprog
        ON t_gvprog~/bic/zipgvprog = mproj~/bic/zipgvprog
       AND t_gvprog~langu = 'R'
      LEFT JOIN /bic/tziprimplc  AS t_rimplc
        ON t_rimplc~/bic/ziprimplc = mproj~/bic/ziprimplc
       AND t_rimplc~langu = 'R'
      LEFT JOIN /bic/tzipconcst  AS t_concst
        ON t_concst~/bic/zipconcst = mproj~/bic/zipconcst
       AND t_concst~langu = 'R'
      LEFT JOIN /bic/tziprojtyp  AS t_rojtyp
        ON t_rojtyp~/bic/ziprojtyp = mproj~/bic/ziprojtyp
       AND t_rojtyp~langu = 'R'
      LEFT JOIN /bic/tzipconsmd  AS t_consmd
        ON t_consmd~/bic/zipconsmd = mproj~/bic/zipconsmd
       AND t_consmd~langu = 'R'
      LEFT JOIN /bic/tziprstprs  AS t_rstprs
        ON t_rstprs~/bic/ziprstprs = mproj~/bic/ziprstprs
       AND t_rstprs~langu = 'R'
      LEFT JOIN /bic/tzipingeog  AS t_ingeog
        ON t_ingeog~/bic/zipingeog = mproj~/bic/zipingeog
       AND t_ingeog~langu = 'R'

      WHERE d016~/bic/ziprper   = @iv_ziprper
        AND mproj~/bic/ziprimpst <> 'PS_99'

      INTO CORRESPONDING FIELDS OF TABLE @et_data.

    IF et_data IS INITIAL.
      RETURN.
    ENDIF.

    SORT et_data BY /bic/ziprper /bic/zportfcom /bic/ziproject.

*  SELECT COUNT( DISTINCT /bic/ziproject )
*  FROM /bic/azip_d016
*  WHERE /bic/ziprper = '20242'
*  INTO @DATA(lv_count_d016).
*
*  WRITE: / 'Проектов в D016:', lv_count_d016.
*
*    SELECT COUNT( DISTINCT d016~/bic/ziproject )
*      FROM /bic/azip_d016 AS d016
*      INNER JOIN /bic/mziproject AS mproj
*        ON mproj~/bic/ziproject = d016~/bic/ziproject
*       AND mproj~dateto = '99991231'
*      WHERE d016~/bic/ziprper = '20252'
*        AND mproj~/bic/ziprimpst <> 'PS_99'
*      INTO @DATA(lv_count_active).
*
*    WRITE: / 'Активных проектов:', lv_count_active.

  "------------------------------------------------------------
  " п.41: Партнеры — /BIC/AZIP_C016
  "------------------------------------------------------------
  SELECT
      c016~/bic/ziprper   AS ziprper,
      c016~/bic/zportfcom AS zportfcom,
      c016~/bic/ziproject AS ziproject,
      c016~/bic/zippartnr AS zippartnr,
      t_partnr~txtlg      AS zippartnrtxt,
      SUM( c016~/bic/zipprrate ) AS zipprrate
    FROM /bic/azip_c016 AS c016
    LEFT JOIN /bic/tzippartnr AS t_partnr
      ON t_partnr~/bic/zippartnr = c016~/bic/zippartnr
     AND t_partnr~langu = 'R'
    WHERE c016~/bic/ziprper = @iv_ziprper
    GROUP BY
      c016~/bic/ziprper,
      c016~/bic/zportfcom,
      c016~/bic/ziproject,
      c016~/bic/zippartnr,
      t_partnr~txtlg
    INTO TABLE @DATA(lt_partner_raw).

  SORT lt_partner_raw BY ziprper zportfcom ziproject zippartnr.

  "------------------------------------------------------------
  " п.42: Сроки реализации — /BIC/AZIP_D096
  " FIX: добавлен GROUP BY чтобы убрать дубликаты на уровне SELECT,
  "      поле zipctask берём через MAX, zipdecrsn через MAX
  "------------------------------------------------------------
  SELECT
      d096~/bic/ziprper        AS ziprper,
      d096~/bic/zportfcom      AS zportfcom,
      d096~/bic/ziproject      AS ziproject,
      d096~/bic/ziprtask       AS ziprtask,
      MAX( t_task~txtlg )      AS ziprtasktxt,
      MAX( d096~/bic/zipctask )  AS zipctask,
      MAX( d096~/bic/zipfindat ) AS zipfindat_plan,
      MAX( d096~/bic/zipdecrsn ) AS zipdecrsn
    FROM /bic/azip_d096 AS d096
    LEFT JOIN /bic/tziprtask AS t_task
      ON t_task~/bic/ziprtask = d096~/bic/ziprtask
     AND t_task~langu = 'R'
    WHERE d096~/bic/ziprper   = @iv_ziprper
      AND d096~/bic/zipdttype = 'DT_01'
      AND d096~/bic/zipstctrl = 'QP00'
      AND d096~/bic/zipbeflag = ''
    GROUP BY
      d096~/bic/ziprper,
      d096~/bic/zportfcom,
      d096~/bic/ziproject,
      d096~/bic/ziprtask
    INTO TABLE @DATA(lt_d096_plan).

  SORT lt_d096_plan BY ziprper zportfcom ziproject ziprtask.

  SELECT
      d096~/bic/ziprper   AS ziprper,
      d096~/bic/zportfcom AS zportfcom,
      d096~/bic/ziproject AS ziproject,
      d096~/bic/ziprtask  AS ziprtask,
      MAX( d096~/bic/zipfindat ) AS zipfindat_fact
    FROM /bic/azip_d096 AS d096
    WHERE d096~/bic/ziprper   = @iv_ziprper
      AND d096~/bic/zipdttype = 'DT_02'
      AND d096~/bic/zipstctrl = 'QF00'
      AND d096~/bic/zipbeflag = ''
    GROUP BY
      d096~/bic/ziprper,
      d096~/bic/zportfcom,
      d096~/bic/ziproject,
      d096~/bic/ziprtask
    INTO TABLE @DATA(lt_d096_fact).

  SORT lt_d096_fact BY ziprper zportfcom ziproject ziprtask.

  SELECT
      d096~/bic/ziprper   AS ziprper,
      d096~/bic/zportfcom AS zportfcom,
      d096~/bic/ziproject AS ziproject,
      d096~/bic/ziprtask  AS ziprtask,
      MAX( d096~/bic/zipfindat ) AS zipfindat_fcst
    FROM /bic/azip_d096 AS d096
    WHERE d096~/bic/ziprper   = @iv_ziprper
      AND d096~/bic/zipdttype = 'DT_04'
      AND d096~/bic/zipstctrl = 'QF00'
      AND d096~/bic/zipbeflag = ''
    GROUP BY
      d096~/bic/ziprper,
      d096~/bic/zportfcom,
      d096~/bic/ziproject,
      d096~/bic/ziprtask
    INTO TABLE @DATA(lt_d096_fcst).

  SORT lt_d096_fcst BY ziprper zportfcom ziproject ziprtask.

  "------------------------------------------------------------
  " п.43: Риски проекта — /BIC/AZIP_D006
  " FIX: убран фильтр >= 1 (тип NUMC1 — некорректное сравнение),
  "      исключаем только пустые/нулевые значения
  "------------------------------------------------------------
  SELECT
      d006~/bic/ziprper   AS ziprper,
      d006~/bic/zportfcom AS zportfcom,
      d006~/bic/ziproject AS ziproject,
      d006~/bic/zipriskcl AS zipriskcl,
      t_riskcl~txtlg      AS zipriskcltxt,
      d006~/bic/zipriskdc AS zipriskdc,
      d006~/bic/ziprisklm AS ziprisklm,
      d006~/bic/ziprisksr AS ziprisksr,
      t_risksr~txtsh      AS ziprisksrtxt,
      d006~/bic/zipriskmt AS zipriskmt
    FROM /bic/azip_d006 AS d006
    LEFT JOIN /bic/tzipriskcl AS t_riskcl
      ON t_riskcl~/bic/zipriskcl = d006~/bic/zipriskcl
     AND t_riskcl~langu = 'R'
    LEFT JOIN /bic/tziprisksr AS t_risksr
      ON t_risksr~/bic/ziprisksr = d006~/bic/ziprisksr
    WHERE d006~/bic/ziprper = @iv_ziprper
      AND NOT (
           ( d006~/bic/zipriskdc IS INITIAL OR d006~/bic/zipriskdc = '' )
       AND ( d006~/bic/ziprisklm IS INITIAL OR d006~/bic/ziprisklm = '' )
       AND ( d006~/bic/zipriskmt IS INITIAL OR d006~/bic/zipriskmt = '' )
       AND ( t_risksr~txtsh IS INITIAL OR t_risksr~txtsh = '' )
       AND ( d006~/bic/ziprisksr IS INITIAL OR d006~/bic/ziprisksr = 0 )
      )
    INTO TABLE @DATA(lt_d006_raw).

  SORT lt_d006_raw BY ziprper zportfcom ziproject zipriskcl.

  "------------------------------------------------------------
  " п.44: КПЭ проекта — /BIC/AZIP_C026 + /BIC/AZIP_D036
  "------------------------------------------------------------

  " 44.3 + 44.4: CR / DT_01 текущий период
  SELECT
      c026~/bic/zportfcom        AS zportfcom,
      c026~/bic/ziproject        AS ziproject,
      c026~/bic/ziparticl        AS ziparticl,
      t_articl~txtlg             AS ziparticltxt,
      MAX( c026~/bic/zipproglc ) AS zipproglc,
      SUM( c026~/bic/zipsumm )   AS zipsummln,
      SUM( c026~/bic/zipsumth )  AS zipsumth
    FROM /bic/azip_c026 AS c026
    LEFT JOIN /bic/tziparticl AS t_articl
      ON t_articl~/bic/ziparticl = c026~/bic/ziparticl
     AND t_articl~langu = 'R'
    WHERE c026~/bic/ziprper   = @iv_ziprper
      AND c026~/bic/zipversn  = 'CR'
      AND c026~/bic/zipdttype = 'DT_01'
      AND c026~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND c026~calyear     = '0000'
      AND c026~calquarter  = '00000'
    GROUP BY
      c026~/bic/zportfcom,
      c026~/bic/ziproject,
      c026~/bic/ziparticl,
      t_articl~txtlg
    INTO TABLE @DATA(lt_c026_cr).

  SORT lt_c026_cr BY zportfcom ziproject ziparticl.

  " 44.5: период -1
  SELECT
      c026~/bic/zportfcom      AS zportfcom,
      c026~/bic/ziproject      AS ziproject,
      c026~/bic/ziparticl      AS ziparticl,
      SUM( c026~/bic/zipsumm ) AS zipsummln_prev
    FROM /bic/azip_c026 AS c026
    WHERE c026~/bic/ziprper   = @lv_prev_per
      AND c026~/bic/zipversn  = 'CR'
      AND c026~/bic/zipdttype = 'DT_01'
      AND c026~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND c026~calyear     = '0000'
      AND c026~calquarter  = '00000'
    GROUP BY
      c026~/bic/zportfcom,
      c026~/bic/ziproject,
      c026~/bic/ziparticl
    INTO TABLE @DATA(lt_c026_prev).

  SORT lt_c026_prev BY zportfcom ziproject ziparticl.

  " 44.7: причина отклонения PREV (ziprsntyp = '01')
  " FIX: сортировка включает zipproglc для корректного BINARY SEARCH
  SELECT
      d036~/bic/zportfcom  AS zportfcom,
      d036~/bic/ziproject  AS ziproject,
      d036~/bic/ziparticl  AS ziparticl,
      d036~/bic/zipproglc  AS zipproglc,
      d036~/bic/zipdecrsn  AS zipdecrsn_prev
    FROM /bic/azip_d036 AS d036
    WHERE d036~/bic/ziprper   = @iv_ziprper
      AND d036~/bic/ziprsntyp = '01'
      AND d036~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND d036~calyear     = '0000'
      AND d036~calquarter  = '00000'
    INTO TABLE @DATA(lt_d036_rsn01).

  SORT lt_d036_rsn01 BY zportfcom ziproject ziparticl zipproglc.

  " 44.8: план на этапе Выбор CP_20
  " FIX: после SELECT оставляем только MAX(ziprper) на группу — берём последний период
  SELECT
      c026~/bic/zportfcom      AS zportfcom,
      c026~/bic/ziproject      AS ziproject,
      c026~/bic/ziparticl      AS ziparticl,
      c026~/bic/ziprper        AS ziprper,
      SUM( c026~/bic/zipsumm ) AS zipsummln_select
    FROM /bic/azip_c026 AS c026
    WHERE c026~/bic/zipproglc = 'CP_20'
      AND c026~/bic/ziprper  <= @iv_ziprper
      AND c026~/bic/zipversn = 'CR'
      AND c026~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND c026~calyear     = '0000'
      AND c026~calquarter  = '00000'
    GROUP BY
      c026~/bic/zportfcom,
      c026~/bic/ziproject,
      c026~/bic/ziparticl,
      c026~/bic/ziprper
    INTO TABLE @DATA(lt_c026_cp20).

  " FIX: оставляем только запись с максимальным периодом по каждой статье
  SORT lt_c026_cp20 BY zportfcom ziproject ziparticl ziprper DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_c026_cp20 COMPARING zportfcom ziproject ziparticl.
  SORT lt_c026_cp20 BY zportfcom ziproject ziparticl.

  " 44.9: причина отклонения SELECT (CP_20, ziprsntyp = '01')
  " FIX: аналогично берём последний период
  SELECT
      d036~/bic/zportfcom  AS zportfcom,
      d036~/bic/ziproject  AS ziproject,
      d036~/bic/ziparticl  AS ziparticl,
      d036~/bic/ziprper    AS ziprper,
      d036~/bic/zipdecrsn  AS zipdecrsn_select
    FROM /bic/azip_d036 AS d036
    WHERE d036~/bic/ziprsntyp = '01'
      AND d036~/bic/zipproglc = 'CP_20'
      AND d036~/bic/ziprper  <= @iv_ziprper
      AND d036~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND d036~calyear     = '0000'
      AND d036~calquarter  = '00000'
    INTO TABLE @DATA(lt_d036_cp20).

  " FIX: оставляем только запись с максимальным периодом по каждой статье
  SORT lt_d036_cp20 BY zportfcom ziproject ziparticl ziprper DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_d036_cp20 COMPARING zportfcom ziproject ziparticl.
  SORT lt_d036_cp20 BY zportfcom ziproject ziparticl.

  " 44.10: план на этапе Определения CP_30
  " FIX: аналогично берём последний период
  SELECT
      c026~/bic/zportfcom      AS zportfcom,
      c026~/bic/ziproject      AS ziproject,
      c026~/bic/ziparticl      AS ziparticl,
      c026~/bic/ziprper        AS ziprper,
      SUM( c026~/bic/zipsumm ) AS zipsummln_define
    FROM /bic/azip_c026 AS c026
    WHERE c026~/bic/zipproglc = 'CP_30'
      AND c026~/bic/zipversn = 'CR'
      AND c026~/bic/ziprper  <= @iv_ziprper
      AND c026~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND c026~calyear     = '0000'
      AND c026~calquarter  = '00000'
    GROUP BY
      c026~/bic/zportfcom,
      c026~/bic/ziproject,
      c026~/bic/ziparticl,
      c026~/bic/ziprper
    INTO TABLE @DATA(lt_c026_cp30).

  " FIX: оставляем только запись с максимальным периодом по каждой статье
  SORT lt_c026_cp30 BY zportfcom ziproject ziparticl ziprper DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_c026_cp30 COMPARING zportfcom ziproject ziparticl.
  SORT lt_c026_cp30 BY zportfcom ziproject ziparticl.

  " 44.11: причина отклонения DEFINE (CP_30, ziprsntyp = '05')
  SELECT
      d036~/bic/zportfcom  AS zportfcom,
      d036~/bic/ziproject  AS ziproject,
      d036~/bic/ziparticl  AS ziparticl,
      d036~/bic/zipdecrsn  AS zipdecrsn_define
    FROM /bic/azip_d036 AS d036
    WHERE d036~/bic/ziprper   = @iv_ziprper
      AND d036~/bic/ziprsntyp = '05'
      AND d036~/bic/zipproglc = 'CP_30'
      AND d036~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND d036~calyear     = '0000'
      AND d036~calquarter  = '00000'
    INTO TABLE @DATA(lt_d036_cp30).

  SORT lt_d036_cp30 BY zportfcom ziproject ziparticl.

" 44.12: Утверждённый план UV / DT_01 (период -1, как в BI)
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsummln_approved
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/ziprper   = @lv_prev_per
    AND c026~/bic/zipversn  = 'UV'
    AND c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/ziparticl IN (
        'IA05010000',
        'IA05040000',
        'IA05050000',
        'IA05060000',
        'IA05110000',
        'IA05120000',
        'IA05130000',
        'IA05220000',
        'IA17010000',
        'IA17020000',
        'IA17030000'
      )
    AND c026~calyear     = '0000'
    AND c026~calquarter  = '00000'
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_uv).

SORT lt_c026_uv BY zportfcom ziproject ziparticl.

  " 44.14: причина отклонения APPROVED (ziprsntyp = '05')
  SELECT
      d036~/bic/zportfcom  AS zportfcom,
      d036~/bic/ziproject  AS ziproject,
      d036~/bic/ziparticl  AS ziparticl,
      d036~/bic/zipdecrsn  AS zipdecrsn_approved
    FROM /bic/azip_d036 AS d036
    WHERE d036~/bic/ziprper   = @iv_ziprper
      AND d036~/bic/ziprsntyp = '05'
      AND d036~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND d036~calyear     = '0000'
      AND d036~calquarter  = '00000'
    INTO TABLE @DATA(lt_d036_rsn05).

  SORT lt_d036_rsn05 BY zportfcom ziproject ziparticl.

  " 44.15: прогноз DT_04
  SELECT
      c026~/bic/zportfcom      AS zportfcom,
      c026~/bic/ziproject      AS ziproject,
      c026~/bic/ziparticl      AS ziparticl,
      SUM( c026~/bic/zipsumm ) AS zipsummln_forecast
    FROM /bic/azip_c026 AS c026
    WHERE c026~/bic/ziprper   = @iv_ziprper
      AND c026~/bic/zipdttype = 'DT_04'
      AND c026~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND c026~calyear     = '0000'
      AND c026~calquarter  = '00000'
    GROUP BY
      c026~/bic/zportfcom,
      c026~/bic/ziproject,
      c026~/bic/ziparticl
    INTO TABLE @DATA(lt_c026_fcst).

  SORT lt_c026_fcst BY zportfcom ziproject ziparticl.

  " 44.17: причина отклонения FORECAST (ziprsntyp = '04')
  SELECT
      d036~/bic/zportfcom  AS zportfcom,
      d036~/bic/ziproject  AS ziproject,
      d036~/bic/ziparticl  AS ziparticl,
      d036~/bic/zipdecrsn  AS zipdecrsn_forecast
    FROM /bic/azip_d036 AS d036
    WHERE d036~/bic/ziprper   = @iv_ziprper
      AND d036~/bic/ziprsntyp = '04'
      AND d036~/bic/ziparticl IN (
          'IA05010000',
          'IA05040000',
          'IA05050000',
          'IA05060000',
          'IA05110000',
          'IA05120000',
          'IA05130000',
          'IA05220000',
          'IA17010000',
          'IA17020000',
          'IA17030000'
        )
      AND d036~calyear     = '0000'
      AND d036~calquarter  = '00000'
    INTO TABLE @DATA(lt_d036_rsn04).

  SORT lt_d036_rsn04 BY zportfcom ziproject ziparticl.


"============================================================
" п.45: Бюджет — /BIC/AZIP_C026
"============================================================

DATA: lv_calyear_num  TYPE i,
      lv_prev_year    TYPE i,
      lv_calyear_p1   TYPE i,
      lv_calyear_p2   TYPE i,
      lv_calyear_p3   TYPE i,
      lv_calyear_p4   TYPE i,
      lv_calyear_p5   TYPE i,
      lv_year_str     TYPE char4.

IF iv_ziprper CP '?.----'.
  lv_year_str = iv_ziprper(4).
ELSE.
  lv_year_str = iv_ziprper(4).
ENDIF.

lv_calyear_num = lv_year_str.
lv_prev_year   = lv_calyear_num - 1.
lv_calyear_p1  = lv_calyear_num + 1.
lv_calyear_p2  = lv_calyear_num + 2.
lv_calyear_p3  = lv_calyear_num + 3.
lv_calyear_p4  = lv_calyear_num + 4.
lv_calyear_p5  = lv_calyear_num + 5.

DATA: lv_q1 TYPE char5,
      lv_q2 TYPE char5,
      lv_q3 TYPE char5,
      lv_q4 TYPE char5.

lv_q1 = |{ lv_calyear_num }1|.
lv_q2 = |{ lv_calyear_num }2|.
lv_q3 = |{ lv_calyear_num }3|.
lv_q4 = |{ lv_calyear_num }4|.

"------------------------------------------------------------
" 45.6 Факт ДО периода — все годы строго < (выбранный год - 1)
" т.е. если период 20252, то берём calyear < 2024
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    t_articl~txtlg           AS ziparticltxt,
    c026~/bic/zipproduc      AS zipproduc,
    t_produc~txtlg           AS zipproductxt,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fb
  FROM /bic/azip_c026 AS c026
  LEFT JOIN /bic/tziparticl AS t_articl
    ON t_articl~/bic/ziparticl = c026~/bic/ziparticl
   AND t_articl~langu = 'R'
  LEFT JOIN /bic/tzipproduc AS t_produc
    ON t_produc~/bic/zipproduc = c026~/bic/zipproduc
   AND t_produc~langu = 'R'
  WHERE c026~/bic/zipdttype  = 'DT_02'
    AND c026~/bic/zipstctrl  = 'QF00'
    AND c026~/bic/ziprper    = @iv_ziprper
    AND c026~calyear         < @lv_prev_year    " строго меньше (год-1)
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl,
    t_articl~txtlg,
    c026~/bic/zipproduc,
    t_produc~txtlg
  INTO TABLE @DATA(lt_c026_bud_fb).

SORT lt_c026_bud_fb BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.6 Факт ЗА предыдущий год — calyear = выбранный год - 1
" т.е. если период 20252, то берём calyear = 2024
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    t_articl~txtlg           AS ziparticltxt,
    c026~/bic/zipproduc      AS zipproduc,
    t_produc~txtlg           AS zipproductxt,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fc
  FROM /bic/azip_c026 AS c026
  LEFT JOIN /bic/tziparticl AS t_articl
    ON t_articl~/bic/ziparticl = c026~/bic/ziparticl
   AND t_articl~langu = 'R'
  LEFT JOIN /bic/tzipproduc AS t_produc
    ON t_produc~/bic/zipproduc = c026~/bic/zipproduc
   AND t_produc~langu = 'R'
  WHERE c026~/bic/zipdttype  = 'DT_02'
    AND c026~/bic/zipstctrl  = 'QF00'
    AND c026~/bic/ziprper    = @iv_ziprper
    AND c026~calyear         = @lv_prev_year    " ровно год-1
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl,
    t_articl~txtlg,
    c026~/bic/zipproduc,
    t_produc~txtlg
  INTO TABLE @DATA(lt_c026_bud_fc).

SORT lt_c026_bud_fc BY zportfcom ziproject ziparticl.

*"------------------------------------------------------------
*" 45.6 Факт ЗА ВЫБРАННЫЙ период
*"------------------------------------------------------------
*SELECT
*    c026~/bic/zportfcom      AS zportfcom,
*    c026~/bic/ziproject      AS ziproject,
*    c026~/bic/ziparticl      AS ziparticl,
*    t_articl~txtlg           AS ziparticltxt,
*    c026~/bic/zipproduc      AS zipproduc,
*    t_produc~txtlg           AS zipproductxt,
*    SUM( c026~/bic/zipsumm ) AS zipsumm_fc
*  FROM /bic/azip_c026 AS c026
*  LEFT JOIN /bic/tziparticl AS t_articl
*    ON t_articl~/bic/ziparticl = c026~/bic/ziparticl
*   AND t_articl~langu = 'R'
*  LEFT JOIN /bic/tzipproduc AS t_produc
*    ON t_produc~/bic/zipproduc = c026~/bic/zipproduc
*   AND t_produc~langu = 'R'
*  WHERE c026~/bic/zipdttype = 'DT_02'
*    AND c026~/bic/zipstctrl = 'QF00'
*    AND c026~/bic/ziprper   = @iv_ziprper
*    AND c026~calyear = @lv_prev_year
*    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
*          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
*          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
*          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
*          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
*          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
*          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
*          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
*          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
*          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
*          'IA03040800','IA03040900','IA03041000','IA03041100' )
*  GROUP BY
*    c026~/bic/zportfcom,
*    c026~/bic/ziproject,
*    c026~/bic/ziparticl,
*    t_articl~txtlg,
*    c026~/bic/zipproduc,
*    t_produc~txtlg
*  INTO TABLE @lt_c026_bud_fc.
*
*SORT lt_c026_bud_fc BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.7 План за год
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_py
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_num
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_py).

SORT lt_c026_bud_py BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.8-45.11 План по кварталам
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    c026~calquarter         AS calquarter,
    SUM( c026~/bic/zipsumm ) AS zipsumm_pq
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_num
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~calquarter    IN ( @lv_q1, @lv_q2, @lv_q3, @lv_q4 )
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl,
    c026~calquarter
  INTO TABLE @DATA(lt_c026_bud_pq).

SORT lt_c026_bud_pq BY zportfcom ziproject ziparticl calquarter.

"------------------------------------------------------------
" 45.12 Опер. факт за год
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fy
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_02'
    AND c026~calyear       = @lv_calyear_num
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_fy).

SORT lt_c026_bud_fy BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.13-45.16 Опер. факт по кварталам
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    c026~calquarter         AS calquarter,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fq
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_02'
    AND c026~calyear       = @lv_calyear_num
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~calquarter    IN ( @lv_q1, @lv_q2, @lv_q3, @lv_q4 )
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl,
    c026~calquarter
  INTO TABLE @DATA(lt_c026_bud_fq).

SORT lt_c026_bud_fq BY zportfcom ziproject ziparticl calquarter.

"------------------------------------------------------------
" 45.17 Год+1 План
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_pp1
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_p1
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_pp1).

SORT lt_c026_bud_pp1 BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.18 Год+2 Прогноз
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fp2
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_p2
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_fp2).

SORT lt_c026_bud_fp2 BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.19 Год+3 Прогноз
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fp3
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_p3
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_fp3).

SORT lt_c026_bud_fp3 BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.20 Год+4 Прогноз
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fp4
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~calyear       = @lv_calyear_p4
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_fp4).

SORT lt_c026_bud_fp4 BY zportfcom ziproject ziparticl.

"------------------------------------------------------------
" 45.22 Год+5 Прогноз
"------------------------------------------------------------
SELECT
    c026~/bic/zportfcom      AS zportfcom,
    c026~/bic/ziproject      AS ziproject,
    c026~/bic/ziparticl      AS ziparticl,
    SUM( c026~/bic/zipsumm ) AS zipsumm_fp5
  FROM /bic/azip_c026 AS c026
  WHERE c026~/bic/zipdttype = 'DT_01'
    AND c026~/bic/zipversn  = 'CR'
    AND c026~/bic/ziprper   = @iv_ziprper
    AND c026~calyear       = @lv_calyear_p5
    AND c026~/bic/ziparticl IN ( 'IA02020000','IA02000000','IA02010000',
          'IA04010000','IA04020000','IA04030000','IA04040000','IA04050000',
          'IA04060000','IA04120000','IA04070000','IA04080000','IA04090000',
          'IA04100000','IA04110000','IA07000000','IA08000000','IA09000000',
          'IA10000000','IA11000000','IA12000000','IA03010100','IA03010200',
          'IA03020100','IA03020200','IA03020300','IA03020400','IA03020500',
          'IA03020600','IA03010400','IA03030100','IA03030200','IA03030300',
          'IA03030400','IA03030500','IA03030600','IA03030700','IA03030800',
          'IA03030900','IA03031000','IA03031100','IA03040100','IA03040200',
          'IA03040300','IA03040400','IA03040500','IA03040600','IA03040700',
          'IA03040800','IA03040900','IA03041000','IA03041100' )
  GROUP BY
    c026~/bic/zportfcom,
    c026~/bic/ziproject,
    c026~/bic/ziparticl
  INTO TABLE @DATA(lt_c026_bud_fp5).

SORT lt_c026_bud_fp5 BY zportfcom ziproject ziparticl.

  "============================================================
  " Главный цикл
  "============================================================
  LOOP AT et_data ASSIGNING FIELD-SYMBOL(<ls_data>).

    "----------------------------
    " п.41: Партнеры проекта
    "----------------------------
    CLEAR lt_partner.
    CLEAR lv_sum_rate.

    LOOP AT lt_partner_raw ASSIGNING FIELD-SYMBOL(<ls_partner_raw>)
      WHERE ziprper   = <ls_data>-/bic/ziprper
        AND zportfcom = <ls_data>-/bic/zportfcom
        AND ziproject = <ls_data>-/bic/ziproject.

      CLEAR ls_partner.
      ls_partner-/bic/zippartnr    = <ls_partner_raw>-zippartnr.
      ls_partner-/bic/zippartnrtxt = <ls_partner_raw>-zippartnrtxt.
      ls_partner-/bic/zipprrate    = <ls_partner_raw>-zipprrate.
      APPEND ls_partner TO lt_partner.
      lv_sum_rate = lv_sum_rate + <ls_partner_raw>-zipprrate.
    ENDLOOP.

    IF lt_partner IS NOT INITIAL.
      CLEAR ls_partner.
      ls_partner-/bic/zippartnr    = 'TOTAL'.
      ls_partner-/bic/zippartnrtxt = 'Сумма:'.
      ls_partner-/bic/zipprrate    = lv_sum_rate.
      APPEND ls_partner TO lt_partner.
    ENDIF.

    <ls_data>-partner = lt_partner.

    "----------------------------
    " п.42: Сроки реализации
    " FIX: дубликаты убраны на уровне SELECT (GROUP BY),
    "      поэтому здесь каждая задача встречается ровно один раз
    "----------------------------
    CLEAR lt_d096.

    LOOP AT lt_d096_plan ASSIGNING FIELD-SYMBOL(<ls_plan>)
      WHERE ziprper   = <ls_data>-/bic/ziprper
        AND zportfcom = <ls_data>-/bic/zportfcom
        AND ziproject = <ls_data>-/bic/ziproject.

      CLEAR ls_d096.
      CLEAR lv_days_diff.
      CLEAR lv_deviation.

      ls_d096-/bic/ziprtask    = <ls_plan>-ziprtask.
      ls_d096-/bic/ziprtasktxt = <ls_plan>-ziprtasktxt.
      ls_d096-/bic/zipdecrsn   = <ls_plan>-zipdecrsn.

      IF <ls_plan>-zipctask = 'X'.
        ls_d096-/bic/zipctask = 'X'.
      ENDIF.

      ls_d096-/bic/plan = <ls_plan>-zipfindat_plan.
      IF ls_d096-/bic/plan IS NOT INITIAL.
        ls_d096-/bic/plantxt =
          |{ ls_d096-/bic/plan+6(2) }.{ ls_d096-/bic/plan+4(2) }.{ ls_d096-/bic/plan+0(4) }|.
      ENDIF.

      DATA(lv_findat_plan) = <ls_plan>-zipfindat_plan.
      DATA(lv_findat_used) = CONV /bic/azip_d096-/bic/zipfindat( space ).

      READ TABLE lt_d096_fact ASSIGNING FIELD-SYMBOL(<ls_fact>)
        WITH KEY ziprper   = <ls_plan>-ziprper
                 zportfcom = <ls_plan>-zportfcom
                 ziproject = <ls_plan>-ziproject
                 ziprtask  = <ls_plan>-ziprtask
        BINARY SEARCH.

      IF sy-subrc = 0 AND <ls_fact>-zipfindat_fact IS NOT INITIAL.
        lv_findat_used = <ls_fact>-zipfindat_fact.
      ELSE.
        READ TABLE lt_d096_fcst ASSIGNING FIELD-SYMBOL(<ls_fcst>)
          WITH KEY ziprper   = <ls_plan>-ziprper
                   zportfcom = <ls_plan>-zportfcom
                   ziproject = <ls_plan>-ziproject
                   ziprtask  = <ls_plan>-ziprtask
          BINARY SEARCH.
        IF sy-subrc = 0.
          lv_findat_used = <ls_fcst>-zipfindat_fcst.
        ENDIF.
      ENDIF.

      ls_d096-/bic/fact&forecast = lv_findat_used.
      IF ls_d096-/bic/fact&forecast IS NOT INITIAL.
        ls_d096-/bic/fact&forecasttxt =
          |{ ls_d096-/bic/fact&forecast+6(2) }.{ ls_d096-/bic/fact&forecast+4(2) }.{ ls_d096-/bic/fact&forecast+0(4) }|.
      ENDIF.

      IF lv_findat_plan IS NOT INITIAL AND lv_findat_used IS NOT INITIAL.
        lv_days_diff = lv_findat_plan - lv_findat_used.
        lv_deviation = lv_days_diff / 30.
        ls_d096-/bic/zipdeviat = lv_deviation.
      ENDIF.

      APPEND ls_d096 TO lt_d096.
    ENDLOOP.

    <ls_data>-zts_d096 = lt_d096.

    "----------------------------
    " п.43: Риски проекта
    "----------------------------
    CLEAR lt_risk.

    LOOP AT lt_d006_raw ASSIGNING FIELD-SYMBOL(<ls_d006>)
      WHERE ziprper   = <ls_data>-/bic/ziprper
        AND zportfcom = <ls_data>-/bic/zportfcom
        AND ziproject = <ls_data>-/bic/ziproject.

      CLEAR ls_risk.
      ls_risk-/bic/zipriskcl    = <ls_d006>-zipriskcl.
      ls_risk-/bic/zipriskcltxt = <ls_d006>-zipriskcltxt.
      ls_risk-/bic/zipriskdc    = <ls_d006>-zipriskdc.
      ls_risk-/bic/ziprisklm    = <ls_d006>-ziprisklm.
      ls_risk-/bic/ziprisksr    = <ls_d006>-ziprisksr.
      ls_risk-/bic/ziprisksrtxt = <ls_d006>-ziprisksrtxt.
      ls_risk-/bic/zipriskmt    = <ls_d006>-zipriskmt.
      APPEND ls_risk TO lt_risk.
    ENDLOOP.

    <ls_data>-d006 = lt_risk.

    "----------------------------
    " п.44: КПЭ проекта
    "----------------------------
    CLEAR lt_kpi.

    LOOP AT lt_c026_cr ASSIGNING FIELD-SYMBOL(<ls_cr>)
      WHERE zportfcom = <ls_data>-/bic/zportfcom
        AND ziproject = <ls_data>-/bic/ziproject.

      CLEAR ls_kpi.

      ls_kpi-/bic/ziparticl    = <ls_cr>-ziparticl.
      ls_kpi-/bic/ziparticltxt = <ls_cr>-ziparticltxt.

      " 44.3 Актуальное значение плана
      ls_kpi-/bic/zipsummln = round( val = <ls_cr>-zipsummln dec = 2 ).

      " 44.4 Актуальное значение в USD
      ls_kpi-/bic/zipsumth = round( val = <ls_cr>-zipsumth dec = 2 ).

      " 44.5 Значение период -1
      READ TABLE lt_c026_prev ASSIGNING FIELD-SYMBOL(<ls_prev>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipsummln_prev = round( val = <ls_prev>-zipsummln_prev dec = 2 ).
      ENDIF.

      " 44.6 Дельта = 44.5 - 44.3 (ориентир: BI показывает prev - current)
      ls_kpi-/bic/zipsummln_delta_prev = round( val = ls_kpi-/bic/zipsummln_prev - ls_kpi-/bic/zipsummln dec = 2 ).

      " 44.7 Причина отклонения PREV
      " FIX: сортировка lt_d036_rsn01 включает zipproglc — BINARY SEARCH корректен
      READ TABLE lt_d036_rsn01 ASSIGNING FIELD-SYMBOL(<ls_rsn01>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
                 zipproglc = <ls_cr>-zipproglc
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipdecrsn_prev = <ls_rsn01>-zipdecrsn_prev.
      ENDIF.

      " 44.8 План Выбор CP_20
      " FIX: lt_c026_cp20 уже содержит только MAX период — берём первую запись
      READ TABLE lt_c026_cp20 ASSIGNING FIELD-SYMBOL(<ls_cp20>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipsummln_select = round( val = <ls_cp20>-zipsummln_select dec = 2 ).
      ENDIF.

      " 44.9 Причина отклонения SELECT
      " FIX: lt_d036_cp20 уже содержит только MAX период
      READ TABLE lt_d036_cp20 ASSIGNING FIELD-SYMBOL(<ls_rsn20>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipdecrsn_select = <ls_rsn20>-zipdecrsn_select.
      ENDIF.

      " 44.10 План Определения CP_30
      " FIX: lt_c026_cp30 уже содержит только MAX период
      READ TABLE lt_c026_cp30 ASSIGNING FIELD-SYMBOL(<ls_cp30>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipsummln_define = round( val = <ls_cp30>-zipsummln_define dec = 2 ).
      ENDIF.

      " 44.11 Причина отклонения DEFINE
      READ TABLE lt_d036_cp30 ASSIGNING FIELD-SYMBOL(<ls_rsn30>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipdecrsn_define = <ls_rsn30>-zipdecrsn_define.
      ENDIF.

      " 44.12 Утверждённый план UV
      READ TABLE lt_c026_uv ASSIGNING FIELD-SYMBOL(<ls_uv>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipsummln_approved = round( val = <ls_uv>-zipsummln_approved dec = 2 ).
      ENDIF.

      " 44.13 Дельта 2 = 44.3 - 44.12 (ориентир: BI показывает current - approved)
      ls_kpi-/bic/zipsummln_delta_approved = round( val = ls_kpi-/bic/zipsummln_approved - ls_kpi-/bic/zipsummln dec = 2 ).

      " 44.14 Причина отклонения APPROVED
      READ TABLE lt_d036_rsn05 ASSIGNING FIELD-SYMBOL(<ls_rsn05>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipdecrsn_approved = <ls_rsn05>-zipdecrsn_approved.
      ENDIF.

      " 44.15 Прогноз DT_04
      READ TABLE lt_c026_fcst ASSIGNING FIELD-SYMBOL(<ls_fcst2>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipsummln_forecast = round( val = <ls_fcst2>-zipsummln_forecast dec = 2 ).
      ENDIF.

      " 44.16 Дельта 3 = 44.3 - 44.15
      ls_kpi-/bic/zipsummln_delta_forecast = round( val = ls_kpi-/bic/zipsummln - ls_kpi-/bic/zipsummln_forecast dec = 2 ).

      " 44.17 Причина отклонения FORECAST
      READ TABLE lt_d036_rsn04 ASSIGNING FIELD-SYMBOL(<ls_rsn04>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_cr>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_kpi-/bic/zipdecrsn_forecast = <ls_rsn04>-zipdecrsn_forecast.
      ENDIF.

      APPEND ls_kpi TO lt_kpi.
    ENDLOOP.

    <ls_data>-d036 = lt_kpi.


    "----------------------------
    " п.45: Бюджет
    "----------------------------
    DATA: lt_bud   TYPE ztt_c026,
          ls_bud   LIKE LINE OF lt_bud.

    CLEAR lt_bud.

    LOOP AT lt_c026_bud_fb ASSIGNING FIELD-SYMBOL(<ls_bud_fb>)
      WHERE zportfcom = <ls_data>-/bic/zportfcom
        AND ziproject = <ls_data>-/bic/ziproject.

      CLEAR ls_bud.

      ls_bud-/bic/ziparticl    = <ls_bud_fb>-ziparticl.
      ls_bud-/bic/ziparticltxt = <ls_bud_fb>-ziparticltxt.
      ls_bud-/bic/zipproduc    = <ls_bud_fb>-zipproduc.
      ls_bud-/bic/zipproductxt = <ls_bud_fb>-zipproductxt.

      ls_bud-/bic/zipsummln_fb = round( val = <ls_bud_fb>-zipsumm_fb dec = 2 ).

      READ TABLE lt_c026_bud_fc ASSIGNING FIELD-SYMBOL(<ls_bud_fc>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fc = round( val = <ls_bud_fc>-zipsumm_fc dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_py ASSIGNING FIELD-SYMBOL(<ls_bud_py>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_py = round( val = <ls_bud_py>-zipsumm_py dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_pq ASSIGNING FIELD-SYMBOL(<ls_bud_pq1>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q1
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_pq1 = round( val = <ls_bud_pq1>-zipsumm_pq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_pq ASSIGNING FIELD-SYMBOL(<ls_bud_pq2>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q2
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_pq2 = round( val = <ls_bud_pq2>-zipsumm_pq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_pq ASSIGNING FIELD-SYMBOL(<ls_bud_pq3>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q3
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_pq3 = round( val = <ls_bud_pq3>-zipsumm_pq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_pq ASSIGNING FIELD-SYMBOL(<ls_bud_pq4>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q4
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_pq4 = round( val = <ls_bud_pq4>-zipsumm_pq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fy ASSIGNING FIELD-SYMBOL(<ls_bud_fy>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fy = round( val = <ls_bud_fy>-zipsumm_fy dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fq ASSIGNING FIELD-SYMBOL(<ls_bud_fq1>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q1
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fq1 = round( val = <ls_bud_fq1>-zipsumm_fq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fq ASSIGNING FIELD-SYMBOL(<ls_bud_fq2>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q2
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fq2 = round( val = <ls_bud_fq2>-zipsumm_fq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fq ASSIGNING FIELD-SYMBOL(<ls_bud_fq3>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q3
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fq3 = round( val = <ls_bud_fq3>-zipsumm_fq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fq ASSIGNING FIELD-SYMBOL(<ls_bud_fq4>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
                 calquarter = lv_q4
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fq4 = round( val = <ls_bud_fq4>-zipsumm_fq dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_pp1 ASSIGNING FIELD-SYMBOL(<ls_bud_pp1>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_pp1 = round( val = <ls_bud_pp1>-zipsumm_pp1 dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fp2 ASSIGNING FIELD-SYMBOL(<ls_bud_fp2>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fp2 = round( val = <ls_bud_fp2>-zipsumm_fp2 dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fp3 ASSIGNING FIELD-SYMBOL(<ls_bud_fp3>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fp3 = round( val = <ls_bud_fp3>-zipsumm_fp3 dec = 2 ).
      ENDIF.

      READ TABLE lt_c026_bud_fp4 ASSIGNING FIELD-SYMBOL(<ls_bud_fp4>)
        WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                 ziproject = <ls_data>-/bic/ziproject
                 ziparticl = <ls_bud_fb>-ziparticl
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_bud-/bic/zipsummln_fp4 = round( val = <ls_bud_fp4>-zipsumm_fp4 dec = 2 ).
      ENDIF.


        READ TABLE lt_c026_bud_fp5 ASSIGNING FIELD-SYMBOL(<ls_bud_fp5>)
          WITH KEY zportfcom = <ls_data>-/bic/zportfcom
                   ziproject = <ls_data>-/bic/ziproject
                   ziparticl = <ls_bud_fb>-ziparticl
          BINARY SEARCH.
        IF sy-subrc = 0.
          ls_bud-/bic/zipsummln_fp5 = round( val = <ls_bud_fp5>-zipsumm_fp5 dec = 2 ).
        ENDIF.

  "------------------------------------------------------------
  " Определяем отчетный квартал из IV_ZIPRPER (формат YYYYQ)
  "------------------------------------------------------------
  DATA(lv_report_q_bud) = CONV i( iv_ziprper+4(1) ).

  "------------------------------------------------------------
  " План на остаток года: кварталы ПОСЛЕ отчетного (Q+1 ... Q4)
  " Пример: если период 20252 (Q2), берём PQ3 + PQ4
  "------------------------------------------------------------
  DATA: lv_plan_rest_bud TYPE p LENGTH 15 DECIMALS 2.
  CASE lv_report_q_bud.
    WHEN 1.
      lv_plan_rest_bud = ls_bud-/bic/zipsummln_pq2 +
                         ls_bud-/bic/zipsummln_pq3 +
                         ls_bud-/bic/zipsummln_pq4.
    WHEN 2.
      lv_plan_rest_bud = ls_bud-/bic/zipsummln_pq3 +
                         ls_bud-/bic/zipsummln_pq4.
    WHEN 3.
      lv_plan_rest_bud = ls_bud-/bic/zipsummln_pq4.
    WHEN 4.
      lv_plan_rest_bud = 0.
  ENDCASE.

      "------------------------------------------------------------
      " Факт накопленный с 1 квартала до отчетного квартала
      " Пример: если период 20252 (Q2), берём FQ1 + FQ2
      "------------------------------------------------------------
      DATA: lv_fact_accum_bud TYPE p LENGTH 15 DECIMALS 2.
      CASE lv_report_q_bud.
        WHEN 1.
          lv_fact_accum_bud = ls_bud-/bic/zipsummln_fq1.
        WHEN 2.
          lv_fact_accum_bud = ls_bud-/bic/zipsummln_fq1 +
                              ls_bud-/bic/zipsummln_fq2.
        WHEN 3.
          lv_fact_accum_bud = ls_bud-/bic/zipsummln_fq1 +
                              ls_bud-/bic/zipsummln_fq2 +
                              ls_bud-/bic/zipsummln_fq3.
        WHEN 4.
          lv_fact_accum_bud = ls_bud-/bic/zipsummln_fq1 +
                              ls_bud-/bic/zipsummln_fq2 +
                              ls_bud-/bic/zipsummln_fq3 +
                              ls_bud-/bic/zipsummln_fq4.
      ENDCASE.

      "------------------------------------------------------------
      " 45.5 Общее значение:
      "   FB  = Факт до (года-1), т.е. calyear < год-1
      "   FC  = Факт за (год-1), т.е. calyear = год-1
      "   lv_plan_rest_bud = План на остаток текущего года (с Q(отчетный+1) по Q4)
      "   lv_fact_accum_bud = Факт накопленный с Q1 по отчетный квартал
      "   PP1..FP5 = Год+1..+5
      "
      " Формула:
      "   FB + FC + lv_plan_rest_bud + lv_fact_accum_bud + PP1 + FP2 + FP3 + FP4 + FP5
      "------------------------------------------------------------
      ls_bud-/bic/zipsumm_tot = round(
        val =   ls_bud-/bic/zipsummln_fb          " Факт до (год < год-1)
              + ls_bud-/bic/zipsummln_fc          " Факт за (год-1)
              + lv_plan_rest_bud                  " План остаток текущего года
              + lv_fact_accum_bud                 " Факт накопленный с Q1 по отчетный квартал
              + ls_bud-/bic/zipsummln_pp1         " Год+1 План
              + ls_bud-/bic/zipsummln_fp2         " Год+2 Прогноз
              + ls_bud-/bic/zipsummln_fp3         " Год+3 Прогноз
              + ls_bud-/bic/zipsummln_fp4         " Год+4 Прогноз
              + ls_bud-/bic/zipsummln_fp5         " Год+5 Прогноз
        dec = 2 ).

      APPEND ls_bud TO lt_bud.
    ENDLOOP.

<ls_data>-c026 = lt_bud.
  ENDLOOP.

ENDFUNCTION.