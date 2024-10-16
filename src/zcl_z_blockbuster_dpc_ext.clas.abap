class ZCL_Z_BLOCKBUSTER_DPC_EXT definition
  public
  inheriting from ZCL_Z_BLOCKBUSTER_DPC
  create public .

public section.
protected section.

  methods CATALOGSET_CREATE_ENTITY
    redefinition .
  methods CATALOGSET_GET_ENTITY
    redefinition .
  methods CATALOGSET_GET_ENTITYSET
    redefinition .
  methods GENRESSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_BLOCKBUSTER_DPC_EXT IMPLEMENTATION.


METHOD catalogset_create_entity.

  " Declaração de variáveis
  DATA: l_last_id TYPE int4,                    " Último ID no catálogo
        l_catalog TYPE zloc_catalog,            " Estrutura do catálogo
        l_genre   TYPE zloc_genre,              " Estrutura de gênero
        l_todos   TYPE TABLE OF numc2,          " Tabela para armazenar IDs de gênero
        w_todos   TYPE string,                  " String temporária para IDs concatenados
        l_id      TYPE zloc_genre-id,           " Variável temporária para IDs
        t_split   TYPE TABLE OF string.         " Tabela para armazenar valores de 'Genre'

  " Container para mensagens
  DATA(l_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  " Lê os dados de entrada fornecidos pelo serviço
  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  " Consulta para buscar todos os gêneros
  SELECT *
    FROM zloc_genre
    INTO TABLE @DATA(t_genre).

  " Mapeamento dos campos obrigatórios do catálogo
  l_catalog-id = er_entity-id.              " Mapeia o campo ID
  l_catalog-title = er_entity-title.        " Mapeia o campo Title

  " Campo 'Ryear' (Edm.Int16)
  IF er_entity-ryear IS NOT INITIAL.
    l_catalog-ryear = er_entity-ryear.
  ENDIF.

  " Campo 'Rated' (String)
  l_catalog-rated = er_entity-rated.

  " Campo 'Released' (Edm.DateTime) - Formatação da data
  IF er_entity-released IS NOT INITIAL.
    DATA l_released TYPE string.
    DATA l_catalog_released TYPE string.

    " Converte o campo Edm.DateTime para string e extrai a data no formato YYYYMMDD
    l_released = er_entity-released.

    " Concatena com o horário padrão em formato UTC (YYYY-MM-DDTHH:MM:SSZ)
    l_catalog_released = l_released(8) && 'T00:00:00Z'.

    " Atribui o valor convertido ao campo de destino
    l_catalog-released = l_catalog_released.
  ENDIF.

  " Campo 'Runtime' já recebido corretamente como números
  l_catalog-runtime = er_entity-runtime.

  " Mapeamento do campo 'Genre' (string) - Split e conversão
  SPLIT er_entity-genre AT ',' INTO TABLE t_split.

  LOOP AT t_split INTO DATA(l_values).
    CONDENSE l_values NO-GAPS.  " Remove espaços
    READ TABLE t_genre INTO l_genre WITH KEY genre = l_values.
    IF sy-subrc IS INITIAL.
      APPEND l_genre-id TO l_todos.
    ENDIF.
  ENDLOOP.

  " Concatenação dos IDs de gênero
  CLEAR l_catalog-genre.
  LOOP AT l_todos INTO w_todos.
    IF l_catalog-genre IS INITIAL.
      l_catalog-genre = w_todos.
    ELSE.
      CONCATENATE l_catalog-genre w_todos INTO l_catalog-genre SEPARATED BY ','.
    ENDIF.
  ENDLOOP.

  " Mapeamento do campo 'Director' (string)
  l_catalog-director = er_entity-director.

  " Mapeamento dos campos opcionais
  l_catalog-writer   = er_entity-writer.
  l_catalog-actors   = er_entity-actors.
  l_catalog-plot     = er_entity-plot.
  l_catalog-language = er_entity-language.
  l_catalog-country  = er_entity-country.

  " Mapeamento do campo 'Metascore' (Byte)
  IF er_entity-metascore IS NOT INITIAL.
    l_catalog-metascore = er_entity-metascore.
  ENDIF.

  " Determina o próximo ID para inserir no catálogo
  SELECT SINGLE MAX( id )
    INTO l_last_id
    FROM zloc_catalog.

  l_catalog-id = l_last_id + 1.

  " Insere o novo registro no catálogo
  INSERT zloc_catalog FROM l_catalog.

  " Tratamento de erro na inserção
  IF sy-subrc <> 0.
    l_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Creation Error'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = l_msg.
  ENDIF.

  " Limpa e move dados do catálogo para a entidade de retorno
  CLEAR er_entity.
  MOVE-CORRESPONDING l_catalog TO er_entity.

  " Conversão de timestamp para data/hora local
  DATA l_date TYPE d.       " Variável para armazenar a data
  DATA l_time TYPE t.       " Variável para armazenar a hora
  DATA l_timestamp TYPE timestamp.  " Timestamp gerado

  l_date = l_released(8).   " Extrai a data do campo 'released'
  l_time = '000000'.        " Define a hora padrão

  " Converte a data e a hora para um timestamp com base no fuso horário local
  CONVERT DATE l_date TIME l_time INTO TIME STAMP l_timestamp TIME ZONE sy-zonlo.

  " Atualiza a entidade de retorno com o timestamp convertido
  er_entity-released = l_timestamp.

  " Limpa os dados do catálogo
  CLEAR l_catalog.

ENDMETHOD.


  METHOD catalogset_get_entity.

    DATA: l_id TYPE zloc_catalog-id.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_cat     TYPE zloc_catalog.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'Id'.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id Error'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
    l_id = ls_key_tab-value.

    SELECT SINGLE *
      INTO ls_cat
      FROM zloc_catalog
     WHERE id = l_id.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_cat TO er_entity.

      CONVERT DATE ls_cat-released
              TIME  '000000'
              INTO TIME STAMP er_entity-released
              TIME ZONE sy-zonlo.

    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Movie Not Found'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD catalogset_get_entityset.

    DATA: ls_entityset TYPE zcl_z_blockbuster_mpc=>ts_catalog.

    DATA: lt_orderby TYPE STANDARD TABLE OF string,
          wa_orderby TYPE string.

    LOOP AT it_order INTO DATA(wa_order).
      TRANSLATE wa_order-order TO UPPER CASE.
      IF wa_order-order EQ 'DESC'.
        wa_order-order = 'Descending'.
      ELSE.
        wa_order-order = 'Ascending'.
      ENDIF.
      APPEND |{ wa_order-property } { wa_order-order }|
          TO lt_orderby.
    ENDLOOP.
    CONCATENATE LINES OF lt_orderby INTO wa_orderby SEPARATED BY space.

    IF wa_orderby IS INITIAL.
      wa_orderby = 'Id ascending'.
    ENDIF.





    DATA: lv_filter        TYPE string,
          lv_value         TYPE string,
          lv_filter_string TYPE string,
          lv_key           TYPE string,
          lt_conditions    TYPE TABLE OF string,
          lt_results       TYPE TABLE OF string,
          lv_part          TYPE string,
          lv_temp          TYPE string,
          lv_position      TYPE i,
          lv_key_pos       TYPE i,
          lv_len           TYPE i.

    lv_filter = iv_filter_string.

    " Passo 1: Remover os parênteses externos
    REPLACE ALL OCCURRENCES OF '(' IN lv_filter WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN lv_filter WITH ''.

    " Passo 2: Separar os diferentes filtros pelo operador 'or'
    SPLIT lv_filter AT 'and' INTO TABLE lt_conditions.

    " Passo 3: Loop para processar cada condição separada
    LOOP AT lt_conditions INTO lv_part.


      " Remover 'substringof' e espaços desnecessários
      REPLACE ALL OCCURRENCES OF 'substringof' IN lv_part WITH ''.
      CONDENSE lv_part.

      " Encontrar a posição da vírgula que separa o valor e a chave
      FIND ',' IN lv_part MATCH OFFSET lv_position.

      " Passo 4: Extrair o valor (parte antes da vírgula)
      lv_len = lv_position - 3. " Calcula o tamanho do valor
      lv_value = lv_part+1(lv_len). " Extrai o valor sem aspas

      " Remover aspas simples restantes do valor
      REPLACE ALL OCCURRENCES OF '''' IN lv_value WITH ''.
*  REPLACE ALL OCCURRENCES OF '''' IN lv_part WITH ''.
      CHECK lv_part NE '''true'''.


      " Passo 5: Extrair a chave (parte depois da vírgula)
      lv_key_pos = lv_position + 2. " Posição da chave
      lv_key = lv_part+lv_key_pos. " Pega a chave a partir da vírgula
      CONDENSE lv_key.

      " Adicionar à tabela de resultados
      CONCATENATE lv_key ' LIKE ''%' lv_value '%''' INTO lv_temp.
      APPEND lv_temp TO lt_results.

    ENDLOOP.

    DATA: lv_final_string TYPE string,
          lv_result       TYPE string,
          lv_first        TYPE abap_bool VALUE abap_true.

    " Inicializar a string final
    lv_final_string = ''.

    LOOP AT lt_results INTO lv_temp.

      " Se a string final não está vazia, significa que não é o primeiro item
      IF lv_final_string IS NOT INITIAL.
        CONCATENATE lv_final_string 'and' space INTO lv_final_string SEPARATED BY space.
      ENDIF.

      " Concatenar a condição atual na string final
      lv_final_string = lv_final_string && lv_temp.

    ENDLOOP.
*CONCATENATE lv_final_string '.' into lv_final_string.

    " Exibir a string final
    lv_filter_string = lv_final_string.





    SELECT *
      FROM zloc_catalog
      WHERE (lv_filter_string)
      ORDER BY (wa_orderby)
      INTO TABLE @DATA(t_movies)
      UP TO @is_paging-top ROWS
      OFFSET @is_paging-skip.


    DATA: lv_date         TYPE dats,
          lv_edm_datetime TYPE string.

    LOOP AT t_movies INTO DATA(wa_movies).
      CLEAR ls_entityset.
      MOVE-CORRESPONDING wa_movies TO ls_entityset.

      CONVERT DATE wa_movies-released
              TIME  '000000'
              INTO TIME STAMP ls_entityset-released
              TIME ZONE sy-zonlo.

      APPEND ls_entityset TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD genresset_get_entityset.

    DATA: ls_entityset TYPE zcl_z_blockbuster_mpc=>ts_genres.
    SELECT *
      FROM zloc_genre
      ORDER BY genre
      INTO TABLE @DATA(t_genres).

    LOOP AT t_genres INTO DATA(wa_genres).
      CLEAR ls_entityset.
      MOVE-CORRESPONDING wa_genres TO ls_entityset.
      APPEND ls_entityset TO et_entityset.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
