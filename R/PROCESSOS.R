library(ggplot2)

# Número de ações de controle concetrado ajuizadas por ano ----
processos_analise_1 <- processos |>
  dplyr::select(
    classe, numero, data_autuacao
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(ano = lubridate::year(data_autuacao))

processos_analise_1 |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n = dplyr::n()
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = n)) +
  ggplot2::geom_col() +
  ggplot2::scale_x_continuous(
    breaks = seq(1988, lubridate::year(Sys.Date()), by = 4),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  ggplot2::labs(
    title = "Número de ações de controle concentrado ajuizadas por ano",
    x = "Ano",
    y = "Número de ações"
  ) +
  decJ::theme_decJ(y_num = TRUE)

# Número de ações de controle concnetrado ajuizadas por ano e por classe ----
processos_analise_2 <- processos |>
  dplyr::select(
    classe, numero, data_autuacao
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(ano = lubridate::year(data_autuacao))

processos_analise_2 |>
  dplyr::group_by(ano, classe) |>
  dplyr::summarise(
    n = dplyr::n()
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = n, fill = classe)) +
  ggplot2::geom_col(position = "stack") +
  ggplot2::scale_x_continuous(
    breaks = seq(1988, lubridate::year(Sys.Date()), by = 4),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  ggplot2::labs(
    title = "Número de ações de controle concentrado ajuizadas por ano e por classe",
    x = "Ano",
    y = "Número de ações"
  ) +
  decJ::theme_decJ(y_num = TRUE)

# Número de ADPF ajuizadas por ano ----
processos_analise_3 <- processos |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::select(
    classe, numero, data_autuacao
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(ano = lubridate::year(data_autuacao))
processos_analise_3 |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n = dplyr::n()
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = n)) +
  ggplot2::geom_col() +
  ggplot2::scale_x_continuous(
    breaks = seq(1988, lubridate::year(Sys.Date()), by = 4),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  ggplot2::labs(
    title = "Número de ADPF ajuizadas por ano",
    x = "Ano",
    y = "Número de ADPF"
  ) +
  decJ::theme_decJ(y_num = TRUE)

# ADPF 2014 a 2024 ----
processos_adpf <- processos |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::filter(lubridate::year(data_autuacao) >= 2014) |>
  dplyr::filter(lubridate::year(data_autuacao) <= 2024)

# juris_adpf <- decJ::stf_jurisprudencia(classe = "ADPF", base = "decisoes", quantidade = 3000)
# dplyr::glimpse(juris_adpf)

# juris_adpf_filter <- juris_adpf |>
#   dplyr::filter(processo_numero %in% processos_adpf$numero) |>
#   dplyr::filter(lubridate::year(julgamento_data) <= 2024) |>
#   dplyr::select(id, uf = procedencia_geografica_uf_sigla, incidente = processo_classe_processual_unificada_incidente_sigla, numero = processo_numero, dt_julgamento = julgamento_data, decisao = decisao_texto, leg = documental_legislacao_citada_texto)



# ADPF

processos |>
  dplyr::select(classe, numero) |>
  dplyr::distinct() |>
  dplyr::filter(classe == "ADPF") |>
  nrow()
