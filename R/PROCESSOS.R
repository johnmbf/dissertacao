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
