# raw_processos ---

processos <- raw_processos |>
  janitor::clean_names()

processos <- processos |>
  tidyr::separate(
    processo,
    into = c("classe", "numero"),
    sep = " ",
    convert = TRUE
  )

processos <- processos |>
  tidyr::separate_rows(
    assunto_relacionado,
    sep = "\\s*\\|\\s*"
  )

# raw_decisoes ---

decisoes <- raw_decisoes |>
  janitor::clean_names()

decisoes <- decisoes |>
  tidyr::separate(
    processo,
    into = c("classe", "numero"),
    sep = " ",
    convert = TRUE
  )

decisoes <- decisoes |>
  dplyr::mutate(
    data = lubridate::ymd(as.Date(data)),
    andamento_agrupado = as.factor(andamento_agrupado),
    subgrupo = as.factor(subgrupo)
  )

# raw_legitimados ---
legitimados <- raw_legitimados |>
  janitor::clean_names()

legitimados <- legitimados |>
  tidyr::separate(
    processo,
    into = c("classe", "numero"),
    sep = " ",
    convert = TRUE
  )