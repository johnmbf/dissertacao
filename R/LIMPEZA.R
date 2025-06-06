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

# raw_legitimados ---
