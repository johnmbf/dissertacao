extrair_texto_juris_stf <- function(dir) {
  lista_dec <- list.files(dir, full.names = TRUE)
  lista_dec <- data.frame(
    doc = lista_dec
  )
  lista_dec <- lista_dec |>
    dplyr::mutate(
      doc_id = stringr::str_split_i(doc, "\\/", -1) |> stringr::str_remove("\\.pdf")
    )

  areas <- readRDS("DATA/MISC/area.rds")

  purrr::map2_df(lista_dec$doc, lista_dec$doc_id, ~ {
    p <- tabulizer::get_n_pages(.x)

    texto <- tabulizer::extract_text(.x, pages = 1:p, area = areas) |>
      paste(collapse = "") |>
      stringr::str_squish() |>
      stringr::str_trim()

    data.frame(doc_id = .y, texto = texto)
  }, .progress = TRUE)
}

dec_texto <- extrair_texto_juris_stf("DATA/ACORDAOS/")

adpf_recorte <- clean_processos |>
  dplyr::filter(
    classe == "ADPF",
    lubridate::year(dt_auto) >= 2014 & lubridate::year(dt_auto) <= 2024
  ) |>
  dplyr::select(classe, numero) |>
  dplyr::distinct()

dec_acordao <- dec_acordao |>
  dplyr::filter(
    lubridate::year(julgamento_data) <= 2024,
    processo_numero %in% adpf_recorte$numero
  ) |>
  dplyr::mutate(
    doc_id = stringr::str_split_i(inteiro_teor_url, "=", -1)
  ) |>
  dplyr::select(
    id,
    titulo,
    procedencia_geografica_uf_sigla,
    processo_numero,
    processo_classe_processual_unificada_incidente_sigla,
    relator_processo_nome,
    relator_acordao_nome,
    julgamento_data,
    documental_legislacao_citada_texto,
    doc_id
  )

dec_acordao_texto <-
  dplyr::left_join(
    dec_acordao,
    dec_texto,
    by = "doc_id"
  )

iramuteq <- dec_acordao_texto |>
  dplyr::mutate(
    cod = paste0(
      "\n****",
      " *dec_",
      id,
      " *uf_",
      procedencia_geografica_uf_sigla,
      " *adpf_",
      processo_numero,
      " *rel_",
      relator_processo_nome,
      " *dt_",
      lubridate::year(julgamento_data)
    )
  ) |>
  dplyr::select(cod, texto) |>
  dplyr::relocate(cod, texto)

readr::write_delim(iramuteq, "IRAMUTEQ/acordao.txt", "\n", col_names = FALSE, quote = "none")
saveRDS(dec_texto, "IRAMUTEQ/dec_text.rds")
