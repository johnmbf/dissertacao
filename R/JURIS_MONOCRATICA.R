adpf_corpus <- quanteda::corpus(
  juris_adpf_filter,
  docid_field = "id",
  text_field = "decisao"
)
adpf_tokens <- quanteda::tokens(
  adpf_corpus,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
) |>
  quanteda::tokens_tolower() |>
  quanteda::tokens_remove(quanteda::stopwords("pt"))

iramuteq <- sapply(adpf_tokens, paste, collapse = " ") |>
  as.data.frame() |>
  tibble::rownames_to_column()

names(iramuteq) <- c("cod_dec", "tratado")

iramuteq <-
  iramuteq |>
  dplyr::mutate(
    codigo = paste0(
      "\n****",
      " *dec_",
      cod_dec,
      " *ano_",
      lubridate::year(juris_adpf_filter$dt_julgamento),
      " *uf_",
      juris_adpf_filter$uf
    )
  ) |>
  dplyr::relocate(codigo, tratado)

readr::write_delim(iramuteq, "DATA/iramuteq.txt", "\n", col_names = FALSE, quote = "none")
