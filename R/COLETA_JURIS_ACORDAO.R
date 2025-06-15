recorte |> View()


# juris_acordao <- decJ::stf_jurisprudencia(
#   classe = "ADPF",
#   base = "acordaos",
#   quantidade = 3000
# )

# BAIXAR PDF ----
## Carregar a função nova até que seja inserida no decJ
source("R/CODIGO DE COLETA.R")

## Filtrar apenas as ADPF 2014-2024
processos <- processos |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::filter(lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024)

## Buscar os códigos de decisao
dec_cod <- dec_acordao |>
  dplyr::filter(processo_numero %in% processos$numero) |>
  dplyr::filter(lubridate::year(julgamento_data) <= 2024) |>
  dplyr::select(inteiro_teor_url) |>
  dplyr::mutate(doc_id = stringr::str_split_i(inteiro_teor_url, "=", -1)) |>
  dplyr::select(doc_id)

## Baixar as decisoes e salvar em DATA/DEC
purrr::walk(dec_cod$doc_id, ~ {
  baixar_juris(.x, "DATA/DEC")
}, .progress = TRUE)

# Extrair o texto
