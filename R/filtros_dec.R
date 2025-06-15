# dec_monocratica <- decJ::stf_jurisprudencia(
#   classe = "ADPF",
#   base = 'decisoes',
#   quantidade = 3000
# )
#
# saveRDS(dec_monocratica, 'dec_monocratica.rds')

# dec_acordao <- decJ::stf_jurisprudencia(
#   classe = "ADPF",
#   base = 'acordaos',
#   quantidade = 3000
# )

# saveRDS(dec_acordao, 'dec_acordao.rds')

dec_monocratica <- readRDS("dec_monocratica.rds")
dec_acordao <- readRDS("dec_acordao.rds")

processos <- processos |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::select(classe, numero, data_autuacao) |>
  dplyr::filter(lubridate::year(data_autuacao) >= 2014, lubridate::year(data_autuacao) <= 2024) |>
  dplyr::distinct()

dec_acordao |> dplyr::glimpse()
dec_acordao <- dec_acordao |>
  dplyr::filter(processo_numero %in% processos$numero) |>
  dplyr::filter(julgamento_data <= lubridate::ymd("2024-12-31"))
dec_monocratica <- dec_monocratica |>
  dplyr::filter(processo_numero %in% processos$numero)
