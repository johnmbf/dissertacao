recorte |> View()


juris_acordao <- decJ::stf_jurisprudencia(
  classe = "ADPF",
  base = "acordaos",
  quantidade = 3000
)

# baixar depois
juris_acordao_filter <- juris_acordao |>
  dplyr::filter(processo_numero %in% recorte_adpf_partidos_presidente$numero)
