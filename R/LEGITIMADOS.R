# Filtrar as ações propostas por Partidos e Presidente da República
leg_ativo <- readxl::read_xlsx("DATA/legitimados_ativo.xlsx")

# Juntar leg_ativo com a tabela de legitimados
adpf_com_leg <- dplyr::left_join(
  adpf_tratado_leg,
  leg_ativo,
  by = c("ativo" = "ativo")
)

# Filtrar apenas as ações propostas por Partidos e Presidente da República
adpf_partidos_presidente <- adpf_com_leg %>%
  dplyr::filter(
    tipo == "8" | tipo == "1"
  )
recorte_adpf_partidos_presidente <- adpf_partidos_presidente %>%
  dplyr::select(classe, numero) |>
  dplyr::distinct()


# Resolver o problema
legitimados_adpf <- leg_total_n |>
  dplyr::filter(numero %in% processos_adpf_recorte$numero) |>
  dplyr::left_join(
    leg_ativo,
    by = "ativo"
  )
