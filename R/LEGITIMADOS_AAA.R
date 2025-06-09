processos_adpf <- processos |>
  dplyr::filter(classe == 'ADPF')

processos_adpf_recorte <- processos_adpf |>
  dplyr::filter(lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024)


adpf_legitimados <- legitimados |>
  dplyr::filter(classe == "ADPF", numero %in% processos_adpf_recorte$numero)

adpf_tratado_leg_total <- legitimados |>
  dplyr::mutate(
    ativo = ifelse(legitimado_polo_ativo |> stringr::str_split_i("\\s+\\-\\s+", 1) |>
      stringr::str_length() >= legitimado_polo_ativo |>
      stringr::str_split_i("\\s+\\-\\s+", -1) |>
      stringr::str_length(), legitimado_polo_ativo |> stringr::str_split_i("\\s+\\-\\s+", 1), legitimado_polo_ativo |> stringr::str_split_i("\\s+\\-\\s+", -1))
  ) |>
  dplyr::mutate(
    passivo = ifelse(legitimado_polo_passivo |> stringr::str_split_i("\\s+\\-\\s+", 1) |>
      stringr::str_length() >= legitimado_polo_passivo |>
      stringr::str_split_i("\\s+\\-\\s+", -1) |>
      stringr::str_length(), legitimado_polo_passivo |> stringr::str_split_i("\\s+\\-\\s+", 1), legitimado_polo_passivo |> stringr::str_split_i("\\s+\\-\\s+", -1))
  ) |>
  dplyr::select(-legitimado_polo_ativo, -legitimado_polo_passivo) |>
  dplyr::mutate(
    ativo = decJ::utilitario_remover_acentos(ativo),
    passivo = decJ::utilitario_remover_acentos(passivo)
  ) |>
  dplyr::filter(classe == "ADPF")

leg_total_n <- adpf_tratado_leg_total |>
  dplyr::select(classe, numero, ativo) |>
  dplyr::distinct() |>
  dplyr::mutate(
    ativo = dplyr::case_when(
      stringr::str_detect(ativo, "PROCURADORIA-GERAL DA REPUBLICA|PROCURADORA-GERAL DA REPUBLICA") ~ "PROCURADOR-GERAL DA REPUBLICA",
      stringr::str_detect(ativo, "PARTIDO SOCIALISMO E LIBERDADE") ~ "PARTIDO SOCIALISMO E LIBERDADE",
      stringr::str_detect(ativo, "CONSELHO FEDERAL DA ORDEM DOS ADVOGADOS DO BRASIL") ~ "ORDEM DOS ADVOGADOS DO BRASIL CONSELHO FEDERAL",
      stringr::str_detect(ativo, "PRESIDENTA DA REPUBLICA") ~ "PRESIDENTE DA REPUBLICA",
      TRUE ~ ativo
    )
  ) |>
  group_by(ativo) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::ungroup()

leg_total_n |>
  dplyr::slice_max(order_by = n, n = 10) |>
  ggplot() +
  geom_col(aes(x = n, y = reorder(ativo, n)), fill = "#5B96A5") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(
    x = "Número de ações",
    y = "Legitimado"
  ) +
  decJ::theme_decJ()

decJ::save_decJ("PLOT/leg_ativo_adpf.png", l = 24)
