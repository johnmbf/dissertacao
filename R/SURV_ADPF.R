# Juntar análise de sobrevivência com os dados de decisões judiciais

processo_sobrevivencia <- processos_adpf |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, tem_rito_art_12) |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processo_sobrevivencia <- processo_sobrevivencia |>
  dplyr::filter(numero %in% recorte$numero)

processo_sobrevivencia <- dplyr::left_join(
  processo_sobrevivencia,
  decisao_tipos,
  by = "numero"
)

# processo_sobrevivencia <- processo_sobrevivencia |> dplyr::filter(merito == "Sim")

processo_sobrevivencia$tem_rito_art_12 <- as.factor(processo_sobrevivencia$tem_rito_art_12)
processo_sobrevivencia$merito <- as.factor(processo_sobrevivencia$merito)

ajuste_km <- survfit(Surv(tempo, evento) ~ tem_rito_art_12 + strata(descricao), data = processo_sobrevivencia)

print(ajuste_km)
ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  # legend.title = "Rito Art. 12",
  # legend.labs = c("Não", "Sim"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência (Processo Ativo)",
  title = "Curva de Sobrevivência de Processos por Rito",
  ggtheme = decJ::theme_decJ()
)

modelo_estratificado <- coxph(Surv(tempo, evento) ~ tem_rito_art_12 + strata(descricao), data = processo_sobrevivencia)
summary(modelo_estratificado)

cox.zph(modelo_estratificado)
