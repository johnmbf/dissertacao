# Surv ----

## Tempo de resposta ----

### Modelo Nulo ----

library(survival)
library(survminer)

processo_sobrevivencia <- processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, tem_rito_art_12) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processo_sobrevivencia$tem_rito_art_12 <- as.factor(processo_sobrevivencia$tem_rito_art_12)

ajuste_km <- survfit(Surv(tempo, evento) ~ 1, data = processo_sobrevivencia)

print(ajuste_km)

ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  color = "#5B96A5",
  ggtheme = decJ::theme_decJ()
)

modelo_cox <- coxph(Surv(tempo, evento) ~ 1, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox)
teste_cox

ggcoxzph(teste_cox)

### Modelo Relator ----

processo_sobrevivencia <- processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, relator_atual) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processo_sobrevivencia$relator_atual <- as.factor(processo_sobrevivencia$relator_atual)

ajuste_km <- survfit(Surv(tempo, evento) ~ relator_atual, data = processo_sobrevivencia)

print(ajuste_km)

modelo_cox <- coxph(Surv(tempo, evento) ~ relator_atual, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox)
print(teste_cox)

### Modelo 10 relatores
relator_10 <- processos |>
  dplyr::select(classe, numero, relator_atual) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::group_by(relator_atual) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::slice_max(n, n = 10)

processo_sobrevivencia <- processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, relator_atual) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  ) |>
  dplyr::right_join(relator_10, by = "relator_atual")

processo_sobrevivencia$relator_atual <- as.factor(processo_sobrevivencia$relator_atual)

ajuste_km <- survfit(Surv(tempo, evento) ~ relator_atual, data = processo_sobrevivencia)

print(ajuste_km)

modelo_cox <- coxph(Surv(tempo, evento) ~ relator_atual, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox)
print(teste_cox)

modelo_corrigido <- coxph(
  Surv(tempo, evento) ~ relator_atual +
    tt(relator_atual == "MIN. LUIZ FUX") +
    tt(relator_atual == "MIN. NUNES MARQUES"),
  data = processo_sobrevivencia,
  tt = function(x, t, ...) {
    x * log(t + 1)
  }
)
summary(modelo_corrigido)

### Modelo Legitimado ----

processos <- processos |>
  dplyr::left_join(
    legitimados_adpf,
    by = "numero"
  )


processo_sobrevivencia <- processos |>
  dplyr::select(classe.x, numero, data_autuacao, data_baixa, tipo) |>
  dplyr::filter(classe.x == "ADPF") |>
  dplyr::filter(numero %in% legitimados_adpf$numero) |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processo_sobrevivencia$tipo <- as.factor(processo_sobrevivencia$tipo)

ajuste_km <- survfit(Surv(tempo, evento) ~ tipo, data = processo_sobrevivencia)

print(ajuste_km)

ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  legend.title = "Tipo de Legitimado",
  legend.labs = c(
    "Outros",
    "Presidente",
    "Mesa do Senado",
    "Mesa da Câmara dos Deputados",
    "Mesa da Assembleia Legislativa",
    "Governador",
    "Procurador-Geral da República",
    "OAB",
    "Partido Político",
    "Sociedade Civil Organizada"
  ),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  # color = "#5B96A5",
  ggtheme = decJ::theme_decJ()
)

modelo_cox <- coxph(Surv(tempo, evento) ~ tipo, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox)
teste_cox

ggcoxzph(teste_cox)

# Modelo final que trata o tipo5 (e o tipo3, por precaução) como dependente do tempo
modelo_final_tipo <- coxph(
  Surv(tempo, evento) ~ tipo +
    tt(tipo == "5") +
    tt(tipo == "3"), # Adicionar o tipo3 é uma boa prática aqui
  data = processo_sobrevivencia,
  tt = function(x, t, ...) {
    x * log(t + 1)
  }
)

summary(modelo_final_tipo)

### Modelo Rito 12 ----

processo_sobrevivencia <- processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, tem_rito_art_12) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processo_sobrevivencia$tem_rito_art_12 <- as.factor(processo_sobrevivencia$tem_rito_art_12)

ajuste_km <- survfit(Surv(tempo, evento) ~ tem_rito_art_12, data = processo_sobrevivencia)

print(ajuste_km)

ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Rito Art. 12",
  legend.labs = c("Não", "Sim"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  # color = "#5B96A5",
  ggtheme = decJ::theme_decJ()
)

modelo_cox <- coxph(Surv(tempo, evento) ~ tem_rito_art_12, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox, terms = FALSE)
teste_cox

ggcoxzph(teste_cox)


modelo_corrigido_art12 <- coxph(
  Surv(tempo, evento) ~ tt(tem_rito_art_12 == "Sim"),
  data = processo_sobrevivencia,
  tt = function(x, t, ...) {
    x * log(t + 1)
  }
)

summary(modelo_corrigido_art12)

# Modelo final estratificado pela variável que viola a premissa de PH
modelo_estratificado <- coxph(
  Surv(tempo, evento) ~ strata(tem_rito_art_12),
  data = processo_sobrevivencia
)

# Este modelo não dará um coeficiente para 'tem_rito_art_12',
# o que é o comportamento esperado.
summary(modelo_estratificado)

# A melhor forma de apresentar o resultado é com um gráfico das curvas de sobrevivência
plot(survfit(modelo_estratificado),
  col = c("red", "blue"),
  xlab = "Tempo (dias)", ylab = "Probabilidade de Sobrevivência"
)
legend("topright", legend = c("Rito Art. 12: Não", "Rito Art. 12: Sim"), col = c("red", "blue"), lty = 1)

# Resultado da ação ----
dec_recorte <- processos |>
  dplyr::filter(em_tramitacao == "Não", classe == "ADPF") |>
  dplyr::select(classe, numero) |>
  dplyr::distinct()

decisoes_adpf <- decisoes |>
  dplyr::filter(classe == "ADPF", numero %in% dec_recorte$numero)

dec_final <- decisoes_adpf |>
  dplyr::filter(
    descricao == "Improcedente" | descricao == "Procedente" | descricao == "Procedente em parte"
  )


decisao_tipos <- decisoes_adpf |>
  dplyr::select(classe, numero, descricao, data) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::mutate(
    merito = dplyr::case_when(
      descricao == "Improcedente" ~ "Sim",
      descricao == "Procedente" ~ "Sim",
      descricao == "Procedente em parte" ~ "Sim",
      TRUE ~ "Não"
    ),
    descricao = dplyr::case_when(
      descricao == "Improcedente" ~ "Improcedente",
      descricao == "Procedente" ~ "Procedente",
      descricao == "Procedente em parte" ~ "Procedente em parte",
      descricao == "Prejudicado" ~ "Prejudicado",
      descricao == "DECISÃO DO(A) RELATOR(A) - PREJUDICADO" ~ "Prejudicado",
      TRUE ~ "Sem mérito"
    )
  ) |>
  dplyr::group_by(numero) |>
  dplyr::arrange(data) |>
  dplyr::slice_max(order_by = data, n = 1) |>
  dplyr::ungroup()

decisao_tipos |>
  dplyr::group_by(merito) |>
  dplyr::summarise(n = dplyr::n())

decisao_tipos |>
  dplyr::group_by(descricao) |>
  dplyr::summarise(n = dplyr::n())

decisao_tipos |>
  ggplot() +
  geom_bar(aes(x = merito, fill = descricao), position = "dodge") +
  scale_fill_manual(
    name = "Resultado",
    values = decJ::utilitario_tscolor("1989_AG")
  ) +
  decJ::theme_decJ()

decJ::save_decJ("PLOT/resultado_adpf.png", l = 24)

processo_sobrevivencia <- processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::left_join(decisao_tipos, by = "numero") |>
  dplyr::select(-classe.y) |>
  dplyr::distinct() |>
  dplyr::mutate(data_baixa = as.Date(data_baixa, tryFormats = "%d/%m/%Y") |> lubridate::ymd()) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1, 0)
  ) |>
  tidyr::replace_na(list(data = lubridate::today())) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data) / lubridate::ddays(1)
  )

processo_sobrevivencia$merito <- as.factor(processo_sobrevivencia$merito)
processo_sobrevivencia$descricao <- as.factor(processo_sobrevivencia$descricao)
processo_sobrevivencia$descricao <- relevel(processo_sobrevivencia$descricao, ref = "Sem mérito")

ajuste_km <- survfit(Surv(tempo, evento) ~ merito, data = processo_sobrevivencia)

print(ajuste_km)

ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Merito",
  legend.labs = c("Não", "Sim"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  # color = "#5B96A5",
  ggtheme = decJ::theme_decJ()
)

teste_logrank <- survdiff(Surv(tempo, evento) ~ merito, data = processo_sobrevivencia)
print(teste_logrank)

modelo_cox <- coxph(Surv(tempo, evento) ~ merito, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox, terms = FALSE)
teste_cox

ggcoxzph(teste_cox)

modelo_corrigido_merito <- coxph(
  Surv(tempo, evento) ~ merito + tt(merito == "Sim"),
  data = processo_sobrevivencia,
  tt = function(x, t, ...) {
    x * log(t + 1)
  }
)

summary(modelo_corrigido_merito)

ajuste_km <- survfit(Surv(tempo, evento) ~ descricao, data = processo_sobrevivencia)

print(ajuste_km)

ggsurvplot(
  ajuste_km,
  data = processo_sobrevivencia,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  # legend.title = "Merito",
  # legend.labs = c("Não", "Sim"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  # color = "#5B96A5",
  ggtheme = decJ::theme_decJ()
)

teste_logrank <- survdiff(Surv(tempo, evento) ~ descricao, data = processo_sobrevivencia)
print(teste_logrank)

modelo_cox <- coxph(Surv(tempo, evento) ~ descricao, data = processo_sobrevivencia)

summary(modelo_cox)

teste_cox <- cox.zph(modelo_cox, terms = FALSE)
teste_cox

ggcoxzph(teste_cox)

modelo_final_descricao <- coxph(
  Surv(tempo, evento) ~ descricao +
    tt(descricao == "Procedente") +
    tt(descricao == "Procedente em parte") +
    tt(descricao == "Improcedente"),
  data = processo_sobrevivencia,
  tt = function(x, t, ...) {
    x * log(t + 1)
  }
)

summary(modelo_final_descricao)
