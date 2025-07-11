library(ggplot2)
library(survival)
library(survminer)
library(gt)
library(decJ)

knitr::opts_chunk$set(
  warning = FALSE, error = FALSE, message = FALSE,
  fig_width = 12,
  fig_height = 8,
  fig_dpi = 600,
  echo = FALSE
)

cores <- c("#D9523F", "#D1CFC2", "#ACBCCC", "#588383", "#AA6F4E", "#8898A1")

clean_processos <- readRDS('DATA/CLEAN/clean_processos.rds')
clean_decisoes <- readRDS('DATA/CLEAN/clean_decisoes.rds')
adpf_rec <- readRDS('DATA/MISC/adpf_rec.rds')

clean_processos |>
  # seleciona as colunas
  dplyr::select(classe, numero, data_autuacao) |>
  # deixa apenas as colunas únicas
  dplyr::distinct() |>
  dplyr::mutate(
    # cria uma coluna para indicar se a ação está ou não no recorte de pesquisa (2014-2024)
    recorte = ifelse(lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024, "1", "0"),
    # cria uma coluna com o ano
    ano = lubridate::year(data_autuacao)
  ) |> 
  dplyr::group_by(ano) |> 
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::select(ano, n, recorte) |>
  dplyr::distinct() |>
  ggplot(aes(x = ano, y = n)) +
  geom_col(aes(fill = recorte)) +
  geom_smooth(se = FALSE, color = cores[1]) +
  scale_fill_manual(
    breaks = NULL,
    values = c("1" = cores[5], "0" = cores[4])
  ) +
  scale_x_continuous(
    breaks = seq(1988, lubridate::year(Sys.Date()), by = 4)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,600)
  ) +
  labs(
    y = "Número de Processos",
    x = "Ano"
  ) +
  decJ::theme_decJ(y_num = TRUE)

decJ::save_decJ("PLOT/controle_1.png", l = 24, dpi = 300)

clean_processos |>
  dplyr::select(classe, numero, em_tramitacao, data_autuacao) |>
  dplyr::distinct() |>
  dplyr::mutate(ano = lubridate::year(data_autuacao)) |>
  dplyr::select(ano, em_tramitacao) |>
  dplyr::group_by(ano, em_tramitacao) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::distinct() |> 
  ggplot() +
  geom_col(aes(x = ano, y = n, fill = em_tramitacao)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  scale_fill_manual(
    name = "Encerrado",
    labels = c("Sim", "Não"),
    values = c("Não" = cores[4], "Sim" = cores[3])
  ) +
  labs(
    x = "Ano",
    y = "Número de processos"
  ) +
  decJ::theme_decJ(y_num = TRUE)
  
decJ::save_decJ("PLOT/controle_2.png", l = 24, dpi = 300)

# Cria uma tabela para análise de sobrevivência
processos_surv <- clean_processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa) |>
  dplyr::distinct() |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

# Cria o modelo nulo
estimador_km <- survfit(Surv(tempo, evento) ~ 1, data = processos_surv)

# Salva o modelo nulo
saveRDS(estimador_km, "DATA/MODELOS/modelo_01.rds")

m <- estimador_km |> broom::glance() |> dplyr::pull(median) |> lubridate::ddays() / lubridate::dyears(1)
m <- m |> round(2)

rm(m)

ggsurvplot(
  estimador_km,
  data = processos_surv,
  pval = FALSE,
  conf.int = FALSE,
  surv.median.line = "hv",
  # risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  palette = cores[4],
  ggtheme = decJ::theme_decJ()
)

decJ::save_decJ("PLOT/surv_01.png", l = 24, dpi = 300)


estimador_km |> broom::glance() |>
  dplyr::select(records, events, rmean, rmean.std.error, median, conf.low, conf.high, nobs) |>
  gt() |>
  cols_label(
    records = "Registros",
    events = "Eventos",
    rmean = "Média",
    rmean.std.error = "Erro padrão",
    median = "Mediana",
    conf.low = "IC Inferior",
    conf.high = "Ic Superior",
    nobs = "Nº Observações"
  ) |>
  fmt_number(
    columns = everything(),
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  tab_options(
    # Remove a borda superior e inferior da tabela
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    # Adiciona uma linha sólida abaixo do cabeçalho
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    # Remove as linhas verticais
    column_labels.vlines.style = "hidden",
    table_body.vlines.style = "hidden",
    # Adiciona uma linha sólida na parte inferior do corpo da tabela
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(2),
    # Remove as linhas horizontais do corpo da tabela
    table_body.hlines.style = "hidden",
    # Centraliza o título da tabela
    heading.align = "center",
    # Posiciona a nota de rodapé
    source_notes.padding = px(10)
  ) |>
  # Alinhar os cabeçalhos das colunas ao centro
  cols_align(
    align = "center",
    columns = everything()
  )


clean_processos |>
  # seleciona as colunas
  dplyr::select(classe, numero, data_autuacao) |>
  dplyr::filter(classe == "ADPF") |>
  # deixa apenas as colunas únicas
  dplyr::distinct() |>
  dplyr::mutate(
    # cria uma coluna para indicar se a ação está ou não no recorte de pesquisa (2014-2024)
    recorte = ifelse(lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024, "1", "0"),
    # cria uma coluna com o ano
    ano = lubridate::year(data_autuacao)
  ) |> 
  dplyr::group_by(ano) |> 
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::select(ano, n, recorte) |>
  dplyr::distinct() |>
  ggplot(aes(x = ano, y = n)) +
  geom_col(aes(fill = recorte)) +
  geom_smooth(se = FALSE, color = cores[1]) +
  scale_fill_manual(
    breaks = NULL,
    values = c("1" = cores[5], "0" = cores[4])
  ) +
  scale_x_continuous(
    breaks = seq(1988, lubridate::year(Sys.Date()), by = 4)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0,200)
  ) +
  labs(
    y = "Número de Processos",
    x = "Ano"
  ) +
  decJ::theme_decJ(y_num = TRUE)

decJ::save_decJ("PLOT/adpf_1.png", l = 24, dpi = 300)

clean_processos |>
  dplyr::select(classe, numero, ramo_do_direito, data_autuacao) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024) |>
  dplyr::select(-data_autuacao) |>
  dplyr::distinct() |>
  dplyr::group_by(ramo_do_direito) |>
  dplyr::summarise(n = dplyr::n()) |> 
  tidyr::drop_na() |> 
  dplyr::slice_max(n = 10, order_by = n) |>
  ggplot(aes(x = n, y = reorder(ramo_do_direito, n))) +
  geom_col(fill = cores[4]) +
  scale_x_continuous(expand = c(0,0), limits = c(0,870)) +
  labs(
    x = "Número de Processos",
    y = "Ramo do Direito"
  ) +
  decJ::theme_decJ(x_num= TRUE)

clean_processos |>
  dplyr::select(classe, numero, assunto_relacionado, data_autuacao) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024) |>
  dplyr::select(-data_autuacao) |>
  dplyr::distinct() |>
  dplyr::group_by(assunto_relacionado) |>
  dplyr::summarise(n = dplyr::n()) |> 
  tidyr::drop_na() |> 
  dplyr::slice_max(n = 10, order_by = n) |>
  ggplot(aes(x = n, y = reorder(assunto_relacionado, n))) +
  geom_col(fill = cores[4]) +
  scale_x_continuous(expand = c(0,0), limits = c(0,870)) +
  labs(
    x = "Número de Processos",
    y = "Assunto"
  ) +
  decJ::theme_decJ(x_num= TRUE)


clean_processos |>
  dplyr::select(classe, numero, relator_atual, data_autuacao) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024) |>
  dplyr::distinct() |>
  tidyr::drop_na(relator_atual) |> 
  dplyr::group_by(relator_atual) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::filter(relator_atual != "MINISTRO PRESIDENTE")


processos_surv <- clean_processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa) |>
  dplyr::filter(classe == "ADPF") |>
  dplyr::distinct() |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

# Cria o modelo nulo
estimador_km <- survfit(Surv(tempo, evento) ~ 1, data = processos_surv)

# Salva o modelo nulo
saveRDS(estimador_km, "DATA/MODELOS/modelo_02.rds")

m <- estimador_km |> broom::glance() |> dplyr::pull(median) |> lubridate::ddays() / lubridate::dyears(1)
m <- m |> round(2)

rm(m)

ggsurvplot(
  estimador_km,
  data = processos_surv,
  pval = FALSE,
  conf.int = FALSE,
  surv.median.line = "hv",
  # risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  palette = cores[4],
  ggtheme = decJ::theme_decJ()
)

decJ::save_decJ("PLOT/surv_02.png", l = 24, dpi = 300)

processos_surv <- clean_processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024) |>
  dplyr::distinct() |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

# Cria o modelo nulo
estimador_km <- survfit(Surv(tempo, evento) ~ 1, data = processos_surv)

# Salva o modelo nulo
saveRDS(estimador_km, "DATA/MODELOS/modelo_03.rds")

ggsurvplot(
  estimador_km,
  data = processos_surv,
  pval = FALSE,
  conf.int = FALSE,
  surv.median.line = "hv",
  # risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  palette = cores[4],
  ggtheme = decJ::theme_decJ()
)

decJ::save_decJ("PLOT/surv_03.png", l = 24, dpi = 300)


processos_surv <- clean_processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, relator_atual) |>
  tidyr::drop_na(relator_atual) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024, relator_atual != "MINISTRO PRESIDENTE") |>
  dplyr::distinct() |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

m_cox <- coxph(Surv(tempo, evento) ~ relator_atual, data = processos_surv)

# Salva o modelo
saveRDS(m_cox, "DATA/MODELOS/modelo_04.rds")

m_cox |> 
  broom::tidy() |> 
  dplyr::mutate(
    term = stringr::str_remove_all(term, "relator_atual"),
    HR = exp(estimate)) |>
      gt() |>
  fmt_number(
    columns = everything(),
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  tab_options(
    # Remove a borda superior e inferior da tabela
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    # Adiciona uma linha sólida abaixo do cabeçalho
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    # Remove as linhas verticais
    column_labels.vlines.style = "hidden",
    table_body.vlines.style = "hidden",
    # Adiciona uma linha sólida na parte inferior do corpo da tabela
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(2),
    # Remove as linhas horizontais do corpo da tabela
    table_body.hlines.style = "hidden",
    # Centraliza o título da tabela
    heading.align = "center",
    # Posiciona a nota de rodapé
    source_notes.padding = px(10)
  ) |>
  # Alinhar os cabeçalhos das colunas ao centro
  cols_align(
    align = "center",
    columns = everything()
  )

rs_mcox <- cox.zph(m_cox, terms = FALSE)

clean_processos |>
  dplyr::select(classe, numero, tem_rito_art_12, data_autuacao) |>
  dplyr::filter(
    classe == "ADPF",
    lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(tem_rito_art_12) |>
  dplyr::summarise(n = dplyr::n())


processos_surv <- clean_processos |>
  dplyr::select(classe, numero, data_autuacao, data_baixa, tem_rito_art_12) |>
  dplyr::filter(classe == "ADPF", lubridate::year(data_autuacao) >= 2014 & lubridate::year(data_autuacao) <= 2024) |>
  dplyr::distinct() |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

# Cria o modelo
estimador_km <- survfit(Surv(tempo, evento) ~ tem_rito_art_12, data = processos_surv)

# Salva o modelo
saveRDS(estimador_km, "DATA/MODELOS/modelo_05.rds")

ggsurvplot(
  estimador_km,
  data = processos_surv,
  pval = TRUE,
  conf.int = FALSE,
  surv.median.line = "hv",
  # risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  palette = c(cores[4], cores[5]),
  ggtheme = decJ::theme_decJ(),
  legend.labs = c("Não", "Sim"),
  legend.title = "Rito abreviado"
)

decJ::save_decJ("PLOT/surv_04.png", l = 24, dpi = 300)


m_cox <- coxph(Surv(tempo, evento) ~ tem_rito_art_12, data = processos_surv)

teste_mcox <- cox.zph(m_cox, terms = FALSE)
ggcoxzph(teste_mcox)

processos_surv$tem_rito_art_12 <- ifelse(processos_surv$tem_rito_art_12 == "Sim", 1, 0)

m_cox <- coxph(
  formula = Surv(tempo, evento) ~ tem_rito_art_12 + tt(tem_rito_art_12),
  data = processos_surv,
  tt = function(x, t, ...) {
    x * t
  }
)

saveRDS(m_cox, "DATA/MODELOS/modelo_06.rds")

m_cox |> broom::tidy() |> knitr::kable()

clean_decisoes_final <- 
  clean_decisoes |>
  dplyr::filter(classe == "ADPF", numero %in% adpf_rec$numero, subgrupo == "Decisão Final") |>
  dplyr::mutate(
    resultado = dplyr::case_when(
      andamento_agrupado == "Procedente" ~ "Procedente",
      andamento_agrupado == "Improcedente" ~ "Improcedente",
      andamento_agrupado == "Procedente em parte" ~ "Procedente em parte",
      andamento_agrupado == "Prejudicado" ~ "Prejudicado",
      TRUE ~ "Sem mérito"
    )
  )

clean_decisoes_final_pt2 <- clean_decisoes_final |>
  dplyr::filter(
    resultado == "Procedente" | resultado == "Improcedente" | resultado == "Procedente em parte"
  ) |>
  dplyr::select(classe, numero, resultado) |>
  dplyr::distinct() |>
  dplyr::mutate(numero = as.numeric(numero))

clean_decisoes_final_pt3 <- clean_decisoes_final |>
  dplyr::filter(resultado == "Prejudicado") |>
  dplyr::select(classe, numero, resultado) |>
  dplyr::distinct() |>
  dplyr::mutate(numero = as.numeric(numero))

adpf_rec_teste <- adpf_rec |>
  dplyr::left_join(
    clean_decisoes_final_pt2,
    by = dplyr::join_by(numero)
  ) |>
  dplyr::left_join(
    clean_decisoes_final_pt3,
    by = dplyr::join_by(numero),
    keep = FALSE,
    relationship = "one-to-one"
  )

adpf_rec_teste <- adpf_rec_teste |>
  mutate(
    resultado = ifelse(is.na(resultado.x), resultado.y, resultado.x)
  ) |>
  dplyr::select(numero, resultado)


decisoes_final <- adpf_rec_teste
decisoes_final$numero <- as.numeric(decisoes_final$numero)

rm(clean_decisoes_final_pt2, clean_decisoes_final_pt3, adpf_rec_teste)

processos_resultado <- clean_processos |>
  dplyr::filter(classe == "ADPF", numero %in% adpf_rec$numero, em_tramitacao == "Não") |>
  dplyr::select(
    numero, data_autuacao, data_baixa, tem_rito_art_12, em_tramitacao
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(numero = as.numeric(numero)) |>
  dplyr::left_join(
    decisoes_final,
    by = "numero"
  ) |>
  tidyr::replace_na(list(resultado = "Sem mérito")) |>
  dplyr::select(-em_tramitacao) |>
  dplyr::mutate(
    evento = ifelse(!is.na(data_baixa), 1 , 0)
  ) |>
  tidyr::replace_na(list(data_baixa = lubridate::ymd("2025-06-06"))) |>
  dplyr::mutate(
    tempo = lubridate::interval(data_autuacao, data_baixa) / lubridate::ddays(1)
  )

processos_resultado |>
  dplyr::group_by(resultado) |>
  dplyr::summarise(n = dplyr::n()) |>
  ggplot() +
  geom_col(aes(x = resultado, y = n)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 450)) +
  labs(
    x = "Resultado",
    y = "Número de processos"
  ) +
  decJ::theme_decJ(y_num = TRUE)

estimador_km <- survfit(Surv(tempo, evento) ~ resultado, data = processos_resultado)

# Salva o modelo
saveRDS(estimador_km, "DATA/MODELOS/modelo_07.rds")

ggsurvplot(
  estimador_km,
  data = processos_resultado,
  pval = TRUE,
  conf.int = FALSE,
  surv.median.line = "hv",
  # risk.table = TRUE,
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  palette = c(cores[1],cores[2],cores[3], cores[4], cores[5]),
  ggtheme = decJ::theme_decJ()
)

decJ::save_decJ("PLOT/surv_05.png", l = 24, dpi = 300)

m_cox <- coxph(Surv(tempo, evento) ~ resultado, data = processos_resultado)

saveRDS(m_cox, "DATA/MODELOS/modelo_08.rds")


teste_mcox <- cox.zph(m_cox, terms = FALSE)

estimador_km <- survfit(Surv(tempo, evento) ~ resultado + tem_rito_art_12, data = processos_resultado)

# Salva o modelo
saveRDS(estimador_km, "DATA/MODELOS/modelo_09.rds")

m_cox <- coxph(Surv(tempo, evento) ~ (tem_rito_art_12 + resultado), data = processos_resultado)

saveRDS(m_cox, "DATA/MODELOS/modelo_10.rds")
