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
clean_legitimados <- readRDS('DATA/CLEAN/clean_legitimados.rds')
adpf_rec <- readRDS('DATA/MISC/adpf_rec.rds')
legitimados_tipo <- read.csv('DATA/CSV/legitimados_tipo.csv') |> dplyr::select(-X)

# Tratamentos iniciais

## Limpa a tabela de legitimados
legitimados_adpf <- clean_legitimados |>
  dplyr::filter(
    classe == "ADPF",
    numero %in% adpf_rec$numero
  ) |>
  dplyr::select(-classe) |>
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
  )

## Adiciona o tipo de legitimados

legitimados_adpf <- legitimados_adpf |>
  dplyr::left_join(legitimados_tipo,
                   by = "ativo") |>
  dplyr::mutate(
    tipo = tipo |> as.factor() |> forcats::fct_recode(
      "Outros" = "0",
      "Presidente" = "1",
      "Mesa do SF" = "2",
      "Mesa do CD" = "3",
      "Mesa da AL" = "4",
      "Governador" = "5",
      "PGR" = "6",
      "OAB" = "7",
      "Partidos" = "8",
      "Entidades" = "9"
     )
  )

legitimados_adpf |>
  dplyr::select(numero, tipo) |>
  dplyr::distinct() |>
  dplyr::group_by(tipo) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::ungroup() |>
  ggplot() +
  geom_col(aes(x = n, y = reorder(tipo, n)), fill = cores[4]) +
  labs(
    x = "Número de processos",
    y = "Legitimado"
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 380)) +
  decJ::theme_decJ(x_num = TRUE)

decJ::save_decJ("PLOT/fig_leg_2.png", l = 24, dpi = 300)
