knitr::opts_chunk$set(
  warning = FALSE, error = FALSE, message = FALSE
)

# Importa os processos
raw_processos <- readxl::read_xlsx("DATA/RAW/raw_processos.xlsx", na = "*NI*") # <1>

# Importa as decisões
raw_decisoes <- readxl::read_xlsx("DATA/RAW/raw_decisoes.xlsx",
  na = "*NI*",
  col_types = c("guess", "date", "guess", "guess", "guess", "guess") # <2>
)

# Importa os legitimados
raw_legitimados <- readxl::read_xlsx("DATA/RAW/raw_legitimados.xlsx",
  na = "*NI*"
)

# Salvar os arquivos raw em rds
saveRDS(raw_processos, "DATA/RAW/raw_processos.rds")
saveRDS(raw_decisoes, "DATA/RAW/raw_decisoes.rds")
saveRDS(raw_legitimados, "DATA/RAW/raw_legitimados.rds")

# Extrai as decisões monocráticas
raw_monocraticas <- decJ::stf_jurisprudencia(
  classe = "ADPF",
  base = 'decisoes',
  quantidade = 3000
)

# Extrai os acórdãos
raw_acordaos <- decJ::stf_jurisprudencia(
  classe = "ADPF",
  base = "acordaos",
  quantidade = 3000
)

# Salva os arquivos raw em rds
saveRDS(raw_monocraticas, "DATA/RAW/raw_monocraticas.rds")
saveRDS(raw_acordaos, "DATA/RAW/raw_acordaos.rds")

# Função stf_jurisprudencia do pacote decJ

stf_jurisprudencia = function(busca = NULL, classe = NULL, base = c("acordaos", "decisoes"), quantidade = 25){

  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51") # <1>

  if (!is.null(busca) & is.null(classe)) { # <2>
    body <- busca_jurisprudencia # <2>
    body$query$bool$filter[[1]]$query_string$query <- busca # <2>
    body$post_filter$bool$must[[1]]$term$base <- base # <2>
  } else if (is.null(busca) & !is.null(classe)) { # <2>
    body <- busca_classe # <2>
    body$query$bool$filter$query_string$query <- classe # <2>
    body$post_filter$bool$must$term$base <- base # <2>
  } else if ((!is.null(busca) & !is.null(classe))) { # <2>
    cli::cli_alert_danger("Essa funcao so funciona com busca por palavras chaves OU por classe. Ainda estamos desenvolvendo uma forma de trabalhar com as duas buscas juntas.") # <2>
    return(NULL) # <2>
  } # <2>

  num_iteracoes <- ceiling(quantidade / 250) # <3>

  if (quantidade > 250) {
    body$size <- 250
  } else {
    body$size <- quantidade
  }
 
  purrr::map_dfr(1:num_iteracoes, purrr::slowly(~{ 
    body$from <- (.x - 1) * 250
    htmlSTF <- httr::POST( # <4>
      "https://jurisprudencia.stf.jus.br/api/search/search", # <4>
      body = body, # <4>
      encode = "json", header # <4>
    ) # <4>
    getContent <- jsonlite::fromJSON(httr::content(htmlSTF, "text")) # <4>
    dados <- getContent$result$hits$hits$`_source` # <4>
  }, rate = purrr::rate_delay(5)), .progress = list(format = "Extraindo {cli::pb_bar} {cli::pb_elapsed}")) 
}

raw_processos <- readRDS('DATA/RAW/raw_processos.rds')
raw_decisoes <- readRDS('DATA/RAW/raw_decisoes.rds')
raw_legitimados <- readRDS('DATA/RAW/raw_legitimados.rds')
raw_monocraticas <- readRDS('DATA/RAW/raw_monocraticas.rds')
raw_acordaos <- readRDS('DATA/RAW/raw_acordaos.rds')

tibble::tribble(
  ~"Tabela", ~"Nº Colunas", ~"Nº Linhas", ~"Nº de Dados",
  "raw_processos", ncol(raw_processos), nrow(raw_processos), ncol(raw_processos) * nrow(raw_processos),
  "raw_decisoes", ncol(raw_decisoes), nrow(raw_decisoes), ncol(raw_decisoes) * nrow(raw_decisoes),
  "raw_legitimados", ncol(raw_legitimados), nrow(raw_legitimados), ncol(raw_legitimados) * nrow(raw_legitimados),
  "raw_acordaos", ncol(raw_acordaos), nrow(raw_acordaos), ncol(raw_acordaos) * nrow(raw_acordaos),
  "raw_monocraticas", ncol(raw_monocraticas), nrow(raw_monocraticas), ncol(raw_monocraticas) * nrow(raw_monocraticas)
) |> knitr::kable()

# Limpa o nome das variáveis
clean_processos <- janitor::clean_names(raw_processos)
clean_decisoes <- janitor::clean_names(raw_decisoes)
clean_legitimados <- janitor::clean_names(raw_legitimados)
clean_acordaos <- janitor::clean_names(raw_acordaos)
clean_monocraticas <- janitor::clean_names(raw_monocraticas)
