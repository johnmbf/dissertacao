# PACOTES ---------------------------------------------------------------------

library(chromote)
library(httr2)
library(ggplot2)
library(survival)
library(survminer)
library(gt)
library(decJ)
library(quanteda.textplots)
library(factoextra)
library(FactoMineR)
library(ggrepel)

# OBJETOS ---------------------------------------------------------------------

cores <- c(decJ::utilitario_tscolor("1989"), decJ::utilitario_tscolor("taylorSwift"))

clean_processos <- readRDS("DATA/CLEAN/clean_processos.rds")
clean_decisoes <- readRDS("DATA/CLEAN/clean_decisoes.rds")
clean_legitimados <- readRDS("DATA/CLEAN/clean_legitimados.rds")

adpf_rec <- readRDS("DATA/MISC/adpf_rec.rds")
dic_legitimados <- readRDS("DATA/MISC/dic_legitimados.rds")

# FUNÇÕES ---------------------------------------------------------------------

extrair_texto_juris_stf <- function(dir) {
  lista_dec <- list.files(dir, full.names = TRUE)
  lista_dec <- data.frame(
    doc = lista_dec
  )
  lista_dec <- lista_dec |>
    dplyr::mutate(
      doc_id = stringr::str_split_i(doc, "\\/", -1) |> stringr::str_remove("\\.pdf")
    )

  areas <- readRDS("DATA/MISC/area.rds")

  purrr::map2_df(lista_dec$doc, lista_dec$doc_id, ~ {
    p <- tabulizer::get_n_pages(.x)

    texto <- tabulizer::extract_text(.x, pages = 1:p, area = areas) |>
      paste(collapse = "") |>
      stringr::str_squish() |>
      stringr::str_trim()

    data.frame(doc_id = .y, texto = texto)
  }, .progress = TRUE)
}

baixar_juris <- function(doc_id, path) {
  output_file_path <- glue::glue("{path}/{doc_id}.pdf")

  b <- ChromoteSession$new()
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
  url_principal <- glue::glue("https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID={doc_id}")
  b$Page$navigate(url_principal, wait_ = TRUE)
  Sys.sleep(5)
  c_value <- b$Network$getCookies()$cookies[[2]]$value
  c_name <- b$Network$getCookies()$cookies[[2]]$name
  c_value2 <- b$Network$getCookies()$cookies[[1]]$value
  c_name2 <- b$Network$getCookies()$cookies[[1]]$name

  request("https://redir.stf.jus.br/paginadorpub/paginador.jsp") |>
    req_url_query(
      docTP = "TP",
      docID = doc_id
    ) |>
    req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      `Accept-Language` = "pt-BR,pt;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6,es-ES;q=0.5,es;q=0.4,fr-FR;q=0.3,fr;q=0.2,es-CL;q=0.1",
      `Cache-Control` = "max-age=0",
      Connection = "keep-alive",
      Referer = glue::glue("https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID={doc_id}"),
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "same-origin",
      `Sec-Fetch-User` = "?1",
      `Upgrade-Insecure-Requests` = "1",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36 Edg/137.0.0.0",
      `sec-ch-ua` = '"Microsoft Edge";v="137", "Chromium";v="137", "Not/A)Brand";v="24"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"Windows"',
      Cookie = glue::glue("{c_name2}={c_value2}; {c_name}={c_value}")
    ) |>
    req_perform(path = glue::glue("{path}/{doc_id}.pdf"))
  b$close()

  if (!file.exists(output_file_path)) {
    # A mensagem é sobre o documento específico com aquele doc_id
    message(glue::glue("AVISO: O documento com a identificação '{doc_id}' não foi criado."))
  } else {
    message(glue::glue("Sucesso: Documento '{doc_id}.pdf' salvo."))
  }
}
