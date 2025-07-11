## Remove NAs
dec_adpf <- dec_adpf |> tidyr::drop_na(texto)
dec_adpf <- dec_adpf |> dplyr::mutate(
  texto = decJ::utilitario_remover_acentos(texto),
  texto = gsub("ü", "u", texto),
  texto = gsub("Ü", "U", texto),
  texto = stringr::str_remove_all(texto, "º")
)

dec_adpf <- readRDS("DATA/CLEAN/dec_adpf.rds")

## Criação de um corpus
corpus_adpf <- quanteda::corpus(
  dec_adpf,
  docid_field = "doc_id",
  text_field = "texto"
)

## Criação dos tokens

tokens_adpf <- quanteda::tokens(
  corpus_adpf,
  remove_punct = T,
  remove_symbols = T,
  remove_numbers = T,
  remove_url = T,
  remove_separators = T,
  split_hyphens = T,
  split_tags = T
)

tokens_adpf <- quanteda::tokens_tolower(
  tokens_adpf
)

## Criação dos dicionários

dicionario.Criar <- function(token, n = 2, arquivo) {
  ngrams <- quanteda::tokens_ngrams(
    token,
    n = n,
    concatenator = " "
  )

  ngrams.dfm <- quanteda::dfm(ngrams)
  ngrams.freq <- quanteda::topfeatures(ngrams.dfm, n = 500)

  write.csv2(ngrams.freq,
    file = paste(arquivo, n, "gram.csv", sep = ""),
    quote = F,
    row.names = T,
    fileEncoding = "UTF-8"
  )
}

## Aplicar sobre 6 grams
# dicionario.Criar(tokens_adpf, n = 6, "DATA/MISC/")
dic_6 <- read.delim("DATA/MISC/dic6.txt", header = FALSE)
tokens_adpf <- quanteda::tokens_compound(
  tokens_adpf,
  pattern = quanteda::phrase(dic_6$V1),
  concatenator = "_"
)

## Aplicar sobre 5 grams
# dicionario.Criar(tokens_adpf, n = 5, "DATA/MISC/")
dic_5 <- read.delim("DATA/MISC/dic5.txt", header = FALSE)
tokens_adpf <- quanteda::tokens_compound(
  tokens_adpf,
  pattern = quanteda::phrase(dic_5$V1),
  concatenator = "_"
)

## Aplicar sobre 4 grams
# dicionario.Criar(tokens_adpf, n = 4, "DATA/MISC/")
dic_4 <- read.delim("DATA/MISC/dic4.txt", header = FALSE)
tokens_adpf <- quanteda::tokens_compound(
  tokens_adpf,
  pattern = quanteda::phrase(dic_4$V1),
  concatenator = "_"
)

## Aplicar sobre 3 grams
# dicionario.Criar(tokens_adpf, n = 3, "DATA/MISC/")
dic_3 <- read.delim("DATA/MISC/dic3.txt", header = FALSE)
tokens_adpf <- quanteda::tokens_compound(
  tokens_adpf,
  pattern = quanteda::phrase(dic_3$V1),
  concatenator = "_"
)

## Remover stopwords
tokens_adpf <- quanteda::tokens_remove(
  tokens_adpf,
  pattern = quanteda::stopwords("pt")
)

## Aplicar sobre 2 grams
# dicionario.Criar(tokens_adpf, n = 2, "DATA/MISC/")
dic_2 <- read.delim("DATA/MISC/dic2.txt", header = FALSE)
tokens_adpf <- quanteda::tokens_compound(
  tokens_adpf,
  pattern = quanteda::phrase(dic_2$V1),
  concatenator = "_"
)

## Lematização
lemas <- read.delim("DATA/MISC/lexique_pt.txt", header = FALSE, col.names = c("palavra", "lema", "cat"))
lemas <- lemas |>
  dplyr::mutate(
    dplyr::across(c(palavra, lema), decJ::utilitario_remover_acentos)
  )

tokens_adpf <- quanteda::tokens_replace(
  tokens_adpf,
  lemas$palavra,
  lemas$lema
)

# Salvar para colocar no iramuteq
iramuteq <- sapply(tokens_adpf, paste, collapse = " ") |>
  as.data.frame() |>
  tibble::rownames_to_column()
names(iramuteq) <- c("doc_id", "texto")
