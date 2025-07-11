library(FactoMineR)
library(factoextra)

data_temp <- processos_resultado |>
  dplyr::select(numero, tipo, resultado) |>
  dplyr::distinct() |>
  dplyr::filter(resultado != "Sem julgamento")

tbl_cont <- table(data_temp$tipo, data_temp$resultado)
res.ca <- CA(tbl_cont, graph = FALSE)
fviz_ca(res.ca, repel = TRUE, geom = "text")

data_temp$tipo <- factor(data_temp$tipo)
data_temp$resultado <- factor(data_temp$resultado)
# data_temp <- mltools::one_hot(data.table::as.data.table(data_temp), cols = "tipo")
# data_temp <- data_temp |> dplyr::select(-numero)

m_logit <- nnet::multinom(resultado ~ tipo, data = data_temp, model = TRUE)
m_logit_s <- nnet::multinom(resultado ~ tipo - 1, data = data_temp, model = TRUE) # escolha

all.equal(
  predict(m_logit, type = "probs"),
  predict(m_logit_s, type = "probs")
)

m_0 <- nnet::multinom(resultado ~ 1, data = data_temp, model = TRUE)

m_logit <- glm(resultado ~ tipo - 1, data = data_temp, family = "binomial")

effects::allEffects(m_logit) |> plot()
