# Importar os dados ---

raw_processos <- readxl::read_xlsx("DATA/RAW/raw_processos.xlsx", na = "*NI*")
raw_decisoes <- readxl::read_xlsx("DATA/RAW/raw_decisoes.xlsx",
  na = "*NI*",
  col_types = c("guess", "date", "guess", "guess", "guess", "guess")
)
raw_legitimados <- readxl::read_xlsx("DATA/RAW/raw_legitimados.xlsx",
  na = "*NI*"
)