# Comentários: código para reprodução da tabela "Table 1: Summary Statistics for Population and Urbanized Areas, 1975-2015."

# Dados georreferenciados de Brasília
url <- "https://raw.githubusercontent.com/lucvsw/mestrado_ucb/main/dados/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_new_data <- read_excel(temp_file)

# Vetor de anos desejados
anos <- c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)  # Adicione os anos que você precisa

# Loop pelos anos
for (ano in anos) {
  # Filtrar os dados para o ano atual
  dados_ano <- get(paste0("bsb_new_data", ano))
  
  # Calcula a porcentagem de valores zero para cada variável
  percent_zeros <- sapply(dados_ano[c("pop", "total_hec")], function(x) mean(x == 0) * 100)
  
  # Exibe as estatísticas descritivas, incluindo a porcentagem de zeros
  statistics <- describe(dados_ano[c("pop", "total_hec")])
  statistics <- statistics[c("n", "mean", "sd", "max")]
  statistics$percent_zeros <- percent_zeros
  
  # Imprimir os resultados
  cat("Ano:", ano, "\n")
  print(statistics)
  cat("\n")
}