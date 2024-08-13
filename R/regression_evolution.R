# Esses são os códigos para produzir a Table 2: The Evolution of Density and Urbanization Gradients in Brasília: 1975 to 2015.

# Dados georreferenciados de Brasília
url <- "https://raw.githubusercontent.com/lucvsw/mestrado_ucb/main/dados/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_painel <- read_excel(temp_file)

bsb_data <- bsb_painel %>%
  filter(pop > 0, total_hec > 10)

# Anos para os quais você deseja executar as regressões
anos <- c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

# Listas para armazenar os resultados das regressões
reg_pop_list <- list()
reg_area_list <- list()

# Loop através dos anos
for (ano in anos) {
  # Filtrar os dados para o ano corrente
  bsb_data_ano <- bsb_data %>%
    filter(year == !!ano)
  
  # Executar a regressão para a população
  reg_pop <- lm(log(pop) ~ dist_km + I(dist_km^2), data = bsb_data_ano)
  reg_pop_list[[as.character(ano)]] <- reg_pop
  
  # Executar a regressão para a área urbana
  reg_area <- lm(log(total_hec) ~ dist_km + I(dist_km^2), data = bsb_data_ano)
  reg_area_list[[as.character(ano)]] <- reg_area
}

# Apresentar os resultados das regressões para todos os anos
for (ano in anos) {
  cat("\nResultados das regressões para o ano de", ano, "\n")
  cat("\nRegressão População:\n")
  print(summary(reg_pop_list[[as.character(ano)]]))
  cat("\nRegressão Área Urbana:\n")
  print(summary(reg_area_list[[as.character(ano)]]))
}