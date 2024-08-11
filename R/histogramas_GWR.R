# Comentários: código para a reprodução da Figura: "Figure 3: Distribution of GWR Coefficients in 1975 and 2015". O texto incluído na figura foi incluído por fora.

# Dados georreferenciados de Brasília
url <- "https://raw.githubusercontent.com/lucvsw/mestrado_ucb/main/dados/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_painel <- read_excel(temp_file)

# Filtrar os dados para o ano de 1975
bsb_data_1975 <- bsb_painel %>%
  filter(year == 1975)

# Filtrar os dados para o ano de 2015
bsb_data_2015 <- bsb_painel %>%
  filter(year == 2015)

# Coordenadas
coords_1975 <- cbind(bsb_data_1975$x, bsb_data_1975$y)
coords_2015 <- cbind(bsb_data_2015$x, bsb_data_2015$y)

# Definir os parâmetros para o loop
parametros <- list(
  list(data = bsb_data_1975, coords = coords_1975, var_dep = "pop", nome = "pop_1975"),
  list(data = bsb_data_1975, coords = coords_1975, var_dep = "total_hec", nome = "area_1975"),
  list(data = bsb_data_2015, coords = coords_2015, var_dep = "pop", nome = "pop_2015"),
  list(data = bsb_data_2015, coords = coords_2015, var_dep = "total_hec", nome = "area_2015")
)

# Loop sobre os parâmetros
for (param in parametros) {
  # Seleção do bandwidth
  bwG <- gwr.sel(as.formula(paste("log(", param$var_dep, ") ~ dist_km + I(dist_km^2)", sep = "")), 
                 data = param$data, 
                 coords = param$coords, 
                 gweight = gwr.Gauss, 
                 verbose = FALSE)
  
  # Geração do modelo GWR
  gwrG <- gwr(as.formula(paste("log(", param$var_dep, ") ~ dist_km + I(dist_km^2)", sep = "")), 
              data = param$data, 
              coords = param$coords, 
              bandwidth = bwG, 
              gweight = gwr.Gauss, 
              hatmatrix = TRUE)
  
  # Armazenar os resultados em variáveis com nomes dinâmicos
  assign(paste0("bwG_", param$nome), bwG)
  assign(paste0("gwrG_", param$nome), gwrG)
}

# Lista dos objetos GWR e nomes para os resultados
gwr_models <- list(
  gwrG_pop_1975 = gwrG_pop_1975,
  gwrG_area_1975 = gwrG_area_1975,
  gwrG_pop_2015 = gwrG_pop_2015,
  gwrG_area_2015 = gwrG_area_2015
)

# Loop sobre os modelos GWR
for (model_name in names(gwr_models)) {
  # Extrair coeficientes do modelo
  coeficientes <- as.data.frame(gwr_models[[model_name]]$SDF)
  
  # Calcular estatísticas descritivas para cada coeficiente
  estatisticas <- sapply(coeficientes, function(x) {
    c(
      Mean = mean(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),  # Mediana
      `St. dev.` = sd(x, na.rm = TRUE),
      `Avg. St. Err.` = sqrt(var(x, na.rm = TRUE) / length(x)),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      `# Obs.` = sum(!is.na(x))
    )
  })
  
  # Identificar se é densidade populacional ou área urbana e o ano
  tipo <- ifelse(grepl("pop", model_name), "Densidade Populacional", "Área Urbana")
  ano <- ifelse(grepl("1975", model_name), "1975", "2015")
  
  # Imprimir uma mensagem de identificação
  cat("\nEstatísticas para", tipo, ano, "\n")
  
  # Atribuir o nome ao objeto das estatísticas descritivas
  assign(paste0("estatisticas_", model_name), estatisticas)
  
  # Imprimir estatísticas descritivas apenas para X.Intercept. e dist_km
  print(estatisticas[, c("X.Intercept.", "dist_km")])
}


# Lista de configurações dos histogramas
histogram_configs <- list(
  list(
    name = "Densidade populacional 1975",
    data = gwrG_pop_1975$SDF$dist_km,
    binwidth = 0.02,
    xlim = c(-0.4, 0.4),
    subtitle = "3C. Population Density, 1975"
  ),
  list(
    name = "Área Urbana Ocupada 1975",
    data = gwrG_area_1975$SDF$dist_km,
    binwidth = 0.03,
    xlim = c(-1.3, 0.3),
    subtitle = "3A. Urbanized area, 1975"
  ),
  list(
    name = "Densidade populacional 2015",
    data = gwrG_pop_2015$SDF$dist_km,
    binwidth = 0.1,
    xlim = c(-1, 3.5),
    subtitle = "3D. Population Density, 2015"
  ),
  list(
    name = "Área Urbana Ocupada 2015",
    data = gwrG_area_2015$SDF$dist_km,
    binwidth = 0.007,
    xlim = c(-0.2, 0.1),
    subtitle = "3B. Urbanized area, 2015"
  )
)

# Criar uma lista para armazenar os histogramas
histograms <- list()

# Loop para criar os histogramas
for (config in histogram_configs) {
  df <- data.frame(coef_dist_km = config$data)
  histograms[[config$name]] <- ggplot(df, aes(x = coef_dist_km)) +
    geom_histogram(binwidth = config$binwidth, fill = "grey", color = "black") +
    labs(subtitle = config$subtitle, x = "Dist. to CBD Coef.", y = "Frequency") +
    xlim(config$xlim) +
    theme_cowplot()
}

# Usar grid.arrange para organizar e plotar os histogramas
grid.arrange(
  histograms[["Área Urbana Ocupada 1975"]],
  histograms[["Área Urbana Ocupada 2015"]],
  histograms[["Densidade populacional 1975"]],
  histograms[["Densidade populacional 2015"]],
  ncol = 2
)

