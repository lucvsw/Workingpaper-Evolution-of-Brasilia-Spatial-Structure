# Dados georreferenciados de Brasília
url <- "https://raw.githubusercontent.com/lucvsw/mestrado_ucb/main/dados/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_painel <- read_excel(temp_file)

# Filtrar os dados para o ano de 2015
bsb_data_2015 <- bsb_painel %>%
  filter(year == 2015)

# Convertendo bsb_data_2015 para um objeto sf
bsb2015ra_sf <- st_as_sf(bsb_data_2015, coords = c("x", "y"))

# Definindo a projeção correta para bsb2015ra_sf para corresponder à do shapefile
st_crs(bsb2015ra_sf) <- st_crs(new_shapefile_mollweide_bsb)

# Criando uma coluna "dummy" vazia para cada RA usando "code_muni" como identificador
unique_muni_codes <- unique(new_shapefile_mollweide_bsb$code_muni)
for (muni_code in unique_muni_codes) {
  bsb2015ra_sf[paste0("dummy_", muni_code)] <- 0
}

# Loop através de cada município e define 1 para as coordenadas dentro dos limites
for (muni_code in unique_muni_codes) {
  muni_geometry <- new_shapefile_mollweide_bsb[new_shapefile_mollweide_bsb$code_muni == muni_code, ]
  bsb2015ra_sf[paste0("dummy_", muni_code)] <- ifelse(st_within(bsb2015ra_sf, muni_geometry, sparse = FALSE), 1, 0)
}

view(bsb2015ra_sf)

# Lista dos códigos de municípios e a nova sequência de nomes dummy
code_muni_list <- unique(new_shapefile_mollweide_bsb$code_muni)
dummy_names <- paste0("dummy_", 1:length(code_muni_list))

# Substituir os nomes das colunas em bsb2015ra_sf
for (i in seq_along(code_muni_list)) {
  old_name <- paste0("dummy_", code_muni_list[i])
  new_name <- dummy_names[i]
  
  if (old_name %in% names(bsb2015ra_sf)) {
    names(bsb2015ra_sf)[names(bsb2015ra_sf) == old_name] <- new_name
  }
}

# Filtrar os dados para observações com total_hec > 10
filtered_data <- subset(bsb2015ra_sf, total_hec > 10)

# Listas para armazenar os resultados das regressões
reg_pop_list <- list()
reg_area_list <- list()

# Termos de distância para as regressões
distance_terms <- list(
  "dist_km",
  "dist_km + I(dist_km^2)",
  "dist_km + I(dist_km^2) + I(dist_km^3)"
)

# Função para criar a fórmula de regressão sem dummies
create_formula_no_dummies <- function(dep_var, dist_term) {
  as.formula(paste0("log(", dep_var, ") ~ ", dist_term))
}

# Função para criar a fórmula de regressão com dummies
create_formula_with_dummies <- function(dep_var, dist_term) {
  as.formula(paste0("log(", dep_var, ") ~ ", dist_term, " + ", paste(paste0("dummy_", 1:9), collapse = " + ")))
}

# Loop para as regressões sem dummies
for (dist_term in distance_terms) {
  # Regressões para área urbana sem dummies
  formula_area_no_dummies <- create_formula_no_dummies("total_hec", dist_term)
  reg_area <- lm(formula_area_no_dummies, data = filtered_data)
  reg_area_list[[dist_term]] <- reg_area
  
  # Regressões para pop sem dummies
  formula_pop_no_dummies <- create_formula_no_dummies("pop", dist_term)
  reg_pop <- lm(formula_pop_no_dummies, data = filtered_data)
  reg_pop_list[[dist_term]] <- reg_pop
}

# Regressões com dummies e três termos de distância
dist_term_three_terms <- "dist_km + I(dist_km^2) + I(dist_km^3)"
formula_area_no_dummies_three_terms <- create_formula_no_dummies("total_hec", dist_term_three_terms)
reg_area_no_dummies_three_terms <- lm(formula_area_no_dummies_three_terms, data = filtered_data)
reg_area_list[[dist_term_three_terms]] <- reg_area_no_dummies_three_terms

formula_pop_no_dummies_three_terms <- create_formula_no_dummies("pop", dist_term_three_terms)
reg_pop_no_dummies_three_terms <- lm(formula_pop_no_dummies_three_terms, data = filtered_data)
reg_pop_list[[dist_term_three_terms]] <- reg_pop_no_dummies_three_terms

# Regressões com dummies e três termos de distância
formula_area_with_dummies <- create_formula_with_dummies("total_hec", dist_term_three_terms)
reg_area_with_dummies <- lm(formula_area_with_dummies, data = filtered_data)
reg_area_list[[paste0(dist_term_three_terms, " + dummies")]] <- reg_area_with_dummies

formula_pop_with_dummies <- create_formula_with_dummies("pop", dist_term_three_terms)
reg_pop_with_dummies <- lm(formula_pop_with_dummies, data = filtered_data)
reg_pop_list[[paste0(dist_term_three_terms, " + dummies")]] <- reg_pop_with_dummies

# Apresentar os resultados das regressões
cat("\nResultados das regressões para área urbana:\n")
for (dist_term in names(reg_area_list)) {
  cat("\nRegressão com termo de distância:", dist_term, "\n")
  print(summary(reg_area_list[[dist_term]]))
}

cat("\nResultados das regressões para população:\n")
for (dist_term in names(reg_pop_list)) {
  cat("\nRegressão com termo de distância:", dist_term, "\n")
  print(summary(reg_pop_list[[dist_term]]))
}