# Esse é o código utilizado para manipular e criar o dataframe contendo dados da população, área urbanizada e distância ao CBD. Para que ele funcione, é necessário possuir os rasters obtidos pelo GHS-POP e GHS-BUILT-S do GHSL (aqui, não há os códigos para baixá-los, basta acessar: https://human-settlement.emergency.copernicus.eu/download.php)

# Pacotes necessários
library(geobr)
library(raster)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Shapefiles dos municípios do arranjo populacional de Brasília
brasilia1_geobr <- read_municipality(code_muni = 5300108, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia2_geobr <- read_municipality(code_muni = 5215603, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia3_geobr <- read_municipality(code_muni = 5217609, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia4_geobr <- read_municipality(code_muni = 5200258, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia5_geobr <- read_municipality(code_muni = 5205497, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia6_geobr <- read_municipality(code_muni = 5212501, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia7_geobr <- read_municipality(code_muni = 5215231, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia8_geobr <- read_municipality(code_muni = 5219753, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia9_geobr <- read_municipality(code_muni = 5221858, year = 2015, simplified = TRUE, showProgress = FALSE)

# Crie uma lista com todos os objetos brasilia_geobr, isto é, todos os municípios do arranjo de BSB
bsb_geobr_list <- list(
  brasilia1_geobr, brasilia2_geobr, brasilia3_geobr,
  brasilia4_geobr, brasilia5_geobr, brasilia6_geobr,
  brasilia7_geobr, brasilia8_geobr, brasilia9_geobr
)

# Unindo shapefiles
arrange_sf_bsb <- do.call(rbind, bsb_geobr_list)

ggplot() + 
  geom_sf(data = arrange_sf_bsb, color=NA, fill = '#1ba185') +
  theme_void()

# Transformar as coordenadas para a projeção Mollweide (ESRI:54009)
new_shapefile_mollweide_bsb <- st_transform(arrange_sf_bsb, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## Area Of Interest (AOI) - bsb
# Definindo a extensão da área de interesse com base nas coordenadas geográficas da região de Brasília
Brasilia_poly_new <- extent(-4912066.219352, -4369012.094852, -2244411.320875, -1689227.922019) # Coordenadas Mollweide ESRI:54009

# Vetores com os anos e nomes base dos objetos
anos <- seq(1975, 2015, by = 5)

# Loop para criar os novos objetos mascarados
for (ano in anos) {
  # Obtenha os objetos de população e área urbanizada para o ano atual
  pop_object <- get(paste0("pop", ano))
  built_object <- get(paste0("built", ano))
  
  # Cortando os dados do raster da população e área urbanizada com a área da região Brasília
  pop_bsb_raw <- crop(pop_object, Brasilia_poly_new)
  built_bsb_raw <- crop(built_object, Brasilia_poly_new)
  
  # Limitando os dados de população e área urbanizada ao shapefile do Arranjo Populacional de Brasília
  new_masked_pop <- raster::mask(pop_bsb_raw, new_shapefile_mollweide_bsb)
  new_masked_built <- raster::mask(built_bsb_raw, new_shapefile_mollweide_bsb)
  
  # Atribuir os novos objetos com nomes dinâmicos
  assign(paste0("new_masked_pop", ano, "_bsb"), new_masked_pop)
  assign(paste0("new_masked_built", ano, "_bsb"), new_masked_built)
  
  # Plotando os resultados
  plot(new_masked_pop, main = paste("Masked Population", ano))
  plot(new_masked_built, main = paste("Masked Built Area", ano))
} # Agora, temos rasters dos dados limitados ao shapefile de BSB para os anos de 1975-2015, em intervalos de cinco anos.

# Loop para criar os dataframes de 1975-2015 com as variáveis de População, Área Urbanizada e distância ao centro de Brasília
for (ano in anos) {
  # Converte os rasters para data frames
  pop_df <- as.data.frame(get(paste0("new_masked_pop", ano, "_bsb")), xy = TRUE) %>% drop_na()
  built_df <- as.data.frame(get(paste0("new_masked_built", ano, "_bsb")), xy = TRUE) %>% drop_na()
  
  # Atribuindo novos nomes às colunas
  df_populacao <- setNames(pop_df, c("x", "y", "pop"))
  df_builttotal <- setNames(built_df, c("x", "y", "total"))
  
  # Mesclando os dataframes
  df_merged <- cbind(df_populacao, total = df_builttotal$total) %>% drop_na() # Dataframe com as colunas de população e área
  
  # Transforma o data frame em um objeto SpatialPointsDataFrame
  bsbdf_urb <- SpatialPointsDataFrame(coords = df_merged[,c("x", "y")], data = df_merged)
  
  # Criando um objeto SpatialPoints para o centro da cidade
  city_center_bsb <- SpatialPoints(cbind(-4686183.065074, -1943368.445251))
  
  # Calcula as distâncias euclidianas dos pontos em relação ao CBD (rodoviária)
  distancias_urb <- spDists(bsbdf_urb, city_center_bsb)
  
  # Incluindo a distância ao CBD no dataframe
  df_merged_total <- cbind(df_merged, dist = distancias_urb[, 1])
  
  # Criando uma variável de distância ao centro em km
  df_merged_total$dist_km <- df_merged_total$dist/1000
  
  # Criando a área ocupada em hectares
  df_merged_total$total_hec <- df_merged_total$total/10000
  
  # Atribuir o dataframe resultante com um nome dinâmico
  assign(paste0("df", ano, "_merged_total_bsb_new"), df_merged_total)
}

# Loop para adicionar a coluna "year" em cada dataframe
for (ano in anos) {
  # Obter o dataframe correspondente ao ano
  df_name <- paste0("df", ano, "_merged_total_bsb_new")
  df <- get(df_name)
  
  # Adicionar a coluna "year"
  df <- mutate(df, year = ano)
  
  # Salvar de volta o dataframe com a nova coluna
  assign(df_name, df)
} # Isso é feito para criar um painel de dados com todos os anos

# Combine todos os dataframes em um único dataframe de painel
bsb_new_data_painel <- bind_rows(lapply(anos, function(ano) {
  get(paste0("df", ano, "_merged_total_bsb_new"))
}))

# Especificar o nome do arquivo de saída
nome_arquivo <- "bsb_new_data_painel.xlsx"

# Salvar o dataframe de painel como um arquivo Excel
write.xlsx(bsb_new_data_painel, file = nome_arquivo, rowNames = FALSE)

# Visualizar o dataframe de painel
View(bsb_new_data_painel)