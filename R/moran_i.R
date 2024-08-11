# Comentários: código para a reprodução da "Figure 5: The Formation of Clusters in the Metropolitan Region of Brasília between 1975 and 2015."

library(ggplot2)
library(sf)
library(raster)
library(spdep)
library(dplyr)
library(classInt)
library(scales)

# URLs dos arquivos rasters no GitHub
urls <- c(
  "https://github.com/lucvsw/mestrado_ucb/raw/main/dados/new_masked_built1975_bsb.tif",
  "https://github.com/lucvsw/mestrado_ucb/raw/main/dados/new_masked_built2015_bsb.tif",
  "https://github.com/lucvsw/mestrado_ucb/raw/main/dados/new_masked_pop1975_bsb.tif",
  "https://github.com/lucvsw/mestrado_ucb/raw/main/dados/new_masked_pop2015_bsb.tif"
)

# Definir os nomes dos objetos raster correspondentes
raster_names <- c("new_masked_built1975_bsb", "new_masked_built2015_bsb", "new_masked_pop1975_bsb", "new_masked_pop2015_bsb")

# Inicializar uma lista para armazenar os rasters
rasters <- list()

# Loop para baixar e carregar os rasters
for (i in 1:length(urls)) {
  url <- urls[i]
  temp_file <- tempfile(fileext = ".tif")
  download.file(url, temp_file, mode = "wb")
  rasters[[raster_names[i]]] <- raster(temp_file)
}

# Função para calcular os índices de Moran Global e Local
calcular_indices_moran <- function(raster_data) {
  rook_matrix <- matrix(c(1,1,1,
                          1,0,1,
                          1,1,1), nrow = 3)
  
  moran_global <- Moran(raster_data, w = rook_matrix)
  cat("Índice de Moran Global:", moran_global, "\n")
  
  moran_local <- MoranLocal(raster_data, w = rook_matrix)
  
  zClass <- classIntervals(values(moran_local), n = 5, style = "kmeans")
  moran_local$Category <- cut(values(moran_local), breaks = zClass$brks, labels = c("Low", "Low-High", "Medium", "High-low", "High"))
  
  moran_values <- values(moran_local)
  
  coordinates_df <- data.frame(x = coordinates(moran_local)[, 1],
                               y = coordinates(moran_local)[, 2])
  
  moran_df <- cbind(coordinates_df, MoranLocal = moran_values)
  
  moran_df_filtered <- moran_df %>% na.omit()
  
  return(list(moran_global = moran_global, moran_df_filtered = moran_df_filtered))
}

# Calcular os índices de Moran para cada raster
resultado_built1975 <- calcular_indices_moran(rasters$new_masked_built1975_bsb)
resultado_built2015 <- calcular_indices_moran(rasters$new_masked_built2015_bsb)
resultado_pop1975 <- calcular_indices_moran(rasters$new_masked_pop1975_bsb)
resultado_pop2015 <- calcular_indices_moran(rasters$new_masked_pop2015_bsb)

# Loop para gerar e plotar os gráficos
resultados <- list(resultado_built1975, resultado_built2015, resultado_pop1975, resultado_pop2015)
titulos <- c('Built-up Area, 1975', 'Built-up Area, 2015', 'Population Density, 1975', 'Population Density, 2015')

for (i in 1:length(resultados)) {
  plot <- ggplot() +
    geom_raster(aes(x = x, y = y, fill = MoranLocal.Category), data = resultados[[i]]$moran_df_filtered, show.legend = FALSE) +
    geom_sf(data = new_shapefile_mollweide_bsb, fill = "transparent", color = "darkgrey", size = 0.8) +
    geom_sf(data = regioesadm_shapefile, fill = "transparent", color = "darkgrey", lwd = .2) +
    geom_sf(data = tombamento_shapefile, fill = "transparent", color = "darkgrey", lwd = .35) +
    scale_fill_gradientn(colours = c("#F0F0F0", "blue", "lightblue", "pink", "red"),
                         name = NULL) +
    labs(x = 'Longitude', y = 'Latitude',
         title = NULL,
         subtitle = paste('Brasília -', titulos[i])) +
    theme_void() +
    no_axis
  print(plot)
}

for (i in 1:length(resultados)) {
  moran_df_filtered <- resultados[[i]]$moran_df_filtered
  
  # Converter o data frame para um objeto spatial
  coordinates(moran_df_filtered) <- ~x+y
  
  # Cortar e mascarar o raster com base no shapefile
  moran_df_filtered <- st_as_sf(moran_df_filtered, coords = c("x", "y"), crs = st_crs(new_shapefile_mollweide_bsb))
  moran_df_filtered <- st_intersection(moran_df_filtered, new_shapefile_mollweide_bsb)
  
  plot <- ggplot() +
    geom_sf(data = moran_df_filtered, aes(fill = MoranLocal.Category), show.legend = FALSE) +
    geom_sf(data = tombamento_shapefile, fill = "transparent", color = "darkgrey", lwd = .35) +
    scale_fill_manual(values = c("Low" = "#F0F0F0", "Low-High" = "blue", "Medium" = "lightblue", "High-low" = "pink", "High" = "red"),
                      name = NULL) +
    labs(x = 'Longitude', y = 'Latitude',
         title = NULL,
         subtitle = paste('Brasília -', titulos[i])) +
    theme_void() +
    no_axis
  print(plot)
}
