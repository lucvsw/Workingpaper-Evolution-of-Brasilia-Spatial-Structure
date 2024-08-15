# Comentários: este arquivo produz a Figura: "Figure 2: Spatial Distribution of the Urbanized Area and Population in Brasília". A área do tombamento ao lado de cada um dos mapas foi adicionada posteriormente, fora do R, para produzir a figura como está no artigo.

# Pacotes necessários

library(httr)
library(readxl)
library(dplyr)
library(ggplot2)
library(colorspace)
library(sf)
library(geobr)
library(gridExtra)

# Dados georreferenciados de Brasília

url <- "https://raw.githubusercontent.com/lucvsw/Workingpaper-Evolution-of-Brasilia-Spatial-Structure/main/data/bsb_new_data_painel.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Dados em painel
bsb_painel <- read_excel(temp_file)


# Filtrar os dados para o ano de 1975
bsb_1975 <- bsb_painel %>%
  filter(year == 1975)

# Filtrar os dados para o ano de 2015
bsb_2015 <- bsb_painel %>%
  filter(year == 2015)

# Shapefile das regiões administrativas

# URLs para os arquivos do shapefile no GitHub
base_url <- "https://raw.githubusercontent.com/lucvsw/Workingpaper-Evolution-of-Brasilia-Spatial-Structure/main/data/regioes_administrativas/"
files <- c("regioes_administrativas.shp",
           "regioes_administrativas.shx",
           "regioes_administrativas.dbf",
           "regioes_administrativas.prj")

# Função para baixar e salvar arquivos temporariamente
download_shapefile <- function(url, destfile) {
  GET(url, write_disk(destfile, overwrite = TRUE))
}

# Criar um diretório temporário
temp_dir <- tempdir()

# Baixar cada arquivo do shapefile
lapply(files, function(file) {
  download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
})

# Ler o shapefile usando sf::st_read
shapefile_path <- file.path(temp_dir, "regioes_administrativas.shp")
regioesadm_shapefile <- st_read(shapefile_path)


# Transformar as coordenadas para a projeção Mollweide (ESRI:54009)
regioesadm_shapefile <- st_transform(regioesadm_shapefile, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Fazendo com que o shapefile tenha apenas as dimensoes XY em vez de XYZ
regioesadm_shapefile <- st_zm(regioesadm_shapefile)

# Shapefile do arranjo populacional de Brasília
## Arranjos populacionais
brasilia1_geobr <- read_municipality(code_muni = 5300108, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia2_geobr <- read_municipality(code_muni = 5215603, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia3_geobr <- read_municipality(code_muni = 5217609, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia4_geobr <- read_municipality(code_muni = 5200258, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia5_geobr <- read_municipality(code_muni = 5205497, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia6_geobr <- read_municipality(code_muni = 5212501, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia7_geobr <- read_municipality(code_muni = 5215231, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia8_geobr <- read_municipality(code_muni = 5219753, year = 2015, simplified = TRUE, showProgress = FALSE)
brasilia9_geobr <- read_municipality(code_muni = 5221858, year = 2015, simplified = TRUE, showProgress = FALSE)

# Crie uma lista com todos os objetos brasilia_geobr
bsb_geobr_list <- list(
  brasilia1_geobr, brasilia2_geobr, brasilia3_geobr,
  brasilia4_geobr, brasilia5_geobr, brasilia6_geobr,
  brasilia7_geobr, brasilia8_geobr, brasilia9_geobr
)

# Unindo shapefiles
arrange_sf_bsb <- do.call(rbind, bsb_geobr_list)

# Transformar as coordenadas para a projeção Mollweide (ESRI:54009)
new_shapefile_mollweide_bsb <- st_transform(arrange_sf_bsb, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Função para criar o mapa dos arranjos populacionais
criar_mapa <- function(data, fill_var, title, fill_name, fill_limits = NULL, show_legend = TRUE) {
  mapa <- ggplot() +
    geom_raster(aes(x = x, y = y, fill = !!sym(fill_var)), data = data) +
    geom_sf(data = new_shapefile_mollweide_bsb, fill = "transparent", color = "darkgrey", size = .7) +
    geom_sf(data = regioesadm_shapefile, fill = "transparent", color = "darkgrey", lwd = .2) +
    geom_sf(data = tombamento_shapefile, fill = "transparent", color = "darkgrey", lwd = .35) +
    labs(title = title, subtitle = NULL, caption = NULL) +
    scale_fill_continuous_sequential(palette = "YlGnBu", name = fill_name, limits = fill_limits) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 1, vjust = 1),  # Título maior e à esquerda
      plot.caption = element_text(size = 12, hjust = 1.2, vjust = 14)  # Fonte maior e no canto inferior direito
    )
  
  # Condicional para mostrar ou não a legenda
  if (show_legend) {
    mapa <- mapa + theme(
      legend.position = c(0.01, 0.25),  # Mover a legenda para o canto inferior esquerdo
      legend.direction = "vertical",
      legend.key.height = unit(0.7, 'cm'), 
      legend.key.width = unit(0.7, 'cm'),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  } else {
    mapa <- mapa + theme(legend.position = "none")
  }
  
  return(mapa)
}

# Dados para os mapas de 1975
dens_map1975 <- criar_mapa(bsb_1975, "pop", "2C. Brasília: Population Density, 1975", "Population", c(0, 22000))
urb_map1975 <- criar_mapa(bsb_1975, "total_hec", "2A. Brasília: Urbanized Area, 1975", "Urbanized Area", c(0, 53))

# Dados para os mapas de 2015 sem legenda
dens_map2015 <- criar_mapa(bsb_2015, "pop", "2D. Brasília: Population Density, 2015", "Population", show_legend = FALSE)
urb_map2015 <- criar_mapa(bsb_2015, "total_hec", "2B. Brasília: Urbanized Area, 2015", "Urbanized Area", show_legend = FALSE)

# Mapas da área de tombamento
CBD_bsb <- data.frame(x = -4686183.065074, y = -1943368.445251) # ponto do CBD
criar_mapa_tombamento <- function(data, fill_var, title, fill_name, fill_limits = NULL) {
  ggplot() +
    geom_raster(aes(x = x, y = y, fill = !!sym(fill_var)), data = data) +
    geom_sf(data = tombamento_shapefile, fill = "transparent", color = "darkgrey", lwd = .35) +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    scale_fill_continuous_sequential(palette = "YlGnBu", name = fill_name, limits = fill_limits) +
    theme_void() +
    theme(legend.position = "none") +
    coord_sf(xlim = c(bounds["xmin"], bounds["xmax"]), ylim = c(bounds["ymin"], bounds["ymax"])) +
    geom_point(data = CBD_bsb, aes(x = x, y = y), color = "black", size = 10, shape = 22)
}

# Dados para os mapas do tombamento de 1975
tombamento_dens_map1975 <- criar_mapa_tombamento(bsb_1975, "pop", "2C. Brasília: Population Density, 1975", "Population", c(0, 22000))
tombamento_urb_map1975 <- criar_mapa_tombamento(bsb_1975, "total_hec", "2A. Brasília: Urbanized Area, 1975", "Urbanized Area", c(0, 53))

# Dados para os mapas do tombamento de 2015
tombamento_dens_map2015 <- criar_mapa_tombamento(bsb_2015, "pop", "2D. Brasília: Population Density, 2015", "Population")
tombamento_urb_map2015 <- criar_mapa_tombamento(bsb_2015, "total_hec", "2B. Brasília: Urbanized Area, 2015", "Urbanized Area")

# Apresentar os plots
grid.arrange(urb_map1975, urb_map2015, 
             dens_map1975, dens_map2015,
             ncol=2)
