# Estes são os códigos para reproduzir a figura "Figure A1: Distribution of Population and Urbanized Area in other Urban Agglomerations"

library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)
library(httr)
library(readxl)
library(gridExtra) # Adicionar a biblioteca gridExtra

# Função para carregar dados de um arquivo Excel do GitHub
carregar_dados_github <- function(url, sheet = 1) {
  temp <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp, overwrite = TRUE))
  df <- read_excel(temp, sheet = sheet)
  return(df)
}

# URLs dos arquivos Excel no GitHub
urls <- list(
  "Fortaleza" = "https://github.com/lucvsw/mestrado_ucb/blob/main/dados/frt_new_data_painel.xlsx?raw=true",
  "Curitiba" = "https://github.com/lucvsw/mestrado_ucb/blob/main/dados/cwb_new_data_painel.xlsx?raw=true",
  "Porto Alegre" = "https://github.com/lucvsw/mestrado_ucb/blob/main/dados/poa_new_data_painel.xlsx?raw=true",
  "Recife" = "https://github.com/lucvsw/mestrado_ucb/blob/main/dados/rec_new_data_painel.xlsx?raw=true",
  "Salvador" = "https://github.com/lucvsw/mestrado_ucb/blob/main/dados/slvd_new_data_painel.xlsx?raw=true"
)

# Carregar os dados usando a função genérica
dados_list <- lapply(urls, carregar_dados_github)
names(dados_list) <- names(urls)

# Função genérica para calcular porcentagem em faixas de distância
calcular_porcentagem <- function(df, cidade_nome, variavel) {
  df <- df %>%
    filter(year == 2015) %>%
    mutate(faixa_distancia = cut(dist_km,
                                 breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                                 labels = c("0-5km", "5-10km", "10-15km", "15-20km", "20-25km", "25-30km", ">30km"),
                                 right = FALSE)) %>%
    group_by(faixa_distancia) %>%
    summarise(total = sum(.data[[variavel]])) %>%
    mutate(porcentagem = (total / sum(total)) * 100,
           cidade = cidade_nome) %>%
    st_drop_geometry()
  
  return(df)
}

# Aplicar a função a cada cidade para população e área urbana
porcentagens <- list()
for (cidade in names(dados_list)) {
  dados <- dados_list[[cidade]]
  porcentagem_pop <- calcular_porcentagem(dados, cidade, "pop")
  porcentagem_area <- calcular_porcentagem(dados, cidade, "total_hec")
  porcentagem_pop$tipo <- "Population"
  porcentagem_area$tipo <- "Urbanized Area"
  porcentagens[[cidade]] <- bind_rows(porcentagem_pop, porcentagem_area)
}

# Função para criar gráficos
criar_grafico <- function(df, cidade_nome, figura_num) {
  ggplot(df, aes(x = faixa_distancia, y = porcentagem, fill = tipo)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = paste0(sprintf("%.0f", porcentagem), "%")), 
              vjust = -0.5, 
              position = position_dodge(width = 0.9), 
              size = 4) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(values = c("Population" = "grey", "Urbanized Area" = "#555555")) +
    labs(title = paste(figura_num, "- Distribution of Population and Urbanized Area in", cidade_nome),
         x = "Distance from CBD (km)",
         y = "Percentage by ring",
         fill = NULL,
         caption = NULL) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position = c(1.05, 0.5),
          plot.margin = unit(c(1, 5, 1, 1), "cm"))
}

# Lista de identificadores de figuras
figura_numeros <- c("A1.1", "A1.2", "A1.3", "A1.4", "A1.5")

# Criar e exibir os gráficos
graficos <- lapply(seq_along(names(porcentagens)), function(i) {
  cidade <- names(porcentagens)[i]
  criar_grafico(porcentagens[[cidade]], cidade, figura_numeros[i])
})

# Exibir gráficos em uma única visualização
do.call(grid.arrange, c(graficos, ncol = 2))
