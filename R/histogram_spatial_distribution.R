### Histograma para 2015 - Área urbana
# Vetores com os anos
anos <- c(1975, 2015)

# Loop para calcular a distribuição percentual da população e área urbanizada em cada ring de distância ao CBD para 1975 e 2015
for (ano in anos) {
  # Filtrar os dados para o ano específico
  subset_df_total <- bsb_new_data_painel %>%
    filter(year == ano)
  
  # Dividir as distâncias em faixas
  subset_df_total$dist_f <- cut(subset_df_total$dist_km, breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                                labels = c("0-5km", "5-10km", "10-15km", "15-20km", "20-25km", "25-30km", ">30km"))
  
  # Calcular a soma da área urbana em cada faixa de distância
  area_summary_total <- subset_df_total %>%
    group_by(dist_f, year) %>%
    summarize(total_area = sum(total_hec)) %>%
    filter(!is.na(dist_f)) %>%
    group_by(year) %>%
    mutate(percent = total_area / sum(total_area) * 100)
  
  # Adicionando o total da população para cada faixa de distância
  pop_summary_total <- subset_df_total %>%
    group_by(dist_f, year) %>%
    summarize(total_pop = sum(pop)) %>%
    filter(!is.na(dist_f)) %>%
    group_by(year) %>%
    mutate(percent = total_pop / sum(total_pop) * 100)
  
  # Atribuir os dataframes resultantes com nomes dinâmicos
  assign(paste0("area_summary", ano, "total"), area_summary_total)
  assign(paste0("pop_summary", ano, "total"), pop_summary_total)
}

##################################################

# Manipulando os dados para criar os plots de participação percentual de cada faixa de distância de distância na variação total da população e área urbana entre 1975 e 2015
variaveis <- c("pop", "total_hec")

# Lista para armazenar os resultados
resultados_variacao <- list()

for (var in variaveis) {
  # Filtrar os dados para os anos de 1975 e 2015
  subset_df <- bsb_new_data_painel %>%
    filter(year %in% c(1975, 2015))
  
  # Dividir as distâncias em faixas
  subset_df$dist_f <- cut(subset_df$dist_km, breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                          labels = c("0-5km", "5-10km", "10-15km", "15-20km", "20-25km", "25-30km", ">30km"))
  
  # Calcular a soma da variável em cada faixa de distância para os anos de 1975 e 2015
  summary_df <- subset_df %>%
    group_by(dist_f, year) %>%
    summarize(total_value = sum(.data[[var]]), .groups = 'drop')
  
  # Calcular a variação percentual entre os anos de 1975 e 2015 em cada faixa de distância
  variation_df <- summary_df %>%
    pivot_wider(names_from = year, values_from = total_value) %>%
    mutate(variation = `2015` - `1975`) %>% 
    filter(!is.na(dist_f))
  
  # Calcular a variação total
  total_variation <- sum(variation_df$variation)
  
  # Calcular a variação proporcional
  variation_df$prop_variation <- variation_df$variation / total_variation * 100
  
  # Armazenar o resultado na lista
  resultados_variacao[[var]] <- variation_df
}

# Acessar os resultados para população e área urbana
pop_variation_result <- resultados_variacao[["pop"]]
area_variation_result <- resultados_variacao[["total_hec"]]

# Exibir os resultados
pop_variation_result
area_variation_result

###################################

# Combinar os dados da distribuição percentual por ring da área urbana ocupada de 1975 e 2015 em um único dataframe
combined_data <- rbind(area_summary1975total, area_summary2015total)
combined_data$year <- factor(rep(c("1975", "2015"), each = nrow(area_summary1975total)))

# Criar o gráfico de barras combinado para área urbanizada
combined_plot <- ggplot(combined_data, aes(x = dist_f, y = percent, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("1975" = "lightgrey", "2015" = "darkgrey")) +
  labs(x = "Distance (km)",
       y = "Percentage per ring",
       title = NULL,
       subtitle = "Urbanized area's distribution, 1975-2015") +
  theme_cowplot()
combined_plot

# Combinar os dados da distribuição percentual por ring da população de 1975 e 2015 em um único dataframe
combined_data_pop <- rbind(pop_summary1975total, pop_summary2015total)
combined_data_pop$year <- factor(rep(c("1975", "2015"), each = nrow(pop_summary1975total)))

# Criar o gráfico de barras combinado para a população
combined_plot_pop <- ggplot(combined_data_pop, aes(x = dist_f, y = percent, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("1975" = "lightgrey", "2015" = "darkgrey")) +
  labs(x = "Distance (km)",
       y = "Percentage per ring",
       title = NULL,
       subtitle = "Population's distribution, 1975-2015") +
  theme_cowplot()
combined_plot_pop

########################################

# Criar os gráficos de barras da participação percentual de cada ring na variação no total entre 1975 e 2015 
pop_variation_plot2015 <- ggplot(pop_variation_result, aes(x = dist_f, y = prop_variation)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Distance (km)",
       y = NULL,
       title = NULL,
       subtitle = "Population's growth, 1975-2015") +
  theme_cowplot()
pop_variation_plot2015

area_variation_plot2015 <- ggplot(area_variation_result, aes(x = dist_f, y = prop_variation)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Distance (km)",
       y = NULL,
       title = NULL,
       subtitle = "Urbanized area's growth, 1975-2015 ") +
  theme_cowplot()
area_variation_plot2015

grid.arrange(area_variation_plot2015, combined_plot, 
             pop_variation_plot2015, combined_plot_pop, ncol = 2)
