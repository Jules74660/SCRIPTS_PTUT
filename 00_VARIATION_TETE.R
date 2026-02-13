
# faire des boxplots sur les 5 derniers points centré sur 0 à paritr des données Y pour voir la variation de la tête

fichiers_AM <- list.files("INPUT/RESULTATS/", pattern = "AM", full.names = TRUE)

data_AM <- fichiers_AM %>% map_df(~ read_csv(.x, show_col_types = TRUE) %>%
                                    mutate(source = basename(.x))) %>% rename(points = ...1)
 
# rajouter 15 points par blocs
data_AM$trace <- rep(1:15, length.out = nrow(data_AM))

# centrer la variation sur 0 garder que les 3 derniers et renommer

variation <- data_AM %>%
  rename(Y = `Y (cm)`,X = `X (cm)`) %>%
  filter(trace %in% c(13, 14, 15)) %>%
  group_by(source) %>%
  mutate(Y_centre = Y - mean(Y, na.rm = TRUE)) %>%
  ungroup()
  
head(variation)

ggplot(variation, aes(x = factor(trace), y = Y_centre)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.color = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(,
    x = "Point le long de la tête (13 = base, 15 = extrémité)",
    y = "Déviation verticale centrée (cm)"
  ) +
  theme_minimal(base_size = 13)
