library(tidyverse)
library(survival)


csv_path <- "/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/oliver_twist_components.csv"

if (!file.exists(csv_path)) stop("Target oliver_twist_components.csv file is missing.")

tier_6_map <- tibble(
  Name = c(
    "Oliver", "Sikes", "The Jew", "Mr. Bumble", "Noah", "Rose", "Mr. Brownlow",
    "Monks", "Nancy", "Master Bates", "Mr. Giles", "Toby", "Mr. Grimwig", "Harry",
    "Mrs. Bumble", "Mrs. Maylie", "Brittles", "Mr. Losberne", "Mr. Sowerberry",
    "Blathers", "Mr. Chitling", "Mrs. Sowerberry", "Mr. Gamfield"
  ),
  Cluster = c(
    "Protagonist", "Major Secondaries", "Major Secondaries", "Major Secondaries", 
    "Major Secondaries", "Major Secondaries", "Major Secondaries",
    "Peripheral", "Peripheral", "Peripheral", "Peripheral", "Peripheral", 
    "Peripheral", "Peripheral", "Peripheral", "Peripheral", "Peripheral", 
    "Peripheral", "Peripheral", "Peripheral", "Peripheral", "Peripheral", "Peripheral"
  )
)

tier_6_characters <- tier_6_map$Name


df_raw <- read_csv(csv_path, show_col_types = FALSE)

timeline_all <- df_raw %>%
  rename(Character = character, Chapter = chapter, Count = count) %>%
  mutate(
    Character = str_trim(as.character(Character)),
    Character = case_when(
      Character == "Fagin" ~ "The Jew",
      TRUE ~ Character
    ),
    Chapter = as.integer(Chapter)
  ) %>%
  filter(!is.na(Chapter)) %>%
  group_by(Character, Chapter) %>% 
  summarise(Presence = as.numeric(max(as.numeric(Count), na.rm = TRUE) > 0), .groups = 'drop') %>%
  rename(Name = Character) %>%
  filter(Name %in% tier_6_characters) %>%
  complete(Name = tier_6_characters, Chapter = 1:57, fill = list(Presence = 0)) %>%
  left_join(tier_6_map, by = "Name")


universal_recurrence <- timeline_all %>%
  filter(Presence == 1) %>%
  arrange(Name, Chapter) %>%
  group_by(Name) %>%
  mutate(Interval = Chapter - lag(Chapter)) %>%
  filter(!is.na(Interval)) %>% 
  summarise(
    Total_Active_Chapters = sum(Presence),
    Mean_Absence_Interval = mean(Interval),
    SD_Absence_Interval   = sd(Interval),
    Coeff_of_Variation    = SD_Absence_Interval / Mean_Absence_Interval,
    Distribution_Type     = ifelse(Coeff_of_Variation < 1.0, "Predictable", "Bursty"),
    .groups = 'drop'
  )

markov_results <- timeline_all %>%
  arrange(Name, Chapter) %>%
  group_by(Name) %>%
  mutate(
    Next_State = lead(Presence),
    Transition = case_when(
      Presence == 0 & Next_State == 0 ~ "P_00", 
      Presence == 0 & Next_State == 1 ~ "P_01", 
      Presence == 1 & Next_State == 0 ~ "P_10", 
      Presence == 1 & Next_State == 1 ~ "P_11", 
      TRUE ~ as.character(NA)
    )
  ) %>%
  filter(!is.na(Transition)) %>%
  count(Transition) %>%
  mutate(Probability = n / sum(n)) %>%
  dplyr::select(-n) %>%  
  pivot_wider(names_from = Transition, values_from = Probability, values_fill = list(Probability = 0))

for(col in c("P_00", "P_01", "P_10", "P_11")) {
  if(!col %in% names(markov_results)) markov_results[[col]] <- 0
}

absence_spells <- timeline_all %>%
  arrange(Name, Chapter) %>%
  group_by(Name) %>%
  mutate(spell_id = cumsum(Presence != lag(Presence, default = first(Presence))) + 1) %>%
  group_by(Name, spell_id, Presence) %>%
  summarise(Absence_Duration = n(), Max_Chapter = max(Chapter), .groups = "drop") %>%
  filter(Presence == 0) %>%
  mutate(Reintroduced = ifelse(Max_Chapter == 57, 0, 1))

survival_summary <- absence_spells %>%
  group_by(Name) %>%
  summarise(
    Total_Absence_Spells = n(),
    Median_Absence_Length = median(Absence_Duration),
    Reintroduction_Rate  = sum(Reintroduced) / n(),
    .groups = "drop"
  )

comprehensive_rhythm_metrics <- tier_6_map %>%
  left_join(universal_recurrence, by = "Name") %>%
  left_join(markov_results, by = "Name") %>%
  left_join(survival_summary, by = "Name") %>%
  arrange(Cluster, desc(Total_Active_Chapters))

cluster_rhythm_summary <- comprehensive_rhythm_metrics %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Active_Chapters     = mean(Total_Active_Chapters, na.rm = TRUE),
    Mean_Absence_Gap        = mean(Mean_Absence_Interval, na.rm = TRUE),
    Pooled_CV               = mean(Coeff_of_Variation, na.rm = TRUE),
    Avg_Reintroduction_Prob = mean(P_01, na.rm = TRUE),
    Avg_Sustained_Prob      = mean(P_11, na.rm = TRUE),
    Median_Absence_Duration = mean(Median_Absence_Length, na.rm = TRUE),
    .groups = 'drop'
  )

write_csv(comprehensive_rhythm_metrics, "/Users/ale/TIER6_CHARACTERS_RHYTHM_ANALYSIS.csv")
write_csv(cluster_rhythm_summary, "/Users/ale/CLUSTER_LEVEL_RHYTHM_SUMMARY.csv")


plot_data <- timeline_all %>%
  mutate(Name = factor(Name, levels = rev(tier_6_characters)))

dispersion_plot <- ggplot(plot_data, aes(x = Chapter, y = Name)) +
  geom_tile(aes(fill = factor(Presence)), color = "white", lwd = 0.2) +
  scale_fill_manual(values = c("0" = "#F2F4F4", "1" = "#2C3E50"), 
                    labels = c("Absent", "Present"), name = "State") +
  scale_x_continuous(breaks = seq(1, 57, by = 4), expand = c(0,0)) +
  labs(
    title = "Oliver Twist: Longitudinal Character Dispersion (Tier 6 Cohort)",
    subtitle = "Binary presence/absence barcode distribution mapping over 57 structural chapters",
    x = "Narrative Chapter Timeline",
    y = ""
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b=4)),
    plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b=12)),
    axis.text.y = element_text(face = "bold", color = "#2C3E50", size = 9),
    axis.text.x = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave("/Users/ale/OLIVER_TWIST_TIER6_DISPERSION.png", plot = dispersion_plot, width = 11, height = 7, dpi = 300)


kmeans_dispersion_plot <- ggplot(plot_data, aes(x = Chapter, y = Name)) +
  geom_tile(aes(fill = Cluster), color = "white", lwd = 0.2) +

  aes(alpha = factor(Presence)) +
  scale_alpha_manual(values = c("0" = 0.1, "1" = 1.0), guide = "none") +
  scale_fill_manual(values = c("Protagonist" = "#8B0000", "Major Secondaries" = "#2E4053", "Peripheral" = "#1E8449")) +
  scale_x_continuous(breaks = seq(1, 57, by = 5), expand = c(0,0)) +
  facet_grid(Cluster ~ ., scales = "free_y", space = "free_y", drop = TRUE) +
  labs(
    title = "Oliver Twist Structural Rhythms by K-Means Profiles",
    subtitle = "Faceted tracking blocks isolating presence bands across clustering segments",
    x = "Narrative Chapter Timeline",
    y = ""
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b=10)),
    strip.text.y = element_text(face = "bold", angle = 270, size = 9),
    strip.background = element_rect(fill = "#EAEDED"),
    axis.text.y = element_text(face = "bold", size = 8),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

ggsave("/Users/ale/OLIVER_TWIST_KMEANS_DISPERSION.png", plot = kmeans_dispersion_plot, width = 11, height = 7, dpi = 300)

print("SUCCESS: Both individual and clustered dispersion charts are saved inside /Users/ale/")
