library(tidyverse)
library(survival)

csv_path <- "/Users/ale/Desktop/Oliver Twist/oliver_twist_components.csv"

if (!file.exists(csv_path)) stop("Target oliver_twist_components.csv file is missing at the specified path.")


tier_6_map <- tibble(
  Name = c(
    "Oliver", "Sikes", "The Jew", "Mr. Bumble", "Noah", "Rose", "Mr. Brownlow",
    "Monks", "Nancy", "Master Bates", "Mr. Giles", "Toby", "Mr. Grimwig", "Harry",
    "Mrs. Bumble", "Mrs. Maylie", "Brittles", "Mr. Losberne", "Mr. Sowerberry",
    "Blathers", "Mr. Chitling", "Mrs. Sowerberry", "Mr. Gamfield"
  ),
  Cluster = c(
    "K1", "K2", "K2", "K2",
    "K2", "K2", "K2",
    "K3", "K3", "K3", "K3", "K3",
    "K3", "K3", "K3", "K3", "K3",
    "K3", "K3", "K3", "K3", "K3", "K3"
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
    Distribution_Type     = ifelse(Coeff_of_Variation < 1.0, "Predictable (Periodic)", "Unpredictable (Bursty)"),
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
  summarise(
    Absence_Duration = n(),
    Max_Chapter = max(Chapter),
    .groups = "drop"
  ) %>%
  filter(Presence == 0) %>%
  mutate(
    Reintroduced = ifelse(Max_Chapter == 57, 0, 1)
  )

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
  arrange(Cluster, Coeff_of_Variation)

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

print("=== CLUSTER LEVEL MACRO RHYTHM PERFORMANCE ===")
print(cluster_rhythm_summary)

print("=== INDIVIDUAL TIER 6 METRICS BY CLUSTER ASSIGNMENT ===")
print(comprehensive_rhythm_metrics, n = Inf)


write_csv(comprehensive_rhythm_metrics, "/Users/ale/Desktop/TIER6_CHARACTERS_RHYTHM_ANALYSIS.csv")
write_csv(cluster_rhythm_summary, "/Users/ale/Desktop/CLUSTER_LEVEL_RHYTHM_SUMMARY.csv")


absence_with_clusters <- absence_spells %>%
  left_join(tier_6_map, by = "Name") %>%
  mutate(Cluster = as.factor(Cluster))

surv_fit_cluster <- survfit(Surv(Absence_Duration, Reintroduced) ~ Cluster, data = absence_with_clusters)


png("/Users/ale/Desktop/OLIVER_TWIST_SURVIVAL_BY_CLUSTER.png", width = 2400, height = 1600, res = 300)
plot(surv_fit_cluster, conf.int = FALSE, col = c("#8B0000", "#2E4053", "#1E8449"), lwd = 2.5,
     xlab = "Chapters Spent in Absence (Time-at-Risk)",
     ylab = "Probability of Remaining Absent",
     main = "Narrative Reintroduction Survival Function by K-Means Cluster",
     font.main = 2, family = "serif")


legend("topright", legend = levels(absence_with_clusters$Cluster),
       col = c("#8B0000", "#2E4053", "#1E8449"), lwd = 2.5, bty = "n")
grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray70")
dev.off()

cat("\n==================================================================\n")
cat("SUCCESS: Survival analysis processed smoothly using K1/K2/K3 names.\n")
cat("Target Destination Verified: /Users/ale/Desktop/\n")
cat("==================================================================\n")
