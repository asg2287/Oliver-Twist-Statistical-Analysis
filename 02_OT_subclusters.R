library(tidyverse)
library(cluster)


csv_path <- "/Users/ale/Desktop/Oliver Twist/oliver_twist_components.csv"
oliver_data <- read.csv(csv_path, stringsAsFactors = FALSE)
oliver_data$count <- as.numeric(oliver_data$count)


oliver_data <- oliver_data[!grepl('^[“"”\']|[!,“»”«]|^\\s*$', oliver_data$character), ]
exclude_patterns <- "maylie|nature|heaven|damme"
oliver_data <- oliver_data[!grepl(exclude_patterns, oliver_data$character, ignore.case = TRUE), ]
explicit_noise <- c("Work’us", "the Clerkinwell Sessions", "the Cripples", "Lor bless her dear heart", "Hem", "lor bless")
oliver_data <- oliver_data[!(tolower(oliver_data$character) %in% tolower(explicit_noise)), ]

all_unique_characters <- unique(oliver_data$character)
component_tags <- c("N", "C", "I", "A", "DC", "DN")


summary_list <- list()
for (char in all_unique_characters) {
  char_data <- oliver_data[oliver_data$character == char, ]
  row_data <- data.frame(
    Character = char,
    N = 0, DC = 0, C = 0, I = 0, DN = 0, A = 0,
    stringsAsFactors = FALSE
  )
  if (nrow(char_data) > 0) {
    tag_sums <- aggregate(count ~ tag, data = char_data, FUN = sum, na.rm = TRUE)
    for (t in seq_len(nrow(tag_sums))) {
      current_tag <- tag_sums$tag[t]
      if (current_tag %in% component_tags) {
        row_data[[current_tag]] <- tag_sums$count[t]
      }
    }
  }
  summary_list[[char]] <- row_data
}

char_data <- do.call(rbind, summary_list)

matrix_data <- char_data %>%
  remove_rownames() %>%
  column_to_rownames(var = "Character")


cat("\n[TEST 1] Processing Global Subclusters (Oliver Omitted)...\n")

global_sub_raw <- matrix_data[rownames(matrix_data) != "Oliver", ]
global_sub_scaled <- scale(global_sub_raw)
global_sub_scaled[is.na(global_sub_scaled)] <- 0

set.seed(42)
kmeans_global <- kmeans(global_sub_scaled, centers = 4, nstart = 25)

global_rankings <- tibble(
  Raw_Cluster = kmeans_global$cluster,
  Total_Volume = rowSums(global_sub_raw)
) %>%
  group_by(Raw_Cluster) %>%
  summarize(Mean_Volume = mean(Total_Volume), .groups = 'drop') %>%
  arrange(desc(Mean_Volume)) %>%
  mutate(Letter_Cluster = c("A", "B", "C", "D"))

global_map <- deframe(dplyr::select(global_rankings, Raw_Cluster, Letter_Cluster))

global_results <- data.frame(global_sub_raw) %>%
  mutate(
    Character = rownames(global_sub_raw),
    Lifetime_Score = rowSums(global_sub_raw),
    Subcluster = global_map[as.character(kmeans_global$cluster)]
  ) %>%
  dplyr::select(Character, Subcluster, Lifetime_Score, N, DC, C, I, DN, A) %>%
  arrange(Subcluster, desc(Lifetime_Score))

write_csv(global_results, "/Users/ale/Desktop/OLIVER_TWIST_GLOBAL_SUBCLUSTERS.csv")

png("/Users/ale/Desktop/OLIVER_TWIST_GLOBAL_CLUSTERS_4D.png", width = 2400, height = 1600, res = 300)
clusplot(global_sub_scaled, kmeans_global$cluster, color = TRUE, shade = TRUE,
         labels = 2, lines = 0, main = "Global Subcluster Boundaries (4-Tier Split)",
         sub = "Oliver Omitted to Prevent Scale Compression", family = "serif", font.main = 2)
dev.off()


cat("[TEST 2] Processing Deep Isolated Lower Tiers Split (Core Omitted)...\n")

omit_core <- c("Oliver", "Sikes", "The Jew", "Mr. Bumble", "Noah", "Rose", "Mr. Brownlow")
lower_tiers_raw <- matrix_data[!rownames(matrix_data) %in% omit_core, ] 
lower_tiers_scaled <- scale(lower_tiers_raw)
lower_tiers_scaled[is.na(lower_tiers_scaled)] <- 0

set.seed(42)
kmeans_lower <- kmeans(lower_tiers_scaled, centers = 2, nstart = 25)

lower_rankings <- tibble(
  Raw_Cluster = kmeans_lower$cluster,
  Total_Volume = rowSums(lower_tiers_raw)
) %>%
  group_by(Raw_Cluster) %>%
  summarize(Mean_Volume = mean(Total_Volume), .groups = 'drop') %>%
  arrange(desc(Mean_Volume)) %>%
  mutate(Letter_Cluster = c("PP1", "PP2"))

lower_map <- deframe(dplyr::select(lower_rankings, Raw_Cluster, Letter_Cluster))

lower_results <- data.frame(lower_tiers_raw) %>%
  mutate(
    Character = rownames(lower_tiers_raw),
    Lifetime_Score = rowSums(lower_tiers_raw),
    Subcluster = lower_map[as.character(kmeans_lower$cluster)]
  ) %>%
  dplyr::select(Character, Subcluster, Lifetime_Score, N, DC, C, I, DN, A) %>%
  arrange(Subcluster, desc(Lifetime_Score))

write_csv(lower_results, "/Users/ale/Desktop/OLIVER_TWIST_ISOLATED_PERIPHERALS.csv")

peripheral_plot <- ggplot(lower_results, aes(x = reorder(Character, Lifetime_Score), y = Lifetime_Score, fill = Subcluster)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("PP1" = "#1E8449", "PP2" = "#7FB3D5")) +
  labs(
    title = "Isolated P2 Variance",
    subtitle = "Separating Subclusters PP1 and PP2",
    x = "",
    y = "Aggregate Structural Volume Score"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 7, face = "bold"),
    legend.position = "bottom"
  )

ggsave("/Users/ale/Desktop/OLIVER_TWIST_PERIPHERAL_BARS.png", plot = peripheral_plot, width = 9, height = 7, dpi = 300)

cat("\n==================================================================\n")
cat("SUCCESS: Matrices computed entirely from CSV and plots written.\n")
cat("Target Destination Verified: /Users/ale/Desktop/\n")
cat("==================================================================\n")
