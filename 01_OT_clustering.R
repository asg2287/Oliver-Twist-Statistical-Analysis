if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
library(ggplot2)
library(scales)


output_dir <- "/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/"
desktop_dir <- "/Users/ale/Desktop/"
file_path <- file.path(output_dir, "oliver_twist_components.csv")

oliver_data <- read.csv(file_path, stringsAsFactors = FALSE)
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
    character = char, novel = "oliver_twist",
    N = 0, C = 0, I = 0, A = 0, DC = 0, DN = 0,
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

char_summary <- do.call(rbind, summary_list)
char_summary$TOTAL_SCORES <- rowSums(char_summary[, component_tags])
char_summary <- char_summary[char_summary$TOTAL_SCORES > 0, ]


pareto_df <- char_summary[order(-char_summary$TOTAL_SCORES), c("character", "TOTAL_SCORES")]
pareto_df$Cumulative_Score <- cumsum(pareto_df$TOTAL_SCORES)
total_footprint <- sum(pareto_df$TOTAL_SCORES)
pareto_df$Percentage <- if(total_footprint > 0) (pareto_df$Cumulative_Score / total_footprint) * 100 else 0

write.csv(pareto_df, file = file.path(output_dir, "test1_pareto_analysis.csv"), row.names = FALSE)


set.seed(42)
scaled_matrix <- scale(char_summary[, component_tags])
scaled_matrix[is.na(scaled_matrix)] <- 0

kmeans_fit <- kmeans(scaled_matrix, centers = 3, nstart = 25)


raw_clusters <- kmeans_fit$cluster
cluster_labels <- character(length(raw_clusters))


for (i in seq_along(raw_clusters)) {
  c_num <- raw_clusters[i]
  c_size <- sum(raw_clusters == c_num)
  c_max_score <- max(char_summary$TOTAL_SCORES[raw_clusters == c_num])
  
  if (c_max_score == max(char_summary$TOTAL_SCORES)) {
    cluster_labels[i] <- "K1"
  } else if (c_size < 10) {
    cluster_labels[i] <- "K2"
  } else {
    cluster_labels[i] <- "K3"
  }
}
char_summary$KMeans_Cluster <- cluster_labels

write.csv(char_summary[, c("character", component_tags, "TOTAL_SCORES", "KMeans_Cluster")],
          file = file.path(output_dir, "test2_kmeans_clusters.csv"), row.names = FALSE)

char_summary$Components_Checked <- rowSums(char_summary[, component_tags] > 0)

char_summary$Functional_Tier <- "Peripheral Noise (<= 2)"
char_summary$Functional_Tier[char_summary$Components_Checked == 3] <- "Minor Secondary Tier (3)"
char_summary$Functional_Tier[char_summary$Components_Checked %in% c(4, 5)] <- "Major Secondary Tier (4-5)"
char_summary$Functional_Tier[char_summary$Components_Checked == 6] <- "Protagonist Tier (6)"

final_analysis <- char_summary[order(-char_summary$TOTAL_SCORES),
                               c("character", component_tags, "TOTAL_SCORES", "KMeans_Cluster", "Components_Checked", "Functional_Tier")]

write.csv(final_analysis, file = file.path(output_dir, "test3_functional_tiers.csv"), row.names = FALSE)


top_30_pareto <- head(pareto_df, 30)
top_30_pareto$index <- 1:nrow(top_30_pareto)

max_raw_score <- max(top_30_pareto$TOTAL_SCORES)
scale_factor <- max_raw_score / 100

p1 <- ggplot(top_30_pareto, aes(x = factor(character, levels = character))) +
  geom_bar(aes(y = TOTAL_SCORES), stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_path(aes(y = Percentage * scale_factor, group = 1), color = "darkred", size = 1.2) +
  geom_point(aes(y = Percentage * scale_factor), color = "darkred", size = 2) +
  geom_vline(xintercept = 7.5, linetype = "dashed", color = "black", size = 1) +
  scale_y_continuous(
    name = "Total Score",
    sec_axis = sec_axis(~ . / scale_factor, name = "Cumulative Percentage (%)", labels = function(x) paste0(round(x), "%"))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, face = "bold"),
    axis.title.y.right = element_text(color = "darkred", face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  ) +
  labs(title = "Narrative Architecture Footprint: Pareto Power Law Distribution", x = "Character Node")

ggsave(filename = file.path(desktop_dir, "figure1_pareto_chart.png"), plot = p1, width = 12, height = 6.5)

p2 <- ggplot(final_analysis, aes(x = Components_Checked, y = TOTAL_SCORES, color = KMeans_Cluster)) +
  geom_jitter(size = 4, width = 0.15, alpha = 0.85) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("K1" = "#D95F02", "K2" = "#7570B3", "K3" = "#1B9E77")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    legend.position = "right"
  ) +
  labs(
    title = "Number of Components Checked vs Total Score",
    x = "Number of Components Checked (2,3,4,5 or 6)",
    y = "Total Score (Log Scale)",
    color = "K-Means Cluster Assignment"
  ) +
  geom_text(aes(label = as.character(character)), hjust = -0.15, vjust = 0.5, size = 2.8, check_overlap = TRUE, color = "black")

ggsave(filename = file.path(desktop_dir, "figure2_character_landscape.png"), plot = p2, width = 12, height = 7)

cat("\nDone! Visualizations compiled and exported successfully.\n")
