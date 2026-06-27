
rm(list = ls())

required_packages <- c("tidyverse", "MASS", "ggplot2", "ggrepel")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}

output_dir <- "/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/"
components_file <- file.path(output_dir, "oliver_twist_components.csv")

if (!file.exists(components_file)) {
  stop(sprintf("[CRITICAL ERROR] Target file missing: %s", components_file))
}


oliver_data <- read.csv(components_file, stringsAsFactors = FALSE)
oliver_data$count <- as.numeric(oliver_data$count)
oliver_data$chapter <- as.numeric(oliver_data$chapter)

oliver_data <- oliver_data[!grepl('^[“"”\']|[!,“»”«]|^\\s*$', oliver_data$character), ]
exclude_patterns <- "maylie|nature|heaven|damme"
oliver_data <- oliver_data[!grepl(exclude_patterns, oliver_data$character, ignore.case = TRUE), ]
explicit_noise <- c("Work’us", "the Clerkinwell Sessions", "the Cripples", "Lor bless her dear heart", "Hem", "lor bless")
oliver_data <- oliver_data[!(tolower(oliver_data$character) %in% tolower(explicit_noise)), ]

text_leaks <- c("X..Hem.....said.Mr..Bumble..", "X.Lor.bless.her.dear.heart..uttered.by.the.nurse.at.Oliver.s.birth")
oliver_data <- oliver_data[!oliver_data$character %in% text_leaks, ]
oliver_data <- oliver_data[!grepl("said|uttered|bless|heart", oliver_data$character, ignore.case = TRUE), ]

component_tags <- c("N", "DC", "C", "I", "DN", "A")


structured_long <- oliver_data %>%
  filter(tag %in% component_tags & !is.na(chapter)) %>%
  group_by(chapter, character, tag) %>%
  summarize(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = tag, values_from = count, values_fill = 0)

for(metric in component_tags) {
  if(!metric %in% colnames(structured_long)) structured_long[[metric]] <- 0
}

structured_long$Total_Score <- rowSums(structured_long[, component_tags])
structured_long <- structured_long %>% filter(Total_Score > 0)

calc_matrix <- structured_long %>% dplyr::select(all_of(component_tags))
center_vector <- colMeans(calc_matrix, na.rm = TRUE)
covariance_matrix <- cov(calc_matrix, use = "pairwise.complete.obs")

if (det(covariance_matrix) == 0) {
  diag(covariance_matrix) <- diag(covariance_matrix) + 1e-6
}

structured_long$Local_Mahalanobis_Sq <- mahalanobis(calc_matrix, center = center_vector, cov = covariance_matrix)

final_output <- structured_long %>%
  mutate(
    Volume = case_when(chapter <= 19 ~ 1, chapter <= 38 ~ 2, TRUE ~ 3),
    Graph_Chapter = chapter
  ) %>%
  rename(Chapter = chapter, Character = character) %>%
  dplyr::select(Volume, Chapter, Graph_Chapter, Character, Local_Mahalanobis_Sq, Total_Score, N, DC, C, I, DN, A) %>%
  arrange(Chapter, desc(Local_Mahalanobis_Sq))


visual_data <- final_output %>%
  filter(!grepl("oliver", Character, ignore.case = TRUE))


component_titles <- c(
  "N" = "Name Mentions (N)",
  "DC" = "Discussion of Character by Others (DC)",
  "C" = "Communication (C)",
  "I" = "Interiority (I)",
  "DN" = "Description by Narrator (DN)",
  "A" = "Action (A)"
)


cat("\nGenerating individual component figures...\n")

for (comp in component_tags) {
  

  comp_data <- visual_data %>%
    dplyr::select(Chapter, Character, Score = all_of(comp)) %>%
    filter(Score > 0)
  

  p <- ggplot(comp_data, aes(x = Chapter, y = Score, color = Character)) +
   
    geom_point(alpha = 0.8, size = 2.5, position = position_jitter(width = 0.2, height = 0)) +
    geom_text_repel(
      aes(label = Character),
      size = 3,
      max.overlaps = 15,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.color = "grey70",
      show.legend = FALSE
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Structural Trajectory:", component_titles[comp]),
      subtitle = "Faceted tracking of independent character presence (Excluding Oliver)",
      x = "Chapter",
      y = "Component Metric Score"
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 16)
    )
  

  print(p)
  

  file_name <- sprintf("figure1_component_%s_no_oliver.png", comp)
  comp_destination <- file.path(output_dir, file_name)
  
  ggsave(
    filename = comp_destination,
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  cat(sprintf(" -> Saved: %s\n", file_name))
}

cat("\n==================================================================\n")
cat("SUCCESS: All 6 separate component figures compiled and saved!\n")
cat(sprintf("Target Folder: %s\n", output_dir))
cat("==================================================================\n")
