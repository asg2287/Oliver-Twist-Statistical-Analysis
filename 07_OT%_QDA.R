library(MASS)
library(ggplot2)
library(compositions)
library(tidyr)
library(dplyr)

csv_path <- "/Users/ale/Desktop/Oliver Twist/oliver_twist_components.csv"
data_long <- read.csv(csv_path)

data_wide <- pivot_wider(data_long, names_from = tag, values_from = count, values_fill = 0)

data_wide <- data_wide %>%
  mutate(character = case_when(
    character == "Fagin" ~ "The Jew",
    TRUE ~ character
  ))


exclude_chars <- c(
  "Clerkinwell Sessions", "the Cripples", "Hem !",
  "“Lor bless her dear heart” uttered by the nurse at Oliver’s birth",
  "Heaven", "nature", "Nature", "work’us", "Work’us", "maylie", "Maylie",
  "hem! lor", "damme", "Damme", "God", "god"
)

data_wide <- data_wide[!(data_wide$character %in% exclude_chars), ]


pct_cols <- c("N", "DC", "C", "I", "DN", "A")
row_totals <- rowSums(data_wide[, pct_cols])

data_wide <- data_wide[row_totals > 0, ]
data_wide[, pct_cols] <- data_wide[, pct_cols] / row_totals[row_totals > 0]


data_wide$Tier <- "K3"
data_wide[data_wide$character %in% c("Oliver"), "Tier"] <- "K1"
data_wide[data_wide$character %in% c("Sikes", "The Jew", "Mr. Bumble", "Noah", "Rose", "Mr. Brownlow"), "Tier"] <- "K2"
data_wide$Tier <- factor(data_wide$Tier, levels = c("K1", "K2", "K3"))


eps <- 1e-6
comp_matrix <- data_wide[, pct_cols]
comp_matrix[comp_matrix == 0] <- eps
comp_acomp <- acomp(comp_matrix)
ilr_data <- as.data.frame(ilr(comp_acomp))
colnames(ilr_data) <- paste0("ILR_", 1:ncol(ilr_data))

mda_ready <- cbind(ilr_data, Tier = data_wide$Tier, Character = data_wide$character)


qda_model <- qda(Tier ~ ., data = mda_ready[, c("ILR_1", "ILR_2", "ILR_3", "ILR_4", "ILR_5", "Tier")])
qda_values <- predict(qda_model)

lda_ref <- lda(Tier ~ ., data = mda_ready[, c("ILR_1", "ILR_2", "ILR_3", "ILR_4", "ILR_5", "Tier")])
lda_values <- predict(lda_ref)
trace_variance <- (lda_ref$svd)^2 / sum((lda_ref$svd)^2)

plot_qda <- cbind(as.data.frame(lda_values$x), Tier = mda_ready$Tier, Character = mda_ready$Character)


qda_plot <- ggplot(plot_qda, aes(x = LD1, y = LD2, color = Tier)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = Character), vjust = -0.8, size = 2.5, check_overlap = TRUE, show.legend = FALSE) +
  labs(
    title = "Quadratic Discriminant Analysis (QDA) of Oliver Twist Cast",
    subtitle = paste0("Canonical Space Projection | LD1: ", round(trace_variance[1]*100, 1),
                      "%, LD2: ", round(trace_variance[2]*100, 1), "%"),
    x = "First Discriminant Function (LD1)",
    y = "Second Discriminant Function (LD2)"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggsave("/Users/ale/Desktop/OLIVER_TWIST_QDA_SPACE.pdf", plot = qda_plot, width = 9, height = 7)

all_chapter_predictions <- data.frame(
  Character = mda_ready$Character,
  Assigned_Tier = mda_ready$Tier,
  Predicted_Tier = qda_values$class,
  Posterior_Prob = apply(qda_values$posterior, 1, max)
)

output_predictions <- all_chapter_predictions %>%
  group_by(Character) %>%
  summarise(
    Assigned_Tier = first(Assigned_Tier),
    Predicted_Tier = names(which.max(table(Predicted_Tier))),
    Posterior_Prob = round(mean(Posterior_Prob[Predicted_Tier == Predicted_Tier]), 4),
    Total_Chapters_Analyzed = n(),
    .groups = "drop"
  )

write.csv(output_predictions, "/Users/ale/Desktop/OLIVER_TWIST_QDA_Report.csv", row.names = FALSE)

cat("QDA execution complete. Both 'OLIVER_TWIST_QDA_SPACE.pdf' and 'OLIVER_TWIST_QDA_Report.csv' are saved to your Desktop.\n")
