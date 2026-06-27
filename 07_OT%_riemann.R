library(ggplot2); library(reshape2); library(tidyr)

csv_path <- "/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/oliver_twist_components.csv"
output_dir <- "/Users/ale/Desktop/"
total_ch <- 57 


data_long <- read.csv(csv_path, stringsAsFactors = FALSE)

data_raw <- pivot_wider(data_long, names_from = tag, values_from = count)


all_chars <- unique(data_raw$character)
riemann_matrix <- matrix(0, nrow = length(all_chars), ncol = total_ch,
                         dimnames = list(all_chars, paste0("Ch_", 1:total_ch)))

for (char in all_chars) {
  cum_auc <- 0
  
  char_data <- subset(data_raw, character == char)
  
  for (ch in 1:total_ch) {
 
    row <- subset(char_data, chapter == ch)
    if(nrow(row) > 0) {
  
      intensity <- sum(as.numeric(row[1, c("N", "DC", "C", "I", "DN", "A")]), na.rm=TRUE)
    } else {
      intensity <- 0
    }
    cum_auc <- cum_auc + intensity
    riemann_matrix[char, ch] <- cum_auc
  }
}


plot_data <- melt(riemann_matrix)
colnames(plot_data) <- c("Character", "Chapter", "Cumulative_AUC")
plot_data$Chapter <- as.numeric(gsub("Ch_", "", plot_data$Chapter))


top_chars <- names(sort(riemann_matrix[, total_ch], decreasing=TRUE)[1:12])
plot_data <- subset(plot_data, Character %in% top_chars)

full_plot <- ggplot(plot_data, aes(x = Chapter, y = Cumulative_AUC, color = Character)) +
  geom_line(size = 1) + 
  labs(title = "Oliver Twist: Structural Trajectories",
       x = "Chapter", y = "Cumulative Structural Mass (AUC)") +
  theme_minimal()

ggsave(paste0(output_dir, "OLIVER_RIEMANN_TRAJECTORIES.pdf"), full_plot, width = 11, height = 7)
