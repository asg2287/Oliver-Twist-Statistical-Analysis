
rm(list = ls())
library(tidyverse); library(stats); library(ggplot2)

csv_path <- "/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/oliver_twist_components_percentages.csv"
desktop_path <- "/Users/ale/Desktop/"


data_raw <- read.csv(csv_path, stringsAsFactors = FALSE)
char_col <- "character"
pct_cols <- c("Pct_N", "Pct_DC", "Pct_C", "Pct_I", "Pct_DN")

X_raw <- data_raw %>% 
  group_by(across(all_of(char_col))) %>% 
  summarize(across(all_of(pct_cols), sum), .groups = 'drop') %>%
  as.data.frame()


X_raw$Pct_A <- 1 - rowSums(X_raw[, pct_cols]) 
rownames(X_raw) <- X_raw[[char_col]]
pct_matrix <- as.matrix(X_raw[, c(pct_cols, "Pct_A")])
pct_matrix[pct_matrix <= 0] <- 0.01


ilr_transform <- function(X) {
  D <- ncol(X)
  logX <- log(X)
  res <- matrix(0, nrow=nrow(X), ncol=D-1)
  for(i in 1:(D-1)) {
    res[,i] <- sqrt(i / (i + 1)) * (rowMeans(logX[, 1:i, drop=FALSE]) - logX[, i+1])
  }
  return(res)
}


characters_ilr <- ilr_transform(pct_matrix)

global_center  <- colMeans(characters_ilr)
cov_mat        <- cov(characters_ilr) + diag(1e-6, ncol(characters_ilr))
d2_scores      <- mahalanobis(characters_ilr, center = global_center, cov = solve(cov_mat))


final_df <- data.frame(Character = rownames(X_raw), Architectural_D2 = round(d2_scores, 6))
write.csv(final_df[order(-final_df$Architectural_D2), ], file.path(desktop_path, "Oliver_Architectural_D2.csv"), row.names=F)


z_components <- list("Name Mentions"="Pct_N", "Discussion"="Pct_DC", "Communication"="Pct_C", 
                     "Interiority"="Pct_I", "Narrator Description"="Pct_DN", "Action"="Pct_A")

for (comp_name in names(z_components)) {
  col_name <- z_components[[comp_name]]
  z_data <- data.frame(Character = rownames(X_raw), 
                       Z = (X_raw[[col_name]] - mean(X_raw[[col_name]])) / sd(X_raw[[col_name]]))
  
  p_z <- ggplot(z_data, aes(x = reorder(Character, Z), y = Z, fill = Z)) +
    geom_bar(stat = "identity") + scale_fill_gradient2(low = "#3b0f70", mid = "#b13da1", high = "#fec287") +
    coord_flip() + theme_minimal() + labs(title = paste("Component Profile:", comp_name), y = "Z-Score")
  
  ggsave(file.path(desktop_path, paste0("DEVIATION_SPECTRUM_", gsub(" ", "_", comp_name), ".pdf")), p_z, width=9, height=11)
}
cat("\nPipeline complete. All files generated.\n")
