library(dplyr)
library(tidyr)
library(ggplot2)


core_and_mid_tier <- data.frame(
  Character = c(
    "Oliver Twist", "Fagin", "Bill Sikes", "Nancy", "The Artful Dodger", "Charley Bates",
    "Mr. Brownlow", "Rose Maylie", "Mrs. Maylie", "Mr. Grimwig", "Monks", "Mr. Bumble",
    "Mrs. Corney", "Noah Claypole", "Charlotte", "Mr. Losberne", "Harry Maylie", 
    "Toby Crackit", "Tom Chitling", "Barney", "Fang (Magistrate)", "Mr. Sowerberry",
    "Mrs. Sowerberry", "Giles", "Brittles", "Dick"
  ),
  N  = c(984, 512, 380, 310, 245, 180, 210, 195, 110, 85, 140, 290, 130, 115, 75, 95, 65, 55, 45, 30, 28, 40, 25, 35, 22, 20),
  DC = c(1120, 580, 410, 340, 280, 195, 230, 210, 125, 92, 165, 310, 145, 122, 82, 105, 72, 63, 48, 35, 30, 44, 28, 39, 26, 22),
  C  = c(490, 260, 185, 140, 110, 78, 98, 88, 48, 34, 68, 128, 54, 46, 28, 38, 24, 22, 16, 10, 9, 14, 8, 11, 7, 6),
  I  = c(910, 420, 290, 190, 145, 88, 112, 95, 42, 25, 74, 185, 62, 51, 22, 31, 18, 14, 11, 5, 4, 12, 5, 8, 4, 3),
  DN = c(115, 62, 44, 31, 24, 14, 18, 15, 6, 4, 11, 29, 9, 8, 3, 4, 2, 2, 1, 0, 0, 2, 0, 1, 0, 0),
  A  = c(680, 390, 270, 215, 175, 120, 145, 130, 75, 58, 98, 195, 92, 78, 48, 62, 43, 36, 28, 19, 18, 26, 16, 22, 14, 13)
)


missing_peripherals <- data.frame(
  Character = c(
    "Agnes Fleming", "Old Sally", "Mrs. Thingummy", "Mr. Limbkins", "Gamfield (Sweep)",
    "Mr. Giles", "Mrs. Bedwin", "Mr. Brownlow's Cook", "Morris Bolter (Noah's alias)",
    "Blathers (Officer)", "Duff (Officer)", "The Landlord", "Kags", "Chitling's Girl",
    "An Unnamed Warder", "The Turnkey", "Mr. Sowerberry's Apprentice", "The Beadle's Assistant",
    "A Pauper Nurse", "The Workhouse Master", "A Young Thief", "The Cripples Landlord",
    "Martha (Pauper)", "Apothecary's Assistant", "The Coachman", "The Guard", 
    "A Gaoler", "Fagin's Jail Guard", "The Executioner"
  ),
  N  = c(8, 7, 5, 4, 4, 3, 6, 2, 5, 4, 4, 3, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1),
  DC = c(9, 8, 6, 5, 4, 3, 7, 2, 6, 4, 4, 3, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 0),
  C  = c(2, 2, 1, 1, 1, 0, 2, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  I  = c(1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  DN = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  A  = c(5, 4, 4, 3, 3, 2, 4, 1, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1)
)

full_network <- rbind(core_and_mid_tier, missing_peripherals)
metrics <- c("N", "DC", "C", "I", "DN", "A")

spearman_matrix <- cor(as.matrix(full_network[, metrics]), method = "spearman")
spearman_melted <- as.data.frame(spearman_matrix)
spearman_melted$Var1 <- rownames(spearman_melted)
spearman_melted <- spearman_melted %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")


p1 <- ggplot(spearman_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", lwd = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Correlation)), color = "white", size = 3.5) +
  scale_fill_viridis_c(option = "mako", direction = -1, end = 0.9) +
  theme_minimal() +
  labs(
    title = "Oliver Twist Joint Distribution",
    subtitle = "Spearman Rank Coupling",
    x = NULL, y = NULL
  ) +
  theme(plot.title = element_text(face = "bold", size = 11))


labeled_nodes <- full_network[full_network$Character %in% c("Oliver Twist", "Fagin", "Bill Sikes", "Rose Maylie", "Mr. Brownlow", "Monks"), ]

p2 <- ggplot(full_network, aes(x = DC, y = I)) +
  geom_point(color = "#b23b3b", size = 3.5, alpha = 0.6) +
  geom_text(data = labeled_nodes, aes(label = Character), 
            vjust = -1.2, fontface = "bold", size = 3) +
  theme_minimal() +
  labs(
    title = "Joint Scatter Space: DC vs. I",
    x = "Discussion of Character",
    y = "Interiority"
  ) +
  theme(plot.title = element_text(face = "bold", size = 11))



oliver <- rep(1, chapters)
oliver[12:14] <- 0
oliver[28:32] <- 0

fagin <- rep(0, chapters)
fagin[8:22] <- 1
fagin[25:26] <- 1
fagin[34:39] <- 1
fagin[42:47] <- 1
fagin[52:53] <- 1

rose_maylie <- rep(0, chapters)
rose_maylie[28:41] <- 1
rose_maylie[46:51] <- 1


monks <- rep(0, chapters)
monks[c(26, 37, 38, 39, 49, 51)] <- 1


compute_dickensian_joint_prob <- function(charX, charY, nameX, nameY, dynamic_type) {
  n <- length(charX)
  p11 <- sum(charX == 1 & charY == 1) / n
  p10 <- sum(charX == 1 & charY == 0) / n
  p01 <- sum(charX == 0 & charY == 1) / n
  p00 <- sum(charX == 0 & charY == 0) / n
  
  cat(sprintf("=== Joint Probabilities [%s Mode]: %s vs. %s ===\n", dynamic_type, nameX, nameY))
  cat(sprintf("  P(1, 1) [Co-presence/Symmetry]:   %.3f\n", p11))
  cat(sprintf("  P(1, 0) [Only %s Manifest]:       %.3f\n", nameX, p10))
  cat(sprintf("  P(0, 1) [Only %s Manifest]:       %.3f\n", nameY, p01))
  cat(sprintf("  P(0, 0) [Mutual Structural Void]: %.3f\n", p00))
  
  if (dynamic_type == "Spatial Segregation") {
    exclusion_index <- p10 + p01
    cat(sprintf("  --> Structural Polarization Index: %.3f (High = Segregated Worlds)\n\n", exclusion_index))
  } else if (dynamic_type == "Predatory Stalking") {
    asymmetry <- p11 / (p11 + p01)
    cat(sprintf("  --> Stalker Dependency Ratio:      %.3f (If close to 1.0, %s is purely functional)\n\n", asymmetry, nameY))
  }
}


cat("\n--- RUNNING RE-ENGINEERED PROBABILITY PIPELINE ---\n\n")
compute_dickensian_joint_prob(fagin, rose_maylie, "Fagin", "Rose Maylie", "Spatial Segregation")
compute_dickensian_joint_prob(oliver, monks, "Oliver", "Monks", "Predatory Stalking")

ggsave("/Users/ale/Desktop/oliver_twist_joint_correlation.png", plot = p1, width = 6, height = 5, dpi = 300)
ggsave("/Users/ale/Desktop/oliver_twist_scatterspace.png", plot = p2, width = 6, height = 5, dpi = 300)

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_plot <- p1 + p2 + plot_layout(ncol = 2)
  ggsave("/Users/ale/Desktop/oliver_twist_structural_distribution.png", plot = combined_plot, width = 11, height = 5, dpi = 300)
  cat("--- SUCCESS: Visualizations and joint statistics compiled to Desktop! ---\n")
} else {
  cat("--- SUCCESS: Individual scatter and matrix PNGs exported to Desktop! ---\n")
}
