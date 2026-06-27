
library(tidyverse)


core_clean <- data.frame(
  Character = c(
    "Oliver Twist", "Fagin", "Bill Sikes", "Nancy", "The Artful Dodger", "Charley Bates",
    "Mr. Brownlow", "Rose Maylie", "Mrs. Maylie", "Mr. Grimwig", "Monks", "Mr. Bumble",
    "Mrs. Corney", "Noah Claypole", "Charlotte", "Mr. Losberne", "Harry Maylie", 
    "Toby Crackit", "Tom Chitling", "Barney", "Fang (Magistrate)", "Mr. Sowerberry",
    "Mrs. Sowerberry", "Giles", "Brittles"
  ),
  N  = c(984, 512, 380, 310, 245, 180, 210, 195, 110, 85, 140, 290, 130, 115, 75, 95, 65, 55, 45, 30, 28, 40, 25, 35, 22),
  DC = c(1120, 580, 410, 340, 280, 195, 230, 210, 125, 92, 165, 310, 145, 122, 82, 105, 72, 63, 48, 35, 30, 44, 28, 39, 26),
  C  = c(490, 260, 185, 140, 110, 78, 98, 88, 48, 34, 68, 128, 54, 46, 28, 38, 24, 22, 16, 10, 9, 14, 8, 11, 7),
  I  = c(910, 420, 290, 190, 145, 88, 112, 95, 42, 25, 74, 185, 62, 51, 22, 31, 18, 14, 11, 5, 4, 12, 5, 8, 4),
  DN = c(115, 62, 44, 31, 24, 14, 18, 15, 6, 4, 11, 29, 9, 8, 3, 4, 2, 2, 1, 0, 0, 2, 0, 1, 0),
  A  = c(680, 390, 270, 215, 175, 120, 145, 130, 75, 58, 98, 195, 92, 78, 48, 62, 43, 36, 28, 19, 18, 26, 16, 22, 14)
)


tail_clean <- data.frame(
  Character = c(
    "Agnes Fleming", "Old Sally", "Mrs. Thingummy", "Mr. Limbkins", "Gamfield (Sweep)",
    "Mrs. Bedwin", "Mr. Brownlow's Cook", "Blathers (Officer)", "Duff (Officer)", 
    "The Landlord", "Kags", "Chitling's Girl", "An Unnamed Warder", "The Turnkey", 
    "Mr. Sowerberry's Apprentice", "The Beadle's Assistant", "A Pauper Nurse", 
    "The Workhouse Master", "A Young Thief", "The Cripples Landlord", "Martha (Pauper)", 
    "Apothecary's Assistant", "The Coachman", "The Executioner"
  ),
  N  = c(8, 7, 5, 4, 4, 6, 2, 4, 4, 3, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1),
  DC = c(9, 8, 6, 5, 4, 7, 2, 4, 4, 3, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 0),
  C  = c(2, 2, 1, 1, 1, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  I  = c(1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  DN = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  A  = c(5, 4, 4, 3, 3, 4, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)
)


master_49_data <- rbind(core_clean, tail_clean) %>%
  mutate(TOTAL_SCORE = N + DC + C + I + DN + A) %>%
  dplyr::select(Character, TOTAL_SCORE, N, DC, C, I, DN, A)


write_csv(master_49_data, "/Users/ale/Desktop/OLIVER_TWIST_49_CHARACTERS_MASTER_DATA.csv")



chapters <- 53
oliver <- rep(1, chapters); oliver[c(12:14, 28:32)] <- 0
fagin <- rep(0, chapters); fagin[c(8:22, 25:26, 34:39, 42:47, 52:53)] <- 1
rose_maylie <- rep(0, chapters); rose_maylie[c(28:41, 46:51)] <- 1
monks <- rep(0, chapters); monks[c(26, 37, 38, 39, 49, 51)] <- 1

export_joint_prob <- function(charX, charY, nameX, nameY, analytic_profile) {
  n <- length(charX)
  tibble(
    Comparison_Pair = paste(nameX, "vs", nameY),
    Analytical_Model = analytic_profile,
    P_11_Both_Present = sum(charX == 1 & charY == 1) / n,
    P_10_CharX_Only   = sum(charX == 1 & charY == 0) / n,
    P_01_CharY_Only   = sum(charX == 0 & charY == 1) / n,
    P_00_Both_Absent  = sum(charX == 0 & charY == 0) / n,
    Diagnostic_Index  = ifelse(analytic_profile == "Spatial Segregation", 
                               P_10_CharX_Only + P_01_CharY_Only, 
                               P_11_Both_Present / (P_11_Both_Present + P_01_CharY_Only))
  )
}

jp_table_1 <- export_joint_prob(fagin, rose_maylie, "Fagin", "Rose Maylie", "Spatial Segregation")
jp_table_2 <- export_joint_prob(oliver, monks, "Oliver", "Monks", "Predatory Stalking")

joint_probability_matrix_output <- rbind(jp_table_1, jp_table_2)


write_csv(joint_probability_matrix_output, "/Users/ale/Desktop/OLIVER_TWIST_JOINT_PROBABILITIES.csv")

cat("\n==================================================================\n")
cat("SUCCESS: Extracted data sets written directly to Desktop!\n")
cat(" -> /Users/ale/Desktop/OLIVER_TWIST_49_CHARACTERS_MASTER_DATA.csv\n")
cat(" -> /Users/ale/Desktop/OLIVER_TWIST_JOINT_PROBABILITIES.csv\n")
cat("==================================================================\n")
