library(tidyverse); library(ggplot2); library(ggrepel)

raw_comp_data <- read_csv("/Users/ale/Desktop/May Pipeline All Books/oliver_twist/may/oliver_twist_components.csv") %>% 
  rename(Character = character, Chapter = chapter)

excluded_chars <- c("Work’us", "Damme", "Heaven", "Nature", 
                    "“Lor bless her dear heart” uttered by the nurse at Oliver’s birth", 
                    "Maylie", "the Clerkinwell Sessions", "“ Hem ! “", "the Cripples")

plot_data <- raw_comp_data %>%
  filter(!Character %in% excluded_chars) %>% 
  pivot_wider(names_from = tag, values_from = count)


generate_all_figures <- function(component) {
  

  top_12 <- plot_data %>% 
    group_by(Character) %>% 
    summarise(total = sum(!!sym(component), na.rm = TRUE)) %>% 
    slice_max(total, n = 12) %>% pull(Character)
  
  f1_1 <- ggplot(plot_data %>% filter(Character %in% top_12), aes(x = Chapter, y = !!sym(component))) + 
    geom_area(fill = "#A35C5C", alpha = 0.7) + 
    facet_wrap(~Character, ncol = 3, scales = "free_y") + 
    theme_minimal() + labs(title = paste("FIGURE_1_1_", component, "_TRAJECTORIES.png"))
  ggsave(paste0("FIGURE_1_1_", component, "_TRAJECTORIES.png"), f1_1, width = 10, height = 8)
  

  f1_2 <- plot_data %>%
    group_by(Character) %>%
    summarise(Value = sum(!!sym(component), na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(Character, Value), y = Value)) +
    geom_bar(stat = "identity", fill = "#A35C5C") + coord_flip() + theme_minimal() +
    labs(title = paste("FIGURE_1_2_", component, "_MACRO_LANDSCAPE.png"))
  ggsave(paste0("FIGURE_1_2_", component, "_MACRO_LANDSCAPE.png"), f1_2, width = 10, height = 10)
  

  f1_3 <- ggplot(plot_data, aes(x = Chapter, y = !!sym(component), fill = Character)) +
    geom_area(alpha = 0.8, color = "white", size = 0.1) + 
    theme_minimal() +
    labs(title = paste("FIGURE_1_3_", component, "_GEOMETRIC_MASS.png"), y = "Intensity")
  ggsave(paste0("FIGURE_1_3_", component, "_GEOMETRIC_MASS.png"), f1_3, width = 12, height = 6)
  

  key_chars <- c("Oliver", "Sikes", "The Jew", "Rose")
  f1_4 <- ggplot(plot_data %>% filter(Character %in% key_chars), aes(x = Chapter, y = !!sym(component), color = Character)) +
    geom_line(linewidth = 1) + facet_wrap(~Character, ncol = 1) + theme_minimal() +
    labs(title = paste("FIGURE_1_4_", component, "_TRAJECTORY_PANEL.png"))
  ggsave(paste0("FIGURE_1_4_", component, "_TRAJECTORY_PANEL.png"), f1_4)
  

  f1_5 <- ggplot(plot_data %>% filter(Character %in% c("Oliver", "Sikes")), 
                 aes(x = Chapter, y = !!sym(component), color = Character)) +
    geom_line(alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE, span = 0.3) +
    theme_minimal() +
    labs(title = paste("FIGURE_1_5_", component, "_COVARIANCE.png"))
  ggsave(paste0("FIGURE_1_5_", component, "_COVARIANCE.png"), f1_5, width = 8, height = 5)
  

  x_c <- component; y_c <- "I" 
  

  if (x_c != y_c) {
    topology_data <- plot_data %>%
      group_by(Character) %>%
      summarise(x_val = sum(!!sym(x_c), na.rm = TRUE), y_val = sum(!!sym(y_c), na.rm = TRUE), total_score = x_val + y_val)
    
    f1_6 <- ggplot(topology_data, aes(x = x_val, y = y_val, size = total_score, color = total_score)) +
      geom_point(alpha = 0.7) +
      geom_text_repel(aes(label = Character), size = 3) +
      geom_vline(xintercept = median(topology_data$x_val), linetype = "dashed", color = "grey") +
      geom_hline(yintercept = median(topology_data$y_val), linetype = "dashed", color = "grey") +
      scale_color_viridis_c() +
      theme_minimal() +
      labs(title = paste("FIGURE_1_6_", x_c, "_TOPOLOGY_MAP.png"), x = paste("Accumulated", x_c), y = paste("Accumulated", y_c))
    ggsave(paste0("FIGURE_1_6_", x_c, "_TOPOLOGY_MAP.png"), f1_6, width = 12, height = 10)
    
    f1_7 <- ggplot(topology_data %>% filter(Character != "Oliver"), 
                   aes(x = x_val, y = y_val, size = total_score, color = total_score)) +
      geom_point(alpha = 0.7) +
      geom_text_repel(aes(label = Character), size = 3) +
      geom_vline(xintercept = median(topology_data$x_val), linetype = "dashed", color = "grey") +
      geom_hline(yintercept = median(topology_data$y_val), linetype = "dashed", color = "grey") +
      scale_color_viridis_c() +
      coord_cartesian(xlim = c(0, quantile(topology_data$x_val, 0.8)), 
                      ylim = c(0, quantile(topology_data$y_val, 0.8))) +
      theme_minimal() +
      labs(title = paste("FIGURE_1_7_", x_c, "_TOPOLOGY_MAP.png"), x = paste("Accumulated", x_c), y = paste("Accumulated", y_c))
    ggsave(paste0("FIGURE_1_7_", x_c, "_TOPOLOGY_MAP.png"), f1_7, width = 12, height = 10)
  }
}

for (comp in c("N", "C", "I", "A", "DC", "DN")) {
  generate_all_figures(comp)
}
