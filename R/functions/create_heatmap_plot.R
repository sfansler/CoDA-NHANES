## create heatmap plot for PA intensity (var): var can be set to "SeB", "LiPA", "MVPA", "Non-wake"
create_heatmap_plot <- function(data, ID, var){
  data <- data %>% mutate(var_flag = case_when(PA == var ~ 1, .default = 0))
  plot_data <- data %>% filter(SEQN == ID)
  
  ggplot(data = plot_data, aes(x = Time, y = as.factor(DayofWeek), fill = factor(var_flag))) +
    geom_tile() +  scale_fill_manual(values = c("0" = "dodgerblue", "1" = "darkorange"), name = "State", labels = c("Wake", "Sleep")) +  
    theme_minimal() +
    labs(title = paste("Participant", ID),
         x = "Time of Day", y = "Day") +
    scale_x_continuous(
      breaks = c(0, 360, 720, 1080, 1440),
      labels = c("12AM", "6AM", "12PM", "6PM", "12AM"),
      expand = c(0, 0)) +
    geom_vline(xintercept = c(0, 360, 720, 1080, 1440), linetype = "dashed", color = "grey") +
    theme(plot.title = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.title.x = element_text(size = 16),  
          axis.title.y = element_text(size = 16),
          panel.grid.major = element_blank(),   
          panel.grid.minor = element_blank()) +
    scale_y_discrete(limits = rev(levels(factor(plot_data$DayofWeek))))
}

## create heatmap plot for PA intensity (var): var can be set to "SeB", "LiPA", "MVPA", "Non-wake"
create_heatmap_plot_2 <- function(data, ID){
  plot_data <- data %>% filter(SEQN == ID) %>% mutate(PA = factor(PA, levels = c("SeB", "LiPA", "MVPA", "Sleep", "Non-wear", "Unknown")),
                                                      DayofWeek = factor(DayofWeek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday")))

  ggplot(data = plot_data, aes(x = Time, y = as.factor(DayofWeek), fill = factor(PA))) +
    geom_tile() +  scale_fill_manual(values = c("MVPA" = "#FF5733", "LiPA" = "#0A9F9D", "SeB" = "#D4AF37", "Sleep" = "black", "Unknown" = "gray"), 
                                     name = "Behavior"
                                      ) +
    theme_minimal() +
    labs(#title = paste("Participant", ID),
         x = "Time of Day", y = "") +
    scale_x_continuous(
      breaks = c(0, 360, 720, 1080, 1440),
      labels = c("12AM", "6AM", "12PM", "6PM", "12AM"),
      expand = c(0, 0)) +
    theme(plot.title = element_text(size = 16),
          axis.text.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),  
          axis.title.y = element_text(size = 16, face = "bold"),
          panel.grid.major = element_blank(),   
          panel.grid.minor = element_blank(),
          legend.key.size = unit(0.6, "cm"),
          legend.text = element_text(size = 11)) +
    scale_y_discrete(limits = rev(levels(factor(plot_data$DayofWeek))))
}

