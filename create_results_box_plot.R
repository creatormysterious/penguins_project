#function to create results box plot

create_box_plot <- function(data, 
                            x_var = "species", 
                            y_var = "culmen_depth_mm", 
                            title = "Comparing Average Culmen Depth Across Species", 
                            x_label = "Species", 
                            y_label = "Culmen Depth (mm)", 
                            colors = c("Adelie" = "darkorange", 
                                       "Chinstrap" = "purple", 
                                       "Gentoo" = "cyan4"), 
                            comparisons = list(c("Adelie", "Chinstrap"), 
                                               c("Adelie", "Gentoo"), 
                                               c("Chinstrap", "Gentoo")),
                            textsize = 4) {
  
  # Create the box plot
  ggplot(data = data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_boxplot(aes(fill = species), width = 0.5, outlier.shape = NA, alpha = 0.7) +  # Box plot with no outliers and reduced width
    theme_minimal() +
    labs(
      title = title,
      x = x_label,  # Adds x-axis label
      y = y_label   # Adds y-axis label
    ) +
    scale_fill_manual(values = colors) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), # Title is centered
      axis.title = element_text(size = 11),  # Keep x-axis title
      axis.text.x = element_blank(),         # Remove x-axis labels
      axis.ticks.x = element_blank(),        # Remove x-axis ticks
      axis.text.y = element_text(size = 11), # Keep y-axis labels
      axis.ticks = element_line(color = "black"), # Keep y-axis ticks
      axis.line = element_line(color = "black"),
      panel.grid = element_blank()
    ) +
    geom_signif(
      comparisons = comparisons,            # Define which species to compare
      map_signif_level = TRUE,               # Automatically maps p-values to significance stars
      step_increase = 0.1,                   # Increase the height of the significance lines
      color = "black",                       # Color of the significance lines and stars
      textsize = textsize,                   # Font size for significance stars
      na.rm = TRUE,                          # Remove NA results (this avoids NS lines from showing)
      annotations = c("***", "**", NA)      # Manually specify stars and remove non-significant comparisons
    )
}

