#' @export
plot_make_prices <- function(new_or_used_input, make_input) {
  
  df <- read.csv("data/cars_data_db.csv")
  
  # Visualize distributions of NEW make and model
  make_df <- df %>% group_by(new_or_used, make) %>% dplyr::summarize(count = n(), avg_price = mean(price, na.rm = TRUE)) %>% ungroup() %>% filter(count>5)
  model_df <- df %>% group_by(new_or_used, make, model_clean) %>% dplyr::summarize(count = n(), avg_price = mean(price, na.rm = TRUE)) %>% ungroup()
  
  rm(df)
  gc()
  
  # Filter data based on input
  filtered_data <- make_df %>%
    filter(new_or_used == new_or_used_input)
  filtered_data_model <- model_df %>%
    filter(new_or_used == new_or_used_input & make == make_input)
  
  rm(make_df, model_df)
  gc()
  
  # Order data by average price descending
  ordered_data <- filtered_data %>%
    arrange(desc(avg_price))
  ordered_data_model <- filtered_data_model %>%
    arrange(desc(avg_price))
  
  rm(filtered_data, filtered_data_model)
  gc()
  
  # Create a column for highlighting
  ordered_data <- ordered_data %>%
    mutate(highlighted = ifelse(make == make_input, "Highlighted", "Regular"))
  
  # Define modern blue gradient colors to match app theme
  blue_primary <- "#3b82f6"
  blue_dark <- "#1e40af"
  highlight_color <- "#f59e0b"  # Amber for highlight instead of red
  
  # Create the make plot with modern styling
  make_graph <- ggplot(ordered_data, aes(x = avg_price, y = reorder(make, avg_price), fill = highlighted)) +
    geom_bar(stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = c("Highlighted" = highlight_color, "Regular" = blue_primary)) +
    scale_x_continuous(
      limits = c(min(ordered_data$avg_price) - 1000, max(ordered_data$avg_price)), 
      oob = scales::rescale_none,
      labels = scales::dollar_format()
    ) +
    labs(
      title = paste0("Average Prices of ", toupper(substr(new_or_used_input, 1, 1)), 
                     substr(new_or_used_input, 2, nchar(new_or_used_input)), " Cars"),
      subtitle = paste("Highlighting", make_input),
      x = "Average Price",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#e5e7eb", linewidth = 0.5),
      panel.background = element_rect(fill = "#fafafa", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.y = element_text(size = 11, color = "#374151", face = "bold"),
      axis.text.x = element_text(size = 10, color = "#6b7280"),
      axis.title.x = element_text(size = 12, color = "#1f2937", face = "bold", margin = margin(t = 10)),
      plot.title = element_text(hjust = 0.5, size = 16, color = "#1f2937", face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#6b7280", margin = margin(b = 15)),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    geom_text(aes(label = paste0("n=", count)), hjust = -0.1, size = 3.5, color = "#6b7280", fontface = "bold")
  
  # Create gradient fill for model plot
  model_graph <- ggplot(ordered_data_model, aes(x = reorder(model_clean, -avg_price), y = avg_price, fill = avg_price)) +
    geom_bar(stat = "identity", alpha = 0.9) +
    scale_fill_gradient(low = blue_primary, high = blue_dark) +
    scale_y_continuous(
      limits = c(0, max(ordered_data_model$avg_price) * 1.15),  # Add space for labels
      labels = scales::dollar_format()
    ) +
    labs(
      title = paste0("Average Prices of ", toupper(substr(new_or_used_input, 1, 1)), 
                     substr(new_or_used_input, 2, nchar(new_or_used_input)), " ", make_input), 
      x = NULL,
      y = "Average Price"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.5),
      panel.background = element_rect(fill = "#fafafa", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#374151", face = "bold"),
      axis.text.y = element_text(size = 10, color = "#6b7280"),
      axis.title.y = element_text(size = 12, color = "#1f2937", face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 16, color = "#1f2937", face = "bold", margin = margin(b = 15)),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    geom_text(aes(label = paste0("n=", count)), vjust = -0.5, size = 3.5, color = "#6b7280", fontface = "bold")
  
  rm(ordered_data, ordered_data_model)
  gc()
  
  return(list(make_graph, model_graph))
}