 
#' @export
plot_make_prices <- function(new_or_used_input, make_input) {
  
  df <- read.csv("data/cars_data_db.csv")
  
  # Disualize distributions of NEW make and model
  make_df <- df %>% group_by(new_or_used, make) %>% dplyr::summarize(count = n(), avg_price = mean(price, na.rm = TRUE)) %>% ungroup()
  model_df <- df %>% group_by(new_or_used, make, model_clean) %>% dplyr::summarize(count = n(), avg_price = mean(price, na.rm = TRUE)) %>% ungroup()
 
  rm(df)
  gc()
  
  # Input validation
  if (!new_or_used_input %in% c("New", "Used")) {
    stop("new_or_used_input must be either 'New' or 'Used'")
  }
  
  if (!make_input %in% unique(df$make)) {
    stop(paste("make_input must be one of:", paste(unique(df$make), collapse=", ")))
  }
  
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
  
  
  # Create the plot
  make_graph <- ggplot(ordered_data, aes(x = avg_price, y = reorder(make, avg_price), fill = highlighted)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Highlighted" = "#E41A1C", "Regular" = "#377EB8")) +
    scale_x_continuous(limits = c(min(ordered_data$avg_price) - 1000, max(ordered_data$avg_price)), 
                       oob = scales::rescale_none)+
    labs(
      title = paste0("Average Prices of ", toupper(substr(new_or_used_input, 1, 1)), 
                     substr(new_or_used_input, 2, nchar(new_or_used_input)), " Cars"),
      subtitle = paste("Highlighting", make_input),
      x = "Average Price ($)",
      y = "Make"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    ) +
    # Add count as text
    geom_text(aes(label = paste0("n = ", count)), hjust = -0.05, size = 3)
  
  model_graph <- ggplot(ordered_data_model, aes(x = reorder(model_clean, -avg_price), y = avg_price, fill = avg_price)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(min(ordered_data_model$avg_price) - 1000, max(ordered_data_model$avg_price)), 
                       oob = scales::rescale_none)+
    labs(
      title = paste0("Average Prices of ", toupper(substr(new_or_used_input, 1, 1)), 
                     substr(new_or_used_input, 2, nchar(new_or_used_input)), " ", make_input), 
      x = "Model",
      y = "Average Price ($)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    ) +
    # Add count as text
    geom_text(aes(label = paste0("n = ", count)), vjust = -.5, size = 3)
  
  return(list(make_graph, model_graph))
}






