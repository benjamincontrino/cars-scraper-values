# similar to our scrape cars function, this time im taking in a url, and predicting what the price should be with our RF

# ============================================
# MAIN SCRAPING FUNCTION 
# ============================================

#' @export
predict_cars <- function(base_url, max_pages = 10, write_new_csv = "NO") {
  
  # Modify URL to get maximum results per page (100)
  if (grepl("\\?", base_url)) {
    url <- paste0(base_url, "&page_size=100")
  } else {
    url <- paste0(base_url, "?page_size=100")
  }
  
  # Set up realistic headers
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36"
  
  headers <- c(
    `User-Agent` = ua,
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.5",
    `Connection` = "keep-alive",
    `Upgrade-Insecure-Requests` = "1",
    `Sec-Fetch-Dest` = "document",
    `Sec-Fetch-Mode` = "navigate",
    `Sec-Fetch-Site` = "none",
    `Sec-Fetch-User` = "?1",
    `Cache-Control` = "max-age=0"
  )
  
  # Create empty dataframe
  all_cars <- data.frame()
  
  message("Starting Cars.com scraper...")
  message("URL: ", url)
  message("Max pages to scrape: ", max_pages)
  
  # ============================================
  # LOOP THROUGH PAGES
  # ============================================
  
  for (page_num in 1:max_pages) {
    # Build paginated URL
    if (page_num > 1) {
      page_url <- gsub("&page=\\d+", "", url)
      page_url <- paste0(page_url, "&page=", page_num)
    } else {
      page_url <- url
    }
    
    message("\n========================================")
    message("Scraping page ", page_num, " of ", max_pages)
    message("URL: ", page_url)
    
    # Add delay to avoid being blocked
    if (page_num > 1) {
      delay <- runif(1, 3, 5)
      message("Waiting ", round(delay, 1), " seconds...")
      Sys.sleep(delay)
    }
    
    # ============================================
    # MAKE HTTP REQUEST WITH RETRY LOGIC
    # ============================================
    
    max_retries <- 3
    response <- NULL
    
    for (retry in 1:max_retries) {
      response <- try(
        httr::GET(
          url = page_url,
          httr::add_headers(.headers = headers),
          httr::timeout(30),
          httr::config(followlocation = TRUE)
        ),
        silent = TRUE
      )
      
      if (!inherits(response, "try-error") && httr::status_code(response) == 200) {
        break
      } else {
        if (retry < max_retries) {
          warning("Retry ", retry, " of ", max_retries)
          Sys.sleep(retry * 2)
        } else {
          warning("Failed after ", max_retries, " attempts")
          return(all_cars)
        }
      }
    }
    
    # ============================================
    # PARSE HTML
    # ============================================
    
    page_content <- httr::content(response, "text", encoding = "UTF-8")
    page <- read_html(page_content)
    
    # ============================================
    # DETECT TOTAL MATCHES AND PAGES (first page only)
    # ============================================
    
    if (page_num == 1) {
      # Try to find the total match count
      total_matches_text <- page %>% 
        html_node("body") %>% 
        html_text()
      
      # Look for "10,000+ matches" or similar
      matches_pattern <- str_extract(total_matches_text, "[0-9,]+\\+?\\s+matches")
      if (!is.na(matches_pattern)) {
        message("\n Total listings found: ", matches_pattern)
      }
      
      # Try to detect max page number from pagination
      pagination_links <- page %>% 
        html_nodes("a[href*='page=']") %>% 
        html_attr("href")
      
      if (length(pagination_links) > 0) {
        # Extract page numbers from URLs
        page_numbers <- str_extract_all(pagination_links, "page=(\\d+)")
        page_numbers <- unlist(page_numbers)
        page_numbers <- as.numeric(gsub("page=", "", page_numbers))
        
        if (length(page_numbers) > 0) {
          max_page_available <- max(page_numbers, na.rm = TRUE)
          message(" Maximum pages available: ", max_page_available)
          message(" With 100 results per page, that's up to ", 
                  format(max_page_available * 100, big.mark = ","), " vehicles total")
          
          # Warn if user requested more than available
          if (max_pages > max_page_available) {
            message("  Note: You requested ", max_pages, " pages but only ", max_page_available, 
                    " are available.")
            message("    Adjusting to scrape ", max_page_available, " pages instead.")
            max_pages <<- max_page_available
          }
        }
      }
      
      message("========================================\n")
    }
    
    # ============================================
    # FIND VEHICLE CARDS
    # ============================================
    
    # Try multiple selectors - be specific
    selectors <- c(
      "div.vehicle-card",
      "article[class*='vehicle']",
      "div[class*='vehicle-card']",
      "div.listing-row"
    )
    
    car_cards <- NULL
    
    for (selector in selectors) {
      car_cards <- page %>% html_nodes(selector)
      if (length(car_cards) > 0 && length(car_cards) <= 150) {
        message("Using selector: ", selector, " (found ", length(car_cards), " elements)")
        break
      }
    }
    
    if (is.null(car_cards) || length(car_cards) == 0) {
      message("No vehicle cards found. Stopping.")
      break
    }
    
    # If we found too many (like 818), filter to only actual listings
    if (length(car_cards) > 150) {
      message("Found ", length(car_cards), " elements - filtering to actual vehicle cards...")
      
      filtered_cards <- list()
      for (i in 1:length(car_cards)) {
        card <- car_cards[i]
        has_heading <- length(card %>% html_nodes("h2, h3, [class*='heading']")) > 0
        has_price <- length(card %>% html_nodes("[class*='price']")) > 0
        
        if (has_heading && has_price) {
          filtered_cards <- c(filtered_cards, list(card))
        }
      }
      
      if (length(filtered_cards) > 0) {
        car_cards <- filtered_cards
        message("Filtered to ", length(car_cards), " vehicle cards")
      }
    }
    
    message("Processing ", length(car_cards), " vehicle cards...")
    
    # ============================================
    # EXTRACT DATA FROM EACH VEHICLE CARD
    # ============================================
    
    page_data <- data.frame()
    vehicles_extracted <- 0
    
    for (i in 1:length(car_cards)) {
      tryCatch({
        car <- car_cards[[i]]
        
        # Get all text content from this card
        all_text <- car %>% html_text(trim = TRUE)
        
        # Skip if too short
        if (is.na(all_text) || nchar(all_text) < 50) next
        
        # ----------------------------------------
        # EXTRACT YEAR, MAKE, MODEL FROM HEADING
        # ----------------------------------------
        
        heading <- NA
        heading_patterns <- c("h2", "h3", "[class*='heading']", "[class*='title']")
        
        for (pattern in heading_patterns) {
          heading_node <- car %>% html_node(pattern)
          if (!is.null(heading_node)) {
            heading <- html_text(heading_node, trim = TRUE)
            if (!is.na(heading) && nchar(heading) > 5) break
          }
        }
        
        year <- NA
        make <- NA
        model <- NA
        
        if (!is.na(heading)) {
          # Parse heading like "2021 Nissan Rogue SL"
          parts <- str_match(heading, "^(\\d{4})\\s+([^\\s]+)\\s+(.+)$")
          if (!is.na(parts[1,1])) {
            year <- parts[1,2]
            make <- parts[1,3]
            model <- parts[1,4]
          }
        }
        
        # Skip if we couldn't extract basic vehicle info
        if (is.na(make) || is.na(model)) next
        
        # ----------------------------------------
        # EXTRACT PRICE
        # ----------------------------------------
        
        price <- NA
        price_node <- car %>% html_node("[class*='primary-price'], [class*='listing-row__price']")
        if (is.null(price_node)) {
          price_node <- car %>% html_node("[class*='price']")
        }
        
        if (!is.null(price_node)) {
          price_text <- html_text(price_node, trim = TRUE)
          if (!is.na(price_text)) {
            # Extract only the first price number (ignores price drops like "+ $2,587")
            price_match <- str_extract(price_text, "\\$?[0-9,]+")
            if (!is.na(price_match)) {
              price <- gsub("[^0-9]", "", price_match)
            }
          }
        }
        
        # Skip if no price found
        if (is.na(price)) next
        
        # ----------------------------------------
        # EXTRACT MILEAGE
        # ----------------------------------------
        
        miles <- NA
        if (grepl("\\d+,?\\d*\\s*mi", all_text)) {
          miles_match <- str_extract(all_text, "\\d+,?\\d*(?=\\s*mi)")
          if (!is.na(miles_match)) {
            miles <- gsub(",", "", miles_match)
          }
        }
        
        # ----------------------------------------
        # EXTRACT DEALER NAME AND LOCATION
        # ----------------------------------------
        
        dealer_name <- NA
        dealer_location <- NA
        dealer_node <- car %>% html_node("[class*='dealer']")
        
        if (!is.null(dealer_node)) {
          dealer_text <- html_text(dealer_node, trim = TRUE)
          if (!is.na(dealer_text)) {
            # Clean up extra whitespace
            dealer_text <- gsub("\\s+", " ", dealer_text)
            
            # Extract dealer name (before opening parenthesis)
            dealer_name_match <- str_extract(dealer_text, "^[^\\(\\n]+")
            if (!is.na(dealer_name_match)) {
              dealer_name <- trimws(dealer_name_match)
            }
            
            # Extract location (City, ST with optional distance)
            location_match <- str_extract(dealer_text, "[A-Za-z\\s]+,\\s*[A-Z]{2}\\s*\\([^\\)]*\\)")
            if (is.na(location_match)) {
              location_match <- str_extract(dealer_text, "[A-Za-z\\s]+,\\s*[A-Z]{2}")
            }
            if (!is.na(location_match)) {
              dealer_location <- trimws(location_match)
            }
          }
        }
        
        # ----------------------------------------
        # DETERMINE NEW OR USED
        # ----------------------------------------
        
        new_or_used <- "Used"
        if (grepl("\\bNew\\b", all_text, ignore.case = TRUE)) {
          new_or_used <- "New"
        }
        
        # ----------------------------------------
        # ADD TO DATA FRAME
        # ----------------------------------------
        
        car_data <- data.frame(
          make = make,
          model = model,
          year = year,
          miles = miles,
          price = price,
          new_or_used = new_or_used,
          dealer_name = dealer_name,
          dealer_location = dealer_location,
          stringsAsFactors = FALSE
        )
        
        page_data <- rbind(page_data, car_data)
        vehicles_extracted <- vehicles_extracted + 1
        
      }, error = function(e) {
        # Skip problematic entries silently
        NULL
      })
    }
    
    message("Successfully extracted ", vehicles_extracted, " complete vehicle records")
    
    # ============================================
    # ADD PAGE DATA TO OVERALL RESULTS
    # ============================================
    
    if (nrow(page_data) > 0) {
      all_cars <- rbind(all_cars, page_data)
      message("Total vehicles so far: ", nrow(all_cars))
    } else {
      message("No vehicles found on page ", page_num, ". Stopping.")
      break
    }
  }
  
  cars_data <- all_cars
  
  
  rm(all_cars, heading_node, dealer_node, page, page_data, parts, price_node, response, car_cards, car, car_data)
  gc()
  # ============================================
  # CLEAN AND PROCESS DATA
  # ============================================
  
  if (nrow(cars_data) > 0) {
    message("\n==============================================")
    message("Data cleaning and processing...")
    message("==============================================")
    
    # Convert to appropriate types
    cars_data$price <- as.numeric(cars_data$price)
    cars_data$miles <- as.numeric(cars_data$miles)
    cars_data$year <- as.numeric(cars_data$year)
    
    # Standardize text fields
    cars_data$make <- str_to_title(cars_data$make)
    cars_data$new_or_used <- str_to_title(cars_data$new_or_used)
    
    message("Records before deduplication: ", nrow(cars_data))
    
    # Store count before deduplication
    records_before <- nrow(cars_data)
    
    # Remove duplicate listings
    cars_data <- cars_data %>%
      distinct(make, model, year, price, dealer_name, .keep_all = TRUE)
    
    records_after <- nrow(cars_data)
    message("Records after deduplication: ", records_after)
    message("Removed ", records_before - records_after, " duplicates")
    
    # Print summary statistics
    message("\n==============================================")
    message("SCRAPING SUMMARY")
    message("==============================================")
    message("Total unique vehicles scraped: ", nrow(cars_data))
    message("\nData completeness:")
    
    for (col in names(cars_data)) {
      non_na <- sum(!is.na(cars_data[[col]]))
      pct <- round(non_na / nrow(cars_data) * 100, 1)
      message(sprintf("  %-18s: %5d / %5d (%5.1f%%)", col, non_na, nrow(cars_data), pct))
    }
    
    message("\nPrice range: $", 
            format(min(cars_data$price, na.rm = TRUE), big.mark = ","), " - $", 
            format(max(cars_data$price, na.rm = TRUE), big.mark = ","))
    message("Year range: ", min(cars_data$year, na.rm = TRUE), " - ", 
            max(cars_data$year, na.rm = TRUE))
    message("Mileage range: ", 
            format(min(cars_data$miles, na.rm = TRUE), big.mark = ","), " - ", 
            format(max(cars_data$miles, na.rm = TRUE), big.mark = ","), " miles")
    
    message("\nTop 5 Makes:")
    print(head(sort(table(cars_data$make), decreasing = TRUE), 5))
    
    message("\nNew vs Used:")
    print(table(cars_data$new_or_used, useNA = "ifany"))
    
    # read in key
    cars_data <- cars_data %>%
      left_join(read.csv("data/cars_make_model_clean_key.csv"), by = c("make", "model")) %>%
      dplyr::select(make, model, model_clean, everything()) %>%
      # remove potential cars with missing link
      filter(!is.na(model_clean)) %>%
      mutate(miles = ifelse(is.na(miles), 0, miles))
    
    
  } else {
    message("\nERROR: No data was collected. Check the website structure.")
  }
  
  
  
  rf_model <- readRDS("data/cars_rf_model.rds")[[1]]
  cars_data <- bind_cols(predict(rf_model, cars_data), cars_data) %>%
    dplyr::rename(predicted_price = .pred) %>%
    mutate(price_over_expectation = price - predicted_price) %>%
    arrange(price_over_expectation) %>%
    dplyr::select(make, model, model_clean, year, miles, new_or_used, dealer_name, dealer_location, price, predicted_price, price_over_expectation)
  
  # ============================================
  # CREATE INTERACTIVE TABLE
  # ============================================
  
  # Assuming your data is loaded as 'df' or similar
  # If not, load it:
  # df <- read_csv("your_data.csv")
  
  # Prepare the data for display
  table_data <- cars_data %>%
    # Round predicted price for cleaner display
    mutate(predicted_price = round(predicted_price, 0))
  
  # Create the interactive table
  car_table <- datatable(
    table_data,
    
    # Table options
    options = list(
      pageLength = nrow(table_data),      # Show all rows
      scrollY = "600px",                  # Set table height with vertical scroll
      scrollX = TRUE,                     # Horizontal scrolling
      autoWidth = TRUE,
      searchHighlight = TRUE,             # Highlight search terms
      dom = 'Bfrtip',                     # Layout: Buttons, filter, table, info, pagination
      paging = FALSE,                     # Disable pagination
      columnDefs = list(                  # Center all columns
        list(className = 'dt-center', targets = '_all')
      )
    ),
    
    # Column names (prettier headers)
    colnames = c(
      'Make', 
      'Model', 
      'Model Clean', 
      'Year', 
      'Miles', 
      'New/Used', 
      'Dealer', 
      'Location', 
      'Price', 
      'Predicted Price', 
      'Value?'
    ),
    
    # Make it filterable
    filter = 'top',
    
    # Row names
    rownames = FALSE,
    
    # Class for styling
    class = 'cell-border stripe hover'
    
  ) %>%
    
    # ============================================
  # FORMAT PRICE COLUMNS AS CURRENCY
  # ============================================
  
  formatCurrency(
    columns = c('price', 'predicted_price', 'price_over_expectation'),
    currency = "$",
    digits = 0,
    mark = ","
  ) %>%
    
    # ============================================
  # COLOR CODE PRICE_OVER_EXPECTATION
  # ============================================
  
  formatStyle(
    'price_over_expectation',
    backgroundColor = styleInterval(
      cuts = c(-10000, -5000, -2000, -500, 0, 500, 2000, 5000, 10000),
      values = c(
        '#66BB66',  # Very negative (medium green) - Great deal!
        '#88CC88',  # Negative (lighter green)
        '#AADDAA',  # Slightly negative (light green)
        '#D3F8D3',  # Small negative (very light green)
        '#FFFFFF',  # Zero (white)
        '#FFE6E6',  # Small positive (very light red)
        '#FFBBBB',  # Slightly positive (light red)
        '#FF9999',  # Positive (lighter red)
        '#FF7777',  # Very positive (medium red)
        '#FF5555'   # Extremely positive (bright red) - Overpriced!
      )
    ),
    fontWeight = 'bold'
  ) %>%
    
    # ============================================
  # ADDITIONAL STYLING
  # ============================================
  
  
  # Format miles with commas
  formatRound(
    'miles',
    digits = 0,
    mark = ","
  )
  
  rm(list = setdiff(ls(), "car_table"))
  gc()
  # ============================================
  # ALTERNATIVE: SAVE AS HTML FILE
  # ============================================
  
  # If you want to save it as a standalone HTML file:
  # htmlwidgets::saveWidget(car_table, "car_price_analysis.html")
  # message("Table saved to: car_price_analysis.html")
  
  
  # ============================================
  # COLOR LEGEND REFERENCE
  # ============================================
  
  
  return(car_table)
}


# Your specific URL with filters
# base_url <- "https://www.cars.com/shopping/results/?year_min=2016&list_price_min=0&list_price_max=75000&mileage_max=80000&body_style_slugs%5B%5D=suv&body_style_slugs%5B%5D=sedan&zip=28271&maximum_distance=100&sort=best_match_desc"
 
