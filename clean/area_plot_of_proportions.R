set.seed(123) # For reproducibility

# Sample dates
dates <- seq(as.Date("2022-01-01"), by = "day", length.out = 10)

# Create sample data
sample_data <- expand.grid(
    DATE = dates, 
    STORE_ID = c(1, 2), 
    SELL_ITEM_ID = c('A', 'B', 'C'),
    PROMO_TYPE = c("Promo1", "Promo2", "Promo3")
)

# Adding sales quantities
sample_data$SALES_QTY <- sample(50:100, nrow(sample_data), replace = TRUE)
sample_data$PROMO_SALES_QTY <- sample(0:50, nrow(sample_data), replace = TRUE)

# View the first few rows of the sample dataset
head(sample_data)

library(dplyr)

# Calculate total sales per day, item, and store
total_sales_per_day <- sample_data %>%
    group_by(DATE, SELL_ITEM_ID, STORE_ID) %>%
    summarize(Total_Sales = sum(SALES_QTY), .groups = 'drop')

# Calculate promo sales proportions
sales_proportions <- sample_data %>%
    left_join(total_sales_per_day, by = c("DATE", "SELL_ITEM_ID", "STORE_ID")) %>%
    mutate(
        Promo_Proportion = PROMO_SALES_QTY / Total_Sales
    )

sales_proportions %>% View()

library(ggplot2)

ggplot(sales_proportions, aes(x = DATE, y = Promo_Proportion, fill = PROMO_TYPE)) +
    geom_area(position = 'stack', alpha = 0.6) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
        title = "Proportion of Sales by Promo Type Over Time (Sample Data)",
        x = "Date",
        y = "Proportion of Promo Sales"
    ) +
    theme_minimal()


# Version 2 ----
set.seed(123) # For reproducibility

# Sample dates
dates <- seq(as.Date("2022-01-01"), by = "day", length.out = 10)

# Create sample data
sample_data <- expand.grid(
    DATE = dates, 
    STORE_ID = c(1, 2), 
    SELL_ITEM_ID = c('A'), # Restricting to one item
    PROMO_TYPE = c("Promo1", "Promo2", "No Promo")
)

# Adding sales quantities
sample_data$SALES_QTY <- sample(10:20, nrow(sample_data), replace = TRUE)

# View the first few rows of the sample dataset
head(sample_data)

sample_data %>% View()

# Calculate total sales per day
total_sales_per_day <- sample_data %>%
    group_by(DATE) %>%
    summarize(Total_Sales = sum(SALES_QTY), .groups = 'drop')

# Join with original data to calculate proportions
sales_proportions <- sample_data %>%
    left_join(total_sales_per_day, by = "DATE") %>%
    mutate(
        Promo_Proportion = SALES_QTY / Total_Sales
    )

sales_proportions %>% 
    dplyr::filter(STORE_ID == 1) %>% 
    arrange(DATE)

ggplot(sales_proportions, aes(x = DATE, y = Promo_Proportion, fill = PROMO_TYPE)) +
    geom_area(position = 'fill', alpha = 0.6) + # Use 'fill' position adjustment
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
        title = "Proportion of Sales by Promo Type Over Time (Sample Data)",
        x = "Date",
        y = "Proportion of Promo Sales"
    ) +
    theme_minimal()

