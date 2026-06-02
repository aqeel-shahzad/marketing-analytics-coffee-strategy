# =============================================================================
# 1. Load packages
# =============================================================================

library(tidyverse)
library(data.table)
library(lubridate)
library(janitor)
library(skimr)
library(scales)
library(cluster)
library(factoextra)
library(MASS)
library(caret)
library(knitr)
library(kableExtra)
library(patchwork)
library(conflicted)
library(ggrepel)
library(tibble)

# Set preferred functions where package names overlap
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("margin", "ggplot2")

set.seed(40495557)

# =============================================================================
# 2. Load data
# =============================================================================

# Loaded transaction and prospect customer datasets.

retail_raw <- fread("Retail transaction data.csv")
prospect_raw <- fread("Prospect customers info.csv")


# =============================================================================
# 3. Initial data checks
# =============================================================================

# Checked structure, dimensions and variable names.

glimpse(retail_raw)
glimpse(prospect_raw)

dim(retail_raw)
dim(prospect_raw)

names(retail_raw)
names(prospect_raw)

# =============================================================================
# 4. Data quality checks
# =============================================================================

# Checked missing values, duplicates, counts and numerical ranges.

# Missing values by variable
missing_retail <- retail_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

missing_prospect <- prospect_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

missing_retail
missing_prospect


# Duplicate row checks
sum(duplicated(retail_raw))
sum(duplicated(prospect_raw))


# Number of unique customers and invoices in the transaction data
retail_raw |>
  summarise(
    rows = n(),
    unique_customers = n_distinct(CustomerID),
    unique_invoices = n_distinct(InvoiceNo),
    unique_products = n_distinct(ProductID),
    unique_categories = n_distinct(ProductCategory)
  )


# Basic checks for numerical variables
retail_raw |>
  summarise(
    min_unit_price = min(UnitPrice, na.rm = TRUE),
    max_unit_price = max(UnitPrice, na.rm = TRUE),
    min_quantity = min(Quantity, na.rm = TRUE),
    max_quantity = max(Quantity, na.rm = TRUE),
    min_income = min(Income, na.rm = TRUE),
    max_income = max(Income, na.rm = TRUE),
    min_age = min(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE)
  )


# Check product category counts, including missing categories
retail_raw |>
  count(ProductCategory, sort = TRUE)

# =============================================================================
# 5. Data cleaning, preprocessing and outlier treatment
# =============================================================================

# Cleaned transaction data and created transaction-line spend.

retail_clean_initial <- retail_raw |>
  distinct() |>
  filter(!is.na(ProductCategory)) |>
  mutate(
    InvoiceDate = as.Date(InvoiceDate),
    TotalSpend = UnitPrice * Quantity
  )

# Confirm initial cleaned dimensions before outlier treatment
dim(retail_clean_initial)

# Check missing values after initial cleaning
retail_clean_initial |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

# Check for negative or zero values in key transaction variables
retail_clean_initial |>
  summarise(
    zero_or_negative_price = sum(UnitPrice <= 0, na.rm = TRUE),
    zero_or_negative_quantity = sum(Quantity <= 0, na.rm = TRUE),
    zero_or_negative_spend = sum(TotalSpend <= 0, na.rm = TRUE)
  )

# Inspect high-value transaction lines for potential outliers
top_transaction_lines <- retail_clean_initial |>
  arrange(desc(TotalSpend)) |>
  dplyr::select(
    InvoiceNo, InvoiceDate, CustomerID, ProductID, ProductCategory,
    UnitPrice, Quantity, TotalSpend
  ) |>
  head(20)

top_transaction_lines

# =============================================================================
# 5.1 Outlier diagnostics
# =============================================================================

# Reviewed transaction-line outliers using percentile thresholds.

quantity_99 <- quantile(retail_clean_initial$Quantity, 0.99, na.rm = TRUE)
spend_99    <- quantile(retail_clean_initial$TotalSpend, 0.99, na.rm = TRUE)

quantity_995 <- quantile(retail_clean_initial$Quantity, 0.995, na.rm = TRUE)
spend_995    <- quantile(retail_clean_initial$TotalSpend, 0.995, na.rm = TRUE)

quantity_999 <- quantile(retail_clean_initial$Quantity, 0.999, na.rm = TRUE)
spend_999    <- quantile(retail_clean_initial$TotalSpend, 0.999, na.rm = TRUE)

outlier_thresholds <- tibble(
  Threshold = c("99th percentile", "99.5th percentile", "99.9th percentile"),
  Quantity = c(quantity_99, quantity_995, quantity_999),
  TotalSpend = c(spend_99, spend_995, spend_999)
)

outlier_thresholds

# Flag transaction-line outliers for review
transaction_outliers <- retail_clean_initial |>
  mutate(
    Quantity_Outlier_99 = Quantity > quantity_99,
    Spend_Outlier_99    = TotalSpend > spend_99,
    Quantity_Outlier_999 = Quantity > quantity_999,
    Spend_Outlier_999    = TotalSpend > spend_999,
    Any_Outlier_99 = Quantity_Outlier_99 | Spend_Outlier_99
  ) |>
  filter(Any_Outlier_99) |>
  arrange(desc(TotalSpend)) |>
  dplyr::select(
    InvoiceNo, InvoiceDate, CustomerID, ProductID, ProductCategory,
    UnitPrice, Quantity, TotalSpend,
    Quantity_Outlier_99, Spend_Outlier_99,
    Quantity_Outlier_999, Spend_Outlier_999
  )

transaction_outliers |> head(30)

# Summarise 99th percentile outlier review
transaction_outliers |>
  summarise(
    Outlier_Rows = n(),
    Unique_Customers = n_distinct(CustomerID),
    Total_Outlier_Spend = sum(TotalSpend),
    Share_of_Total_Sales = sum(TotalSpend) / sum(retail_clean_initial$TotalSpend)
  )

# Outlier review by product category
transaction_outliers |>
  group_by(ProductCategory) |>
  summarise(
    Outlier_Rows = n(),
    Outlier_Spend = sum(TotalSpend),
    Max_Quantity = max(Quantity),
    Max_Line_Spend = max(TotalSpend),
    .groups = "drop"
  ) |>
  arrange(desc(Outlier_Spend))

# =============================================================================
# 5.2 Customer-level influence check using Cook's Distance
# =============================================================================

# Used Cook's Distance to identify influential customer-level observations.

analysis_date_initial <- max(retail_clean_initial$InvoiceDate) + days(1)

customer_data_initial <- retail_clean_initial |>
  group_by(CustomerID) |>
  summarise(
    Recency           = as.numeric(analysis_date_initial - max(InvoiceDate)),
    Frequency         = n_distinct(InvoiceNo),
    Monetary          = sum(TotalSpend, na.rm = TRUE),
    AvgBasketValue    = Monetary / Frequency,
    TotalQuantity     = sum(Quantity, na.rm = TRUE),
    CategoryDiversity = n_distinct(ProductCategory),
    ProductDiversity  = n_distinct(ProductID),
    CoffeeSpend       = sum(TotalSpend[ProductCategory == "Coffee"], na.rm = TRUE),
    CoffeeQuantity    = sum(Quantity[ProductCategory == "Coffee"], na.rm = TRUE),
    CoffeeInvoices    = n_distinct(InvoiceNo[ProductCategory == "Coffee"]),
    Married           = dplyr::first(Married),
    HouseholdSize     = dplyr::first(HouseholdSize),
    Income            = dplyr::first(Income),
    Age               = dplyr::first(Age),
    Work              = dplyr::first(Work),
    Education         = dplyr::first(Education),
    .groups = "drop"
  ) |>
  mutate(
    CoffeeBuyer = ifelse(CoffeeSpend > 0, 1, 0),
    CoffeeShare = ifelse(Monetary > 0, CoffeeSpend / Monetary, 0),
    LogMonetary = log(Monetary)
  )

influence_model <- lm(
  LogMonetary ~ Frequency + Recency + CategoryDiversity +
    ProductDiversity + CoffeeSpend + Income + Age + HouseholdSize,
  data = customer_data_initial
)

customer_data_influence <- customer_data_initial |>
  mutate(
    Cooks_Distance = cooks.distance(influence_model),
    Influence_4_over_n = Cooks_Distance > (4 / nrow(customer_data_initial)),
    Influence_gt_1 = Cooks_Distance > 1
  ) |>
  arrange(desc(Cooks_Distance))

customer_data_influence |>
  dplyr::select(
    CustomerID, Monetary, Frequency, Recency, CoffeeSpend, CoffeeQuantity,
    CoffeeShare, Cooks_Distance, Influence_4_over_n, Influence_gt_1
  ) |>
  head(20)

# Specifically inspect C1187
customer_data_influence |>
  filter(CustomerID == "C1187") |>
  dplyr::select(
    CustomerID, Monetary, Frequency, Recency, CoffeeSpend, CoffeeQuantity,
    CoffeeShare, Cooks_Distance, Influence_4_over_n, Influence_gt_1
  )

# =============================================================================
# 5.3 Sensitivity check: with vs without C1187 extreme transaction
# =============================================================================

# Compared customer summaries with and without the extreme coffee transaction.

retail_with_extreme <- retail_clean_initial

retail_without_c1187_extreme <- retail_clean_initial |>
  filter(!(InvoiceNo == 540815 &
             CustomerID == "C1187" &
             ProductID == "P85123" &
             Quantity == 1930))

create_customer_summary <- function(data) {
  analysis_date_temp <- max(data$InvoiceDate) + days(1)
  
  data |>
    group_by(CustomerID) |>
    summarise(
      Recency = as.numeric(analysis_date_temp - max(InvoiceDate)),
      Frequency = n_distinct(InvoiceNo),
      Monetary = sum(TotalSpend, na.rm = TRUE),
      CoffeeSpend = sum(TotalSpend[ProductCategory == "Coffee"], na.rm = TRUE),
      CoffeeQuantity = sum(Quantity[ProductCategory == "Coffee"], na.rm = TRUE),
      CoffeeBuyer = ifelse(CoffeeSpend > 0, 1, 0),
      .groups = "drop"
    )
}

customer_with_extreme <- create_customer_summary(retail_with_extreme)
customer_without_c1187_extreme <- create_customer_summary(retail_without_c1187_extreme)

sensitivity_summary <- tibble(
  Version = c("With C1187 extreme transaction", "Without C1187 extreme transaction"),
  Customers = c(nrow(customer_with_extreme), nrow(customer_without_c1187_extreme)),
  Total_Sales = c(sum(customer_with_extreme$Monetary), sum(customer_without_c1187_extreme$Monetary)),
  Mean_Monetary = c(mean(customer_with_extreme$Monetary), mean(customer_without_c1187_extreme$Monetary)),
  Median_Monetary = c(median(customer_with_extreme$Monetary), median(customer_without_c1187_extreme$Monetary)),
  Max_Monetary = c(max(customer_with_extreme$Monetary), max(customer_without_c1187_extreme$Monetary)),
  Total_Coffee_Spend = c(sum(customer_with_extreme$CoffeeSpend), sum(customer_without_c1187_extreme$CoffeeSpend)),
  Mean_Coffee_Spend = c(mean(customer_with_extreme$CoffeeSpend), mean(customer_without_c1187_extreme$CoffeeSpend)),
  Median_Coffee_Spend_Buyers = c(
    median(customer_with_extreme$CoffeeSpend[customer_with_extreme$CoffeeBuyer == 1]),
    median(customer_without_c1187_extreme$CoffeeSpend[customer_without_c1187_extreme$CoffeeBuyer == 1])
  ),
  Max_Coffee_Spend = c(max(customer_with_extreme$CoffeeSpend), max(customer_without_c1187_extreme$CoffeeSpend))
)

print(sensitivity_summary, width = Inf)

# =============================================================================
# 5.4 Final outlier treatment for segmentation dataset
# =============================================================================

# Removed the confirmed extreme C1187 coffee transaction only.

retail_clean <- retail_clean_initial |>
  filter(!(InvoiceNo == 540815 &
             CustomerID == "C1187" &
             ProductID == "P85123" &
             Quantity == 1930))

final_cleaning_summary <- tibble(
  Rows_Before = nrow(retail_clean_initial),
  Rows_After = nrow(retail_clean),
  Rows_Removed = nrow(retail_clean_initial) - nrow(retail_clean),
  Customers_Before = n_distinct(retail_clean_initial$CustomerID),
  Customers_After = n_distinct(retail_clean$CustomerID),
  Sales_Before = sum(retail_clean_initial$TotalSpend),
  Sales_After = sum(retail_clean$TotalSpend),
  Sales_Removed = Sales_Before - Sales_After,
  Coffee_Spend_Before = sum(retail_clean_initial$TotalSpend[retail_clean_initial$ProductCategory == "Coffee"]),
  Coffee_Spend_After = sum(retail_clean$TotalSpend[retail_clean$ProductCategory == "Coffee"]),
  Coffee_Spend_Removed = Coffee_Spend_Before - Coffee_Spend_After
)

print(final_cleaning_summary, width = Inf)

# Final cleaned transaction summary
retail_clean |>
  summarise(
    rows = n(),
    unique_customers = n_distinct(CustomerID),
    unique_invoices = n_distinct(InvoiceNo),
    unique_products = n_distinct(ProductID),
    unique_categories = n_distinct(ProductCategory),
    total_sales = sum(TotalSpend, na.rm = TRUE),
    average_line_spend = mean(TotalSpend, na.rm = TRUE),
    median_line_spend = median(TotalSpend, na.rm = TRUE),
    max_quantity = max(Quantity, na.rm = TRUE),
    max_line_spend = max(TotalSpend, na.rm = TRUE)
  )

# =============================================================================
# 6. Create customer-level dataset
# =============================================================================

# Aggregated transaction data to customer level for segmentation.

analysis_date <- max(retail_clean$InvoiceDate) + days(1)

customer_data <- retail_clean |>
  group_by(CustomerID) |>
  summarise(
    first_purchase_date = min(InvoiceDate),
    last_purchase_date  = max(InvoiceDate),
    Recency             = as.numeric(analysis_date - max(InvoiceDate)),
    Frequency           = n_distinct(InvoiceNo),
    Monetary            = sum(TotalSpend, na.rm = TRUE),
    AvgBasketValue      = Monetary / Frequency,
    TotalQuantity       = sum(Quantity, na.rm = TRUE),
    CategoryDiversity   = n_distinct(ProductCategory),
    ProductDiversity    = n_distinct(ProductID),
    CoffeeSpend         = sum(TotalSpend[ProductCategory == "Coffee"], na.rm = TRUE),
    CoffeeQuantity      = sum(Quantity[ProductCategory == "Coffee"], na.rm = TRUE),
    CoffeeInvoices      = n_distinct(InvoiceNo[ProductCategory == "Coffee"]),
    Married             = dplyr::first(Married),
    HouseholdSize       = dplyr::first(HouseholdSize),
    Income              = dplyr::first(Income),
    Age                 = dplyr::first(Age),
    ZipCode             = dplyr::first(ZipCode),
    Work                = dplyr::first(Work),
    Education           = dplyr::first(Education),
    .groups = "drop"
  ) |>
  mutate(
    CoffeeBuyer = ifelse(CoffeeSpend > 0, 1, 0),
    CoffeeShare = CoffeeSpend / Monetary
  )

# Replace any undefined coffee share values with zero
customer_data <- customer_data |>
  mutate(
    CoffeeShare = ifelse(is.na(CoffeeShare), 0, CoffeeShare)
  )

# Check customer-level dataset
glimpse(customer_data)

customer_data |>
  summarise(
    customers = n(),
    total_sales = sum(Monetary, na.rm = TRUE),
    avg_monetary = mean(Monetary, na.rm = TRUE),
    median_monetary = median(Monetary, na.rm = TRUE),
    avg_frequency = mean(Frequency, na.rm = TRUE),
    median_frequency = median(Frequency, na.rm = TRUE),
    avg_recency = mean(Recency, na.rm = TRUE),
    coffee_buyers = sum(CoffeeBuyer),
    coffee_buyer_rate = mean(CoffeeBuyer)
  )

# =============================================================================
# 7. Customer-level summary statistics
# =============================================================================

# Summarised customer value, frequency, recency and coffee relevance.

customer_summary <- customer_data |>
  summarise(
    Customers = n(),
    Total_Sales = sum(Monetary, na.rm = TRUE),
    Average_Monetary = mean(Monetary, na.rm = TRUE),
    Median_Monetary = median(Monetary, na.rm = TRUE),
    Average_Frequency = mean(Frequency, na.rm = TRUE),
    Median_Frequency = median(Frequency, na.rm = TRUE),
    Average_Recency = mean(Recency, na.rm = TRUE),
    Coffee_Buyers = sum(CoffeeBuyer),
    Coffee_Buyer_Rate = mean(CoffeeBuyer)
  )

print(customer_summary, width = Inf)

# =============================================================================
# 8. Exploratory data analysis visuals
# =============================================================================

# Created EDA plots and defined a consistent ggplot2 theme.

plot_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, margin = margin(b = 6)),
    plot.subtitle = element_text(colour = "grey40", size = 9, margin = margin(b = 10)),
    plot.caption  = element_text(colour = "grey50", size = 8, hjust = 0),
    axis.title    = element_text(size = 10),
    axis.text     = element_text(size = 9),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 8.1 Create dynamic summary values for plot subtitles
# =============================================================================

customer_n <- nrow(customer_data)

monetary_median <- median(customer_data$Monetary, na.rm = TRUE)
monetary_mean   <- mean(customer_data$Monetary, na.rm = TRUE)

frequency_median <- median(customer_data$Frequency, na.rm = TRUE)
frequency_mean   <- mean(customer_data$Frequency, na.rm = TRUE)

recency_median <- median(customer_data$Recency, na.rm = TRUE)
recency_mean   <- mean(customer_data$Recency, na.rm = TRUE)

coffee_buyer_n <- sum(customer_data$CoffeeBuyer == 1, na.rm = TRUE)
coffee_buyer_pct <- coffee_buyer_n / customer_n

coffee_buyer_data <- customer_data |>
  filter(CoffeeBuyer == 1)

coffee_spend_median <- median(coffee_buyer_data$CoffeeSpend, na.rm = TRUE)
coffee_spend_max    <- max(coffee_buyer_data$CoffeeSpend, na.rm = TRUE)

# =============================================================================
# Plot 1: Raw vs log-transformed Monetary value
# =============================================================================

# Compared raw and log-transformed monetary value to assess skewness.

customer_data <- customer_data |>
  mutate(
    LogMonetary = log(Monetary)
  )

p1_raw <- customer_data |>
  ggplot(aes(x = Monetary)) +
  geom_histogram(
    bins = 35,
    fill = "#D95F02",
    colour = "white",
    linewidth = 0.2
  ) +
  scale_x_continuous(
    labels = label_dollar(prefix = "£"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Raw monetary value",
    subtitle = "Strong positive skew with a long right tail",
    x = "Total spend (£)",
    y = "Number of customers"
  ) +
  plot_theme

p1_log <- customer_data |>
  ggplot(aes(x = LogMonetary)) +
  geom_histogram(
    bins = 35,
    fill = "#1B9E77",
    colour = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "Log-transformed monetary value",
    subtitle = "Log transformation reduces skewness",
    x = "log(total spend)",
    y = "Number of customers"
  ) +
  plot_theme

p1_skew <- p1_raw + p1_log +
  plot_annotation(
    title = "Customer monetary value is strongly right-skewed",
    subtitle = "Side-by-side comparison of raw and log-transformed customer spend",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)."
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 8, colour = "grey50", hjust = 0)
  )

print(p1_skew)

# ----------------------------------------------------------------------------- 
# Plot 2: Distribution of purchase frequency
# -----------------------------------------------------------------------------

# Visualised purchase frequency using invoice counts per customer.

p2 <- customer_data |>
  count(Frequency) |>
  ggplot(aes(x = factor(Frequency), y = n)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Distribution of purchase frequency",
    subtitle = paste0(
      "Count of distinct invoices per customer · median ",
      round(frequency_median, 1),
      ", mean ",
      round(frequency_mean, 1)
    ),
    x        = "Number of purchases",
    y        = "Number of customers",
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p2)

# ----------------------------------------------------------------------------- 
# Plot 3: Distribution of recency
# -----------------------------------------------------------------------------

# Visualised customer recency from the analysis date.

p3 <- customer_data |>
  ggplot(aes(x = Recency)) +
  geom_histogram(
    binwidth  = 10,
    fill      = "#2C7BB6",
    colour    = "white",
    linewidth = 0.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Distribution of customer recency",
    subtitle = paste0(
      "Days since last purchase · median ",
      round(recency_median, 0),
      " days, mean ",
      round(recency_mean, 0),
      " days"
    ),
    x        = "Days since last purchase",
    y        = "Number of customers",
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p3)

# ----------------------------------------------------------------------------- 
# Plot 4: Total sales by product category
# -----------------------------------------------------------------------------

# Compared revenue contribution across product categories.

category_sales <- retail_clean |>
  group_by(ProductCategory) |>
  summarise(
    TotalSales = sum(TotalSpend, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(TotalSales)) |>
  mutate(
    CategoryRank = row_number(),
    ProductCategory = fct_reorder(ProductCategory, TotalSales)
  )

coffee_sales <- category_sales |>
  filter(ProductCategory == "Coffee") |>
  pull(TotalSales)

coffee_rank <- category_sales |>
  filter(ProductCategory == "Coffee") |>
  pull(CategoryRank)

p4 <- category_sales |>
  ggplot(aes(x = TotalSales, y = ProductCategory, fill = ProductCategory)) +
  geom_col(colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = label_dollar(prefix = "£", accuracy = 1)(TotalSales)),
    hjust  = -0.1,
    size   = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = label_dollar(prefix = "£"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "Total sales by product category",
    subtitle = paste0(
      "Coffee ranks #", coffee_rank,
      " by revenue at ",
      label_dollar(prefix = "£", accuracy = 1)(coffee_sales)
    ),
    x        = "Total sales (£)",
    y        = NULL,
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p4)

# ----------------------------------------------------------------------------- 
# Plot 5: Coffee buyers vs non-coffee buyers
# -----------------------------------------------------------------------------

# Summarised coffee buyer and non-coffee buyer groups.

coffee_buyer_summary <- customer_data |>
  mutate(
    BuyerGroup = ifelse(CoffeeBuyer == 1, "Coffee buyer", "Non-coffee buyer")
  ) |>
  count(BuyerGroup) |>
  mutate(
    Pct   = n / sum(n),
    Label = paste0(n, "\n(", percent(Pct, accuracy = 0.1), ")")
  )

p5 <- coffee_buyer_summary |>
  ggplot(aes(x = BuyerGroup, y = n, fill = BuyerGroup)) +
  geom_col(colour = "white", linewidth = 0.2, width = 0.5) +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5, colour = "grey30") +
  scale_fill_manual(
    values = c("Coffee buyer" = "#2C7BB6", "Non-coffee buyer" = "#ABDDA4")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Coffee buyers vs non-coffee buyers",
    subtitle = paste0(
      percent(coffee_buyer_pct, accuracy = 0.1),
      " of customers (", coffee_buyer_n, "/", customer_n,
      ") purchased coffee at least once"
    ),
    x        = NULL,
    y        = "Number of customers",
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p5)

# ----------------------------------------------------------------------------- 
# Plot 6: Coffee spend distribution among coffee buyers
# -----------------------------------------------------------------------------

# Visualised coffee spend among buyers, capped at the 95th percentile.

coffee_spend_95 <- quantile(coffee_buyer_data$CoffeeSpend, 0.95, na.rm = TRUE)

p6 <- coffee_buyer_data |>
  filter(CoffeeSpend <= coffee_spend_95) |>
  ggplot(aes(x = CoffeeSpend)) +
  geom_histogram(
    binwidth  = 5,
    boundary  = 0,
    fill      = "#2C7BB6",
    colour    = "white",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = coffee_spend_median,
    linetype   = "dashed",
    colour     = "#D7191C",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x      = coffee_spend_median + 3,
    y      = Inf,
    label  = paste0(
      "Median: ",
      label_dollar(prefix = "£", accuracy = 0.01)(coffee_spend_median)
    ),
    hjust  = 0,
    vjust  = 1.4,
    size   = 3,
    colour = "#D7191C"
  ) +
  scale_x_continuous(labels = label_dollar(prefix = "£")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Coffee spend among coffee buyers",
    subtitle = paste0(
      "n = ", coffee_buyer_n,
      " · median ",
      label_dollar(prefix = "£", accuracy = 0.01)(coffee_spend_median),
      " · display capped at 95th percentile"
    ),
    x        = "Total coffee spend (£)",
    y        = "Number of customers",
    caption  = paste0(
      "Source: Retail transaction data (10 Jan – 16 Aug 2025). Maximum observed coffee spend: ",
      label_dollar(prefix = "£", accuracy = 1)(coffee_spend_max)
    )
  ) +
  plot_theme

print(p6)

# ----------------------------------------------------------------------------- 
# Plot 7: Income distribution by coffee buyer status
# -----------------------------------------------------------------------------

# Compared income distributions by coffee buyer status.

p7 <- customer_data |>
  mutate(
    BuyerGroup = ifelse(CoffeeBuyer == 1, "Coffee buyer", "Non-coffee buyer")
  ) |>
  ggplot(aes(x = Income, fill = BuyerGroup)) +
  geom_histogram(
    binwidth = 10,
    position = "identity",
    alpha = 0.6,
    colour = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c("Coffee buyer" = "#2C7BB6", "Non-coffee buyer" = "#ABDDA4")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Income distribution by coffee buyer status",
    subtitle = "Comparison of coffee buyers and non-coffee buyers before formal segmentation",
    x        = "Income band (£000s)",
    y        = "Number of customers",
    fill     = NULL,
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "top")

print(p7)

# ----------------------------------------------------------------------------- 
# Plot 8: Age distribution by coffee buyer status
# -----------------------------------------------------------------------------

# Compared age distributions by coffee buyer status.

p8 <- customer_data |>
  mutate(
    BuyerGroup = ifelse(CoffeeBuyer == 1, "Coffee buyer", "Non-coffee buyer")
  ) |>
  ggplot(aes(x = Age, fill = BuyerGroup)) +
  geom_histogram(
    binwidth = 5,
    position = "identity",
    alpha = 0.6,
    colour = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c("Coffee buyer" = "#2C7BB6", "Non-coffee buyer" = "#ABDDA4")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Age distribution by coffee buyer status",
    subtitle = "Comparison of coffee buyers and non-coffee buyers before formal segmentation",
    x        = "Age",
    y        = "Number of customers",
    fill     = NULL,
    caption  = "Source: Retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "top")

print(p8)

# =============================================================================
# 9. RFM analysis
# =============================================================================
# Applied RFM analysis using recency, frequency and monetary value.
# =============================================================================
# 9.1 Prepare RFM base table
# =============================================================================

# Prepared the customer-level RFM dataset.

rfm_base <- customer_data |>
  dplyr::select(
    CustomerID,
    Recency,
    Frequency,
    Monetary,
    CoffeeSpend,
    CoffeeBuyer,
    CoffeeShare,
    Income,
    Age,
    HouseholdSize,
    Married,
    Work,
    Education
  )

# Checked the distribution of the three RFM variables before scoring.

rfm_base |>
  summarise(
    Customers = n(),
    Min_Recency = min(Recency),
    Median_Recency = median(Recency),
    Mean_Recency = mean(Recency),
    Max_Recency = max(Recency),
    Min_Frequency = min(Frequency),
    Median_Frequency = median(Frequency),
    Mean_Frequency = mean(Frequency),
    Max_Frequency = max(Frequency),
    Min_Monetary = min(Monetary),
    Median_Monetary = median(Monetary),
    Mean_Monetary = mean(Monetary),
    Max_Monetary = max(Monetary)
  ) |>
  print(width = Inf)


# =============================================================================
# 9.2 Independent RFM scoring
# =============================================================================

# Created independent RFM scores across the full customer base.

# Reverse scored recency so recent customers receive higher scores.

rfm_independent <- rfm_base |>
  mutate(
    R_score_ind = ntile(desc(Recency), 5),
    F_score_ind = ntile(Frequency, 5),
    M_score_ind = ntile(Monetary, 5),
    RFM_score_ind = paste0(R_score_ind, F_score_ind, M_score_ind),
    RFM_total_ind = R_score_ind + F_score_ind + M_score_ind
  )

# Checked that independent RFM scores were created across the expected score range.

rfm_independent |>
  summarise(
    R_min = min(R_score_ind),
    R_max = max(R_score_ind),
    F_min = min(F_score_ind),
    F_max = max(F_score_ind),
    M_min = min(M_score_ind),
    M_max = max(M_score_ind),
    Total_min = min(RFM_total_ind),
    Total_max = max(RFM_total_ind)
  )

# Created an independent RFM table showing the most common exact RFM score cells.

rfm_independent_table <- rfm_independent |>
  group_by(RFM_score_ind) |>
  summarise(
    Customers = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Coffee_Buyer_Rate = mean(CoffeeBuyer),
    Avg_Coffee_Spend = mean(CoffeeSpend),
    .groups = "drop"
  ) |>
  arrange(desc(Customers))

rfm_independent_table |> head(20)


# =============================================================================
# 9.3 Independent RFM segment creation
# =============================================================================

# Converted independent RFM scores into customer segments.

# Ordered segment rules from specific behavioural groups to broader score groups.

rfm_independent <- rfm_independent |>
  mutate(
    RFM_segment_ind = case_when(
      R_score_ind >= 4 & F_score_ind >= 4 & M_score_ind >= 4 ~ "Champions",
      R_score_ind >= 4 & F_score_ind >= 3 & M_score_ind >= 3 ~ "Loyal high-value",
      R_score_ind >= 4 & F_score_ind <= 2 ~ "Recent low-frequency",
      R_score_ind <= 2 & F_score_ind >= 4 & M_score_ind >= 4 ~ "At-risk high-value",
      R_score_ind <= 2 & F_score_ind <= 2 & M_score_ind <= 2 ~ "Low-value inactive",
      RFM_total_ind >= 11 ~ "Promising",
      RFM_total_ind >= 8  ~ "Moderate",
      RFM_total_ind >= 5  ~ "Needs attention",
      TRUE ~ "Low priority"
    )
  )

# Checked the number of customers assigned to each independent RFM segment.

rfm_independent |>
  count(RFM_segment_ind, sort = TRUE)

# Summarised independent RFM segments by customer value, coffee relevance and demographics.

rfm_independent_segment_summary <- rfm_independent |>
  group_by(RFM_segment_ind) |>
  summarise(
    Customers = n(),
    Customer_Share = n() / nrow(rfm_independent),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Median_Monetary = median(Monetary),
    Total_Monetary = sum(Monetary),
    Coffee_Buyers = sum(CoffeeBuyer),
    Coffee_Buyer_Rate = mean(CoffeeBuyer),
    Avg_Coffee_Spend = mean(CoffeeSpend),
    Median_Age = median(Age),
    Median_Income = median(Income),
    .groups = "drop"
  ) |>
  arrange(desc(Avg_Monetary))

print(rfm_independent_segment_summary, width = Inf)


# =============================================================================
# 9.4 Sequential RFM scoring
# =============================================================================

# Created sequential RFM scores by ranking customers first by recency, then by
# frequency within each recency group, and finally by monetary value within each
# recency-frequency group.

rfm_sequential <- rfm_base |>
  mutate(
    R_score_seq = ntile(desc(Recency), 5)
  ) |>
  group_by(R_score_seq) |>
  mutate(
    F_score_seq = ntile(Frequency, 5)
  ) |>
  ungroup() |>
  group_by(R_score_seq, F_score_seq) |>
  mutate(
    M_score_seq = ntile(Monetary, 5)
  ) |>
  ungroup() |>
  mutate(
    RFM_score_seq = paste0(R_score_seq, F_score_seq, M_score_seq),
    RFM_total_seq = R_score_seq + F_score_seq + M_score_seq
  )

# Checked that sequential RFM scores were created across the expected score range.

rfm_sequential |>
  summarise(
    R_min = min(R_score_seq),
    R_max = max(R_score_seq),
    F_min = min(F_score_seq),
    F_max = max(F_score_seq),
    M_min = min(M_score_seq),
    M_max = max(M_score_seq),
    Total_min = min(RFM_total_seq),
    Total_max = max(RFM_total_seq)
  )

# Created a sequential RFM table showing the most common exact RFM score cells.

rfm_sequential_table <- rfm_sequential |>
  group_by(RFM_score_seq) |>
  summarise(
    Customers = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Coffee_Buyer_Rate = mean(CoffeeBuyer),
    Avg_Coffee_Spend = mean(CoffeeSpend),
    .groups = "drop"
  ) |>
  arrange(desc(Customers))

rfm_sequential_table |> head(20)


# =============================================================================
# 9.5 Sequential RFM segment creation
# =============================================================================

# Converted sequential RFM scores into interpretable customer segments using the
# same labelling logic as the independent RFM method.

# Segment rules were ordered deliberately. In case_when(), earlier rules take
# priority over later rules, so specific behavioural groups are identified before
# broader total-score groups.

rfm_sequential <- rfm_sequential |>
  mutate(
    RFM_segment_seq = case_when(
      R_score_seq >= 4 & F_score_seq >= 4 & M_score_seq >= 4 ~ "Champions",
      R_score_seq >= 4 & F_score_seq >= 3 & M_score_seq >= 3 ~ "Loyal high-value",
      R_score_seq >= 4 & F_score_seq <= 2 ~ "Recent low-frequency",
      R_score_seq <= 2 & F_score_seq >= 4 & M_score_seq >= 4 ~ "At-risk high-value",
      R_score_seq <= 2 & F_score_seq <= 2 & M_score_seq <= 2 ~ "Low-value inactive",
      RFM_total_seq >= 11 ~ "Promising",
      RFM_total_seq >= 8  ~ "Moderate",
      RFM_total_seq >= 5  ~ "Needs attention",
      TRUE ~ "Low priority"
    )
  )

# Checked the number of customers assigned to each sequential RFM segment.

rfm_sequential |>
  count(RFM_segment_seq, sort = TRUE)

# Summarised sequential RFM segments by customer value, coffee relevance and demographics.

rfm_sequential_segment_summary <- rfm_sequential |>
  group_by(RFM_segment_seq) |>
  summarise(
    Customers = n(),
    Customer_Share = n() / nrow(rfm_sequential),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Median_Monetary = median(Monetary),
    Total_Monetary = sum(Monetary),
    Coffee_Buyers = sum(CoffeeBuyer),
    Coffee_Buyer_Rate = mean(CoffeeBuyer),
    Avg_Coffee_Spend = mean(CoffeeSpend),
    Median_Age = median(Age),
    Median_Income = median(Income),
    .groups = "drop"
  ) |>
  arrange(desc(Avg_Monetary))

print(rfm_sequential_segment_summary, width = Inf)


# =============================================================================
# 9.6 Compare independent and sequential RFM segment assignments
# =============================================================================

# Compared independent and sequential RFM segment assignments.

rfm_comparison <- rfm_independent |>
  dplyr::select(
    CustomerID,
    RFM_score_ind,
    RFM_total_ind,
    RFM_segment_ind
  ) |>
  left_join(
    rfm_sequential |>
      dplyr::select(
        CustomerID,
        RFM_score_seq,
        RFM_total_seq,
        RFM_segment_seq
      ),
    by = "CustomerID"
  ) |>
  mutate(
    Same_Segment = RFM_segment_ind == RFM_segment_seq
  )

# Created a cross-tabulation to compare segment assignments across the two methods.

rfm_segment_crosstab <- table(
  rfm_comparison$RFM_segment_ind,
  rfm_comparison$RFM_segment_seq
)

rfm_segment_crosstab

# Calculated the agreement rate between the independent and sequential RFM methods.

rfm_method_agreement <- rfm_comparison |>
  summarise(
    Customers = n(),
    Same_Segment_Count = sum(Same_Segment),
    Same_Segment_Rate = mean(Same_Segment)
  )

rfm_method_agreement


# =============================================================================
# 9.7 RFM segment visualisations
# =============================================================================

# Visualised customer value, segment size and coffee relevance by RFM segment.

# Used the independent RFM solution as the main RFM segmentation view for plotting.

rfm_plot_data <- rfm_independent_segment_summary |>
  mutate(
    RFM_segment_ind = fct_reorder(RFM_segment_ind, Avg_Monetary)
  )


# -----------------------------------------------------------------------------
# Plot 9: Average monetary value by independent RFM segment
# -----------------------------------------------------------------------------

# Compared average customer spend across independent RFM segments.

p9 <- rfm_plot_data |>
  ggplot(aes(x = Avg_Monetary, y = RFM_segment_ind)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = label_dollar(prefix = "£", accuracy = 1)(Avg_Monetary)),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = label_dollar(prefix = "£"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Average monetary value by RFM segment",
    subtitle = "Independent RFM scoring",
    x = "Average customer spend (£)",
    y = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p9)


# -----------------------------------------------------------------------------
# Plot 10: Customer share by independent RFM segment
# -----------------------------------------------------------------------------

# Compared the size of each independent RFM segment as a share of the customer base.

p10 <- rfm_independent_segment_summary |>
  mutate(
    RFM_segment_ind = fct_reorder(RFM_segment_ind, Customer_Share)
  ) |>
  ggplot(aes(x = Customer_Share, y = RFM_segment_ind)) +
  geom_col(fill = "#1B9E77", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = percent(Customer_Share, accuracy = 0.1)),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Customer share by RFM segment",
    subtitle = "Independent RFM scoring",
    x = "Share of customers",
    y = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p10)


# -----------------------------------------------------------------------------
# Plot 11: Coffee buyer rate by independent RFM segment
# -----------------------------------------------------------------------------

# Compared the proportion of coffee buyers across independent RFM segments.

p11 <- rfm_independent_segment_summary |>
  mutate(
    RFM_segment_ind = fct_reorder(RFM_segment_ind, Coffee_Buyer_Rate)
  ) |>
  ggplot(aes(x = Coffee_Buyer_Rate, y = RFM_segment_ind)) +
  geom_col(fill = "#D95F02", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = percent(Coffee_Buyer_Rate, accuracy = 0.1)),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Coffee buyer rate by RFM segment",
    subtitle = "Independent RFM scoring",
    x = "Coffee buyer rate",
    y = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p11)


# -----------------------------------------------------------------------------
# Plot 12: Recency, frequency and monetary profile by segment
# -----------------------------------------------------------------------------

# Compared average recency, frequency and monetary values by RFM segment.

rfm_profile_long <- rfm_independent_segment_summary |>
  dplyr::select(
    RFM_segment_ind,
    Avg_Recency,
    Avg_Frequency,
    Avg_Monetary
  ) |>
  pivot_longer(
    cols = c(Avg_Recency, Avg_Frequency, Avg_Monetary),
    names_to = "Metric",
    values_to = "Value"
  ) |>
  mutate(
    Metric = recode(
      Metric,
      "Avg_Recency" = "Average recency",
      "Avg_Frequency" = "Average frequency",
      "Avg_Monetary" = "Average monetary"
    )
  )

p12 <- rfm_profile_long |>
  ggplot(aes(x = Value, y = reorder(RFM_segment_ind, Value))) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  facet_wrap(~ Metric, scales = "free_x") +
  labs(
    title = "RFM profile by customer segment",
    subtitle = "Independent RFM scoring",
    x = "Average value",
    y = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p12)


# -----------------------------------------------------------------------------
# Plot 13: Independent vs sequential RFM method agreement
# -----------------------------------------------------------------------------

# Visualised agreement between independent and sequential RFM scoring.

rfm_agreement_plot_data <- rfm_comparison |>
  mutate(
    Agreement_Status = ifelse(Same_Segment, "Same segment", "Different segment")
  ) |>
  count(Agreement_Status) |>
  mutate(
    Share = n / sum(n),
    Label = paste0(n, "\n(", percent(Share, accuracy = 0.1), ")")
  )

p13 <- rfm_agreement_plot_data |>
  ggplot(aes(x = Agreement_Status, y = n, fill = Agreement_Status)) +
  geom_col(colour = "white", linewidth = 0.2, width = 0.55) +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5, colour = "grey30") +
  scale_fill_manual(
    values = c("Same segment" = "#1B9E77", "Different segment" = "#D95F02")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Agreement between independent and sequential RFM methods",
    subtitle = "Comparison of customer segment assignments",
    x = NULL,
    y = "Number of customers",
    fill = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p13)


# =============================================================================
# 9.8 Save final RFM outputs for later sections
# =============================================================================

# Joined RFM scores and segment labels to the customer-level dataset.

customer_data_rfm <- customer_data |>
  left_join(
    rfm_independent |>
      dplyr::select(
        CustomerID,
        R_score_ind,
        F_score_ind,
        M_score_ind,
        RFM_score_ind,
        RFM_total_ind,
        RFM_segment_ind
      ),
    by = "CustomerID"
  ) |>
  left_join(
    rfm_sequential |>
      dplyr::select(
        CustomerID,
        R_score_seq,
        F_score_seq,
        M_score_seq,
        RFM_score_seq,
        RFM_total_seq,
        RFM_segment_seq
      ),
    by = "CustomerID"
  )

# Checked the final customer-level dataset containing both independent and sequential RFM outputs.

glimpse(customer_data_rfm)

# Exported RFM outputs for reporting, checking and potential Power BI visualisation.

write.csv(customer_data_rfm, "customer_data_rfm.csv", row.names = FALSE)
write.csv(rfm_independent_table, "rfm_independent_table.csv", row.names = FALSE)
write.csv(rfm_sequential_table, "rfm_sequential_table.csv", row.names = FALSE)
write.csv(rfm_independent_segment_summary, "rfm_independent_segment_summary.csv", row.names = FALSE)
write.csv(rfm_sequential_segment_summary, "rfm_sequential_segment_summary.csv", row.names = FALSE)
write.csv(rfm_comparison, "rfm_method_comparison.csv", row.names = FALSE)

# =============================================================================
# 9.9 Report-ready RFM tables
# =============================================================================

# Created report-ready RFM summary tables.

# -----------------------------------------------------------------------------
# Table 1: Independent RFM segment summary
# -----------------------------------------------------------------------------

# Summarised the independent RFM solution into a compact segment-level table.

rfm_independent_report_table <- rfm_independent_segment_summary |>
  transmute(
    Segment = RFM_segment_ind,
    Customers = Customers,
    Share = percent(Customer_Share, accuracy = 0.1),
    `Avg recency (days)` = round(Avg_Recency, 1),
    `Avg frequency` = round(Avg_Frequency, 2),
    `Avg monetary (£)` = label_dollar(prefix = "£", accuracy = 1)(Avg_Monetary),
    `Coffee buyer rate` = percent(Coffee_Buyer_Rate, accuracy = 0.1)
  ) |>
  arrange(desc(Customers))

rfm_independent_report_table

# Created formatted report table.
rfm_independent_report_table |>
  kable(
    caption = "Independent RFM segment summary",
    align = "lrrrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# -----------------------------------------------------------------------------
# Table 2: Sequential RFM segment summary
# -----------------------------------------------------------------------------

# Summarised the sequential RFM solution into a compact segment-level table.

rfm_sequential_report_table <- rfm_sequential_segment_summary |>
  transmute(
    Segment = RFM_segment_seq,
    Customers = Customers,
    Share = percent(Customer_Share, accuracy = 0.1),
    `Avg recency (days)` = round(Avg_Recency, 1),
    `Avg frequency` = round(Avg_Frequency, 2),
    `Avg monetary (£)` = label_dollar(prefix = "£", accuracy = 1)(Avg_Monetary),
    `Coffee buyer rate` = percent(Coffee_Buyer_Rate, accuracy = 0.1)
  ) |>
  arrange(desc(Customers))

rfm_sequential_report_table

# Created formatted report table.
rfm_sequential_report_table |>
  kable(
    caption = "Sequential RFM segment summary",
    align = "lrrrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )


# -----------------------------------------------------------------------------
# Table 3: Independent vs sequential RFM comparison
# -----------------------------------------------------------------------------

# Compared independent and sequential RFM methods.

ind_champions_n <- rfm_independent_segment_summary |>
  filter(RFM_segment_ind == "Champions") |>
  pull(Customers)

seq_champions_n <- rfm_sequential_segment_summary |>
  filter(RFM_segment_seq == "Champions") |>
  pull(Customers)

ind_largest_segment <- rfm_independent_segment_summary |>
  arrange(desc(Customers)) |>
  slice(1)

seq_largest_segment <- rfm_sequential_segment_summary |>
  arrange(desc(Customers)) |>
  slice(1)

agreement_n <- rfm_method_agreement |>
  pull(Same_Segment_Count)

agreement_rate <- rfm_method_agreement |>
  pull(Same_Segment_Rate)

rfm_method_comparison_table <- tibble(
  `Comparison point` = c(
    "Scoring logic",
    "Main advantage",
    "Main limitation",
    "Champions size",
    "Largest segment",
    "Coffee relevance",
    "Segment agreement"
  ),
  `Independent RFM` = c(
    "Scores recency, frequency and monetary value separately across the full customer base",
    "Clearer global comparison between customers",
    "Can create large dominant groups if R, F and M are strongly aligned",
    paste0(ind_champions_n, " customers"),
    paste0(
      ind_largest_segment$RFM_segment_ind,
      " (",
      ind_largest_segment$Customers,
      " customers; ",
      percent(ind_largest_segment$Customer_Share, accuracy = 0.1),
      ")"
    ),
    "Champions and Promising show the strongest coffee buyer rates",
    paste0(
      agreement_n,
      " of ",
      nrow(rfm_comparison),
      " customers matched (",
      percent(agreement_rate, accuracy = 0.1),
      ")"
    )
  ),
  `Sequential RFM` = c(
    "Scores recency first, then frequency within recency groups, then monetary value within recency-frequency groups",
    "Produces more balanced RFM score cells",
    "Segment scores are less directly comparable across the full population",
    paste0(seq_champions_n, " customers"),
    paste0(
      seq_largest_segment$RFM_segment_seq,
      " (",
      seq_largest_segment$Customers,
      " customers; ",
      percent(seq_largest_segment$Customer_Share, accuracy = 0.1),
      ")"
    ),
    "Champions and Promising also show the strongest coffee buyer rates",
    paste0(
      agreement_n,
      " of ",
      nrow(rfm_comparison),
      " customers matched (",
      percent(agreement_rate, accuracy = 0.1),
      ")"
    )
  )
)

rfm_method_comparison_table

# =============================================================================
# 10. Cluster analysis
# =============================================================================

# Clustered customers using standardised behavioural and coffee-related variables.

# =============================================================================
# 10.1 Prepare clustering dataset
# =============================================================================

# Prepared clustering variables and log-transformed skewed measures.

cluster_base <- customer_data_rfm |>
  mutate(
    LogMonetary        = log(Monetary),
    LogAvgBasketValue  = log(AvgBasketValue),
    LogTotalQuantity   = log(TotalQuantity),
    LogCoffeeSpend     = log1p(CoffeeSpend),
    LogCoffeeQuantity  = log1p(CoffeeQuantity)
  ) |>
  dplyr::select(
    CustomerID,
    Recency,
    Frequency,
    LogMonetary,
    LogAvgBasketValue,
    LogTotalQuantity,
    CategoryDiversity,
    ProductDiversity,
    CoffeeBuyer,
    LogCoffeeSpend,
    LogCoffeeQuantity,
    CoffeeShare,
    Income,
    Age,
    HouseholdSize,
    RFM_segment_ind,
    RFM_total_ind
  )

# Checked clustering dataset size and missing values.

cluster_base |>
  summarise(
    Customers = n(),
    Missing_Values = sum(is.na(across(everything())))
  )

# Selected numeric variables for clustering.

cluster_variables <- cluster_base |>
  dplyr::select(
    Recency,
    Frequency,
    LogMonetary,
    LogAvgBasketValue,
    LogTotalQuantity,
    CategoryDiversity,
    ProductDiversity,
    CoffeeBuyer,
    LogCoffeeSpend,
    LogCoffeeQuantity,
    CoffeeShare,
    Income,
    Age,
    HouseholdSize
  )

# Standardised clustering variables.

cluster_scaled <- scale(cluster_variables)

# Checked the standardised data structure before clustering.

dim(cluster_scaled)

summary(cluster_scaled)


# =============================================================================
# 10.2 Hierarchical clustering
# =============================================================================

# Applied hierarchical clustering using Ward's method.

cluster_distance <- dist(cluster_scaled, method = "euclidean")

hierarchical_model <- hclust(
  cluster_distance,
  method = "ward.D2"
)

# Plotted the dendrogram to visually inspect the hierarchical cluster structure.

plot(
  hierarchical_model,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical clustering dendrogram",
  xlab = "Customers",
  ylab = "Height"
)


# =============================================================================
# 10.3 Elbow method for K-means cluster selection
# =============================================================================

# Calculated WSS values for the elbow method.

set.seed(40495557)

k_values <- 1:10

wss_values <- map_dbl(
  k_values,
  function(k) {
    kmeans(
      cluster_scaled,
      centers = k,
      nstart = 25
    )$tot.withinss
  }
)

elbow_table <- tibble(
  K = k_values,
  Total_Within_SS = wss_values,
  WSS_Reduction = c(NA, -diff(wss_values)),
  Percent_Reduction = c(NA, -diff(wss_values) / wss_values[-length(wss_values)])
)

print(elbow_table, width = 120)

# Created an elbow plot to support the choice of the final number of clusters.

p14 <- elbow_table |>
  ggplot(aes(x = K, y = Total_Within_SS)) +
  geom_line(colour = "#2C7BB6", linewidth = 1) +
  geom_point(colour = "#D95F02", size = 2.5) +
  scale_x_continuous(breaks = k_values) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Elbow method for selecting number of clusters",
    subtitle = "Within-cluster sum of squares by number of clusters",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p14)


# =============================================================================
# 10.4 Select final number of clusters
# =============================================================================

# Selected k = 4 based on the elbow plot and WSS reduction table.

k_final <- 4

# Cut the hierarchical clustering tree into the selected number of clusters.

hierarchical_clusters <- cutree(
  hierarchical_model,
  k = k_final
)

# Added the hierarchical cluster labels to the clustering base dataset.

cluster_base <- cluster_base |>
  mutate(
    Hierarchical_Cluster = factor(hierarchical_clusters)
  )

# Checked the size of each hierarchical cluster.

cluster_base |>
  count(Hierarchical_Cluster, sort = TRUE)


# =============================================================================
# 10.5 K-means clustering with different initial start points
# =============================================================================

# Tested K-means stability across different nstart values.

set.seed(40495557)

nstart_values <- c(1, 5, 10, 25, 50, 100)

kmeans_nstart_results <- map_dfr(
  nstart_values,
  function(nstart_value) {
    
    model <- kmeans(
      cluster_scaled,
      centers = k_final,
      nstart = nstart_value
    )
    
    tibble(
      Nstart = nstart_value,
      Total_Within_SS = model$tot.withinss,
      Between_SS = model$betweenss,
      Total_SS = model$totss,
      Between_SS_Ratio = model$betweenss / model$totss
    )
  }
)

print(kmeans_nstart_results, width = 120)

# Selected nstart = 50 for the final K-means model.

set.seed(40495557)

kmeans_final <- kmeans(
  cluster_scaled,
  centers = k_final,
  nstart = 50
)

# Summarised the final K-means model fit.

kmeans_fit_summary <- tibble(
  K = k_final,
  Total_Within_SS = kmeans_final$tot.withinss,
  Between_SS = kmeans_final$betweenss,
  Total_SS = kmeans_final$totss,
  Between_SS_Ratio = kmeans_final$betweenss / kmeans_final$totss
)

print(kmeans_fit_summary, width = 120)

# =============================================================================
# 10.6 Add K-means cluster labels to customer dataset
# =============================================================================

# Added final K-means cluster labels to the customer dataset.

customer_data_clustered <- customer_data_rfm |>
  mutate(
    KMeans_Cluster = factor(kmeans_final$cluster),
    Hierarchical_Cluster = factor(hierarchical_clusters)
  )

# Checked the number of customers assigned to each K-means cluster.

customer_data_clustered |>
  count(KMeans_Cluster, sort = TRUE)

# Checked the number of customers assigned to each hierarchical cluster.

customer_data_clustered |>
  count(Hierarchical_Cluster, sort = TRUE)

# =============================================================================
# 10.7 Compare hierarchical and K-means cluster assignments
# =============================================================================

# Compared hierarchical and K-means cluster assignments.

cluster_method_crosstab <- table(
  customer_data_clustered$KMeans_Cluster,
  customer_data_clustered$Hierarchical_Cluster
)

cluster_method_crosstab

# =============================================================================
# 10.8 Cluster profiling
# =============================================================================

# Profiled clusters for marketing interpretation.

cluster_profile <- customer_data_clustered |>
  group_by(KMeans_Cluster) |>
  summarise(
    Customers = n(),
    Customer_Share = n() / nrow(customer_data_clustered),
    Avg_Recency = mean(Recency),
    Median_Recency = median(Recency),
    Avg_Frequency = mean(Frequency),
    Median_Frequency = median(Frequency),
    Avg_Monetary = mean(Monetary),
    Median_Monetary = median(Monetary),
    Total_Monetary = sum(Monetary),
    Avg_Basket_Value = mean(AvgBasketValue),
    Avg_Category_Diversity = mean(CategoryDiversity),
    Avg_Product_Diversity = mean(ProductDiversity),
    Coffee_Buyers = sum(CoffeeBuyer),
    Coffee_Buyer_Rate = mean(CoffeeBuyer),
    Avg_Coffee_Spend = mean(CoffeeSpend),
    Median_Coffee_Spend = median(CoffeeSpend),
    Avg_Coffee_Share = mean(CoffeeShare),
    Median_Age = median(Age),
    Median_Income = median(Income),
    Median_Household_Size = median(HouseholdSize),
    Avg_RFM_Total = mean(RFM_total_ind),
    .groups = "drop"
  ) |>
  arrange(desc(Avg_Monetary))

print(cluster_profile, width = Inf)

# =============================================================================
# 10.9 Cluster and RFM segment relationship
# =============================================================================

# Compared K-means clusters with independent RFM segments.

cluster_rfm_mix <- customer_data_clustered |>
  group_by(KMeans_Cluster, RFM_segment_ind) |>
  summarise(
    Customers = n(),
    .groups = "drop"
  ) |>
  group_by(KMeans_Cluster) |>
  mutate(
    Cluster_Share = Customers / sum(Customers)
  ) |>
  ungroup() |>
  arrange(KMeans_Cluster, desc(Customers))

print(cluster_rfm_mix, width = 120)

# Created a wide RFM-cluster comparison table.

cluster_rfm_mix_wide <- cluster_rfm_mix |>
  dplyr::select(KMeans_Cluster, RFM_segment_ind, Customers) |>
  pivot_wider(
    names_from = RFM_segment_ind,
    values_from = Customers,
    values_fill = 0
  )

print(cluster_rfm_mix_wide, width = 120)

# =============================================================================
# 10.10 Cluster visualisations
# =============================================================================

# Created visual summaries to support interpretation of the final cluster solution.

# -----------------------------------------------------------------------------
# Plot 15: K-means cluster sizes
# -----------------------------------------------------------------------------

# Displayed the number and share of customers in each K-means cluster.

cluster_size_plot_data <- customer_data_clustered |>
  count(KMeans_Cluster) |>
  mutate(
    Share = n / sum(n),
    Label = paste0(n, "\n(", percent(Share, accuracy = 0.1), ")")
  )

p15 <- cluster_size_plot_data |>
  ggplot(aes(x = KMeans_Cluster, y = n, fill = KMeans_Cluster)) +
  geom_col(colour = "white", linewidth = 0.2, width = 0.6) +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5, colour = "grey30") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "K-means cluster sizes",
    subtitle = paste0("Final solution: k = ", k_final),
    x = "K-means cluster",
    y = "Number of customers",
    fill = NULL,
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p15)

# -----------------------------------------------------------------------------
# Plot 16: Average monetary value by cluster
# -----------------------------------------------------------------------------

# Compared average customer spend across K-means clusters.

p16 <- cluster_profile |>
  mutate(KMeans_Cluster = fct_reorder(KMeans_Cluster, Avg_Monetary)) |>
  ggplot(aes(x = Avg_Monetary, y = KMeans_Cluster)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = label_dollar(prefix = "£", accuracy = 1)(Avg_Monetary)),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = label_dollar(prefix = "£"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Average monetary value by cluster",
    subtitle = "K-means cluster profiling",
    x = "Average customer spend (£)",
    y = "K-means cluster",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p16)

# -----------------------------------------------------------------------------
# Plot 17: Coffee buyer rate by cluster
# -----------------------------------------------------------------------------

# Compared coffee buyer concentration across K-means clusters.

p17 <- cluster_profile |>
  mutate(KMeans_Cluster = fct_reorder(KMeans_Cluster, Coffee_Buyer_Rate)) |>
  ggplot(aes(x = Coffee_Buyer_Rate, y = KMeans_Cluster)) +
  geom_col(fill = "#D95F02", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = percent(Coffee_Buyer_Rate, accuracy = 0.1)),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Coffee buyer rate by cluster",
    subtitle = "K-means cluster profiling",
    x = "Coffee buyer rate",
    y = "K-means cluster",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p17)

# -----------------------------------------------------------------------------
# Plot 18: Average recency and frequency by cluster
# -----------------------------------------------------------------------------

# Compared recency and frequency together to show how recently and how often each
# cluster purchases.

p18 <- cluster_profile |>
  ggplot(aes(x = Avg_Recency, y = Avg_Frequency, size = Customers, colour = KMeans_Cluster)) +
  geom_point(alpha = 0.8) +
  geom_text(
    aes(label = paste0("C", KMeans_Cluster)),
    vjust = -1,
    size = 3.5,
    colour = "grey20"
  ) +
  scale_size_continuous(range = c(4, 10)) +
  labs(
    title = "Recency and frequency profile by cluster",
    subtitle = "Lower recency and higher frequency indicate stronger engagement",
    x = "Average recency in days",
    y = "Average purchase frequency",
    size = "Customers",
    colour = "Cluster",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "right")

print(p18)

# -----------------------------------------------------------------------------
# Plot 19: Standardised cluster profile heatmap
# -----------------------------------------------------------------------------

# Created a heatmap of standardised cluster averages.

cluster_profile_for_heatmap <- customer_data_clustered |>
  group_by(KMeans_Cluster) |>
  summarise(
    Recency = mean(Recency),
    Frequency = mean(Frequency),
    Monetary = mean(Monetary),
    AvgBasketValue = mean(AvgBasketValue),
    CategoryDiversity = mean(CategoryDiversity),
    ProductDiversity = mean(ProductDiversity),
    CoffeeBuyerRate = mean(CoffeeBuyer),
    CoffeeSpend = mean(CoffeeSpend),
    CoffeeShare = mean(CoffeeShare),
    Income = mean(Income),
    Age = mean(Age),
    .groups = "drop"
  )

cluster_profile_heatmap <- cluster_profile_for_heatmap |>
  mutate(
    across(
      where(is.numeric),
      ~ as.numeric(scale(.))
    )
  ) |>
  pivot_longer(
    cols = -KMeans_Cluster,
    names_to = "Metric",
    values_to = "Standardised_Value"
  )

p19 <- cluster_profile_heatmap |>
  ggplot(aes(x = Metric, y = KMeans_Cluster, fill = Standardised_Value)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#D95F02",
    mid = "white",
    high = "#2C7BB6",
    midpoint = 0
  ) +
  labs(
    title = "Standardised cluster profile heatmap",
    subtitle = "Blue = above average, orange = below average",
    x = NULL,
    y = "K-means cluster",
    fill = "Standardised\nvalue",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right"
  )

print(p19)

# -----------------------------------------------------------------------------
# Plot 20: PCA visualisation of K-means clusters
# -----------------------------------------------------------------------------

# Used PCA to visualise the K-means cluster solution in two dimensions.

pca_model <- prcomp(
  cluster_scaled,
  center = FALSE,
  scale. = FALSE
)

pca_plot_data <- tibble(
  CustomerID = cluster_base$CustomerID,
  PC1 = pca_model$x[, 1],
  PC2 = pca_model$x[, 2],
  KMeans_Cluster = customer_data_clustered$KMeans_Cluster
)

p20 <- pca_plot_data |>
  ggplot(aes(x = PC1, y = PC2, colour = KMeans_Cluster)) +
  geom_point(alpha = 0.65, size = 1.8) +
  labs(
    title = "PCA visualisation of K-means clusters",
    subtitle = "Two-dimensional view of standardised clustering variables",
    x = "Principal component 1",
    y = "Principal component 2",
    colour = "Cluster",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme +
  theme(legend.position = "right")

print(p20)

# =============================================================================
# 10.11 Assign business labels to K-means clusters
# =============================================================================

# Assigned business labels to K-means clusters based on profile results.

cluster_labels <- tibble(
  KMeans_Cluster = factor(c(1, 2, 3, 4)),
  Cluster_Label = c(
    "High-value coffee loyalists",
    "Committed coffee buyers",
    "Low-value inactive customers",
    "General grocery shoppers"
  )
)

customer_data_clustered <- customer_data_clustered |>
  left_join(cluster_labels, by = "KMeans_Cluster")

cluster_profile_labelled <- cluster_profile |>
  left_join(cluster_labels, by = "KMeans_Cluster") |>
  dplyr::select(
    KMeans_Cluster,
    Cluster_Label,
    everything()
  )

print(cluster_profile_labelled, width = Inf)

# =============================================================================
# 10.12 Save clustered customer dataset for later sections
# =============================================================================

# Saved clustered customer outputs.

write_csv(
  customer_data_clustered,
  "customer_data_clustered.csv"
)

write_csv(
  cluster_profile_labelled,
  "cluster_profile_summary.csv"
)

write_csv(
  cluster_rfm_mix,
  "cluster_rfm_mix.csv"
)

# Checked the final clustered dataset.

glimpse(customer_data_clustered)

# =============================================================================
# 11. Linear Discriminant Analysis for prospect customer targeting
# =============================================================================
# Applied LDA to classify prospect customers using shared demographic variables.
# =============================================================================
# 11.1 Prepare LDA modelling data
# =============================================================================

# Prepared LDA data using variables available in both customer datasets.

lda_variables <- c(
  "Married",
  "HouseholdSize",
  "Income",
  "Age",
  "Work",
  "Education"
)

lda_data <- customer_data_clustered |>
  select(
    CustomerID,
    KMeans_Cluster,
    Cluster_Label,
    all_of(lda_variables)
  ) |>
  mutate(
    KMeans_Cluster = factor(KMeans_Cluster)
  )

# Checked the LDA dataset before modelling.

lda_data |>
  summarise(
    Customers = n(),
    Missing_Values = sum(is.na(across(everything()))),
    Clusters = n_distinct(KMeans_Cluster)
  )

lda_data |>
  count(KMeans_Cluster, Cluster_Label, sort = TRUE)

# Prepared the prospect customer dataset using the same predictor variables.

prospect_lda_data <- prospect_raw |>
  select(
    CustomerID,
    all_of(lda_variables)
  )

# Checked the prospect dataset before prediction.

prospect_lda_data |>
  summarise(
    Prospects = n(),
    Missing_Values = sum(is.na(across(everything())))
  )
# =============================================================================
# 11.2 ANOVA tests for cluster separation
# =============================================================================

# Tested cluster separation using one-way ANOVA.

anova_test_variables <- c(
  "Recency",
  "Frequency",
  "Monetary",
  "AvgBasketValue",
  "CategoryDiversity",
  "ProductDiversity",
  "CoffeeSpend",
  "CoffeeQuantity",
  "CoffeeShare",
  "Income",
  "Age",
  "HouseholdSize"
)

anova_results <- map_dfr(
  anova_test_variables,
  function(variable_name) {
    
    formula_text <- paste(variable_name, "~ KMeans_Cluster")
    anova_model <- aov(as.formula(formula_text), data = customer_data_clustered)
    anova_table <- summary(anova_model)[[1]]
    
    ss_between <- anova_table[["Sum Sq"]][1]
    ss_within  <- anova_table[["Sum Sq"]][2]
    eta_squared <- ss_between / (ss_between + ss_within)
    
    tibble(
      Variable = variable_name,
      F_Value = anova_table[["F value"]][1],
      P_Value = anova_table[["Pr(>F)"]][1],
      Eta_Squared = eta_squared
    )
  }
) |>
  mutate(
    Significant_5pct = ifelse(P_Value < 0.05, "Yes", "No"),
    Significant_1pct = ifelse(P_Value < 0.01, "Yes", "No")
  ) |>
  arrange(P_Value)

print(anova_results, width = Inf)

# Created a report-ready ANOVA table.

anova_report_table <- anova_results |>
  transmute(
    Variable = Variable,
    `F value` = round(F_Value, 2),
    `p-value` = ifelse(P_Value < 0.001, "<0.001", round(P_Value, 4)),
    `Eta squared` = round(Eta_Squared, 3),
    `Significant at 5%` = Significant_5pct
  )

anova_report_table

anova_report_table |>
  kable(
    caption = "ANOVA tests for differences across K-means clusters",
    align = "lrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 11.3 Train-test split for LDA model evaluation
# =============================================================================

# Split current customers into training and test samples.

set.seed(40495557)

train_index <- createDataPartition(
  lda_data$KMeans_Cluster,
  p = 0.70,
  list = FALSE
)

lda_train <- lda_data[train_index, ]
lda_test  <- lda_data[-train_index, ]

# Checked that the cluster distribution is preserved in the training and testing data.

lda_train |>
  count(KMeans_Cluster) |>
  mutate(Share = n / sum(n))

lda_test |>
  count(KMeans_Cluster) |>
  mutate(Share = n / sum(n))


# =============================================================================
# 11.4 Standardise LDA predictors
# =============================================================================

# Standardised LDA predictors using training-set parameters.

preprocess_lda <- preProcess(
  lda_train |> select(all_of(lda_variables)),
  method = c("center", "scale")
)

lda_train_scaled <- lda_train |>
  mutate(
    predict(
      preprocess_lda,
      lda_train |> select(all_of(lda_variables))
    )
  )

lda_test_scaled <- lda_test |>
  mutate(
    predict(
      preprocess_lda,
      lda_test |> select(all_of(lda_variables))
    )
  )

prospect_lda_scaled <- prospect_lda_data |>
  mutate(
    predict(
      preprocess_lda,
      prospect_lda_data |> select(all_of(lda_variables))
    )
  )


# =============================================================================
# 11.5 Fit LDA model
# =============================================================================

# Fitted the LDA model using K-means cluster as the outcome.

lda_formula <- as.formula(
  paste(
    "KMeans_Cluster ~",
    paste(lda_variables, collapse = " + ")
  )
)

lda_model <- lda(
  lda_formula,
  data = lda_train_scaled
)

lda_model

# =============================================================================
# 11.6 Evaluate LDA model using confusion matrix
# =============================================================================

# Evaluated LDA predictions using a confusion matrix.

lda_test_predictions <- predict(
  lda_model,
  newdata = lda_test_scaled
)

lda_test_results <- lda_test_scaled |>
  mutate(
    Predicted_Cluster = factor(
      lda_test_predictions$class,
      levels = levels(KMeans_Cluster)
    )
  )

lda_confusion_matrix <- confusionMatrix(
  data = lda_test_results$Predicted_Cluster,
  reference = lda_test_results$KMeans_Cluster
)

lda_confusion_matrix

# Extracted the key model performance metrics for reporting.

lda_model_performance <- tibble(
  Accuracy = lda_confusion_matrix$overall[["Accuracy"]],
  Kappa = lda_confusion_matrix$overall[["Kappa"]],
  No_Information_Rate = lda_confusion_matrix$overall[["AccuracyNull"]],
  Accuracy_P_Value = lda_confusion_matrix$overall[["AccuracyPValue"]]
)

lda_model_performance

# Created a report-ready confusion matrix table.

lda_confusion_table <- as.data.frame(lda_confusion_matrix$table) |>
  rename(
    Predicted_Cluster = Prediction,
    Actual_Cluster = Reference,
    Customers = Freq
  )

lda_confusion_table

lda_confusion_table |>
  kable(
    caption = "LDA confusion matrix on test data",
    align = "lll"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Created a report-ready table of class-level LDA metrics.

lda_class_metrics <- as.data.frame(lda_confusion_matrix$byClass) |>
  rownames_to_column("Cluster") |>
  as_tibble() |>
  select(
    Cluster,
    Sensitivity,
    Specificity,
    `Pos Pred Value`,
    `Neg Pred Value`,
    `Balanced Accuracy`
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

lda_class_metrics

lda_class_metrics |>
  kable(
    caption = "LDA class-level performance metrics",
    align = "lrrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 11.7 Visualise LDA prediction performance
# =============================================================================

# Visualised the LDA confusion matrix.

p21 <- lda_confusion_table |>
  ggplot(aes(x = Actual_Cluster, y = Predicted_Cluster, fill = Customers)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = Customers), colour = "grey20", size = 3.5) +
  scale_fill_gradient(
    low = "white",
    high = "#2C7BB6"
  ) +
  labs(
    title = "LDA confusion matrix",
    subtitle = "Predicted clusters compared with actual K-means clusters",
    x = "Actual cluster",
    y = "Predicted cluster",
    fill = "Customers",
    caption = "Source: Cleaned retail transaction data (10 Jan – 16 Aug 2025)"
  ) +
  plot_theme

print(p21)


# =============================================================================
# 11.8 Apply LDA model to prospect customers
# =============================================================================

# Applied the trained LDA model to prospect customers.

prospect_predictions <- predict(
  lda_model,
  newdata = prospect_lda_scaled
)

prospect_targeting <- prospect_lda_scaled |>
  mutate(
    Predicted_Cluster = factor(
      prospect_predictions$class,
      levels = levels(lda_data$KMeans_Cluster)
    ),
    Cluster_Probability_1 = prospect_predictions$posterior[, "1"],
    Cluster_Probability_2 = prospect_predictions$posterior[, "2"],
    Cluster_Probability_3 = prospect_predictions$posterior[, "3"],
    Cluster_Probability_4 = prospect_predictions$posterior[, "4"],
    Max_Cluster_Probability = apply(prospect_predictions$posterior, 1, max)
  ) |>
  left_join(
    cluster_labels |>
      rename(
        Predicted_Cluster = KMeans_Cluster,
        Predicted_Cluster_Label = Cluster_Label
      ),
    by = "Predicted_Cluster"
  )

# Added the original unscaled prospect variables back for report-ready profiling.

prospect_targeting_report <- prospect_targeting |>
  select(
    CustomerID,
    Predicted_Cluster,
    Predicted_Cluster_Label,
    Cluster_Probability_1,
    Cluster_Probability_2,
    Cluster_Probability_3,
    Cluster_Probability_4,
    Max_Cluster_Probability
  ) |>
  left_join(
    prospect_raw |>
      select(
        CustomerID,
        Married,
        HouseholdSize,
        Income,
        Age,
        ZipCode,
        Work,
        Education
      ),
    by = "CustomerID"
  )

# Checked predicted cluster sizes among prospect customers using unscaled demographics.

prospect_cluster_summary <- prospect_targeting_report |>
  group_by(Predicted_Cluster, Predicted_Cluster_Label) |>
  summarise(
    Prospects = n(),
    Prospect_Share = n() / nrow(prospect_targeting_report),
    Avg_Max_Probability = mean(Max_Cluster_Probability),
    Median_Income = median(Income),
    Median_Age = median(Age),
    Median_HouseholdSize = median(HouseholdSize),
    .groups = "drop"
  ) |>
  arrange(desc(Prospects))

print(prospect_cluster_summary, width = Inf)

# Created a report-ready prospect cluster assignment table.

prospect_cluster_report_table <- prospect_cluster_summary |>
  transmute(
    `Predicted cluster` = Predicted_Cluster,
    `Cluster label` = Predicted_Cluster_Label,
    Prospects = Prospects,
    Share = percent(Prospect_Share, accuracy = 0.1),
    `Avg prediction probability` = percent(Avg_Max_Probability, accuracy = 0.1),
    `Median income (£000s)` = Median_Income,
    `Median age` = Median_Age,
    `Median household size` = Median_HouseholdSize
  )

prospect_cluster_report_table

prospect_cluster_report_table |>
  kable(
    caption = "Predicted cluster membership for prospect customers",
    align = "llrrrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 11.9 Identify priority prospect customers
# =============================================================================

# Flagged primary and secondary prospect targeting groups.

prospect_targeting_report <- prospect_targeting_report |>
  mutate(
    Target_Priority = case_when(
      Predicted_Cluster_Label == "High-value coffee loyalists" ~ "Primary target",
      Predicted_Cluster_Label == "Committed coffee buyers" ~ "Secondary target",
      TRUE ~ "Lower priority"
    )
  )

# Summarised the prospect targeting priorities using unscaled prospect demographics.

prospect_priority_summary <- prospect_targeting_report |>
  group_by(Target_Priority) |>
  summarise(
    Prospects = n(),
    Prospect_Share = n() / nrow(prospect_targeting_report),
    Avg_Max_Probability = mean(Max_Cluster_Probability),
    Median_Income = median(Income),
    Median_Age = median(Age),
    Median_HouseholdSize = median(HouseholdSize),
    .groups = "drop"
  ) |>
  arrange(desc(Prospects))

print(prospect_priority_summary, width = Inf)

# Listed priority prospect targets by predicted cluster and posterior probability.

top_prospect_targets <- prospect_targeting_report |>
  filter(Target_Priority %in% c("Primary target", "Secondary target")) |>
  arrange(Target_Priority, desc(Max_Cluster_Probability)) |>
  select(
    CustomerID,
    Target_Priority,
    Predicted_Cluster,
    Predicted_Cluster_Label,
    Max_Cluster_Probability,
    Income,
    Age,
    HouseholdSize,
    Married,
    Work,
    Education
  )

top_prospect_targets |> head(30)

# =============================================================================
# 11.10 Visualise prospect targeting results
# =============================================================================

# Visualised the number of prospects assigned to each predicted cluster.

p22 <- prospect_cluster_summary |>
  mutate(
    Predicted_Cluster_Label = fct_reorder(Predicted_Cluster_Label, Prospects)
  ) |>
  ggplot(aes(x = Prospects, y = Predicted_Cluster_Label)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = paste0(Prospects, " (", percent(Prospect_Share, accuracy = 0.1), ")")),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Predicted prospect customers by cluster",
    subtitle = "LDA classification applied to prospect customer data",
    x = "Number of prospects",
    y = NULL,
    caption = "Source: Prospect customers info"
  ) +
  plot_theme

print(p22)

# Visualised the prospect targeting priority groups.

p23 <- prospect_priority_summary |>
  mutate(
    Target_Priority = fct_reorder(Target_Priority, Prospects)
  ) |>
  ggplot(aes(x = Prospects, y = Target_Priority)) +
  geom_col(fill = "#D95F02", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = paste0(Prospects, " (", percent(Prospect_Share, accuracy = 0.1), ")")),
    hjust = -0.1,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Prospect targeting priority groups",
    subtitle = "Primary and secondary targets are based on predicted coffee-related clusters",
    x = "Number of prospects",
    y = NULL,
    caption = "Source: Prospect customers info"
  ) +
  plot_theme

print(p23)


# =============================================================================
# 11.11 Save LDA and prospect targeting outputs
# =============================================================================

# Saved LDA and prospect targeting outputs.

write_csv(anova_results, "anova_cluster_separation_results.csv")
write_csv(anova_report_table, "anova_report_table.csv")
write_csv(lda_test_results, "lda_test_predictions.csv")
write_csv(lda_confusion_table, "lda_confusion_matrix.csv")
write_csv(lda_class_metrics, "lda_class_metrics.csv")
write_csv(prospect_targeting_report, "prospect_targeting_lda_predictions.csv")
write_csv(prospect_cluster_summary, "prospect_cluster_summary.csv")
write_csv(prospect_priority_summary, "prospect_priority_summary.csv")
write_csv(top_prospect_targets, "top_prospect_targets.csv")

# Final check of the prospect targeting dataset.

glimpse(prospect_targeting_report)


# =============================================================================
# PART 2: PRODUCT DESIGN, CONJOINT ANALYSIS, PCA AND 4P MIX
# =============================================================================

# Designed the private-label coffee product using conjoint and PCA analysis.

# =============================================================================
# 12. Load Part 2 data
# =============================================================================

# Loaded conjoint, product profile and product attribute datasets.

conjoint_raw <- fread("Conjoint survey results-1.csv")
profiles_raw <- fread("Product profiles.csv")
product_attr_raw <- fread("Product attributes information.csv")

# Checked the structure, dimensions and variable names of the Part 2 datasets.

glimpse(conjoint_raw)
glimpse(profiles_raw)
glimpse(product_attr_raw)

dim(conjoint_raw)
dim(profiles_raw)
dim(product_attr_raw)

names(conjoint_raw)
names(profiles_raw)
names(product_attr_raw)

# =============================================================================
# 13. Initial Part 2 data checks
# =============================================================================

# Checked missing values and duplicate rows before starting conjoint and PCA analysis.

conjoint_missing <- conjoint_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

profiles_missing <- profiles_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

product_attr_missing <- product_attr_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) |>
  arrange(desc(Missing_Count))

conjoint_missing
profiles_missing
product_attr_missing

sum(duplicated(conjoint_raw))
sum(duplicated(profiles_raw))
sum(duplicated(product_attr_raw))

# =============================================================================
# 14. Conjoint profile design assessment
# =============================================================================

# Assessed the 16-profile fractional factorial conjoint design.

profile_design_summary <- tibble(
  Attribute = c("Price", "Format", "Strength", "Origin", "Sustainability"),
  Levels = c(4, 4, 3, 3, 2)
)

full_factorial_profiles <- prod(profile_design_summary$Levels)
selected_profiles <- nrow(profiles_raw)

design_efficiency_summary <- tibble(
  Full_Factorial_Profiles = full_factorial_profiles,
  Selected_Profiles = selected_profiles,
  Profiles_Not_Shown = full_factorial_profiles - selected_profiles,
  Reduction_Percent = 1 - selected_profiles / full_factorial_profiles
)

profile_design_summary
design_efficiency_summary

# Checked level balance within the 16-profile conjoint design.

price_balance <- profiles_raw |>
  count(price) |>
  mutate(Attribute = "Price", Level = as.character(price)) |>
  select(Attribute, Level, n)

format_balance <- profiles_raw |>
  count(format) |>
  mutate(Attribute = "Format", Level = format) |>
  select(Attribute, Level, n)

strength_balance <- profiles_raw |>
  count(strength) |>
  mutate(Attribute = "Strength", Level = strength) |>
  select(Attribute, Level, n)

origin_balance <- profiles_raw |>
  count(origin) |>
  mutate(Attribute = "Origin", Level = origin) |>
  select(Attribute, Level, n)

sustainability_balance <- profiles_raw |>
  count(sustainability) |>
  mutate(Attribute = "Sustainability", Level = sustainability) |>
  select(Attribute, Level, n)

profile_level_balance <- bind_rows(
  price_balance,
  format_balance,
  strength_balance,
  origin_balance,
  sustainability_balance
)

profile_level_balance

# Created a design matrix to assess inter-attribute correlations.

profile_design_matrix <- profiles_raw |>
  mutate(
    price = as.numeric(price),
    format = factor(format),
    strength = factor(strength),
    origin = factor(origin),
    sustainability = factor(sustainability)
  ) |>
  select(price, format, strength, origin, sustainability)

profile_model_matrix <- model.matrix(
  ~ price + format + strength + origin + sustainability,
  data = profile_design_matrix
)[, -1]

profile_design_cor <- cor(profile_model_matrix)

round(profile_design_cor, 3)

max_abs_design_correlation <- max(abs(profile_design_cor[upper.tri(profile_design_cor)]))

design_quality_summary <- tibble(
  Profiles_Selected = selected_profiles,
  Full_Factorial_Profiles = full_factorial_profiles,
  Reduction_Percent = percent(1 - selected_profiles / full_factorial_profiles, accuracy = 0.1),
  Maximum_Absolute_Correlation = round(max_abs_design_correlation, 3)
)

design_quality_summary

# =============================================================================
# 15. Compute conjoint part-worths and willingness to pay
# =============================================================================

# Estimated average and respondent-level conjoint utilities.

# =============================================================================
# 15.1 Prepare conjoint data
# =============================================================================

# Prepared conjoint variables with reference levels and numeric price.

conjoint_data <- conjoint_raw |>
  mutate(
    respondent_id = as.character(respondent_id),
    productNo = as.integer(productNo),
    price = as.numeric(price),
    format = factor(
      format,
      levels = c("Instant", "Capsule", "Ground", "Whole bean")
    ),
    strength = factor(
      strength,
      levels = c("Mild", "Medium", "Dark")
    ),
    origin = factor(
      origin,
      levels = c("House blend", "100% Arabica blend", "Single-origin")
    ),
    sustainability = factor(
      sustainability,
      levels = c("No", "Yes")
    ),
    rating = as.numeric(rating)
  )

# Checked the prepared conjoint dataset.

conjoint_data |>
  summarise(
    Respondents = n_distinct(respondent_id),
    Profiles = n_distinct(productNo),
    Rows = n(),
    Min_Rating = min(rating),
    Max_Rating = max(rating),
    Mean_Rating = mean(rating),
    Missing_Values = sum(is.na(across(everything())))
  )

# Checked average rating by product profile.

profile_rating_summary <- conjoint_data |>
  group_by(productNo, price, format, strength, origin, sustainability) |>
  summarise(
    Avg_Rating = mean(rating),
    Median_Rating = median(rating),
    SD_Rating = sd(rating),
    Responses = n(),
    .groups = "drop"
  ) |>
  arrange(desc(Avg_Rating))

print(profile_rating_summary, width = Inf)


# =============================================================================
# 15.2 Estimate average conjoint model
# =============================================================================

# Fitted pooled conjoint regression to estimate average preferences.

average_conjoint_model <- lm(
  rating ~ price + format + strength + origin + sustainability,
  data = conjoint_data
)

summary(average_conjoint_model)

# Extracted average coefficients from the conjoint model.

average_conjoint_coefficients <- broom::tidy(average_conjoint_model) |>
  mutate(
    term = as.character(term)
  )

average_conjoint_coefficients


# =============================================================================
# 15.3 Convert average conjoint coefficients into part-worth utilities
# =============================================================================

# Converted regression coefficients into zero-centred part-worth utilities.

average_partworths_raw <- tibble(
  Attribute = c(
    "Price",
    "Format", "Format", "Format", "Format",
    "Strength", "Strength", "Strength",
    "Origin", "Origin", "Origin",
    "Sustainability", "Sustainability"
  ),
  Level = c(
    "Price",
    "Instant", "Capsule", "Ground", "Whole bean",
    "Mild", "Medium", "Dark",
    "House blend", "100% Arabica blend", "Single-origin",
    "No", "Yes"
  ),
  Utility = c(
    coef(average_conjoint_model)[["price"]],
    0,
    coef(average_conjoint_model)[["formatCapsule"]],
    coef(average_conjoint_model)[["formatGround"]],
    coef(average_conjoint_model)[["formatWhole bean"]],
    0,
    coef(average_conjoint_model)[["strengthMedium"]],
    coef(average_conjoint_model)[["strengthDark"]],
    0,
    coef(average_conjoint_model)[["origin100% Arabica blend"]],
    coef(average_conjoint_model)[["originSingle-origin"]],
    0,
    coef(average_conjoint_model)[["sustainabilityYes"]]
  )
)

# Zero-centred non-price utilities and retained price as a numeric slope.

average_partworths <- average_partworths_raw |>
  group_by(Attribute) |>
  mutate(
    Part_Worth = ifelse(
      Attribute == "Price",
      Utility,
      Utility - mean(Utility)
    )
  ) |>
  ungroup()

print(average_partworths, width = Inf)


# =============================================================================
# 15.4 Estimate respondent-level part-worths
# =============================================================================

# Estimated respondent-level conjoint models.

estimate_respondent_partworths <- function(df, respondent_id_value) {
  
  model <- lm(
    rating ~ price + format + strength + origin + sustainability,
    data = df
  )
  
  coefs <- coef(model)
  
  # Extracted available coefficients safely.
  get_coef <- function(term_name) {
    if (term_name %in% names(coefs)) {
      return(coefs[[term_name]])
    } else {
      return(0)
    }
  }
  
  partworths_raw <- tibble(
    respondent_id = respondent_id_value,
    Attribute = c(
      "Price",
      "Format", "Format", "Format", "Format",
      "Strength", "Strength", "Strength",
      "Origin", "Origin", "Origin",
      "Sustainability", "Sustainability"
    ),
    Level = c(
      "Price",
      "Instant", "Capsule", "Ground", "Whole bean",
      "Mild", "Medium", "Dark",
      "House blend", "100% Arabica blend", "Single-origin",
      "No", "Yes"
    ),
    Utility = c(
      get_coef("price"),
      0,
      get_coef("formatCapsule"),
      get_coef("formatGround"),
      get_coef("formatWhole bean"),
      0,
      get_coef("strengthMedium"),
      get_coef("strengthDark"),
      0,
      get_coef("origin100% Arabica blend"),
      get_coef("originSingle-origin"),
      0,
      get_coef("sustainabilityYes")
    )
  )
  
  partworths <- partworths_raw |>
    group_by(Attribute) |>
    mutate(
      Part_Worth = ifelse(
        Attribute == "Price",
        Utility,
        Utility - mean(Utility)
      )
    ) |>
    ungroup()
  
  return(partworths)
}

# Estimated respondent-level part-worths from nested respondent data.

respondent_models <- conjoint_data |>
  group_by(respondent_id) |>
  nest() |>
  ungroup() |>
  mutate(
    respondent_partworths = map2(
      data,
      respondent_id,
      estimate_respondent_partworths
    )
  )

# Unnested the respondent-level part-worths into one long table.

respondent_partworths <- respondent_models |>
  select(respondent_partworths) |>
  unnest(respondent_partworths) |>
  mutate(
    respondent_id = as.character(respondent_id)
  )

# Checked respondent-level part-worth output.

respondent_partworths |>
  summarise(
    Respondents = n_distinct(respondent_id),
    Rows = n(),
    Attributes = n_distinct(Attribute),
    Levels = n_distinct(Level)
  )

respondent_partworths |> head(30)

# =============================================================================
# 15.5 Summarise respondent-level part-worths
# =============================================================================

# Summarised the distribution of individual-level part-worths across respondents.

respondent_partworth_summary <- respondent_partworths |>
  group_by(Attribute, Level) |>
  summarise(
    Mean_Part_Worth = mean(Part_Worth),
    Median_Part_Worth = median(Part_Worth),
    SD_Part_Worth = sd(Part_Worth),
    Min_Part_Worth = min(Part_Worth),
    Max_Part_Worth = max(Part_Worth),
    .groups = "drop"
  ) |>
  arrange(Attribute, desc(Mean_Part_Worth))

print(respondent_partworth_summary, width = Inf)

# Created a report-ready part-worth table.

partworth_report_table <- respondent_partworth_summary |>
  transmute(
    Attribute,
    Level,
    `Mean part-worth` = round(Mean_Part_Worth, 3),
    `Median part-worth` = round(Median_Part_Worth, 3),
    `SD` = round(SD_Part_Worth, 3)
  )

partworth_report_table

partworth_report_table |>
  kable(
    caption = "Respondent-level conjoint part-worth summary",
    align = "llrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )


# =============================================================================
# 15.6 Calculate willingness to pay
# =============================================================================

# Converted non-price utilities into WTP using the price coefficient.

average_price_coefficient <- coef(average_conjoint_model)[["price"]]

average_wtp <- average_partworths |>
  filter(Attribute != "Price") |>
  mutate(
    WTP = Part_Worth / abs(average_price_coefficient)
  ) |>
  arrange(Attribute, desc(WTP))

print(average_wtp, width = Inf)

# Calculated respondent-level WTP using each respondent's own price coefficient.

respondent_price_coefficients <- respondent_partworths |>
  filter(Attribute == "Price") |>
  select(
    respondent_id,
    Price_Coefficient = Part_Worth
  )

respondent_wtp <- respondent_partworths |>
  filter(Attribute != "Price") |>
  left_join(
    respondent_price_coefficients,
    by = "respondent_id"
  ) |>
  mutate(
    WTP = Part_Worth / abs(Price_Coefficient)
  )

# Trimmed extreme WTP values caused by near-zero price coefficients.

wtp_lower <- quantile(respondent_wtp$WTP, 0.01, na.rm = TRUE)
wtp_upper <- quantile(respondent_wtp$WTP, 0.99, na.rm = TRUE)

respondent_wtp_clean <- respondent_wtp |>
  filter(
    WTP >= wtp_lower,
    WTP <= wtp_upper
  )

respondent_wtp_summary <- respondent_wtp_clean |>
  group_by(Attribute, Level) |>
  summarise(
    Mean_WTP = mean(WTP, na.rm = TRUE),
    Median_WTP = median(WTP, na.rm = TRUE),
    SD_WTP = sd(WTP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(Attribute, desc(Mean_WTP))

print(respondent_wtp_summary, width = Inf)

# Created a report-ready WTP table.

wtp_report_table <- respondent_wtp_summary |>
  transmute(
    Attribute,
    Level,
    `Mean WTP (£)` = label_dollar(prefix = "£", accuracy = 0.01)(Mean_WTP),
    `Median WTP (£)` = label_dollar(prefix = "£", accuracy = 0.01)(Median_WTP),
    `SD WTP` = round(SD_WTP, 3)
  )

wtp_report_table

wtp_report_table |>
  kable(
    caption = "Respondent-level willingness to pay summary",
    align = "llrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )


# =============================================================================
# 15.7 Visualise average part-worths
# =============================================================================

# Visualised average part-worth utilities by attribute level.

partworth_plot_data <- average_partworths |>
  filter(Attribute != "Price") |>
  mutate(
    Attribute = factor(
      Attribute,
      levels = c("Format", "Strength", "Origin", "Sustainability")
    ),
    Level = fct_reorder(Level, Part_Worth)
  )

p24 <- partworth_plot_data |>
  ggplot(aes(x = Part_Worth, y = Level, fill = Attribute)) +
  geom_col(colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(
    title = "Average conjoint part-worth utilities",
    subtitle = "Positive values indicate attribute levels that increase purchase likelihood",
    x = "Part-worth utility",
    y = NULL,
    fill = NULL,
    caption = "Source: Conjoint survey results"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p24)


# =============================================================================
# 15.8 Visualise willingness to pay
# =============================================================================

# Visualised willingness to pay for each non-price attribute level.

wtp_plot_data <- average_wtp |>
  mutate(
    Attribute = factor(
      Attribute,
      levels = c("Format", "Strength", "Origin", "Sustainability")
    ),
    Level = fct_reorder(Level, WTP)
  )

p25 <- wtp_plot_data |>
  ggplot(aes(x = WTP, y = Level, fill = Attribute)) +
  geom_col(colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(
    aes(label = label_dollar(prefix = "£", accuracy = 0.01)(WTP)),
    hjust = ifelse(wtp_plot_data$WTP >= 0, -0.1, 1.1),
    size = 3,
    colour = "grey30"
  ) +
  facet_wrap(~ Attribute, scales = "free_y") +
  scale_x_continuous(
    labels = label_dollar(prefix = "£"),
    expand = expansion(mult = c(0.15, 0.2))
  ) +
  labs(
    title = "Average willingness to pay by attribute level",
    subtitle = "WTP converts part-worth utility into an equivalent £ value",
    x = "Willingness to pay (£)",
    y = NULL,
    fill = NULL,
    caption = "Source: Conjoint survey results"
  ) +
  plot_theme +
  theme(legend.position = "none")

print(p25)


# =============================================================================
# 15.9 Save part-worth and WTP outputs
# =============================================================================

# Saved conjoint part-worth and WTP outputs for reporting and later product design.

write_csv(average_partworths, "average_conjoint_partworths.csv")
write_csv(respondent_partworths, "respondent_conjoint_partworths.csv")
write_csv(respondent_partworth_summary, "respondent_partworth_summary.csv")
write_csv(average_wtp, "average_wtp.csv")
write_csv(respondent_wtp_clean, "respondent_level_wtp_clean.csv")
write_csv(respondent_wtp_summary, "respondent_wtp_summary.csv")
write_csv(partworth_report_table, "partworth_report_table.csv")
write_csv(wtp_report_table, "wtp_report_table.csv")

# =============================================================================
# 16. Compute attribute importance
# =============================================================================

# Calculated attribute importance from part-worth utility ranges.

attribute_importance <- average_partworths |>
  filter(Attribute != "Price") |>
  group_by(Attribute) |>
  summarise(
    Max_Part_Worth = max(Part_Worth),
    Min_Part_Worth = min(Part_Worth),
    Utility_Range = Max_Part_Worth - Min_Part_Worth,
    .groups = "drop"
  )

# Calculated price importance from the utility change across the tested price range.

price_importance <- tibble(
  Attribute = "Price",
  Max_Part_Worth = average_price_coefficient * min(conjoint_data$price),
  Min_Part_Worth = average_price_coefficient * max(conjoint_data$price),
  Utility_Range = abs(average_price_coefficient) *
    (max(conjoint_data$price) - min(conjoint_data$price))
)

attribute_importance <- bind_rows(
  price_importance,
  attribute_importance
) |>
  mutate(
    Importance = Utility_Range / sum(Utility_Range),
    Importance_Percent = percent(Importance, accuracy = 0.1)
  ) |>
  arrange(desc(Importance))

print(attribute_importance, width = Inf)

# Created a report-ready attribute importance table.

attribute_importance_report_table <- attribute_importance |>
  transmute(
    Attribute,
    `Utility range` = round(Utility_Range, 3),
    `Importance (%)` = Importance_Percent
  )

attribute_importance_report_table

attribute_importance_report_table |>
  kable(
    caption = "Conjoint attribute importance",
    align = "lrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 16.1 Visualise attribute importance
# =============================================================================

# Visualised conjoint attribute importance.

p26 <- attribute_importance |>
  mutate(
    Attribute = fct_reorder(Attribute, Importance)
  ) |>
  ggplot(aes(x = Importance, y = Attribute)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = Importance_Percent),
    hjust = -0.1,
    size = 3.5,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Conjoint attribute importance",
    subtitle = "Importance is based on the part-worth utility range within each attribute",
    x = "Relative importance",
    y = NULL,
    caption = "Source: Conjoint survey results"
  ) +
  plot_theme

print(p26)

# =============================================================================
# 16.2 Save conjoint importance outputs
# =============================================================================

write_csv(attribute_importance, "conjoint_attribute_importance.csv")
write_csv(attribute_importance_report_table, "conjoint_attribute_importance_report_table.csv")

# =============================================================================
# 17. Predict market share using conjoint utilities
# =============================================================================
# Predicted market share using the average conjoint utility model.
# =============================================================================
# 17.1 Define proposed product and competing products
# =============================================================================

# Defined the proposed product and three competitor profiles.

market_share_profiles <- tibble(
  Product = c(
    "Proposed PB coffee",
    "Competitor A",
    "Competitor B",
    "Competitor C"
  ),
  Description = c(
    "Capsule, £5, Dark roast, 100% Arabica blend, sustainability",
    "Capsule, £4, Medium roast, House blend, no sustainability",
    "Capsule, £6, Dark roast, 100% Arabica blend, no sustainability",
    "Capsule, £5, Mild roast, Single-origin, sustainability"
  ),
  price = c(5, 4, 6, 5),
  format = c("Capsule", "Capsule", "Capsule", "Capsule"),
  strength = c("Dark", "Medium", "Dark", "Mild"),
  origin = c("100% Arabica blend", "House blend", "100% Arabica blend", "Single-origin"),
  sustainability = c("Yes", "No", "No", "Yes")
) |>
  mutate(
    price = as.numeric(price),
    format = factor(
      format,
      levels = levels(conjoint_data$format)
    ),
    strength = factor(
      strength,
      levels = levels(conjoint_data$strength)
    ),
    origin = factor(
      origin,
      levels = levels(conjoint_data$origin)
    ),
    sustainability = factor(
      sustainability,
      levels = levels(conjoint_data$sustainability)
    )
  )

market_share_profiles

# =============================================================================
# 17.2 Predict utility for each product
# =============================================================================

# Predicted utility for each product profile.

market_share_utility <- market_share_profiles |>
  mutate(
    Predicted_Utility = as.numeric(
      predict(
        average_conjoint_model,
        newdata = market_share_profiles
      )
    )
  ) |>
  arrange(desc(Predicted_Utility))

print(market_share_utility, width = Inf)

# =============================================================================
# 17.3 Calculate market share using multinomial logit rule
# =============================================================================

# Converted predicted utilities into relative choice probabilities using a logit rule.

market_share_prediction <- market_share_utility |>
  mutate(
    Exp_Utility = exp(Predicted_Utility),
    Predicted_Market_Share = Exp_Utility / sum(Exp_Utility),
    Predicted_Market_Share_Percent = percent(
      Predicted_Market_Share,
      accuracy = 0.1
    )
  ) |>
  arrange(desc(Predicted_Market_Share))

print(market_share_prediction, width = Inf)

# =============================================================================
# 17.4 Create report-ready market share table
# =============================================================================

# Created a concise table for the written report.

market_share_report_table <- market_share_prediction |>
  transmute(
    Product,
    Description,
    `Predicted utility` = round(Predicted_Utility, 3),
    `Predicted market share` = Predicted_Market_Share_Percent
  )

market_share_report_table

market_share_report_table |>
  kable(
    caption = "Predicted market share for proposed PB coffee product",
    align = "llrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 17.5 Extract proposed product result
# =============================================================================

# Extracted the proposed product's predicted market share for direct reporting.

proposed_product_market_share <- market_share_prediction |>
  filter(Product == "Proposed PB coffee") |>
  select(
    Product,
    Predicted_Utility,
    Predicted_Market_Share,
    Predicted_Market_Share_Percent
  )

print(proposed_product_market_share, width = Inf)

# =============================================================================
# 17.6 Utility gap and consumer surplus interpretation
# =============================================================================

# Calculated the utility gap between the proposed product and closest competitor.

proposed_utility <- market_share_prediction |>
  filter(Product == "Proposed PB coffee") |>
  pull(Predicted_Utility)

market_share_gap_analysis <- market_share_prediction |>
  arrange(desc(Predicted_Utility)) |>
  mutate(
    Utility_Gap_vs_Proposed = proposed_utility - Predicted_Utility,
    WTP_Equivalent_Gap = Utility_Gap_vs_Proposed / abs(average_price_coefficient)
  )

print(market_share_gap_analysis, width = Inf)

# Extracted the nearest competitor to the proposed product.

closest_competitor_gap <- market_share_gap_analysis |>
  filter(Product != "Proposed PB coffee") |>
  arrange(desc(Predicted_Utility)) |>
  slice(1) |>
  select(
    Product,
    Predicted_Utility,
    Utility_Gap_vs_Proposed,
    WTP_Equivalent_Gap
  )

print(closest_competitor_gap, width = Inf)

# Created a report-ready table for the utility gap interpretation.

closest_competitor_gap_report_table <- closest_competitor_gap |>
  transmute(
    `Closest competitor` = Product,
    `Competitor utility` = round(Predicted_Utility, 3),
    `Utility advantage of proposed product` = round(Utility_Gap_vs_Proposed, 3),
    `WTP-equivalent advantage (£)` =
      label_dollar(prefix = "£", accuracy = 0.01)(WTP_Equivalent_Gap)
  )

closest_competitor_gap_report_table

closest_competitor_gap_report_table |>
  kable(
    caption = "Utility advantage of proposed PB coffee product over closest competitor",
    align = "lrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Saved the utility gap output for reporting.

write_csv(market_share_gap_analysis, "conjoint_market_share_gap_analysis.csv")
write_csv(closest_competitor_gap_report_table, "closest_competitor_gap_report_table.csv")

# =============================================================================
# 17.7 Visualise predicted market share
# =============================================================================

# Visualised the predicted market share across the proposed product and competitors.

p27 <- market_share_prediction |>
  mutate(
    Product = fct_reorder(Product, Predicted_Market_Share)
  ) |>
  ggplot(aes(x = Predicted_Market_Share, y = Product)) +
  geom_col(fill = "#2C7BB6", colour = "white", linewidth = 0.2) +
  geom_text(
    aes(label = Predicted_Market_Share_Percent),
    hjust = -0.1,
    size = 3.5,
    colour = "grey30"
  ) +
  scale_x_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Predicted market share from conjoint utilities",
    subtitle = "Proposed PB coffee compared with three competing alternatives",
    x = "Predicted market share",
    y = NULL,
    caption = "Source: Conjoint survey results"
  ) +
  plot_theme

print(p27)

# =============================================================================
# 17.8 Save market share outputs
# =============================================================================

# Saved the market share outputs for the report and appendix.

write_csv(market_share_prediction, "conjoint_market_share_prediction.csv")
write_csv(market_share_report_table, "conjoint_market_share_report_table.csv")
write_csv(proposed_product_market_share, "proposed_product_market_share.csv")

# =============================================================================
# 18. PCA: Product positioning
# =============================================================================

# Prepared product attribute data for PCA.

pca_data <- product_attr_raw |>
  rename(sustainability_claim = sustaintability_claim)

pca_product_names <- pca_data$product_name

# Selected numeric product attributes for PCA.
pca_numeric <- pca_data |>
  select(
    num_servings,
    price,
    price_100g,
    strength_level,
    is_decaf,
    sustainability_claim,
    convenience,
    authenticity,
    premium,
    perceived_sustainability,
    taste_quality
  )

dim(pca_numeric)

pca_missing_check <- pca_numeric |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing"
  )

pca_missing_check

# =============================================================================
# 18.1 Run PCA and compute PVEs
# =============================================================================

# Ran PCA on standardised product attributes.
pca_product_model <- prcomp(
  pca_numeric,
  center = TRUE,
  scale. = TRUE
)

pca_product_summary <- summary(pca_product_model)

pc_standard_deviation <- pca_product_model$sdev
singular_values <- pc_standard_deviation * sqrt(nrow(pca_numeric) - 1)

pve_table <- tibble(
  PC = paste0("PC", seq_along(pc_standard_deviation)),
  Singular_Value = singular_values,
  PC_Standard_Deviation = pc_standard_deviation,
  Eigenvalue = pc_standard_deviation^2,
  PVE = pca_product_summary$importance["Proportion of Variance", ],
  Cumulative_PVE = pca_product_summary$importance["Cumulative Proportion", ]
)

print(pve_table, width = Inf)

pve_report_table <- pve_table |>
  transmute(
    `Principal component` = PC,
    `Singular value` = round(Singular_Value, 3),
    `Eigenvalue` = round(Eigenvalue, 3),
    `PVE (%)` = percent(PVE, accuracy = 0.1),
    `Cumulative PVE (%)` = percent(Cumulative_PVE, accuracy = 0.1)
  )

pve_report_table

pve_report_table |>
  kable(
    caption = "PCA singular values and proportion of variance explained",
    align = "lrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 18.2 Loading factors
# =============================================================================

pca_loadings <- as_tibble(
  pca_product_model$rotation,
  rownames = "Variable"
)

loading_report_table <- pca_loadings |>
  transmute(
    Variable,
    PC1 = round(PC1, 3),
    PC2 = round(PC2, 3),
    PC3 = round(PC3, 3),
    PC4 = round(PC4, 3)
  )

print(loading_report_table, width = Inf)

loading_report_table |>
  kable(
    caption = "PCA loading factors for the first four principal components",
    align = "lrrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 18.3 Identify most important PCA attributes
# =============================================================================

variable_importance <- pca_loadings |>
  mutate(
    PC1_Contribution = PC1^2,
    PC2_Contribution = PC2^2,
    PC1_PC2_Contribution = PC1_Contribution + PC2_Contribution
  ) |>
  select(
    Variable,
    PC1,
    PC2,
    PC1_Contribution,
    PC2_Contribution,
    PC1_PC2_Contribution
  ) |>
  arrange(desc(PC1_PC2_Contribution))

print(variable_importance, width = Inf)

variable_importance_report <- variable_importance |>
  transmute(
    Variable,
    `PC1 loading` = round(PC1, 3),
    `PC2 loading` = round(PC2, 3),
    `PC1² + PC2²` = round(PC1_PC2_Contribution, 3)
  )

variable_importance_report |>
  kable(
    caption = "Variable importance based on contribution to PC1 and PC2",
    align = "lrrr"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 18.4 PCA biplot
# =============================================================================

# Created PCA scores and loading vectors for the biplot.
pca_scores <- as_tibble(pca_product_model$x) |>
  mutate(product_name = pca_product_names)

loading_scale <- 3

pca_loadings_plot <- pca_loadings |>
  mutate(
    PC1_scaled = PC1 * loading_scale,
    PC2_scaled = PC2 * loading_scale
  )

p28 <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.4
  ) +
  geom_point(
    data = pca_scores,
    aes(x = PC1, y = PC2),
    colour = "#2C7BB6",
    size = 2.6,
    alpha = 0.85
  ) +
  geom_text_repel(
    data = pca_scores,
    aes(x = PC1, y = PC2, label = product_name),
    size = 2.7,
    colour = "#2C7BB6",
    max.overlaps = 40,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.2
  ) +
  geom_segment(
    data = pca_loadings_plot,
    aes(
      x = 0,
      y = 0,
      xend = PC1_scaled,
      yend = PC2_scaled
    ),
    arrow = arrow(length = grid::unit(0.18, "cm"), type = "closed"),
    colour = "#D95F02",
    linewidth = 0.65
  ) +
  geom_text_repel(
    data = pca_loadings_plot,
    aes(
      x = PC1_scaled,
      y = PC2_scaled,
      label = Variable
    ),
    size = 3,
    colour = "#D95F02",
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.2
  ) +
  labs(
    title = "Coffee product positioning map",
    subtitle = paste0(
      "Products closer together are positioned more similarly; PC1 and PC2 explain ",
      percent(pve_table$Cumulative_PVE[2], accuracy = 0.1),
      " of total variation"
    ),
    x = paste0(
      "Ethical & traditional  \u2190\u2192  Convenient & easy-use (",
      percent(pve_table$PVE[1], accuracy = 0.1),
      ")"
    ),
    y = paste0(
      "Smaller/value pack  \u2190\u2192  Premium & larger pack (",
      percent(pve_table$PVE[2], accuracy = 0.1),
      ")"
    ),
    caption = "Source: Product attributes information. Blue = coffee products; orange = positioning drivers."
  ) +
  plot_theme +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "grey35"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, colour = "grey50", hjust = 0)
  )

print(p28)

# =============================================================================
# 18.5 Managerial perceptual map
# =============================================================================

# Created a brand-coloured perceptual map for managerial interpretation.

pca_scores <- as_tibble(pca_product_model$x) |>
  mutate(
    product_name = pca_product_names,
    Brand = str_extract(product_name, "^[A-Za-z]+")
  )

p29 <- pca_scores |>
  ggplot(aes(x = PC1, y = PC2, colour = Brand, label = product_name)) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.4
  ) +
  
  geom_point(
    size = 3,
    alpha = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    show.legend = FALSE,
    max.overlaps = 40,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.2
  ) +
  
  labs(
    title = "Managerial perceptual map of coffee products",
    subtitle = paste0(
      "Products closer together are positioned more similarly; PC1 and PC2 explain ",
      percent(pve_table$Cumulative_PVE[2], accuracy = 0.1),
      " of total variation"
    ),
    x = paste0(
      "Ethical & traditional  \u2190\u2192  Convenient & easy-use (",
      percent(pve_table$PVE[1], accuracy = 0.1),
      ")"
    ),
    y = paste0(
      "Smaller/value pack  \u2190\u2192  Premium & larger pack (",
      percent(pve_table$PVE[2], accuracy = 0.1),
      ")"
    ),
    colour = "Brand",
    caption = "Source: Product attributes information. Product positions are based on the first two principal components."
  ) +
  
  plot_theme +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "grey35"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, colour = "grey50", hjust = 0)
  )

print(p29)

# =============================================================================
# 18.6 Proposed PB coffee product and 4P strategy
# =============================================================================

# Defined the proposed private-label coffee product.

proposed_pb_product <- tibble(
  product_name = "Proposed PB product",
  product_description = "Private-label sustainable dark roast capsule coffee",
  price = 5,
  format = "Capsule",
  strength = "Dark roast",
  origin = "100% Arabica blend",
  sustainability = "Yes"
)

proposed_pb_product


# Created a concise 4P recommendation table.

pb_4p_strategy <- tibble(
  `4P element` = c("Product", "Price", "Place", "Promotion"),
  `Recommendation` = c(
    "Launch a private-label capsule coffee positioned as a convenient, sustainable, dark roast coffee with a 100% Arabica blend.",
    "Set the price at £5 to balance affordability with a premium-value signal, consistent with the conjoint-tested product profile.",
    "Distribute through the grocery retailer’s stores and online grocery platform, with stronger visibility in coffee, premium own-label, and sustainable product sections.",
    "Promote the product using sustainability, convenience, and quality-based messaging rather than only price discounts."
  ),
  `Analytical justification` = c(
    "The conjoint design tested Format, Price, Strength, Origin, and Sustainability as the main product-value drivers. The selected profile combines capsule convenience, dark roast strength, Arabica quality, and sustainability.",
    "The £5 price point supports a mid-premium PB position: cheaper than many premium branded competitors but more differentiated than value-tier own-label products.",
    "The target customer should be reached where grocery coffee purchases are already made, while online placement supports convenience-led shoppers.",
    "The PCA map shows a white-space opportunity between ethical/traditional products and highly convenience-led branded products. Promotion should reinforce this differentiated middle-ground."
  )
)

pb_4p_strategy


pb_4p_strategy |>
  kable(
    caption = "Proposed PB coffee product: 4P marketing mix strategy",
    align = "lll"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# =============================================================================
# 18.7 Position proposed PB product on the perceptual map
# =============================================================================

# Estimated price per 100g using a £5 pack and approximately 74g of capsule coffee.

proposed_pb_pca_attributes <- tibble(
  product_name = "Private Label brand",
  num_servings = 10,
  price = 5,
  price_100g = 6.75,
  strength_level = 7,
  is_decaf = 0,
  sustainability_claim = 1,
  convenience = 6.9,
  authenticity = 3.8,
  premium = 5.3,
  perceived_sustainability = 5.5,
  taste_quality = 6.0
)

proposed_pb_numeric <- proposed_pb_pca_attributes |>
  dplyr::select(all_of(names(pca_numeric)))

# Projected the proposed private-label product into the PCA space.
proposed_pb_scores <- as_tibble(
  predict(pca_product_model, newdata = proposed_pb_numeric)
) |>
  mutate(
    product_name = proposed_pb_pca_attributes$product_name,
    Brand = "Private Label"
  )

print(proposed_pb_scores, width = Inf)


pca_scores_brand <- as_tibble(pca_product_model$x) |>
  mutate(
    product_name = pca_product_names,
    Brand = case_when(
      str_detect(product_name, regex("Costa", ignore_case = TRUE)) ~ "Costa",
      str_detect(product_name, regex("illy", ignore_case = TRUE)) ~ "illy",
      str_detect(product_name, regex("KENCO", ignore_case = TRUE)) ~ "KENCO",
      str_detect(product_name, regex("Lavazza", ignore_case = TRUE)) ~ "Lavazza",
      str_detect(product_name, regex("Nescafe", ignore_case = TRUE)) ~ "Nescafe",
      str_detect(product_name, regex("Starbucks", ignore_case = TRUE)) ~ "Starbucks",
      str_detect(product_name, regex("Taylors", ignore_case = TRUE)) ~ "Taylors",
      str_detect(product_name, regex("TescoFinest", ignore_case = TRUE)) ~ "TescoFinest",
      TRUE ~ "Other"
    )
  )


p30 <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey75",
    linewidth = 0.35
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey75",
    linewidth = 0.35
  ) +
  
  # Target positioning zone
  annotate(
    "rect",
    xmin = 0.25,
    xmax = 1.25,
    ymin = -0.35,
    ymax = 1.10,
    fill = "#F2F2F2",
    alpha = 0.45,
    colour = "grey65",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  annotate(
    "text",
    x = 0.75,
    y = 1.20,
    label = "Target positioning zone",
    size = 3.4,
    fontface = "bold",
    colour = "grey35"
  ) +
  
  # Existing competitor products
  geom_point(
    data = pca_scores_brand,
    aes(x = PC1, y = PC2, colour = Brand),
    size = 2.7,
    alpha = 0.78
  ) +
  geom_text_repel(
    data = pca_scores_brand,
    aes(x = PC1, y = PC2, label = product_name, colour = Brand),
    size = 2.7,
    max.overlaps = 50,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.15,
    segment.alpha = 0.35,
    show.legend = FALSE
  ) +
  
  # Proposed private-label product
  geom_point(
    data = proposed_pb_scores,
    aes(x = PC1, y = PC2),
    shape = 21,
    size = 6.2,
    fill = "#D7191C",
    colour = "white",
    stroke = 1.4
  ) +
  geom_point(
    data = proposed_pb_scores,
    aes(x = PC1, y = PC2),
    shape = 21,
    size = 6.2,
    fill = NA,
    colour = "grey20",
    stroke = 0.45
  ) +
  geom_label_repel(
    data = proposed_pb_scores,
    aes(x = PC1, y = PC2, label = product_name),
    size = 3.4,
    fontface = "bold",
    fill = "white",
    colour = "grey15",
    label.size = 0.2,
    box.padding = 0.5,
    point.padding = 0.55,
    nudge_y = 0.35,
    nudge_x = 0.20,
    segment.size = 0.25,
    segment.alpha = 0.5,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Perceptual map with proposed private-label coffee product",
    subtitle = paste0(
      "Positioned in an accessible convenience gap; PC1 and PC2 explain ",
      percent(pve_table$PVE[1] + pve_table$PVE[2], accuracy = 0.1),
      " of total variation"
    ),
    x = paste0(
      "Ethical & traditional  \u2190\u2192  Convenient & easy-use (",
      percent(pve_table$PVE[1], accuracy = 0.1),
      ")"
    ),
    y = paste0(
      "Smaller/value pack  \u2190\u2192  Premium & larger pack (",
      percent(pve_table$PVE[2], accuracy = 0.1),
      ")"
    ),
    colour = "Brand",
    caption = "Source: Product attributes information. Red circle = proposed private-label product."
  ) +
  plot_theme +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "grey35"),
    axis.title.x = element_text(size = 10.5, face = "bold"),
    axis.title.y = element_text(size = 10.5, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

print(p30)