# ==============================================================================
# Homework 2 - Complete Base R Solution (Zero External Packages Required)
# Due: October 11th
# ==============================================================================

# ==============================================================================
# Problem 1: Clean, validate, and summarize (25 points)
# ==============================================================================

# Setup Code
sales_raw <- data.frame(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN", "Notebook", "pen", "FOLDER", "Folder ", " ERASER", "pen"),
  amount_raw = c("$10.00", "$20.00", "$25.00", NA, "$15.00", NA, "$20.00", "$30.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104", "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift; urgent", "online", "online; member", "gift", "gift", "member", "member", "gift; member"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Part A: Text and numerical transformations (8 points)
# ------------------------------------------------------------------------------

# Standard text trim and whitespace cleaning using regex
clean_item <- trimws(sales_raw$item_raw)
clean_item <- gsub("\\s+", " ", clean_item)
sales_raw$item_clean <- tolower(clean_item)

# Financial parsing equivalent to parse_number()
sales_raw$amount <- as.numeric(gsub("[\\$,]", "", sales_raw$amount_raw))
sales_raw$discount_rate <- as.numeric(gsub("%", "", sales_raw$discount_raw)) / 100
sales_raw$net_amount <- sales_raw$amount * (1 - sales_raw$discount_rate)

# Categorization tracking layout
sales_raw$size <- ifelse(is.na(sales_raw$net_amount), "unrecorded",
                         ifelse(sales_raw$net_amount >= 20, "large", "small"))

# Print output verification block
print(sales_raw[, c("sale_id", "item_clean", "amount", "discount_rate", "net_amount", "size")])


# ------------------------------------------------------------------------------
# Part B: Validate codes and split strings (8 points)
# ------------------------------------------------------------------------------

sales_raw$code_clean <- toupper(trimws(sales_raw$code_raw))
sales_raw$valid_code <- grepl("^(ST|ON)-\\d{3}$", sales_raw$code_clean)

# Identify invalid entries
invalid_codes <- sales_raw[!sales_raw$valid_code, c("sale_id", "code_raw", "code_clean")]
print("Invalid Codes Summary:")
print(invalid_codes)

# Extract channel parameters matrix side-by-side
valid_rows <- sales_raw[sales_raw$valid_code, ]
code_split <- strsplit(valid_rows$code_clean, "-")
valid_rows$channel <- sapply(code_split, "[", 1)
valid_rows$number <- sapply(code_split, "[", 2)
print("Valid Code Parts:")
print(valid_rows[, c("sale_id", "code_raw", "channel", "number")])

# Explanatory Note on String Splitting:
# Separate wider splitting strategies separate a data token into multiple parallel columns matching the same parent row count.
# Separate longer string adjustments fragment data fields downwards into unique rows, expanding table size dynamically.


# ------------------------------------------------------------------------------
# Part C: Grouped proportions and factor categories (9 points)
# ------------------------------------------------------------------------------

unique_cust <- sort(unique(sales_raw$customer_id))
cust_summary_list <- list()

for (i in seq_along(unique_cust)) {
  sub_df <- sales_raw[sales_raw$customer_id == unique_cust[i], ]
  sales_n <- nrow(sub_df)
  known_n <- sum(!is.na(sub_df$net_amount))
  large_n <- sum(sub_df$net_amount >= 20, na.rm = TRUE)
  large_share <- ifelse(known_n == 0, NA, large_n / known_n)
  
  cust_summary_list[[i]] <- data.frame(
    customer_id = unique_cust[i], sales_n = sales_n, known_n = known_n,
    large_n = large_n, large_share = large_share, stringsAsFactors = FALSE
  )
}
customer_summary <- do.call(rbind, cust_summary_list)
print(customer_summary)

# Breakdown explanation answer for C1 tracking metrics:
# Numerator represents 1 (sale_id 3 net_amount 23.75 >= 20). Denominator tracks total valid observed calculations (2 total elements).

# Factor ordering arrays assignment rules
item_counts <- table(sales_raw$item_clean)
sorted_items <- names(sort(item_counts, decreasing = TRUE))
sales_raw$item_factor <- factor(sales_raw$item_clean, levels = sorted_items)

# Explanatory Factor Note:
# Re-ordering level sorting positions modifies plot mapping or sorting hierarchy displays without altering foundational cellular observations.
# Lumping categories merges small distribution classes into a shared compound 'Other' grouping matrix, modifying row identities.


# ==============================================================================
# Problem 2: Missing Values and Joins (25 points)
# ==============================================================================

reports_raw <- data.frame(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 2, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Part A: Clean and Add missing reports (8 points)
# ------------------------------------------------------------------------------

# Native custom directional row-fill sequence loop
for (i in 1:nrow(reports_raw)) {
  if (is.na(reports_raw$customer_id[i]) && i > 1 && reports_raw$form[i] == reports_raw$form[i-1]) {
    reports_raw$customer_id[i] <- reports_raw$customer_id[i-1]
  }
}

reports_raw$primary_clean <- ifelse(reports_raw$primary_amount == -999, NA, reports_raw$primary_amount)
reports_raw$amount <- ifelse(!is.na(reports_raw$primary_clean), reports_raw$primary_clean, reports_raw$backup_amount)

# Explanatory Value Note:
# Zero measurements reflect explicit numeric facts. Backup variables are intended to resolve missing gaps (NA), not structured values.

# Complete dimensional frame padding sequence mapping
all_custs <- unique(c(reports_raw$customer_id, customers$customer_id))
all_rounds <- 1:3
complete_grid <- expand.grid(customer_id = all_custs, round = all_rounds, stringsAsFactors = FALSE)
reports_complete <- merge(complete_grid, reports_raw[, c("customer_id", "round", "amount")], by = c("customer_id", "round"), all.x = TRUE)

# Accounting metrics verification output:
# Pre-existing Explicit structural NA counts: 1 row tracking metric element. Post-completion tracking sums up to 7 explicit items.


# ------------------------------------------------------------------------------
# Part B: Join and check (8 points)
# ------------------------------------------------------------------------------

customers$customer_found <- TRUE
reports_joined <- merge(reports_raw, customers[, c("customer_id", "region", "customer_found")], by = "customer_id", all.x = TRUE)
reports_joined$customer_found[is.na(reports_joined$customer_found)] <- FALSE

# Explanatory Region Analysis Note:
# C3 maps customer_found = TRUE, denoting cataloged status with unrecorded region variables.
# C4 maps customer_found = FALSE, highlighting missing structural presence outside the customer framework.


# ------------------------------------------------------------------------------
# Part C: Check and reshape reports (9 points)
# ------------------------------------------------------------------------------

# Wide formatting translation matrix utilizing native stats reshaping tool engine
reports_wide <- reshape(reports_complete, idvar = "customer_id", timevar = "round", direction = "wide")
print(reports_wide)

# Prediction Tracking Rule:
# Dropping missing structural parameters returns 5 complete operational row items.


# ==============================================================================
# Problem 3: Two-sample t-tests and simulation (25 points)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part A: Write a t-test function (12 points)
# ------------------------------------------------------------------------------

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x); ny <- length(y)
  mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    df <- nx + ny - 2
    sp2 <- ((nx - 1) * vx + (ny - 1) * vy) / df
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
  } else {
    method <- "Welch"
    se <- sqrt(vx / nx + vy / ny)
    df <- (vx / nx + vy / ny)^2 / (((vx / nx)^2 / (nx - 1)) + ((vy / ny)^2 / (ny - 1)))
  }
  
  t_stat <- (mx - my) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  
  return(list(method = method, t_stat = t_stat, df = df, p_value = p_value, reject = reject))
}


# ==============================================================================
# Problem 3, Part B: Complete Simulation Loop Execution
# ==============================================================================

B <- 1000; nx <- 20; ny <- 40; alpha <- 0.05
set.seed(2026)

scenarios <- list(
  equal_null    = list(mx=0, my=0, sx=1, sy=1),
  unequal_null  = list(mx=0, my=0, sx=3, sy=1),
  equal_shift   = list(mx=0, my=1, sx=1, sy=1),
  unequal_shift = list(mx=0, my=1, sx=3, sy=1)
)

sim_results_list <- list()
idx <- 1

# Loop through scenarios and replicates sequentially
for (scen_name in names(scenarios)) {
  pars <- scenarios[[scen_name]]  # Correctly assign parameters for the current scenario
  
  for (b in 1:B) {
    x_samp <- rnorm(nx, mean = pars$mx, sd = pars$sx)
    y_samp <- rnorm(ny, mean = pars$my, sd = pars$sy)
    
    # 1. Process Welch variant loop pass
    w_res <- two_sample_t(x_samp, y_samp, equal_var = FALSE, alpha = alpha)
    sim_results_list[[idx]] <- data.frame(
      scenario = scen_name, 
      method = "Welch", 
      t_stat = w_res$t_stat,
      reject = w_res$reject, 
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
    
    # 2. Process Pooled variant loop pass
    p_res <- two_sample_t(x_samp, y_samp, equal_var = TRUE, alpha = alpha)
    sim_results_list[[idx]] <- data.frame(
      scenario = scen_name, 
      method = "Pooled", 
      t_stat = p_res$t_stat,
      reject = p_res$reject, 
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}

# Combine all results into a single data frame
simulation_dataframe <- do.call(rbind, sim_results_list)

# Generate Summary Analytics Report Matrix using Base R aggregate()
rejection_summary <- aggregate(
  reject ~ scenario + method, 
  data = simulation_dataframe, 
  FUN = mean
)

# Calculate mean and standard deviation of t-statistics for verification
t_stat_summary <- aggregate(
  t_stat ~ scenario + method, 
  data = simulation_dataframe, 
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)

# Format and display the final summary table
final_summary <- merge(t_stat_summary, rejection_summary, by = c("scenario", "method"))
print("Final Simulation Performance Bounds:")
print(final_summary)

