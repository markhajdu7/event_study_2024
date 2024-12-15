#########################################################################
# 1.DATA PREPARATION
#########################################################################
# Package importing
library(tidyverse)
library(readxl)
library(lubridate)

# File paths

file_path1 <- "IBEX 35 Historical Data.csv"
file_path2 <- "DAX Historical Data.csv"
file_path3 <- "CAC 40 Historical Data.csv"
file_path4 <- "BEL 20 Historical Data.csv"
file_path5 <- "stoxx600.csv"
file_path6 <- "msci_europe.csv"
file_path7 <- "attack_data.csv"


# Data import
df_spain_index <- read_csv(file_path1)
df_germany_index <- read_csv(file_path2)
df_france_index <- read_csv(file_path3)
df_belgium_index <- read_csv(file_path4)
df_stoxx600 <- read_csv(file_path5)
df_msci <- read_csv(file_path6)
df_attack <- read_csv(file_path7)

# Data cleaning
df_attack <- df_attack %>%
  filter(Date != "05/22/2017") %>%  
  mutate(country = c("France", "France", "Belgium", 
                     "France", "Germany", "Germany", 
                     "Spain", "Germany"))



df_belgium_index <- df_belgium_index %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )



df_france_index <- df_france_index %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )



df_germany_index <- df_germany_index %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )


df_spain_index <- df_spain_index %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )

df_stoxx600 <- df_stoxx600 %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )

df_msci <- df_msci %>%
  mutate(
    Date = mdy(Date),
    return = as.numeric(str_replace(`Change %`, "%", "")) / 100
  )

df_attack <- df_attack %>%
  mutate(Date = mdy(Date))

# Filter for the same data range
start_date_filter <- as.Date("2013-04-01")
end_date_filter <- as.Date("2020-06-01")


df_belgium_index <- df_belgium_index %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)



df_france_index <- df_france_index %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

df_germany_index <- df_germany_index %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

df_spain_index <- df_spain_index %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

df_stoxx600 <- df_stoxx600 %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

df_msci <- df_msci %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

df_attack <- df_attack %>%
  filter(Date >= start_date_filter & Date <= end_date_filter) %>%
  arrange(Date)

# combined_df creating for anaylsis
df_france_combined <- df_france_index %>%
  select(Date, return) %>%
  inner_join(df_stoxx600 %>% select(Date, return) %>% rename(bond_return = return), by = "Date")

df_germany_combined <- df_germany_index %>%
  select(Date, return) %>%
  inner_join(df_stoxx600 %>% select(Date, return) %>% rename(bond_return = return), by = "Date") 

df_spain_combined <- df_spain_index %>%
  select(Date, return) %>%
  inner_join(df_stoxx600 %>% select(Date, return) %>% rename(bond_return = return), by = "Date") 

df_belgium_combined <- df_belgium_index %>%
  select(Date, return) %>%
  inner_join(df_stoxx600 %>% select(Date, return) %>% rename(bond_return = return), by = "Date") 


# Preparing dates for the analysis
event_dates <- df_attack %>% 
  select(Date, country) 

# Results table generation
results <- data.frame(
  event_window = character(),
  car = numeric(),
  median_ar = numeric(),
  variance_caar = numeric(),
  t_statistic = numeric(), 
  stringsAsFactors = FALSE
)



#########################################################################
# 2. Calculations
#########################################################################

# Regressions and AR calculation
for (i in 1:nrow(event_dates)) {
  
  day_0 <- event_dates$Date[i]
  country <- event_dates$country[i]
  
  # country specific combined df choosing
  df_combined <- switch(country,
                        "France" = df_france_combined,
                        "Germany" = df_germany_combined,
                        "Spain" = df_spain_combined,
                        "Belgium" = df_belgium_combined)
  
  # Setting the right dates
  event_start_date <- as.Date(day_0 - 5)
  event_end_date <- as.Date(day_0 + 30)
  
  # Estimation period (160 days -->approximately 120 trading days)
  start_date <- as.Date(event_start_date - 160)
  df_regression <- df_combined %>%
    filter(Date >= start_date & Date <= event_start_date) 
  
  # Becsült hozam (predicted return) számítása az átlaghozam alapján
  mean_return <- mean(df_regression$return, na.rm = TRUE)
  
  # Predicted return és AR (abnormal return) számítása
  df_combined <- df_combined %>%
    mutate(
      predicted_return = mean_return,
      ar = return - predicted_return
    )
  
  # Estimation period variance (K = 2 correction)
  n_estimation <- nrow(df_regression)
  K <- 2
  s_n_squared <- ifelse(n_estimation > K,
                        sum((df_regression$return - mean_return)^2, na.rm = TRUE) / (n_estimation - K),
                        NA)
  
  # Event windows
  event_windows <- list(
    "[-1, 0]" = c(-1, 0),
    "[-1, 1]" = c(-1, 1),
    "[0, 1]" = c(0, 1),
    "[1, 2]" = c(1, 2),
    "[0, 2]" = c(0, 2),
    "[0, 5]" = c(0, 5),
    "[1, 10]" = c(1, 10),
    "[0, 10]" = c(0, 10),
    "[0, 20]" = c(0, 20),  
    "[0, 30]" = c(0, 30)   
  )
  
  # CAR and test statistics
  for (window_name in names(event_windows)) {
    window <- event_windows[[window_name]]
    start_day <- window[1]
    end_day <- window[2]
    
    # filtering for the specific window
    day_0_index <- which(df_combined$Date == day_0)
    df_event_window <- df_combined %>%
      slice((day_0_index + start_day):(day_0_index + end_day))
    
    # CAR calculation
    car_value <- sum(df_event_window$ar, na.rm = TRUE)
    median_ar_value <- ifelse(nrow(df_event_window) > 0, median(df_event_window$ar, na.rm = TRUE), NA)
    
    # Length of the event window (m)
    m <- nrow(df_event_window)
    
    # test statistics
    t_stat_value <- ifelse(!is.na(s_n_squared) && s_n_squared > 0 && m > 0,
                           car_value / sqrt(m*s_n_squared),
                           NA)
    
    # degree of freedoms
    df <- n_estimation - K
    
    # critical values (p 0,05 and 0,1)
    critical_value_5 <- qnorm(0.975)
    critical_value_10 <- qnorm(0.95)  
    
    # significance check
    significance_flag <- ifelse(!is.na(t_stat_value) && !is.na(critical_value_5) && abs(t_stat_value) > critical_value_5, "Yes:5%",
                                ifelse(!is.na(t_stat_value) && !is.na(critical_value_10) && abs(t_stat_value) > critical_value_10, "Yes:10%", "No"))
    
    
    # results
    results <- rbind(results, data.frame(
      country = country,
      event_date = day_0,
      event_window = window_name,
      car = car_value,
      median_ar = median_ar_value,
      test_statistic = t_stat_value,
      significant_5pct = significance_flag
    ))
  }
}


#########################################################################
# 4. EXPORT RESULTS TO EXCEL
#########################################################################

library(writexl)

# Filename
file_path <- "event_study_results_mean.xlsx"

# Write dataframes to Excel
write_xlsx(list(
  #"Results_Final" = results_final,
  "Results" = results
), path = file_path)
