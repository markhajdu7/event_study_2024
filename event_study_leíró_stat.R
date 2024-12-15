# Könyvtárak betöltése
library(dplyr)
library(tidyr)
library(openxlsx)

# Leíró statisztika funkció definiálása
descriptive_stats <- function(df, exclude_columns) {
  df_numeric <- df %>% 
    select(-all_of(exclude_columns)) %>% # "Change %" oszlop kizárása
    select(where(is.numeric))            # Csak numerikus oszlopokat tart meg
  
  summary_stats <- df_numeric %>%
    summarise_all(list(
      count = ~sum(!is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      NA_count = ~sum(is.na(.))
    )) %>%
    pivot_longer(everything(), 
                 names_to = c("Variable", ".value"),
                 names_sep = "_")
  
  return(summary_stats)
}

# Szűrési intervallumok
start_date_common <- as.Date("2013-04-01")
end_date_common <- as.Date("2018-01-01")
end_date_extended <- as.Date("2020-04-01")

# Szűrés a táblázatokra
df_belgium_index <- df_belgium_index %>% filter(Date >= start_date_common & Date <= end_date_common)
df_france_index <- df_france_index %>% filter(Date >= start_date_common & Date <= end_date_common)
df_spain_index <- df_spain_index %>% filter(Date >= start_date_common & Date <= end_date_common)

df_germany_index <- df_germany_index %>% filter(Date >= start_date_common & Date <= end_date_extended)
df_stoxx600 <- df_stoxx600 %>% filter(Date >= start_date_common & Date <= end_date_extended)
df_msci <- df_msci %>% filter(Date >= start_date_common & Date <= end_date_extended)

# A táblázatok listája
dfs <- list(
  "Belgium Index" = df_belgium_index,
  "France Index" = df_france_index,
  "Germany Index" = df_germany_index,
  "Spain Index" = df_spain_index,
  "Stoxx600 Index" = df_stoxx600,
  "MSCI Europe Index" = df_msci
)

# Statisztikák kiszámítása minden táblázatra
stats_results <- lapply(dfs, function(df) descriptive_stats(df, exclude_columns = "Change %"))

# Excel fájl írása
output_file <- "descriptive_statistics.xlsx"
wb <- createWorkbook()

# Hozzáadunk minden táblázatot külön munkalapként
for (name in names(stats_results)) {
  addWorksheet(wb, name)
  writeData(wb, sheet = name, stats_results[[name]])
}

# Fájl mentése
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("A leíró statisztikák az alábbi fájlba kerültek mentésre:", output_file, "\n")
