library(ggplot2)

# A táblázatok listája
dfs <- list(
  "Belga Index" = df_belgium_index,
  "Francia Index" = df_france_index,
  "Német Index" = df_germany_index,
  "Spanyol Index" = df_spain_index,
  "STOXX 600 Index" = df_stoxx600,
  "MSCI Europe Index" = df_msci
)

attack_dates <- data.frame(
  Date = as.Date(c("2015-01-07", "2015-11-13", "2016-03-22", "2016-07-14", "2016-07-22", 
                   "2016-12-19", "2017-08-17", "2020-02-19")),
  country = c("France", "France", "Belgium", "France", "Germany", "Germany", "Spain", "Germany")
)

# Az összes index vonaldiagramjának megjelenítése, külön diagramok
plots <- list() # Üres lista a diagramok tárolására
for(index_name in names(dfs)) {
  df <- dfs[[index_name]]
  
  # Átalakítjuk a dátumot Date típusra
  df$Date <- as.Date(df$Date)
  
  # Price diagram
  p1 <- ggplot(df, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = paste(index_name, "Árfolyam"), x = "Dátum", y = "Árfolyam") +
    theme_minimal() +
    theme(panel.grid = element_blank())  # Töröljük a gridet
  
  # Attack dátumok szűrése az index_name alapján
  if (index_name == "Francia Index") {
    attack_dates_filtered <- attack_dates[attack_dates$country == "France", ]
  } else if (index_name == "Belga Index") {
    attack_dates_filtered <- attack_dates[attack_dates$country == "Belgium", ]
  } else if (index_name == "Német Index") {
    attack_dates_filtered <- attack_dates[attack_dates$country == "Germany", ]
  } else if (index_name == "Spanyol Index") {
    attack_dates_filtered <- attack_dates[attack_dates$country == "Spain", ]
  } else {
    attack_dates_filtered <- data.frame()  
  }
  
  # A szűrt támadási dátumok hozzáadása a Price diagramhoz
  for(date in attack_dates_filtered$Date) {
    p1 <- p1 + geom_vline(xintercept = as.numeric(date), color = "red", linetype = "dashed")
  }
  
  # Return diagram (sötétkék vízszintes vonal a 0-nál)
  p2 <- ggplot(df, aes(x = Date, y = return)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "darkblue", linetype = "dashed") + 
    labs(title = paste(index_name, "Hozam"), x = "Dátum", y = "Hozam") +
    theme_minimal() +
    theme(panel.grid = element_blank())  # Töröljük a gridet
  
  # A szűrt támadási dátumok hozzáadása a Return diagramhoz
  for(date in attack_dates_filtered$Date) {
    p2 <- p2 + geom_vline(xintercept = as.numeric(date), color = "red", linetype = "dashed")
  }
  
  # A diagramokat hozzáadjuk a listához
  plots[[index_name]] <- list(p1, p2)
}

# Az összes diagram megjelenítése
for(index_name in names(plots)) {
  print(plots[[index_name]][[1]])  # Price diagram
  print(plots[[index_name]][[2]])  # Return diagram
}
