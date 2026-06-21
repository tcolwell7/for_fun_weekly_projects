


# plot_eu_trade_surplus("overall",2020)
# plot_eu_trade_surplus("goods",2020)
# plot_eu_trade_surplus("services",2025)
# 



plot_eu_trade_surplus <- function(
    trade_type = c("overall", "goods", "services"),
    surplus_year = 2025
) {
  
  
  # -----------------------------
  # 0. Validate inputs
  # -----------------------------
  trade_type <- match.arg(trade_type)
  
  if (!surplus_year %in% 2016:2025) {
    stop("❌ surplus_year must be between 2016 and 2025.")
  }
  
  message("ℹ️ Running EU trade surplus analysis...")
  message("   • Trade type: ", trade_type)
  message("   • Surplus year: ", surplus_year)
  
  # -----------------------------
  # 1. Load the fixed workbook
  # -----------------------------
  file_path <- "tradequarterlyq425seasonallyadjusted.xlsx"
  
  if (!file.exists(file_path)) {
    stop("❌ Required data file 'tradequarterlyq425seasonallyadjusted.xlsx' not found in working directory.")
  }
  
  # -----------------------------
  # 2. Select sheet based on trade type
  # -----------------------------
  sheet_map <- list(
    overall  = 3,
    goods    = 5,
    services = 7
  )
  
  sheet_to_read <- sheet_map[[trade_type]]
  
  message("ℹ️ Reading sheet ", sheet_to_read, " (", trade_type, ")...")
  
  # -----------------------------
  # 3. Load and clean data
  # -----------------------------
  data <- readxl::read_excel(file_path, sheet = sheet_to_read, skip = 3) %>%
    janitor::clean_names()
  
  # Identify split between exports/imports
  imp_row <- data %>%
    mutate(row_id = row_number()) %>%
    filter(is.na(country)) %>%
    pull(row_id)
  
  exp <- data %>%
    slice(1:(imp_row - 1)) %>%
    mutate(flow = "Exports")
  
  imp <- data %>%
    slice((imp_row + 2):nrow(data)) %>%
    mutate(flow = "Imports")
  
  total <- bind_rows(exp, imp)
  
  # EU27 codes
  eu27 <- c(
    "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
    "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
    "PL","PT","RO","SK","SI","ES","SE"
  )
  
  eu <- total %>%
    filter(country_code %in% eu27) %>%
    arrange(country_code)
  
  # -----------------------------
  # 4. Compute trade deficit
  # -----------------------------
  eu_td <- eu %>%
    mutate(across(starts_with("x"), as.numeric)) %>%
    group_by(country_code, country) %>%
    summarise(
      across(starts_with("x"), ~ first(.x) - last(.x)),
      .groups = "drop"
    )
  
  df <- eu_td %>%
    select(country_code, country, x2016:x2025) %>%
    pivot_longer(
      cols = x2016:x2025,
      names_to = "year",
      values_to = "trade_def"
    ) %>%
    mutate(year = stringr::str_remove(year, "x"))
  
  # -----------------------------
  # 5. Identify surplus countries
  # -----------------------------
  surplus_year_chr <- as.character(surplus_year)
  
  surplus_countries <- df %>%
    filter(year == surplus_year_chr, trade_def > 0)
  
  n_surplus <- nrow(surplus_countries)
  
  if (n_surplus == 0) {
    stop("❌ No EU countries recorded a trade surplus in ", surplus_year, " for trade type '", trade_type, "'.")
  }
  
  message("✔️ Found ", n_surplus, " countries with a surplus in ", surplus_year, ".")
  
  df3 <- df %>%
    filter(country_code %in% surplus_countries$country_code)
  
  # -----------------------------
  # 6. Order facets by surplus size
  # -----------------------------
  order_vec <- df3 %>%
    filter(year == surplus_year_chr) %>%
    arrange(desc(trade_def)) %>%
    pull(country)
  
  df3$country <- factor(df3$country, levels = order_vec)
  
  # -----------------------------
  # 7. Add flag colours
  # -----------------------------
  df3 <- df3 %>%
    rowwise() %>%
    mutate(
      surplus_col = flag_cols[[country_code]][1],
      deficit_col = flag_cols[[country_code]][2],
      fill_col = if_else(trade_def >= 0, surplus_col, deficit_col)
    ) %>%
    ungroup()
  
  # -----------------------------
  # 8. Build plot
  # -----------------------------
  p <- ggplot(df3, aes(year, trade_def, fill = fill_col)) +
    geom_col(color = "black", alpha = 0.8, width = 0.7, linewidth = 0.4) +
    scale_fill_identity() +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 1) +
    facet_wrap(~ country, ncol = 2, scales = "free_y") +
    theme_default +
    labs(
      y = "",
      x = "",
      title = paste0("UK–EU27 trade balance (", trade_type, ") in ", surplus_year, " (£m)"),
      subtitle = paste0("Countries with a trade surplus in ", surplus_year),
      caption = "Source: ONS Trade Data"
    ) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "£", big.mark = ",")) +
    theme(
      plot.background = element_rect(fill = "#F7F7F7", colour = NA),
      panel.background = element_rect(fill = "#F7F7F7", colour = NA)
    )
  
  message("✔️ Plot successfully generated.")
  
  return(p)
  
}


# functions for workflow --------

build_trade_df <- function(trade_type = c("overall", "goods", "services")) {
  
  trade_type <- match.arg(trade_type)
  
  file_path <- "tradequarterlyq425seasonallyadjusted.xlsx"
  if (!file.exists(file_path)) {
    stop("❌ Required data file 'tradequarterlyq425seasonallyadjusted.xlsx' not found in working directory.")
  }
  
  sheet_map <- list(
    overall  = 3,
    goods    = 5,
    services = 7
  )
  sheet_to_read <- sheet_map[[trade_type]]
  
  message("ℹ️ Building data for trade type: ", trade_type, " (sheet ", sheet_to_read, ")")
  
  data <- readxl::read_excel(file_path, sheet = sheet_to_read, skip = 3) %>%
    janitor::clean_names()
  
  imp_row <- data %>%
    mutate(row_id = dplyr::row_number()) %>%
    dplyr::filter(is.na(country)) %>%
    dplyr::pull(row_id)
  
  exp <- data %>%
    dplyr::slice(1:(imp_row - 1)) %>%
    dplyr::mutate(flow = "Exports")
  
  imp <- data %>%
    dplyr::slice((imp_row + 2):nrow(data)) %>%
    dplyr::mutate(flow = "Imports")
  
  total <- dplyr::bind_rows(exp, imp)
  
  eu27 <- c(
    "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
    "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
    "PL","PT","RO","SK","SI","ES","SE"
  )
  
  eu <- total %>%
    dplyr::filter(country_code %in% eu27) %>%
    dplyr::arrange(country_code)
  
  eu_td <- eu %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("x"), as.numeric)) %>%
    dplyr::group_by(country_code, country) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("x"), ~ dplyr::first(.x) - dplyr::last(.x)),
      .groups = "drop"
    )
  
  df <- eu_td %>%
    dplyr::select(country_code, country, x2016:x2025) %>%
    tidyr::pivot_longer(
      cols = x2016:x2025,
      names_to = "year",
      values_to = "trade_def"
    ) %>%
    dplyr::mutate(
      year = stringr::str_remove(year, "x"),
      trade_type = trade_type
    )
  
  message("✔️ Data built for trade type: ", trade_type)
  
  df
}


plot_eu_trade_surplus <- function(df, surplus_year = 2025) {
  
  yr <- as.character(surplus_year)
  
  if (!yr %in% df$year) {
    stop("❌ surplus_year ", surplus_year, " not found in supplied df.")
  }
  
  trade_type <- unique(df$trade_type)
  if (length(trade_type) != 1) {
    stop("❌ df must contain exactly one trade_type.")
  }
  
  surplus_countries <- df %>%
    dplyr::filter(year == yr, trade_def > 0)
  
  n_surplus <- nrow(surplus_countries)
  if (n_surplus == 0) {
    stop("❌ No countries with a surplus in ", surplus_year, " for trade type '", trade_type, "'.")
  }
  
  message("ℹ️ Plotting ", n_surplus, " surplus countries for ", trade_type, " in ", surplus_year)
  
  
  # DBT theme (your defaults)
  
  font <- "sans"
  
  grid_y <- element_line(color = "grey90", linewidth = 0.2)
  grid_x <- element_blank()
  
  update_geom_defaults("line", list(linewidth = 0.8))
  update_geom_defaults("bar", list(fill = "#00285f"))
  
  theme_default <-
    theme_minimal() +
    theme(
      plot.margin = margin(t = 15, r = 5.5, b = 5.5, l = 5.5, "pt"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(
        family = font, size = 14, face = "bold",
        hjust = 0, vjust = 5
      ),
      plot.subtitle = element_text(
        family = font, size = 12,
        hjust = 0, vjust = 6
      ),
      plot.caption = element_text(
        family = font, size = 8, hjust = 0
      ),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      
      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor = element_blank(),
      
      axis.ticks = element_blank(),
      
      axis.title = element_text(family = font, size = 12),
      axis.text = element_text(family = font, size = 10),
      
      axis.text.x = element_text(margin = margin(b = 6)),
      axis.text.y = element_text(margin = margin(l = 6))
    )
  
  
  
  flag_cols <- list(
    SE = c("#005293", "#FECB00"),                     # Sweden
    FR = c("#0055A4", "#FFFFFF", "#EF4135"),          # France
    FI = c("#FFFFFF", "#003580"),                     # Finland
    DE = c("#000000", "#DD0000", "#FFCE00"),          # Germany
    NL = c("#AE1C28", "#21468B", "#FFFFFF"),          # Netherlands
    BE = c("#000000", "#FFD90C", "#EF3340"),          # Belgium
    IT = c("#009246", "#FFFFFF", "#CE2B37"),          # Italy
    ES = c("#AA151B", "#F1BF00"),                     # Spain
    PT = c("#006600", "#FF0000", "#FFCC00"),          # Portugal
    DK = c("#C60C30", "#FFFFFF"),                     # Denmark
    AT = c("#ED2939", "#FFFFFF"),                     # Austria
    PL = c("#FFFFFF", "#DC143C"),                     # Poland
    CZ = c("#FFFFFF", "#D7141A", "#11457E"),          # Czechia
    EE = c("#0072CE", "#FFFFFF", "#000000"),          # Estonia
    LV = c("#9E3039", "#FFFFFF"),                     # Latvia
    LT = c("#FDB913", "#006A44", "#C1272D"),          # Lithuania
    IE = c("#169B62", "#FFFFFF", "#FF883E"),          # Ireland
    HU = c("#CE2939", "#FFFFFF", "#477050"),          # Hungary
    SK = c("#FFFFFF", "#0B4EA2", "#EE1C25"),          # Slovakia
    SI = c("#FFFFFF", "#005DA4", "#ED1C24"),          # Slovenia
    RO = c("#002B7F", "#FCD116", "#CE1126"),          # Romania
    BG = c("#FFFFFF", "#00966E", "#D62612"),          # Bulgaria
    HR = c("#FF0000", "#FFFFFF", "#171796"),          # Croatia
    CY = c("#5A8E3E", "#D57800", "#FFFFFF"),          # Cyprus
    LU = c("#EF3340", "#FFFFFF", "#00A3E0"),          # Luxembourg
    MT = c("#FFFFFF", "#CF142B", "#BEBEBE"),          # Malta
    GR = c("#0D5EAF", "#FFFFFF")                      # Greece
  )
  
  
  
  df3 <- df %>%
    dplyr::filter(country_code %in% surplus_countries$country_code)
  
  order_vec <- df3 %>%
    dplyr::filter(year == yr) %>%
    dplyr::arrange(dplyr::desc(trade_def)) %>%
    dplyr::pull(country)
  
  df3$country <- factor(df3$country, levels = order_vec)
  
  df3 <- df3 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      surplus_col = flag_cols[[country_code]][1],
      deficit_col = flag_cols[[country_code]][2],
      fill_col = dplyr::if_else(trade_def >= 0, surplus_col, deficit_col)
    ) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot(df3, ggplot2::aes(year, trade_def, fill = fill_col)) +
    ggplot2::geom_col(color = "black", alpha = 0.8, width = 0.7, linewidth = 0.4) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 1) +
    ggplot2::facet_wrap(~ country, ncol = 2, scales = "free_y") +
    theme_default +
    ggplot2::labs(
      y = "",
      x = "",
      title = paste0("UK–EU27 trade balance (", trade_type, ") in ", surplus_year, " (£m)"),
      subtitle = paste0("Countries with a trade surplus in ", surplus_year),
      caption = "Source: ONS Trade Data"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(prefix = "£", big.mark = ",")
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#F7F7F7", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#F7F7F7", colour = NA)
    )
}


make_trade_gt_table <- function(df, surplus_year = 2025) {
  
  yr <- as.character(surplus_year)
  
  if (!yr %in% df$year) {
    stop("❌ surplus_year ", surplus_year, " not found in supplied df.")
  }
  
  # ---- ORDER ALL COUNTRIES BY SURPLUS SIZE ----
  order_vec <- df %>%
    filter(year == yr) %>%
    arrange(desc(trade_def)) %>%
    pull(country)
  
  df$country <- factor(df$country, levels = order_vec)
  
  # ---- BUILD TREND TABLE FOR ALL COUNTRIES ----
  trend_tbl <- df %>%
    group_by(country, country_code) %>%
    summarise(
      start  = trade_def[year == "2016"],
      end    = trade_def[year == yr],
      change = end - start,
      trend  = list(trade_def),
      .groups = "drop"
    )
  
  # ---- BUILD GT TABLE ----
  gt_tbl <- trend_tbl %>%
    gt() %>%
    gtExtras::gt_plt_sparkline(column = trend, same_limit = FALSE) %>%
    fmt_number(columns = c(start, end, change), decimals = 0) %>%
    tab_header(
      title = md("**Trade Balance Trends (2016–2025)**"),
      subtitle = paste0("EU27 countries ordered by trade balance in ", surplus_year)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00285F"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#003A80"),
        cell_text(color = "white")
      ),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_options(
      table.font.names = "sans",
      table.align = "left", 
      table.background.color = "white",
      data_row.padding = px(6),
      column_labels.background.color = "#F3F2F1",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      table.border.bottom.color = "white"
    )
  
  return(gt_tbl)
}



