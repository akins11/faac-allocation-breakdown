### Global 
num_scale_label <- function(
    num_value, scale = 0.1, cut_short = TRUE, prefix = "₦"
  ) {
  
  if (isTRUE(cut_short)) {
    scales::label_number(
      scale, prefix = prefix, scale_cut = scales::cut_short_scale()
    )(num_value)
  } else {
    scales::label_number(
      scale, prefix = prefix, scale_cut = scales::cut_long_scale()
    )(num_value)
  }
}

num_comma_label <- function(num_value, prefix = "₦") {
  
  scales::label_comma(
    accuracy = NULL,
    prefix = prefix,
    big.mark = ","
  )(num_value)
}

change <- function(present_value, past_value) {
  
  round(((present_value - past_value) / past_value) * 100, 3)
  
}

add_change_label <- function(df, var_name) {
  df |>
    dplyr::mutate(row_chg_lab = dplyr::case_when(
      {{var_name}} < 0 ~ "decrease",
      {{var_name}} == 0 ~ "stable",
      {{var_name}} > 0 ~ "increase",
      .default = "no changes"
    )) 
}

### Data details

get_current_year_data <- function(state_df, lgc_df, previous_yr = FALSE) {

  first_year <- state_df |> dplyr::summarise(min(year)) |> dplyr::pull()
  
  if (isFALSE(previous_yr)) {
    state_tbl <- dplyr::filter(state_df, year == max(year))
    lgc_tbl <- dplyr::filter(lgc_df, year == max(year))
  } else {
    state_tbl <- dplyr::filter(state_df, year == max(year)-1)
    lgc_tbl <- dplyr::filter(lgc_df, year == max(year)-1)
  }
  
  if (identical(distinct(state_tbl, year), distinct(lgc_tbl, year))) {
    
    current_year <- dplyr::distinct(state_tbl, year) |> dplyr::pull()
    state_months <- dplyr::distinct(state_tbl, lubridate::month(date)) |> dplyr::pull()
    lgc_months   <- dplyr::distinct(lgc_tbl, lubridate::month(date)) |> dplyr::pull()
    
    output_list <- list(
      state = state_tbl, 
      lgc = lgc_tbl,
      current_year = current_year,
      previous_year = current_year-1,
      analysis_yr_range = c(first_year, current_year)
    )
    
    if (identical(state_months, lgc_months)) {
      output_list$months <- state_months
    } else {
      if (length(state_months) > length(lgc_months)) {
        output_list$months <- lgc_months
      } else if (length(state_months) < length(lgc_months)) {
        output_list$months <- state_months
      } else {
        output_list$months <- lubridate::intersect(state_months, lgc_months)
      }
    }
    
    return(output_list)
    
  } else {
    stop("The lastest year in the State df is not the same as the lastest year in the LGCs df")
  }
}

get_last_faac_month <- function(state_df, lgc_df, previous_yr = FALSE) {
  
  if (isFALSE(previous_yr)) {
    state_tbl <- dplyr::filter(state_df, year == max(year))
    lgc_tbl <- dplyr::filter(lgc_df, year == max(year))
  } else {
    state_tbl <- dplyr::filter(state_df, year == max(year)-1)
    lgc_tbl <- dplyr::filter(lgc_df, year == max(year)-1)
  }
  
  get_month <- \(df) {
    df |>
      dplyr::mutate(month = lubridate::month(date)) |>
      dplyr::filter(year  == max(year)) |>
      dplyr::filter(month == max(month)) |>
      dplyr::distinct(date, year) |>
      dplyr::mutate(
        year = year,
        month_id   = lubridate::month(date),
        month_abb  = lubridate::month(date, label = TRUE),
        month_name = lubridate::month(date, label = TRUE, abbr = TRUE)
      )
  }
  
  if (identical(get_month(state_tbl), get_month(lgc_tbl))) {
    get_month(state_tbl) |> as.list()
    
  } else {
    stop("The last month in the state dataset is not the same as the month in the LGCs Dataset")
  }
  
}


### Card statistics summary
total_level_summary <- function(df, current_yr) {
  
  overall_total_faac <- df |> 
    dplyr::distinct() |>
    dplyr::summarise(total_faac = sum(faac, na.rm = TRUE)) |>
    dplyr::pull()
  
  # Current year summary 
  ## Get the latest/current year
  current_yr_df <- df |>
    dplyr::mutate(year = year, month = lubridate::month(date)) |>
    dplyr::filter(year == max(year))
  
  ## Get the months in the current year
  current_yr_months <- dplyr::distinct(current_yr_df, month) |> pull()
  
  ## Get Present year value and percentage change 
  current_yr_sumy <- df |>
    dplyr::filter(year %in% c(current_yr, current_yr - 1)) |>
    dplyr::filter(lubridate::month(date) %in% current_yr_months) |>
    # dplyr::group_by(year) |>
    dplyr::summarise(total_faac = sum(faac, na.rm = TRUE), .by = year) |>
    dplyr::mutate(
      lag_total_faac = lag(total_faac, 1),
      change = change(total_faac, lag_total_faac),
      total_faac_label = num_scale_label(total_faac, 0.01)
    ) |>
    dplyr::select(year, total_faac, total_faac_label, change) |>
    as.list() |>
    purrr::transpose(c("prevous", "current"))
  
  list(
    overall_total_faac = overall_total_faac,
    overall_total_faac_label = num_scale_label(overall_total_faac, 0.01),
    current_yr = current_yr_sumy
  )
}

total_yearly_summary_sparkline <- function(df, current_year) {
  
  tooltip_format_js_num <- htmltools::HTML("
      function (_ref) {
        var datum = _ref.datum;
        return React.createElement(
            'div',
            null,
            datum.x && React.createElement(
              'div', null,
              datum.x
            ),
            datum.y && React.createElement(
              'div', null,
              datum.y.toLocaleString('en-US', {notation: 'compact', maximumFractionDigits: 1, compactDisplay: 'short'}).replace(/(\\d+(\\.\\d+)?)([KMB])/, '$1$3').replace('G', 'B')
            )
        );
      }
  ")
  mim_max_js_num <- htmltools::HTML(
    "
    d => d.toLocaleString('en-US', {
    notation: 'compact',
    maximumFractionDigits: 1,
    compactDisplay: 'short'
  }).replace(/(\\d+(\\.\\d+)?)([KMB])/, '$1$3').replace('G', 'B')
    "
  )
  
  f_list <- df |>
    dplyr::filter(year <= current_year) |>
    dplyr::group_by(year) |>
    dplyr::summarise(y = sum(faac, na.rm = TRUE)) |>
    dplyr::rename(x = year) |> # , y = total_faac
    as.list() |> 
    purrr::transpose()
  
  dataui::dui_sparkline(
    data = f_list,
    valueAccessor = htmlwidgets::JS("d => d.y"),
    width = "100%",
    height = 130,
    margin = list(top=30, right=40, bottom=30, left=30),
    ariaLabel = "Yearly Total Allocation",
    renderTooltip = htmlwidgets::JS(tooltip_format_js_num),
    components = list(
      dataui::dui_sparklineseries(
        stroke = "#081C15"
      ),
      dataui::dui_sparkpointseries(
        points = list("min","max"),
        fill = "#1B4332",
        stroke = "#fff",
        renderLabel = htmlwidgets::JS(mim_max_js_num),
        labelPosition = "right",
        size = 5
      ),
      dataui::dui_tooltip(
        components = list(
          dataui::dui_sparkverticalrefline(
            key = "ref-line",
            strokeWidth = 1,
            strokeDasharray = "4 4"
          ),
          dataui::dui_sparkpointseries(
            key = "ref-point",
            stroke = "#fff",
            fill = "#1B4332"
          )
        )
      )
    )
  )
}



high_low_lvl_faac <- function(curr_df, level) {
  
  if (level == "lgc") {
    f_tbl <- curr_df |>
      dplyr::summarise(total_faac = sum(faac, na.rm = TRUE), .by = c(state, lgc))
  } else {
    f_tbl <- curr_df |>
      dplyr::summarise(total_faac = sum(faac, na.rm = TRUE), .by = state)
  }
  
  f_tbl |>
    dplyr::filter(total_faac == min(total_faac) | total_faac == max(total_faac)) |>
    dplyr::mutate(
      which = case_when(
        total_faac == max(total_faac) ~ "Highest",
        total_faac == min(total_faac) ~ "Lowest",
        .default = "between"
      ),
      total_faac_label = num_scale_label(total_faac, 0.01)
    ) |>
    dplyr::arrange(which) |>
    as.list() |>
    purrr::transpose(.names = c("high", "low"))
}


geo_zone_summary <- function(curr_df) {
  
  f_tbl <- curr_df |>
    dplyr::summarise(total_faac = sum(faac, na.rm = TRUE), .by = geo_zone) |>
    dplyr::arrange(dplyr::desc(total_faac)) |>
    dplyr::mutate(
      proportion = prop.table(total_faac)*100,
      total_faac_label = num_scale_label(total_faac),
      rank = dplyr::row_number()
    )
  
  as.list(f_tbl) |> purrr::transpose(.names = dplyr::pull(f_tbl, geo_zone))
  
}

geo_zone_sparkline <- function(df, current_year) {
  
  f_df <- df |>
    dplyr::filter(year <= current_year) |>
    dplyr::group_by(geo_zone, year) |>
    dplyr::summarise(y = sum(faac, na.rm = TRUE), .groups = "drop") 
  
  geo_zone_names <- dplyr::distinct(f_df, geo_zone) |> dplyr::pull()
  
  f_list <- f_df |>
    dplyr::group_split(geo_zone) |>
    purrr::map(\(geo_df) {
      geo_df |>
        dplyr::select(-geo_zone) |>
        dplyr::rename(x = year) |>
        as.list() |>
        purrr::transpose()
    })
  
  tooltip_format_js_num <- htmltools::HTML("
      function (_ref) {
        var datum = _ref.datum;
        return React.createElement(
            'div',
            null,
            datum.x && React.createElement(
              'div', null,
              datum.x
            ),
            datum.y && React.createElement(
              'div', null,
              datum.y.toLocaleString('en-US', {notation: 'compact', maximumFractionDigits: 1, compactDisplay: 'short'}).replace(/(\\d+(\\.\\d+)?)([KMB])/, '$1$3').replace('G', 'B')
            )
        );
      }
  ")
  mim_max_js_num <- htmltools::HTML(
    "
    d => d.toLocaleString('en-US', {
    notation: 'compact',
    maximumFractionDigits: 1,
    compactDisplay: 'short'
  }).replace(/(\\d+(\\.\\d+)?)([KMB])/, '$1$3').replace('G', 'B')
    "
  )
  
  
  spark_list <- purrr::map(f_list, \(geo_zone_list) {
    dataui::dui_sparkline(
      data = geo_zone_list,
      valueAccessor = htmlwidgets::JS("d => d.y"),
      width = "100%",
      height = 60,
      margin = list(top=10, right=30, bottom=10, left=10),
      ariaLabel = "Yearly Total Allocation",
      renderTooltip = htmlwidgets::JS(tooltip_format_js_num),
      components = list(
        dataui::dui_sparklineseries(
          stroke = "#081C15"
        ),
        dataui::dui_sparkpointseries(
          points = list("min","max"),
          fill = "#1B4332",
          stroke = "#fff",
          renderLabel = htmlwidgets::JS(mim_max_js_num),
          labelPosition = "right",
          size = 5
        ),
        dataui::dui_tooltip(
          components = list(
            dataui::dui_sparkverticalrefline(
              key = "ref-line",
              strokeWidth = 1,
              strokeDasharray = "4 4"
            ),
            dataui::dui_sparkpointseries(
              key = "ref-point",
              stroke = "#fff",
              fill = "#1B4332"
            )
          )
        )
      )
    )
  })
  
  names(spark_list) <- geo_zone_names
  
  return(spark_list)
}


### Summary Table
table_theme <- function() {
  
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', 
      fill
    )
    
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  
  text_color <- "#000000"
  text_color_light <- "#212529"
  text_color_lighter <- "#6C757D"
  bg_color <- "#fdfdfd"
  
  reactable::reactableTheme(
    # color = text_color,
    backgroundColor = bg_color,
    highlightColor = "#F0FFF1",
    cellPadding = "8px 12px",
    # style = list(
    #   # fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
    #   fontSize = "0.875rem"
    # ),
    
    # headerStyle = list(
    #   # color = text_color_light,
    #   fontWeight = 400,
    #   fontSize = "0.75rem",
    #   letterSpacing = "1px",
    #   textTransform = "uppercase",
    #   "&:hover, &:focus" = list(color = text_color)
    # ),
    
    # rowHighlightStyle = list(
    #   ".tag" = list(color = text_color, borderColor = text_color_lighter)
    # ),
    
    # Full-width search bar with search icon
    searchInputStyle = list(
      paddingLeft = "1.9rem",
      paddingTop = "0.5rem",
      paddingBottom = "0.5rem",
      width = "100%",
      border = "none",
      backgroundColor = bg_color,
      backgroundImage = search_icon(text_color_light),
      backgroundSize = "1rem",
      backgroundPosition = "left 0.5rem center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
      "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
      "::placeholder" = list(color = text_color_lighter),
      "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
    ),
    
    # paginationStyle = list(color = text_color_light),
    # pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    # pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
}

## Table column formatters
format_currency_col <- function(num_value, scale=0.1, add_class = "") {
  # This function should be applied directly to a reactable column cell
  htmltools::div(
    class = paste("col_row_currency_tooltip", add_class),
    tippy::tippy(
      num_scale_label(num_value, scale = scale),
      tooltip = num_comma_label(num_value),
      animation = "scale", 
      placement = "right",
      arrow = TRUE,
      theme = "light",
      interactive = TRUE
    )
  )
}

format_change_text <- function(num_value, apply_color = TRUE) {
  # This function should be applied directly to a reactable column cell
  if (!is.na(num_value)) {
    if (num_value > 0) {
      text_color <- "#008B45"
    } else if (num_value < 0) {
      text_color <- "#8B2323"
    } else {
      text_color <- "#666"
    }
  } else {
    text_color <- "#666"
  }
  
  if (is.na(num_value)) {
    num_value <- "-/-"
  } else{
    num_value <- round(num_value, 2)
  }
  
  p_tag_style <- ifelse(
    apply_color, 
    paste0("color: ", text_color, "; margin: 0;"),
    "margin: 0;"
  )
  
  htmltools::div(
    class = "col_row_chg_text",
    htmltools::p(
      style = p_tag_style,
      num_value, htmltools::tags$span(
        class = "col_row_chg_text_percent_sign",
        style = "font-size: 1.1rem; margin: 0; padding: 0;",
        "%"
      )
    )
  )
}

format_change_icon <- function(chr_value, include_title = FALSE) {
  # This function should be applied directly to a reactable column cell
  if (isTRUE(include_title)) {
    if (chr_value == "increase") {
      v_icon <- fontawesome::fa("arrow-trend-up", fill="#008B45", height="1rem", title = "Increase")
    } else if (chr_value == "decrease") {
      v_icon <- fontawesome::fa("arrow-trend-down", fill="#8B2323", height="1rem", title = "Decrease")
    } else {
      v_icon <- fontawesome::fa("minus", fill="#666", height="1rem", , title = "No Changes")
    }
  } else {
    if (chr_value == "increase") {
      v_icon <- fontawesome::fa_i("arrow-trend-up", style="color: #008B45; font-size: 1rem;")
    } else if (chr_value == "decrease") {
      v_icon <- fontawesome::fa_i("arrow-trend-down", style="color: #8B2323; font-size: 1rem;")
    } else {
      v_icon <- fontawesome::fa_i("minus", style="color: #666; font-size: 1rem;")
    }
  }
  
  htmltools::div(class = "col_row_chg_icon", v_icon)
}

format_rank_col <- function(num_value, add_class = "") {
  if (num_value %in% c(1, seq(21, 91, 10))) {
    chr_rank <- "st"
  } else if (num_value %in% c(2, seq(22, 92, 10))) {
    chr_rank <- "nd"
  } else if (num_value %in% c(3, seq(23, 93, 10))) {
    chr_rank <- "rd"
  } else {
    chr_rank <- "th"
  }
  
  htmltools::div(
    class = paste("col_row_rank", add_class),
    htmltools::p(
      style="margin: 0;", 
      num_value,
      htmltools::tags$span(style="font-size: 1.2rem;", chr_rank)
    )
  )
}


## State
get_state_tbl_data <- function(
    state_df, curr_state_df, curr_lgc_df, prev_yr, curr_yr, month_ids
) {
  # Only current year and later
  state_df <- dplyr::filter(state_df, year <= curr_yr)
  
  # The last FAAC received 
  curr_faac <- state_df |>
    dplyr::filter(date == max(date)) |>
    dplyr::mutate(date = paste(lubridate::month(date, label=T, abb=F), year)) |>
    dplyr::select(state, date, last_faac = faac)
  
  # Total, average and percentage of total FAAC received
  curr_faac_agg <- curr_state_df |>
    dplyr::mutate(share_derivation_13 =  dplyr::if_else(
      is.na(share_derivation_13), 0, share_derivation_13
    )) |>
    dplyr::summarise(
      total_curr_faac = sum(faac, na.rm = TRUE), 
      avg_curr_faac   = mean(faac, na.rm = TRUE),
      total_curr_derivation_share = sum(share_derivation_13),
      .by = state
    ) |>
    dplyr::arrange(dplyr::desc(total_curr_faac)) |>
    # Percentage share of total FAAC 
    dplyr::mutate(
      share_of_total = prop.table(total_curr_faac)*100,
      state_rank = row_number()
    )
  
  # % Change in FAAC between current year and previous year  
  change_faac <- state_df |>
    dplyr::filter(
      year %in% c(prev_yr, curr_yr), 
      lubridate::month(date) %in% month_ids
    ) |> 
    dplyr::summarise(
      total_faac = sum(faac, na.rm = TRUE),
      .by = c(year, state)
    ) |>
    dplyr::arrange(state, year) |>
    dplyr::group_split(state) |>
    purrr::map(\(state_gp_df) {
      state_gp_df |> 
        dplyr::mutate(
          curr_prev_yr_change = change(total_faac, dplyr::lag(total_faac, 1))
        )
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(year == curr_yr) |> # /!\ == 2024
    dplyr::select(-c(year, total_faac))
  
  # Overall Total FAAC received
  overall_total_faac <- state_df |>
    dplyr::summarise(overall_total_faac = sum(faac, na.rm = TRUE), .by = state)
  
  # Yearly total FAAC (Spark-line)
  yrl_faac <- state_df |>
    dplyr::summarise(
      total_yrl_faac_list = sum(faac, na.rm = TRUE), 
      .by = c(state, year)
    ) |>
    dplyr::select(-year) |>
    dplyr::summarise(total_yrl_faac_list = list(total_yrl_faac_list), .by = state)
  
  # Number of unique LGCs
  num_lgc <- curr_lgc_df |>
    dplyr::distinct(state, lgc) |>
    dplyr::count(state, name = "num_state_lgc")
  
  # Total allocation to LGCs for the current year
  total_lgc_faac <- curr_lgc_df |>
    dplyr::summarise(total_lgcs_faac = sum(faac, na.rm = TRUE), .by = state)
  
  # Join table
  curr_faac |>
    dplyr::left_join(curr_faac_agg, by = "state") |>
    dplyr::left_join(change_faac, by = "state") |>
    dplyr::left_join(overall_total_faac, by = "state") |>
    dplyr::left_join(yrl_faac, by = "state") |>
    dplyr::left_join(dplyr::distinct(state_df, state, geo_zone), by = "state") %>% 
    tibble::add_row(
      state = "Abuja",
      date = unique(.$date), # /!\ change table name.
      last_faac = 0,
      total_curr_faac = 0,
      avg_curr_faac = 0,
      share_of_total = 0,
      state_rank = 0,
      curr_prev_yr_change = 0,
      overall_total_faac = 0,
      total_yrl_faac_list = list(0),
      geo_zone = "North Central",
    ) |>
    dplyr::mutate(
      details = "details"
    ) |>
    dplyr::left_join(num_lgc, by = "state") |>
    dplyr::left_join(total_lgc_faac, by = "state") 
}


## LGCs
get_lgs_tbl_data <- function(
    lgc_df, curr_lgc_df, prev_yr, curr_yr, month_ids  
) {
  # Only current year and later
  lgc_df <- dplyr::filter(lgc_df, year <= curr_yr)
  
  # The last FAAC received 
  curr_faac <- lgc_df |>
    dplyr::filter(date == max(date)) |>
    dplyr::select(state, lgc, last_faac = faac)
  
  # Total, average and percentage of total FAAC received
  curr_faac_agg <- curr_lgc_df |>
    dplyr::summarise(
      total_curr_faac = sum(faac, na.rm = TRUE), 
      avg_curr_faac   = mean(faac, na.rm = TRUE),
      .by = c(state, lgc)
    ) |>
    dplyr::group_split(state) |>
    purrr::map(\(state_tbl) {
      state_tbl |>
        dplyr::arrange(dplyr::desc(total_curr_faac)) |>
        dplyr::mutate(
          share_of_total = prop.table(total_curr_faac)*100,
          lgc_rank = row_number()
        )
    }) |>
    purrr::list_rbind() 
  
  # % Change in FAAC between current year and previous year  
  change_faac <- lgc_df |>
    dplyr::filter(
      year %in% c(prev_yr, curr_yr),
      month(date) %in% month_ids
    ) |>
    dplyr::summarise(
      total_faac = sum(faac, na.rm = TRUE),
      .by = c(year, state, lgc)
    ) |>
    dplyr::arrange(state, lgc, year) |>
    dplyr::group_split(lgc) |>
    purrr::map(\(lgc_gp_tbl) {
      lgc_gp_tbl |> 
        mutate(curr_prev_yr_change = change(total_faac, lag(total_faac, 1)))
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(year == 2024) |>
    dplyr::select(-c(year, total_faac))
  
  # Overall Total FAAC received
  overall_total_faac <- lgc_df |>
    dplyr::summarise(
      overall_total_faac = sum(faac, na.rm = TRUE), 
      .by = c(state, lgc)
    )
  
  # Yearly total FAAC (Spark-line)
  yrl_faac <- lgc_df |>
    dplyr::summarise(
      total_yrl_faac_list = sum(faac, na.rm = TRUE), .by = c(state, lgc, year)
    ) |>
    dplyr::select(-year) |>
    dplyr::summarise(
      total_yrl_faac_list = list(total_yrl_faac_list), .by = c(state, lgc)
    )
  
  # Join table
  curr_faac |>
    dplyr::left_join(curr_faac_agg, by = c("state", "lgc")) |>
    dplyr::left_join(change_faac, by = c("state", "lgc")) |>
    dplyr::left_join(overall_total_faac, by = c("state", "lgc")) |>
    dplyr::left_join(yrl_faac, by = c("state", "lgc"))
}


main_summary_table <- function(state_summary_df, lgc_summary_df) {
  
  row_details <- \(index) {
    state_top_df <- state_summary_df[index, ] 
    
    detail_field <- \(title, value_div) {
      if (any(is.na(value_div))) {
        NULL
      } else {
        htmltools::div(
          class="single-detail-container",
          tags$i(class = "bi bi-dash-circle bullet"),
          htmltools::div(
            class = "detail-label-value",
            htmltools::p(class = "detail-label", title),
            value_div
          )
        )
      }
    }
    
    detail <- htmltools::div(
      class = "faac-table-detail",
      htmltools::div(class = "detail-header", paste(state_top_df$state, "State")),
      detail_field("Geo-political Zone:", htmltools::p(class="detail-value geo", state_top_df$geo_zone)),
      detail_field("Last FAAC Allocation:", htmltools::p(class="detail-value date", state_top_df$date)),
      detail_field("State FAAC Allocation Rank:", format_rank_col(state_top_df$state_rank, "single-detail-value")),
      detail_field("Number of LGCs:", htmltools::p(class="detail-value no", state_top_df$num_state_lgc)),
      detail_field("Total LGCs Allocation:", format_currency_col(state_top_df$total_lgcs_faac, add_class="single-detail-value")),
      detail_field("Total 13% Share of Derivation (Current Yr.):", format_currency_col(state_top_df$total_curr_derivation_share, add_class="single-detail-value")),
      # htmltools::div(class = "detail-more", state_top_df$details),
    )
    
    lgc_low_df <- lgc_summary_df[which(lgc_summary_df$state == state_top_df$state), , drop = FALSE]
    
    if (length(lgc_low_df) > 0) {
      lgcs <- htmltools::tagList(
        htmltools::p(class = "detail-table-title", "Local Government Councils (LGCs):"),
        
        reactable::reactable(
          lgc_low_df,
          searchable = TRUE,
          pagination = FALSE,
          highlight = TRUE,
          defaultColDef = reactable::colDef(
            headerClass = "header",
            align = "left"
            # vAlign = "center"
          ),
          columns = list(
            state = reactable::colDef(show = FALSE),
            lgc = reactable::colDef(
              name="LGC", 
              minWidth = 150,
              sticky = "left",
              cell=\(value) {
              htmltools::tags$span(class = "col-title", value)
            }),
            last_faac = reactable::colDef(
              name="Last Mo.", 
              cell = \(value) format_currency_col(value)
            ),
            total_curr_faac = reactable::colDef(
              name="YTD Total", 
              cell = \(value) format_currency_col(value)
            ),
            avg_curr_faac = reactable::colDef(
              name="Avg. Mo.", 
              cell = \(value) format_currency_col(value)
            ),
            share_of_total = reactable::colDef(
              name="% Share", 
              cell = \(value) format_change_text(value, FALSE)
            ),
            lgc_rank = reactable::colDef(
              name="Rank", 
              cell = \(value) format_rank_col(value)
            ),
            curr_prev_yr_change = reactable::colDef(
              name="YoY %", 
              cell = \(value) format_change_text(value)
            ),
            overall_total_faac = reactable::colDef(
              name="All-Time Total", 
              cell = \(value) format_currency_col(value)
            ),
            total_yrl_faac_list = reactable::colDef(
              name = "Yearly Total",
              sortable = FALSE,
              minWidth = 200,
              cell = reactablefmtr::react_sparkline(
                data = lgc_low_df,
                height = 50,
                line_color = "#081C15",
                line_width = 1,
                line_curve = "cardinal",
                highlight_points = reactablefmtr::highlight_points(min = '#931B1F', max = '#35CE8D'),
                point_size = 1.1,
                show_area = TRUE,
                # area_color = "",
                area_opacity = 0.1,
                tooltip = TRUE,
                tooltip_type = 2,
                tooltip_color = NULL
                # margin = reactablefmtr::margin(t=15,r=2,b=15,l=2),
              )
            )
          ),
          class = "faac-summary-table lgc",
          theme = table_theme()
        )
      )
      detail <- htmltools::tagAppendChild(detail, lgcs)
    }
    detail
  }
  
  summary_tbl <- reactable::reactable(
    state_summary_df,
    searchable = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 15),
    highlight = TRUE,
    onClick = "expand",
    resizable = TRUE,
    defaultColDef = reactable::colDef(
      headerClass = "header",
      align = "left",
      vAlign = "center"
    ),
    columns = list(
      state = reactable::colDef(
        name="State", 
        sticky = "left",
        minWidth = 120,
        cell = \(value) {
        htmltools::tags$span(class = "col-title", value)
      }
      ),
      date = reactable::colDef(show=FALSE),
      last_faac = reactable::colDef(
        name="Last Mo.", 
        cell = \(value) format_currency_col(value)
      ),
      total_curr_faac = reactable::colDef(
        name="YTD Total", 
        cell = \(value) format_currency_col(value)
      ),
      avg_curr_faac = reactable::colDef(
        name="Avg. Mo.", 
        cell = \(value) format_currency_col(value)
      ),
      share_of_total = reactable::colDef(
        name="% Share", 
        cell = \(value) format_change_text(value, FALSE)
      ),
      state_rank = reactable::colDef(
        name="Rank", 
        cell = \(value) format_rank_col(value)
      ),
      curr_prev_yr_change = reactable::colDef(
        name="YoY %", 
        cell = \(value) format_change_text(value)
      ),
      overall_total_faac = reactable::colDef(
        name="All-Time Total",
        cell = \(value) format_currency_col(value)
      ),
      total_yrl_faac_list = reactable::colDef(
        name = "Yearly Total",
        sortable = FALSE,
        minWidth = 200,
        cell = reactablefmtr::react_sparkline(
          data = state_summary_df,
          height = 50,
          line_color = "#081C15",
          line_width = 1,
          line_curve = "cardinal",
          highlight_points = reactablefmtr::highlight_points(min = '#931B1F', max = '#35CE8D'),
          point_size = 1.1,
          show_area = TRUE,
          # area_color = "",
          area_opacity = 0.1,
          tooltip = TRUE,
          tooltip_type = 2,
          tooltip_color = NULL
          # margin = reactablefmtr::margin(t=15,r=2,b=15,l=2),
        )
      ),
      geo_zone = reactable::colDef(show = FALSE),
      details = reactable::colDef(show = FALSE),
      total_lgcs_faac = reactable::colDef(show = FALSE),
      num_state_lgc = reactable::colDef(show = FALSE),
      total_curr_derivation_share = reactable::colDef(show = FALSE)
    ),
    details = row_details,
    wrap = FALSE,
    class = "faac-summary-table",
    rowStyle = list(cursor = "pointer"),
    theme = table_theme()
  )
  
  htmltools::div(
    class = "viz-container faac-summary-table-container",
    summary_tbl
  )
}


### Change
## State
state_change <- function(
    df, current_year, valid_yrs=NULL, output_type="mom", overall=TRUE
  ) {
  
  output_type <- match.arg(output_type, c("mom", "yoy"))
  
  if (output_type == "mom") {
    
    if (isTRUE(overall)) {
      f_df <- df |>
        dplyr::arrange(date) |>
        dplyr::summarise(faac = sum(faac, na.rm = TRUE), .by = date) |> 
        dplyr::mutate(MoM_change = change(faac, dplyr::lag(faac, 1))) |>
        dplyr::arrange(dplyr::desc(date)) |>
        dplyr::mutate(
          year = lubridate::year(date),
          month = lubridate::month(date, abbr = FALSE, label = TRUE)
        ) |>
        dplyr::select(year, month, faac, MoM_change)
    } else {
      f_df <- df |>
        dplyr::filter(year %in% valid_yrs[1:3]) |>
        dplyr::arrange(date) |>
        dplyr::group_split(state) |>
        purrr::map(\(state_tbl) {
          state_tbl |>
            dplyr::mutate(MoM_change = change(faac, dplyr::lag(faac, 1)))
        }) |>
        purrr::list_rbind() |>
        dplyr::arrange(state, dplyr::desc(date)) |>
        dplyr::mutate(month = lubridate::month(date, abbr = FALSE, label = TRUE)) |>
        dplyr::select(state, year, month, faac, MoM_change)
    }
  } else {
    if (isTRUE(overall)) {
      f_df <- df |>
        dplyr::arrange(date) |>
        dplyr::summarise(faac = sum(faac, na.rm = TRUE), .by = date) |>
        dplyr::mutate(YoY_change = change(faac, dplyr::lag(faac, 12))) |>
        dplyr::filter(year(date) != 2007) |>
        dplyr::mutate(
          year = lubridate::year(date), 
          month = lubridate::month(date, abbr = FALSE, label = TRUE)
        ) |>
        dplyr::arrange(month, dplyr::desc(year)) |>
        dplyr::select(year, month, faac, YoY_change)
    } else {
      f_df <- df |>
        dplyr::filter(year %in% valid_yrs) |>
        dplyr::group_split(state) |>
        purrr::map(\(state_tbl) {
          state_tbl |>
            dplyr::arrange(date) |>
            dplyr::mutate(YoY_change = change(faac, dplyr::lag(faac, 12))) 
        }) |>
        purrr::list_rbind() |>
        dplyr::filter(year(date) != valid_yrs[length(valid_yrs)]) |>
        dplyr::mutate(month = lubridate::month(date, abbr=FALSE, label=TRUE)) |>
        dplyr::arrange(state, month, dplyr::desc(year)) |>
        dplyr::select(state, year, month, faac, YoY_change)
    }
  }
  
  dplyr::filter(f_df, year <= current_year)
}

## LGCs
lgc_change <- function(
    df, valid_yrs=NULL, output_type="mom", overall=TRUE
  ) {
  
  output_type <- match.arg(output_type, c("mom", "yoy"))
  
  if (output_type == "mom") {
    if (isTRUE(overall)) {
      df |>
        dplyr::arrange(date) |>
        dplyr::summarise(faac = sum(faac, na.rm = TRUE), .by = date) |> 
        dplyr::mutate(MoM_change = change(faac, dplyr::lag(faac, 1))) |>
        dplyr::arrange(dplyr::desc(lubridate::year(date))) |>
        dplyr::mutate(
          year = lubridate::year(date),
          month = lubridate::month(date, abbr = FALSE, label = TRUE)
        ) |>
        dplyr::select(year, month, faac, MoM_change)
    } else {
      df |>
        dplyr::filter(year %in% valid_yrs[1:3]) |>
        dplyr::arrange(date) |>
        dplyr::group_split(lgc) |>
        purrr::map(\(lgc_tbl) {
          lgc_tbl |>
            dplyr::mutate(MoM_change = change(faac, dplyr::lag(faac, 1)))
        }) |>
        purrr::list_rbind() |>
        dplyr::arrange(dplyr::desc(year)) |>
        dplyr::mutate(month = lubridate::month(date, abbr=FALSE, label=TRUE)) |>
        dplyr::select(state, lgc, year, month, faac, MoM_change)
    }
  } else {
    if (isTRUE(overall)) {
      df |>
        dplyr::arrange(date) |>
        dplyr::summarise(faac = sum(faac, na.rm = TRUE), .by = date) |>
        dplyr::mutate(YoY_change = change(faac, dplyr::lag(faac, 12))) |>
        dplyr::filter(lubridate::year(date) != 2007) |>
        dplyr::mutate(
          year = lubridate::year(date), 
          month = lubridate::month(date, abbr=FALSE, label=TRUE)
        ) |>
        dplyr::select(year, month, faac, YoY_change)
    } else {
      df |>
        dplyr::filter(year %in% valid_yrs[1:3]) |>
        dplyr::group_split(lgc) |>
        purrr::map(\(lgc_tbl) {
          lgc_tbl |>
            dplyr::arrange(date) |>
            dplyr::mutate(YoY_change = change(faac, dplyr::lag(faac, 12))) 
        }) |>
        purrr::list_rbind() |>
        dplyr::filter(year(date) != valid_yrs[length(valid_yrs)]+1) |>
        dplyr::mutate(month = lubridate::month(date, abbr=FALSE, label=TRUE)) |>
        dplyr::select(state, lgc, year, month, faac, YoY_change)
    }
  }
}



faac_change_tbl <- function(
    state_tbl, type, include_state = FALSE, valid_yrs=NULL, table_title=NULL
  ) {
  
  react_columns <- list(
    year = reactable::colDef(name = "Year"),
    month = reactable::colDef(name = "Month"),
    faac = reactable::colDef(
      name = "Allocation",
      cell = \(value) format_currency_col(value)
    )
  )
  if (type == "mom") {
    react_columns[["MoM_change"]] <- reactable::colDef(
      name = "Change",
      cell = \(value) format_change_text(value)
    )
  } else {
    react_columns[["YoY_change"]] <- reactable::colDef(
      name = "Change",
      cell = \(value) format_change_text(value)
    )
  }
  
  react_columns[["row_chg_lab"]] <- reactable::colDef(
    name = "", #indicator
    sortable = FALSE,
    cell = \(value) format_change_icon(value)
  )
  title_year_range <- ""
  
  if (isTRUE(include_state)) {
    state_react_col <- list(
      state = reactable::colDef(
        name = "State",
        minWidth = 120,
        sticky = "left"
      )
    )
    react_columns <- append(state_react_col, react_columns)
    
    title_year_range <- paste0("(", min(valid_yrs)+1, " - ", max(valid_yrs), ")")
  } 
  
  summary_tbl <- reactable::reactable(
    state_tbl,
    searchable = TRUE,
    sortable = FALSE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 15),
    highlight = TRUE,
    resizable = TRUE,
    defaultColDef = reactable::colDef(
      headerClass = "header",
      align = "left"
      # vAlign = "center"
    ),
    columns = react_columns,
    wrap = FALSE,
    class = "faac-summary-table",
    theme = table_theme()
  )
  
  htmltools::div(
    class = "viz-container faac-summary-table-container",
    htmltools::h3(class = "change-title", paste(table_title, title_year_range)),
    summary_tbl
  )
}
