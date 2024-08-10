num_comma_label <- function(num_value, prefix = "₦") {
  
  scales::label_comma(
    accuracy = NULL,
    prefix = prefix,
    big.mark = ","
  )(num_value)
}


page_title_nav <- function(
    page_title, curr_yr_lab, prev_yr_lab, active_link
  ) {
  
  if (active_link == "current") {
    curr_class <- "active"
    prev_class <- ""
    about_class <- ""
  } else if (active_link == "previous") {
    curr_class <- ""
    prev_class <- "active"
    about_class <- ""
  } else {
    curr_class <- ""
    prev_class <- ""
    about_class <- "active"
  }
  
  nav_links <- htmltools::tagList(
    htmltools::tags$li(
      htmltools::tags$a(class = paste("page-nav-link", curr_class), href = "/", curr_yr_lab)
    ),
    htmltools::tags$li(
      htmltools::tags$a(class = paste("page-nav-link", prev_class), href = "previous.html", prev_yr_lab)
    ),
    htmltools::tags$li(
      htmltools::tags$a(class = paste("page-nav-link", about_class), href = "about.html", "About")
    )
  )
  
  htmltools::div(
    class = "page-header",
    htmltools::tags$nav(
      class = "page-title-nav-container",
      
      htmltools::tags$a(class = "page-nav-link-title", href = "/", page_title),
      htmltools::tags$ul(class = "p-navbar", nav_links),
      htmltools::tags$i(
        class = "bi bi-list open-icon", 
        id = "show_sidebar_icon"
      )
    ),
    
    htmltools::div(
      class = "p-sidebar",
      id = "report_sidebar",
      
      htmltools::div(
        class = "icon-container",
        htmltools::tags$i(
          class = "bi bi-x-lg close-icon",
          id = "hide_sidebar_icon"
        ),
      ),
      htmltools::tags$ul(class = "list", nav_links),
    )
  )
  
}


main_table_title_info <- function() {
  
  tbl_title <- c(
    "State", "Last Mo.", "YTD Total", "Avg. Mo.", "% Share", "Rank", "YoY %",
    "All-Time Total", "Yearly Total"
  )
  
  tbl_desc <- c(
    "Name of the state or LGC",
    "Allocation for Last month disbursement of the current year",
    "Year-to-Date allocation",
    "Average monthly allocation for the current year",
    "Percentage of total allocation for the current year",
    "Ranking based on the current year total allocation",
    "Year-on-Year percentage change (Last year and current year)",
    "The overall total from 2007 to the current year.",
    "Total allocation by Year."
  )
  
  tbl_data_items <- htmltools::tagList(
    lapply(seq_len(9), \(i) {
      htmltools::tags$tr(
        htmltools::tags$td(class = "drop-td", tbl_title[i]),
        htmltools::tags$td(class = "drop-td",tbl_desc[i])
      )
    })
  )
  
  htmltools::div(
    class = "tbl-title-info",
    
    htmltools::h3(
      class = "main-tbl-title",
      "State and LGC Summary with Key Metrics"
    ),
    
    htmltools::div(
      class = "desc-tbl-icon-container", # dropdown
      htmltools::tags$i(class = "bi bi-info-circle-fill t-icon"),
      htmltools::div(
        class = "desc-content",
        
        htmltools::tags$table(
          class = "drop-table table table-sm",
          htmltools::tags$tr(
            class = "drop-tr",
            htmltools::tags$th(class="drop-th", "Table Header"),
            htmltools::tags$th(class="drop-th", "Header Description")
          ),
          tbl_data_items
          
        )
      )
    )
  )
}


total_level_card_stats <- function(
    lvl_list, spark_element_tag, lvl, analysis_yr_range
  ) {
  
  curr_list <- lvl_list$current_yr$current
  
  if (curr_list$change > 0) {
    badge_clr <- "green"
    badge_icon <- "bi-arrow-up-circle"
  } else if (curr_list$change < 0) {
    badge_clr <- "red"
    badge_icon <- "bi-arrow-down-circle"
  } else {
    badge_clr <- "grey"
    badge_icon <- "bi-dash-circle"
  }
  
  yr_range <- paste0(
    "(", 
    analysis_yr_range[1], 
    " - '", 
    stringr::str_sub(as.character(analysis_yr_range[2]), 3, 4), 
    ")"
  )
  
  htmltools::div(
    class = "viz-container report-card level-total", 
    
    htmltools::div(
      class = "statistic-col",
      htmltools::div(
        class = "current-yr-row",
        htmltools::p(
          class = "value", 
          tippy::tippy(
            curr_list$total_faac_label,
            tooltip = num_comma_label(curr_list$total_faac),
            animation = "scale", 
            placement = "right",
            arrow = TRUE,
            theme = "light",
            interactive = TRUE
          ),
          htmltools::tags$span(class = "lvl", paste0("(", lvl, ")"))
        ),
        htmltools::p(class = "desc", "Total Current Yr.")
      ),
      htmltools::div(
        class = "total-yr-row",
        htmltools::p(
          class = "value", 
          tippy::tippy(
            lvl_list$overall_total_faac_label,
            tooltip = num_comma_label(lvl_list$overall_total_faac),
            animation = "scale", 
            placement = "right",
            arrow = TRUE,
            theme = "light",
            interactive = TRUE
          ),
        ),
        htmltools::p(
          class = "desc", 
          "Overall Total", 
          htmltools::span(class = "duration", yr_range)
        )
      )
    ),
    htmltools::div(
      class = "viz-col",
      htmltools::div(
        class = "viz-sparkline",
        spark_element_tag
      ),
      htmltools::div(
        class = "change",
        htmltools::div(
          class = paste("badge-container", badge_clr),
          htmltools::p(
            class = "value", 
            tags$i(class = paste("bi", badge_icon)), 
            paste0(round(curr_list$change, 1), "%")
          )
        ),
        htmltools::p(class = "desc", "Change from Prev. Yr.")
      )
    )
  )
}


high_low_card_stats <- function(lvl_type, lvl_list) {
  
  lvl_type <- match.arg(lvl_type, c("state", "lgc"))
  
  if (lvl_type == "state") {
    main_title_high <- paste(lvl_list$high$state, "State")
    main_title_low  <- paste(lvl_list$low$state, "State")
    
    state_desc_tag_high <- NULL
    state_desc_tag_low  <- NULL
  } else {
    main_title_high <- paste(lvl_list$high$lgc, "LGA")
    main_title_low  <- paste(lvl_list$low$lgc, "LGA")
    
    state_desc_tag_high <- htmltools::p(class = "desc", paste(lvl_list$high$state, "State"))
    state_desc_tag_low  <- htmltools::p(class = "desc", paste(lvl_list$low$state, "State"))
    
  }
  
  htmltools::div(
    class = "viz-container high-low-card",
    
    htmltools::div(
      class = "c-row high",
      htmltools::div(
        class = "label",
        htmltools::p(class = "name", main_title_high),
        state_desc_tag_high
      ),
      htmltools::div(
        class = "values",
        htmltools::p(
          class = "amount", 
          tippy::tippy(
            lvl_list$high$total_faac_label,
            tooltip = num_comma_label(lvl_list$high$total_faac),
            animation = "scale", 
            placement = "right",
            arrow = TRUE,
            theme = "light",
            interactive = TRUE
          ),
        ),
        htmltools::p(class = "desc", "Highest Total Year Allocation")
      ),
    ),
    htmltools::div(
      class = "c-row low",
      htmltools::div(
        class = "label",
        htmltools::p(class = "name", main_title_low),
        state_desc_tag_low
      ),
      htmltools::div(
        class = "values",
        htmltools::p(
          class = "amount", 
          tippy::tippy(
            lvl_list$low$total_faac_label,
            tooltip = num_comma_label(lvl_list$low$total_faac),
            animation = "scale", 
            placement = "right",
            arrow = TRUE,
            theme = "light",
            interactive = TRUE
          ),
        ),
        htmltools::p(class = "desc", "Lowest Total Year Allocation")
      ),
    ),
  )
}


geo_zone_card_stats <- function(geo_list, geo_spark_element_list) {
  
  geo_zones_names <- names(geo_list)
  
  all_geo_items <- lapply(geo_zones_names, \(geo) {
    geo_stats <- geo_list[[geo]]
    
    if (geo_stats$rank == 1) {
      chr_rank <- "st"
    } else if (geo_stats$rank == 2) {
      chr_rank <- "nd"
    } else if (geo_stats$rank == 3) {
      chr_rank <- "rd"
    } else {
      chr_rank <- "th"
    }
    
    htmltools::div(
      class = "card-row",
      htmltools::div(
        class = "content",
        htmltools::p(class = "title", geo_stats$geo_zone),
        htmltools::p(
          class = "value", 
          htmltools::tags$span(
            class = "total", 
            tippy::tippy(
              geo_stats$total_faac_label,
              tooltip = num_comma_label(geo_stats$total_faac),
              animation = "scale", 
              placement = "right",
              arrow = TRUE,
              theme = "light",
              interactive = TRUE
            ),
          ), 
          htmltools::tags$span(class = "divider"),
          htmltools::tags$span(class = "percent", paste0(round(geo_stats$proportion, 1), "%")), 
          htmltools::tags$span(class = "divider"),
          htmltools::tags$span(class = "rank", paste0(geo_stats$rank, chr_rank)),
        )
      ),
      
      htmltools::div(class = "spark-viz", geo_spark_element_list[[geo]])
    )
    
  })
  
  htmltools::div(
    class = "viz-container geo-card",
    all_geo_items
  )
}
