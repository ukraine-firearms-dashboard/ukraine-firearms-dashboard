# UKR DASHBOARD ####
# SUMMARY (Optimized, preserves original look & language behavior) ####

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

## UI ####
firearm_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(8, 4, 12),
      row_heights = c("81vh", "95vh"),

      # Map (dominant item color & legend like original)
      card(card_body(leafletOutput(ns("firearm_map"))), full_screen = TRUE),

      layout_column_wrap(
        width = 1,
        height = "81vh",
        heights_equal = "row",
        layout_column_wrap(
          height = "31vh",
          value_box(
            title = NULL,
            value = textOutput(ns("box_last_date")),
            showcase = icon("calendar-days", "fa-1x"),
            theme = "primary",
            showcase_layout = "left center"
          ),
          value_box(
            title = NULL,
            value = textOutput(ns("box_posts_value")),
            showcase = icon("file", "fa-1x"),
            theme = "primary",
            showcase_layout = "left center"
          ),
          value_box(
            title = NULL,
            value = textOutput(ns("box_mentions_value")),
            showcase = icon("person-rifle", "fa-1x"),
            theme = "primary",
            showcase_layout = "left center"
          )
        ),

        card(
          card_header(h3("By type / За типом")),
          card_body(plotlyOutput(ns("firearm_pie"))),
          full_screen = TRUE
        ),

        card(
          card_header(h3("Over time / За часом")),
          card_body(plotlyOutput(ns("firearm_hist"))),
          full_screen = TRUE
        ),
        fill = FALSE
      ),

      card(
        card_header(h3("Content of posts / Зміст постів")),
        card_body(
          DT::DTOutput(ns("firearm_table")),
          fillable = TRUE,
          fill = TRUE,
          min_height = "80vh"
        ),
        full_screen = TRUE
      )
    )
  ) %>%
    tags$div(class = "firearm_card")
}

## SIDE UI ####
firearm_side_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Filters / Фільтри"),
    tags$div(
      tags$div(
        "Translate posts",
        class = ".shiny-input-container .control-label"
      ),
      tags$div(
        shinyWidgets::actionBttn(
          ns("firearm_language_eng"),
          "Yes / Так",
          style = "simple",
          color = "primary",
          size = "sm"
        ),
        shinyWidgets::actionBttn(
          ns("firearm_language_ukr"),
          "No / Ні",
          style = "simple",
          color = "primary",
          size = "sm"
        ),
        class = "filter"
      )
    ),

    pickerInput(
      ns("firearm_item_filter"),
      "Weapon type / Тип зброї",
      multiple = TRUE,
      width = "100%",
      choices = setNames(choices_eng_item, gsub("[.]", "", choices_eng_item)),
      selected = character(0)
    ),

    pickerInput(
      ns("firearm_oblast_filter"),
      "Oblast / Область",
      multiple = TRUE,
      width = "100%",
      choices = setNames(
        choices_eng_oblast,
        gsub("[.]", "", choices_eng_oblast)
      ),
      selected = character(0)
    ),

    airDatepickerInput(
      ns("firearm_date_filter"),
      "Date / Дата",
      addon = "none",
      clearButton = TRUE,
      autoClose = TRUE,
      width = "100%",
      update = "close",
      dateFormat = "dd/MM/yyyy",
      range = TRUE,
      view = c("months"),
      minView = c("months"),
      value = c(lubridate::floor_date(date_min, "month"), date_max),
      minDate = date_min,
      maxDate = date_max
    ),

    tags$div(
      shinyWidgets::actionBttn(
        ns("firearm_reset_filter"),
        icon = icon("ban"),
        label = "Reset",
        style = "simple",
        color = "primary",
        size = "sm"
      ),
      class = "filter"
    )
  )
}

## SERVER ####
firearm_summary_server <- function(
  id,
  firearm_table,
  firearm_summary_table,
  palette_color,
  palette_factor
) {
  moduleServer(id, function(input, output, session) {
    ## LANGUAGE ####
    language_react <- reactiveVal("eng")
    observeEvent(input$firearm_language_eng, {
      language_react("eng")
    })
    observeEvent(input$firearm_language_ukr, {
      language_react("ukr")
    })

    # Update picker choices when language switches (preserve original UX)
    observeEvent(
      language_react(),
      {
        shinyjs::disable("firearm_language_eng")
        shinyjs::disable("firearm_language_ukr")
        shinyjs::disable("firearm_item_filter")
        shinyjs::disable("firearm_oblast_filter")

        lang <- language_react()
        updatePickerInput(
          session,
          "firearm_item_filter",
          choices = setNames(
            if (lang == "eng") choices_eng_item else choices_ukr_item,
            gsub(
              "[.]",
              "",
              if (lang == "eng") choices_eng_item else choices_ukr_item
            )
          ),
          selected = character(0)
        )
        updatePickerInput(
          session,
          "firearm_oblast_filter",
          choices = setNames(
            if (lang == "eng") choices_eng_oblast else choices_ukr_oblast,
            gsub(
              "[.]",
              "",
              if (lang == "eng") choices_eng_oblast else choices_ukr_oblast
            )
          ),
          selected = character(0)
        )

        shinyjs::enable("firearm_language_eng")
        shinyjs::enable("firearm_language_ukr")
        shinyjs::enable("firearm_item_filter")
        shinyjs::enable("firearm_oblast_filter")
      },
      ignoreInit = TRUE
    )

    observeEvent(input$firearm_reset_filter, {
      updatePickerInput(session, "firearm_item_filter", selected = character(0))
      updatePickerInput(
        session,
        "firearm_oblast_filter",
        selected = character(0)
      )
      shinyWidgets::updateAirDateInput(
        session,
        "firearm_date_filter",
        value = c(lubridate::floor_date(date_min, "month"), date_max)
      )
    })

    ## ITEM DICTIONARY (ENG<->UKR) for labels while keeping color by ENG ####
    item_dict <- reactive({
      firearm_summary_table |>
        dplyr::distinct(post_item_eng, post_item_ukr) |>
        dplyr::collect()
    })

    # helper to display item in current language but color by ENG
    display_item <- function(item_eng, lang) {
      if (lang == "ukr") {
        dict <- item_dict()
        m <- dict$post_item_ukr[match(item_eng, dict$post_item_eng)]
        m <- ifelse(is.na(m) | m == "", item_eng, m)
        gsub("[.]", "", m)
      } else {
        gsub("[.]", "", item_eng)
      }
    }

    ## ONE FILTERED DATASET (reactive; immediate render) ####
    filtered_base_raw <- reactive({
      items <- input$firearm_item_filter %||% character(0)
      oblasts <- input$firearm_oblast_filter %||% character(0)
      drange <- input$firearm_date_filter
      if (is.null(drange) || length(drange) != 2 || any(is.na(drange))) {
        drange <- c(lubridate::floor_date(date_min, "month"), date_max)
      }

      where_clauses <- c(
        sprintf(
          "post_date_month BETWEEN DATE '%s' AND DATE '%s'",
          format(as.Date(drange[1]), "%Y-%m-%d"),
          format(as.Date(drange[2]), "%Y-%m-%d")
        ),
        if (length(items)) {
          sprintf(
            "post_item IN (%s)",
            paste(sprintf("'%s'", gsub("'", "''", items)), collapse = ",")
          )
        },
        if (length(oblasts)) {
          sprintf(
            "post_oblast IN (%s)",
            paste(sprintf("'%s'", gsub("'", "''", oblasts)), collapse = ",")
          )
        }
      )

      # Pull from v_base and join UKR text columns for language switch
      base <- dplyr::tbl(
        con,
        dplyr::sql(sprintf(
          "
        SELECT post_id, post_date, post_date_month, post_item, post_oblast,
               post_link, post_author_eng, post_title_eng, post_content_eng
        FROM v_base WHERE %s
      ",
          paste(where_clauses, collapse = " AND ")
        ))
      )

      ukr_cols <- dplyr::tbl(con, "ukr_socialMedia") |>
        dplyr::select(
          post_id,
          post_author_ukr,
          post_title_ukr,
          post_content_ukr
        )

      dplyr::left_join(base, ukr_cols, by = "post_id") |> dplyr::collect()
    })

    filtered_base <- shiny::debounce(filtered_base_raw, 300)

    ## KPIs (match original intent) ####
    output$box_last_date <- renderText({
      d <- filtered_base()
      if (!nrow(d)) {
        return("—")
      }
      format(max(as.Date(d$post_date)), "%d/%m/%Y")
    })
    output$box_posts_value <- renderText({
      d <- filtered_base()
      paste0(
        format(dplyr::n_distinct(d$post_link), big.mark = ","),
        ifelse(lang == "eng", " posts", ' пости')
      )
    })
    output$box_mentions_value <- renderText({
      d <- filtered_base() |>
        distinct(post_id, post_item)
      paste0(
        format(nrow(d), big.mark = ","),
        ifelse(lang == "eng", " items", ' ітемів')
      )
    })

    # ======== VISUALS ======== #

    ## MAP (dominant item color + legend) ####
    output$firearm_map <- leaflet::renderLeaflet({
      df <- filtered_base()
      if (!nrow(df)) {
        return(
          leaflet::leaflet() |>
            leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter)
        )
      }

      # Aggregate and keep top 2 items per oblast
      top2 <- df |>
        dplyr::count(post_oblast, post_item, name = "post_mention") |>
        dplyr::group_by(post_oblast) |>
        dplyr::mutate(
          post_mention_tot = sum(post_mention),
          post_item_prop = post_mention / post_mention_tot,
          rank = dplyr::min_rank(dplyr::desc(post_item_prop))
        ) |>
        dplyr::filter(rank <= 2) |>
        dplyr::ungroup()

      # Coords
      map_coords <- firearm_summary_table |>
        dplyr::distinct(
          post_oblast_eng,
          post_oblast_latitude,
          post_oblast_longitude
        ) |>
        dplyr::rename(post_oblast = post_oblast_eng) |>
        dplyr::collect()

      # One row per oblast with columns: post_item_1/2, post_item_prop_1/2, post_mention_1/2
      map_submit <- top2 |>
        dplyr::left_join(map_coords, by = "post_oblast") |>
        tidyr::drop_na(post_oblast_latitude, post_oblast_longitude) |>
        tidyr::pivot_wider(
          id_cols = c(
            post_oblast,
            post_oblast_latitude,
            post_oblast_longitude,
            post_mention_tot
          ),
          names_from = rank,
          values_from = c(post_item, post_item_prop, post_mention),
          names_glue = "{.value}_{rank}"
        )

      # Helpers
      pal_col <- function(items_eng) {
        out <- palette_color[items_eng]
        out[is.na(out)] <- "#888888"
        unname(out)
      }
      lang <- language_react()
      # display_item() should exist in your module; it shows UKR/ENG label but we color by ENG key
      lab <- function(item_eng) display_item(item_eng, lang)

      # Radius scaling (proportional to share, clamped for readability)
      r_outer <- function(p) pmin(80, pmax(8, p * 90)) # 2nd item ring
      r_inner <- function(p) pmin(80, pmax(8, p * 90)) # Top item circle

      # Build map
      m <- leaflet::leaflet(
        map_submit,
        options = leaflet::leafletOptions(
          attributionControl = FALSE,
          minZoom = 6,
          maxZoom = 10
        )
      ) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter)

      # OUTER halo: 2nd item (only where present)

      m <- m |>
        leaflet::addCircleMarkers(
          lng = ~post_oblast_longitude,
          lat = ~post_oblast_latitude,
          radius = ~ r_outer(ifelse(
            is.na(post_item_prop_1),
            0,
            post_item_prop_1
          )),
          stroke = TRUE,
          color = "#111111",
          weight = 0.7,
          fill = TRUE,
          fillOpacity = 0.9,
          fillColor = ~ pal_col(post_item_1),
          label = ~ lapply(
            paste0(
              ifelse(lang == "eng", "<b>Oblast: </b>", "<b>Область: </b>"),
              gsub("[.]", "", post_oblast),
              "<br>",
              ifelse(
                lang == "eng",
                "<b>Top item: </b>",
                "<b>Головний пункт: </b>"
              ),
              lab(post_item_1),
              " (",
              scales::percent(
                ifelse(is.na(post_item_prop_1), 0, post_item_prop_1),
                accuracy = 1
              ),
              ")",
              if ("post_item_2" %in% names(map_submit)) {
                paste0(
                  "<br>",
                  ifelse(
                    lang == "eng",
                    "<b>2nd item: </b>",
                    "<b>2-й пункт: </b>"
                  ),
                  lab(post_item_2),
                  " (",
                  scales::percent(
                    ifelse(is.na(post_item_prop_2), 0, post_item_prop_2),
                    accuracy = 1
                  ),
                  ")"
                )
              } else {
                ""
              },
              "<br>",
              ifelse(
                lang == "eng",
                "<b>Total mentions: </b>",
                "<b>Згадок всього: </b>"
              ),
              format(post_mention_tot, big.mark = ",", scientific = FALSE)
            ),
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(
            style = list(
              "border-radius" = "0px",
              "border-color" = "transparent",
              "padding" = "1px"
            )
          ),
          group = ifelse(lang == "eng", "Top item", "2-й пункт")
        )

      # INNER circle: top item (with detailed label including both items)
      if ("post_item_2" %in% names(map_submit)) {
        m <- m |>
          leaflet::addCircleMarkers(
            lng = ~post_oblast_longitude,
            lat = ~post_oblast_latitude,
            radius = ~ r_inner(ifelse(
              is.na(post_item_prop_2),
              0,
              post_item_prop_2
            )),
            stroke = TRUE,
            color = "#222222",
            weight = 0.8,
            fill = TRUE,
            fillOpacity = 0.9,
            fillColor = ~ pal_col(post_item_2),
            group = ifelse(lang == "eng", "2nd item", "Головний пункт")
          )

        # Legend (all items used in either ring)
        used_items <- unique(na.omit(c(
          map_submit$post_item_1,
          map_submit$post_item_2
        )))
      }
      used_cols <- pal_col(used_items)
      used_labels <- lab(used_items)

      m |>
        leaflet::addLegend(
          position = "bottomright",
          colors = used_cols,
          labels = used_labels,
          opacity = 0.9,
          title = ''
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c(
            ifelse(lang == "eng", "Top weapon", "Головний пункт"),
            ifelse(lang == "eng", "2nd weapon", "2-й пункт")
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    ## HISTOGRAM (stacked monthly bars, original style) ####
    output$firearm_hist <- plotly::renderPlotly({
      df <- filtered_base() |>
        dplyr::count(post_date_month, post_item, name = "post_mention") |>
        dplyr::arrange(post_date_month, post_item)
      if (!nrow(df)) {
        return(plotly::plot_ly())
      }

      # stable order by palette
      item_levels <- intersect(names(palette_color), unique(df$post_item))
      df$post_item <- factor(df$post_item, levels = item_levels)
      cols <- unname(palette_color[levels(df$post_item)])

      lang <- language_react()
      p <- plotly::plot_ly()
      for (it in levels(df$post_item)) {
        sub <- df[df$post_item == it, , drop = FALSE]
        disp <- display_item(it, lang)
        p <- p |>
          plotly::add_bars(
            data = sub,
            x = ~post_date_month,
            y = ~post_mention,
            name = disp,
            hovertext = ~ paste0(
              "<b>",
              ifelse(lang == "eng", "Date", "Дата"),
              ":</b> ",
              format(post_date_month, "%m/%Y"),
              "<br><b>",
              ifelse(lang == "eng", "Item", "Пункт"),
              ":</b> ",
              disp,
              "<br><b>",
              ifelse(lang == "eng", "Mentions", "Згадки"),
              ":</b> ",
              post_mention
            ),
            hoverinfo = "text",
            marker = list(color = cols[match(it, levels(df$post_item))])
          )
      }

      p |>
        plotly::layout(
          barmode = "stack",
          font = list(family = "Montserrat", color = "black"),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "white",
            font = list(color = "black", family = "Montserrat", size = 10),
            align = "left"
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = TRUE),
          showlegend = F,
          legend = list(orientation = "h", x = 0, y = -0.15),
          xaxis = list(fixedrange = TRUE, title = ""),
          yaxis = list(fixedrange = TRUE, title = ""),
          paper_bgcolor = '#494949',
          plot_bgcolor = '#494949'
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    ## PIE (donut by type, original style) ####
    ## CATEGORIES (horizontal bar plot by type) ####
    output$firearm_pie <- plotly::renderPlotly({
      bar <- filtered_base() |>
        dplyr::count(post_item, name = "post_mention")

      if (!nrow(bar)) {
        return(plotly::plot_ly())
      }

      # Keep stable color mapping (by ENG key), but display labels in current language
      item_levels <- intersect(names(palette_color), unique(bar$post_item))
      bar <- bar |>
        dplyr::arrange(post_mention) |>
        dplyr::mutate(post_item = factor(post_item, levels = item_levels)) # sort big to small

      lang <- language_react()
      labels_clean <- display_item(levels(bar$post_item), lang)

      # Re-label factor for axis, preserving the same level order
      bar$post_item_lab <- factor(
        bar$post_item,
        levels = levels(bar$post_item),
        labels = labels_clean
      )
      y_order <- bar$post_item_lab

      # Compute percentages for text/hover
      total_mentions <- sum(bar$post_mention)
      bar$percent <- round(100 * bar$post_mention / total_mentions, 1)

      # Colors aligned to factor order
      cols <- unname(palette_color[levels(bar$post_item)])
      item_levels <- intersect(names(palette_color), unique(df$post_item))

      plotly::plot_ly(
        data = bar,
        x = ~post_mention,
        y = ~post_item_lab,
        type = "bar",
        orientation = "h",
        marker = list(color = cols), #
        customdata = ~percent, # pass % for hover
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          ifelse(lang == "eng", "Mentions", "Згадки"),
          ": %{x}<br>",
          ifelse(lang == "eng", "Proportion", "Частка"),
          ": %{customdata:.1f}%<extra></extra>"
        )
      ) |>
        plotly::layout(
          showlegend = FALSE, # legend not needed; labels on y-axis
          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0, autoexpand = TRUE),
          font = list(family = "Montserrat", color = "white", size = 12),
          paper_bgcolor = "#494949",
          plot_bgcolor = "#494949",
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            fixedrange = TRUE,
            showticklabels = FALSE
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = y_order, # keep sorted order
            automargin = TRUE,
            fixedrange = TRUE
          )
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    ## TABLE (server-side DT, language-aware columns & correct color badges) ####
    table_init <- reactive({
      d <- filtered_base() |>
        dplyr::arrange(post_date)

      if (!nrow(d)) {
        return(tibble::tibble(
          post_date = as.Date(character()),
          post_author = character(),
          post_title = character(),
          post_content = character(),
          post_oblast = character(),
          post_item = character(),
          post_screenshot = character(),
          post_link = character()
        ))
      }

      lang <- language_react()

      # Choose language-specific fields with ENG fallback
      post_author <- if (lang == "ukr") {
        d$post_author_ukr %||% d$post_author_eng
      } else {
        d$post_author_eng
      }
      post_title <- if (lang == "ukr") {
        d$post_title_ukr %||% d$post_title_eng
      } else {
        d$post_title_eng
      }
      post_cont <- if (lang == "ukr") {
        d$post_content_ukr %||% d$post_content_eng
      } else {
        d$post_content_eng
      }

      # item display label (UKR/ENG) and color by ENG key
      item_label <- display_item(d$post_item, lang)
      item_color <- palette_color[d$post_item]
      item_color[is.na(item_color)] <- "#888888"
      item_badge <- ifelse(
        is.na(item_label),
        NA_character_,
        paste0(
          "<span class='item_tag' style='background-color:",
          item_color,
          ";'>",
          item_label,
          "</span>"
        )
      )

      url_base <- "https://ukraine-firearms-images.ukraine-firearms-dashboard.workers.dev/"
      screenshot <- sprintf(
        "<a href='%1$s' target='_blank'><img src='%1$s' style='height:200px; max-width:300px; object-fit:cover; cursor:zoom-in; transition:transform .18s ease;' onmouseover=\"this.style.transform='scale(5)'; this.style.zIndex=1000; this.style.position='relative';\" onmouseout=\"this.style.transform='scale(1)'; this.style.zIndex=1; this.style.position='relative';\"/></a>",
        paste0(
          url_base,
          format(as.Date(d$post_date), "%Y-%m"),
          "/",
          d$post_id,
          ".png"
        )
      )

      post_link <- dplyr::case_when(
        grepl("facebook", d$post_link, ignore.case = TRUE) ~
          sprintf(
            "<a href='%s' target='_blank'>%s</a>",
            d$post_link,
            as.character(bsicons::bs_icon("facebook"))
          ),
        grepl("t.me", d$post_link, ignore.case = TRUE) ~
          sprintf(
            "<a href='%s' target='_blank'>%s</a>",
            d$post_link,
            as.character(bsicons::bs_icon("telegram"))
          ),
        TRUE ~ sprintf(
          "<a href='%s' target='_blank'>%s</a>",
          d$post_link,
          as.character(bsicons::bs_icon("globe"))
        )
      )

      tibble::tibble(
        post_date = as.Date(d$post_date),
        post_author = post_author,
        post_title = post_title,
        post_content = post_cont,
        post_oblast = gsub("[.]", "", d$post_oblast),
        post_item = item_badge,
        post_screenshot = screenshot,
        post_link = post_link
      )
    })

    output$firearm_table <- DT::renderDT(
      {
        df <- table_init()
        DT::datatable(
          df,
          rownames = FALSE,
          escape = FALSE,
          filter = "none",
          selection = "none",
          options = list(
            deferRender = TRUE,
            scroller = TRUE,
            scrollY = "80vh",
            pageLength = 10,
            ordering = FALSE,
            scrollX = TRUE
          )
        )
      },
      server = TRUE
    )

    ## DOWNLOAD (language-aware) ####
    output$download_table <- downloadHandler(
      filename = function() "ukr_media_data.xlsx",
      content = function(file) {
        x <- table_init()
        link_url <- stringr::str_match(
          x$post_link,
          'href="\\s*(.*?)\\s*" target'
        )[, 2]
        item_text <- stringr::str_replace_all(x$post_item %||% "", "<.*?>", "")
        writexl::write_xlsx(
          dplyr::tibble(
            Date = x$post_date,
            Author = x$post_author,
            Title = x$post_title,
            Content = x$post_content,
            Oblast = x$post_oblast,
            Item = item_text,
            Source = link_url
          ),
          path = file
        )
      }
    )
  })
}
