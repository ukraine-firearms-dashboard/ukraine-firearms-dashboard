#### RESOURCE MAIN UI ####
#### ABOUT UI ####
about_ui <- function(id) {
  ns <- NS(id)
  tagList(htmlOutput(ns("about_content"))) %>% tags$div(class = "about_content")
}

#### ABOUT SERVER ####
about_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$about_content <- renderText({
        paste(
          data %>% unlist() %>% unname() %>% paste(collapse = "<br><br>"),
          ## add small arms surver logo with link side by side with censs logo and link
          "<br><br>",
          "<div style='display: flex; justify-content: right;'>",
          "<div style='margin-right: 10px;'>",
          "<a href='https://censs.org/' target='_blank'>",
          "<img src='img/censs_logo.svg' width='100'>",
          "</a>",
          "</div>",
          "<div style='margin-left: 10px;'>",
          "<a href='https://www.smallarmssurvey.org/' target='_blank'>",
          "<img src='img/SAS-Logo-3x.png' width='100'>",
          "</a>",
          "</div>",
          "</div>"
        )
      })
    }
  )
}
