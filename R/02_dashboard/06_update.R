###
run_workflow <- function(repo, workflow) {
  gh_pat <- Sys.getenv("GITHUB_TOKEN")
  branch <- "main"
  owner <- "ukraine-firearms-dashboard"
  url <- paste0(
    "https://api.github.com/repos/",
    owner,
    "/",
    repo,
    "/actions/workflows/",
    workflow,
    "/dispatches"
  )

  body <- toJSON(
    list(
      ref = branch
    ),
    auto_unbox = TRUE
  )

  res <- POST(
    url,
    add_headers(
      Authorization = paste("Bearer", gh_pat),
      Accept = "application/vnd.github.v3+json"
    ),
    body = body,
    encode = "json"
  )

  status_txt <- if (status_code(res) == 204) {
    paste0(
      "GitHub workflow dispatched successfully! (",
      workflow,
      ") for branch ",
      branch,
      ")"
    )
  } else {
    paste0(
      "Error (",
      status_code(res),
      "): ",
      content(res, "text", encoding = "UTF-8")
    )
  }
  print(status_txt)
}


update_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      full_screen = T,
      card_body(
        'New data and text are dropped into a shared Google Drive folder each month. From the dashboard’s Update page, you trigger GitHub workflows that (1) pull the latest files from Drive, (2) rebuild the internal dataset and monthly screenshots, and (3) publish the refreshed content. The screenshots are served via Cloudflare for fast loading, and the app (hosted on Posit Cloud) automatically shows the new content after the run completes and you refresh the page.'
      )
    ),
    card(
      full_screen = T,
      card_header(h2("Text update/Оновлення тексту")),
      card_body(
        markdown(
          "Use the button below to update the dashboard text content from the Google drive. 
          This includes the resources, about and popup content. 
          The files need to be first downloaded, then changed and re-uploaded <br> 
          This process can take up to 4 minutes. Once the update is complete, please refresh the page to see the changes. <br> <br> 
          Використовуйте кнопку нижче, щоб оновити текстовий контент панелі з Google Drive. 
        Сюди входять ресурси, «Про мене» та спливаючі вікна. <br> 
        Файли потрібно спочатку завантажити, потім змінити і знову завантажити Цей процес може тривати до 4 хвилин. <br> 
        Після завершення оновлення, будь ласка, оновіть сторінку, щоб побачити зміни."
        ),
        actionButton(ns("text_update"), "Update text/Оновити текст")
      )
    ),
    card(
      full_screen = T,
      card_header(h2("Data update/Оновлення даних")),
      card_body(
        markdown(
          "Use the button below to update the dashboard data. The data needs to be stored in `ukraine.firearms.dashboard` Google drive in the folder `/data/Official withdrawal_Month_YY`. The folder needs to contains the `Official withdrawal_Month_YY.xlsx` file and a folder called `main cases` with the screenshots. <br> This process can take up to 10 minutes. Once the update is complete, please refresh the page to see the changes. <br> <br> Використовуйте кнопку нижче, щоб оновити дані інформаційної панелі. Дані мають бути збережені в Google диску `ukraine.firearms.dashboard` у папці `/data/Official withdrawal_Month_YY`. Папка повинна містити файл `Official withdrawal_Month_YY.xlsx` та папку під назвою `main cases` зі скріншотами. <br> Цей процес може зайняти до 10 хвилин. Після завершення оновлення, будь ласка, оновіть сторінку, щоб побачити зміни."
        ),
        actionButton(ns("data_update"), "Update data/Оновити дані")
      )
    )
  )
}

#### UPDATE SERVER ####
update_server <- function(id, parent_session = NULL) {
  moduleServer(
    id,
    function(input, output, parent_session) {
      observeEvent(input$text_update, {
        run_workflow(
          repo = "ukraine-firearms-dashboard",
          workflow = "update_dashboard_text.yml"
        )
        withProgress(
          message = 'Text update in progress / Оновлення тексту триває',
          detail = 'This will take approximatively 4 min/Це займе приблизно 4 хв',
          value = 0,
          {
            n <- 140L
            for (i in seq_len(n)) {
              incProgress(1 / n)
              Sys.sleep(1)
            }
          }
        )
        showNotification(
          'Text update done. Please refresh the page / Оновлення тексту завершено. Будь ласка, оновіть сторінку',
          type = "message"
        )
      })

      observeEvent(input$data_update, {
        run_workflow(
          repo = "ukraine-firearms-dashboard",
          workflow = "update_censs_content.yml"
        )
        run_workflow(
          repo = "ukraine-firearms-images",
          workflow = "update_censs_screenshot.yml"
        )
        withProgress(
          message = 'Data update in progress / Оновлення даних триває',
          detail = 'This will take approximatively 10min / Це займе приблизно 10 хв',
          value = 0,
          {
            n <- 77L
            for (i in seq_len(n)) {
              incProgress(1 / n)
              Sys.sleep(10)
            }
          }
        )
        showNotification(
          'Data update done. Please refresh the page / Оновлення даних завершено. Будь ласка, оновіть сторінку',
          type = "message"
        )
      })
    }
  )
}
