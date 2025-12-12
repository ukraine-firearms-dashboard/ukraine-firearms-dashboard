# UKR DASHBOARD ####
# APP ####

## LOAD MODULES ####
source("R/02_dashboard/00_setup.R")
source("R/02_dashboard/01_login.R")
source("R/02_dashboard/02_summary.R")
source("R/02_dashboard/03_table.R")
source("R/02_dashboard/05_about.R")
source("R/02_dashboard/06_update.R")

## UI ####
ui <- page_fixed(
  ### PARAMETERS ####
  # wait and disconnect ui
  useWaiter(),
  waiterPreloader(
    html = spin_loaders(id = 8, color = "white"),
    color = "#494949"
  ),
  waiterShowOnLoad(
    html = spin_loaders(id = 8, color = "white"),
    color = "#494949"
  ),
  disconnectMessage(
    text = "Your session has timed out. Please reload the page. \n Ваша сесія закінчилась. Будь ласка, оновіть сторінку.",
    refresh = "",
    background = "#494949",
    size = 36,
    width = "full",
    top = "center",
    colour = "white",
    overlayColour = "#494949",
    overlayOpacity = 1,
    css = "box-shadow: none;"
  ),
  # google analytics and css
  tags$head(HTML(
    "<!-- Google tag (gtag.js) --><script async src='https://www.googletagmanager.com/gtag/js?id=G-HV2EQ4DDG8'></script>
                 <script>window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'G-HV2EQ4DDG8');</script>"
  )),
  tags$head(HTML(
    '<script src="https://kit.fontawesome.com/c22d0aa69c.js" crossorigin="anonymous"></script>'
  )),
  tags$head(tags$link(
    rel = "icon",
    type = "image/png",
    href = "img/sas_bw.png"
  )),
  tags$head(
    includeCSS("www/css/00_dashboard.css"),
    includeCSS("www/css/00_dashboard_small.css"),
    includeCSS("www/css/01_resource.css")
  ),
  # bootstrap theme
  theme = bs_theme(
    bg = "#494949",
    fg = "white",
    primary = "white",
    secondary = "#4A90E2",
    success = "white",
    info = NULL,
    warning = NULL,
    danger = NULL,
    heading_font = bslib::font_google(
      "Montserrat",
      wght = "100;200;300;500;700;900",
      local = F
    ),
    base_font = bslib::font_google(
      "Montserrat",
      wght = "100;200;300;500;700;900",
      local = F
    )
  ),
  lang = 'en',
  # title
  page_navbar(
    title = "Ukraine Firearm Knowledge Portal",
    # ### SECTIONS ####
    id = "dashboard",
    nav_spacer(),
    #### LOG IN ####
    nav_panel("Authentification/Аутентифікація", login_ui("ukr_dashboard")),
    #### SUMMARY ####
    nav_panel(
      "Overview/Огляд",
      page_sidebar(
        fillable = T,
        fill = T,
        sidebar = tagList(
          firearm_side_ui("ukr_dashboard"),
          firearm_table_download_ui("ukr_dashboard")
        ),
        firearm_summary_ui("ukr_dashboard")
      )
    ),
    #### ABOUT ####
    nav_panel(
      "About/Про",
      h1("Dashboard description / Опис приладової панелі"),
      about_ui("ukr_dashboard")
    ),
    ## UPDATE ###
    nav_panel(
      "Update/Оновлення",
      update_ui('ukr_dashboard')
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  shiny::addResourcePath("img", "./www/img")
  #### LOG IN ####
  #hide  panels
  nav_hide(id = "dashboard", "Overview/Огляд")
  nav_hide(id = "dashboard", "About/Про")
  nav_hide(id = "dashboard", "Update/Оновлення")
  #### MODULES ####
  # # log in module
  login_server("ukr_dashboard", parent_session = session)
  # # summary module
  firearm_summary_server(
    "ukr_dashboard",
    firearm_table = firearm_table,
    firearm_summary_table = firearm_summary_table,
    palette_factor = palette_factor,
    palette_color = palette_color
  )
  # about module
  about_server("ukr_dashboard", data = about)
  # update module
  update_server("ukr_dashboard", parent_session = session)
}

shinyApp(ui = ui, server = server)
