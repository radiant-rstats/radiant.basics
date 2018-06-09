## alternative hypothesis options
gd_check <- c(
  "Observed" = "observed", "Expected" = "expected",
  "Chi-squared" = "chi_sq", "Deviation std." = "dev_std"
)

## list of function arguments
gd_args <- as.list(formals(goodness))

## list of function inputs selected by user
gd_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  gd_args$data_filter <- if (input$show_filter) input$data_filter else ""
  gd_args$dataset <- input$dataset
  for (i in r_drop(names(gd_args)))
    gd_args[[i]] <- input[[paste0("gd_", i)]]
  gd_args
})

###############################
# Goodness of fit test
###############################
output$ui_gd_var <- renderUI({
  vars <- c("None" = "", groupable_vars())
  selectInput(
    "gd_var", "Select a categorical variable:",
    choices = vars,
    selected = state_single("gd_var", vars),
    multiple = FALSE
  )
})

output$ui_gd_p <- renderUI({
  req(input$gd_var)
  returnTextInput(
    "gd_p", "Probabilities:",
    value = state_init("gd_p", ""),
    placeholder = "Enter probabilities (e.g., 1/2 1/2)"
  )
})

output$ui_goodness <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_goodness == 'Summary'",
        uiOutput("ui_gd_var"),
        uiOutput("ui_gd_p")
      ),
      checkboxGroupInput(
        "gd_check", NULL,
        choices = gd_check,
        selected = state_group("gd_check"),
        inline = FALSE
      )
    ),
    help_and_report(
      modal_title = "Goodness of fit",
      fun_name = "goodness",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/goodness.md"))
    )
  )
})

gd_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * max(length(input$gd_check), 1))
})

gd_plot_width <- function()
  gd_plot() %>% {if (is.list(.)) .$plot_width else 650}

gd_plot_height <- function()
  gd_plot() %>% {if (is.list(.)) .$plot_height else 400}

## output is called from the main radiant ui.R
output$goodness <- renderUI({
  register_print_output("summary_goodness", ".summary_goodness")
  register_plot_output(
    "plot_goodness", ".plot_goodness",
    height_fun = "gd_plot_height",
    width_fun = "gd_plot_width"
  )

  ## two separate tabs
  gd_output_panels <- tabsetPanel(
    id = "tabs_goodness",
    tabPanel("Summary", verbatimTextOutput("summary_goodness")),
    tabPanel(
      "Plot",
      download_link("dlp_goodness"),
      plotOutput("plot_goodness", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Tables",
    tool = "Goodness of fit",
    tool_ui = "ui_goodness",
    output_panels = gd_output_panels
  )
})

gd_available <- reactive({
  if (not_available(input$gd_var)) {
    "This analysis requires a categorical variables with two or more levels.\nIf such a variable type is not available please select another dataset.\n\n" %>% suggest_data("newspaper")
  } else {
    "available"
  }
})

.goodness <- reactive({
  do.call(goodness, gd_inputs())
})

.summary_goodness <- reactive({
  if (gd_available() != "available") return(gd_available())
  summary(.goodness(), check = input$gd_check)
})

.plot_goodness <- reactive({
  if (gd_available() != "available") return(gd_available())
  validate(need(input$gd_check, "\n\n\n           Nothing to plot. Please select a plot type"))
  withProgress(message = "Generating plots", value = 1, {
    plot(.goodness(), check = input$gd_check, shiny = TRUE)
  })
})

observeEvent(input$goodness_report, {
  if (is_empty(input$gd_var)) return(invisible())
  inp_out <- list("", "")
  if (length(input$gd_check) > 0) {
    outputs <- c("summary", "plot")
    inp_out[[1]] <- list(check = input$gd_check)
    inp_out[[2]] <- list(check = input$gd_check, custom = FALSE)
    figs <- TRUE
  } else {
    outputs <- "summary"
    inp_out[[1]] <- list(check = "")
    figs <- FALSE
  }

  update_report(
    inp_main = clean_args(gd_inputs(), gd_args),
    inp_out = inp_out,
    fun_name = "goodness",
    outputs = outputs,
    figs = figs,
    fig.width = gd_plot_width(),
    fig.height = gd_plot_height()
  )
})

download_handler(
  id = "dlp_goodness", 
  fun = download_handler_plot, 
  fn = paste0(input$dataset, "_goodness.png"),
  caption = "Download goodness of fit plot",
  plot = .plot_goodness,
  width = gd_plot_width,
  height = gd_plot_height
)
