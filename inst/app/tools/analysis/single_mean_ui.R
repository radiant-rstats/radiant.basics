###############################
## Single mean - ui
###############################

## alternative hypothesis options
sm_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
sm_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

## list of function arguments
sm_args <- as.list(formals(single_mean))

## list of function inputs selected by user
sm_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  sm_args$data_filter <- if (input$show_filter) input$data_filter else ""
  sm_args$dataset <- input$dataset
  for (i in r_drop(names(sm_args)))
    sm_args[[i]] <- input[[paste0("sm_", i)]]
  sm_args
})

output$ui_sm_var <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- c("None" = "", varnames()[isNum])
  selectInput(
    inputId = "sm_var", label = "Variable (select one):",
    choices = vars, selected = state_single("sm_var", vars), multiple = FALSE
  )
})

output$ui_single_mean <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_single_mean == 'Summary'",
        uiOutput("ui_sm_var"),
        selectInput(
          inputId = "sm_alternative", label = "Alternative hypothesis:",
          choices = sm_alt,
          selected = state_single("sm_alternative", sm_alt, sm_args$alternative),
          multiple = FALSE
        ),
        sliderInput(
          "sm_conf_lev", "Confidence level:", min = 0.85, max = 0.99,
          value = state_init("sm_conf_lev", sm_args$conf_lev), step = 0.01
        ),
        numericInput(
          "sm_comp_value", "Comparison value:",
          state_init("sm_comp_value", sm_args$comp_value)
        )
      ),
      conditionalPanel(
        condition = "input.tabs_single_mean == 'Plot'",
        selectizeInput(
          inputId = "sm_plots", label = "Select plots:",
          choices = sm_plots,
          selected = state_multiple("sm_plots", sm_plots, "hist"),
          multiple = TRUE,
          options = list(placeholder = "Select plots", plugins = list("remove_button", "drag_drop"))
        )
      )
    ),
    help_and_report(
      modal_title = "Single mean",
      fun_name = "single_mean",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/single_mean.md"))
    )
  )
})

sm_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * max(length(input$sm_plots), 1))
})

sm_plot_width <- function()
  sm_plot() %>% {if (is.list(.)) .$plot_width else 650}

sm_plot_height <- function()
  sm_plot() %>% {if (is.list(.)) .$plot_height else 400}

## output is called from the main radiant ui.R
output$single_mean <- renderUI({
  register_print_output("summary_single_mean", ".summary_single_mean")
  register_plot_output(
    "plot_single_mean", ".plot_single_mean",
    height_fun = "sm_plot_height"
  )

  ## two separate tabs
  sm_output_panels <- tabsetPanel(
    id = "tabs_single_mean",
    tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
    tabPanel(
      "Plot",
      download_link("dlp_single_mean"),
      plotOutput("plot_single_mean", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Means",
    tool = "Single mean",
    tool_ui = "ui_single_mean",
    output_panels = sm_output_panels
  )
})

sm_available <- reactive({
  if (not_available(input$sm_var)) {
    "This analysis requires a variable of type numeric or interval. If none are\navailable please select another dataset.\n\n" %>% suggest_data("demand_uk")
  } else if (is.na(input$sm_comp_value)) {
    "Please choose a comparison value"
  } else {
    "available"
  } 
})

.single_mean <- reactive({
  smi <- sm_inputs()
  smi$envir <- r_data
  do.call(single_mean, smi)
})

.summary_single_mean <- reactive({
  if (sm_available() != "available") return(sm_available())
  summary(.single_mean())
})

.plot_single_mean <- reactive({
  if (sm_available() != "available") return(sm_available())
  validate(need(input$sm_plots, "\n\n\n           Nothing to plot. Please select a plot type"))
  withProgress(message = "Generating plots", value = 1, {
    plot(.single_mean(), plots = input$sm_plots, shiny = TRUE)
  })
})

single_mean_report <- function() {
  if (is.empty(input$sm_var)) return(invisible())
  if (length(input$sm_plots) == 0) {
    figs <- FALSE
    outputs <- c("summary")
    inp_out <- list("", "")
  } else {
    outputs <- c("summary", "plot")
    inp_out <- list("", list(plots = input$sm_plots, custom = FALSE))
    figs <- TRUE
  }
  update_report(
    inp_main = clean_args(sm_inputs(), sm_args),
    fun_name = "single_mean", inp_out = inp_out,
    outputs = outputs, figs = figs,
    fig.width = sm_plot_width(),
    fig.height = sm_plot_height()
  )
}

download_handler(
  id = "dlp_single_mean", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_single_mean"),
  type = "png",
  caption = "Save single mean plot",
  plot = .plot_single_mean,
  width = sm_plot_width,
  height = sm_plot_height
)

observeEvent(input$single_mean_report, {
  r_info[["latest_screenshot"]] <- NULL
  single_mean_report()
})

observeEvent(input$single_mean_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_single_mean_screenshot")
})

observeEvent(input$modal_single_mean_screenshot, {
  single_mean_report()
  removeModal() ## remove shiny modal after save
})
