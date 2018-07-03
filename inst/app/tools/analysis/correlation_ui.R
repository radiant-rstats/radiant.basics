###############################
## Correlation
###############################
cor_method <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")

## list of function arguments
cor_args <- as.list(formals(correlation))

## list of function inputs selected by user
cor_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  cor_args$data_filter <- if (input$show_filter) input$data_filter else ""
  cor_args$dataset <- input$dataset
  for (i in r_drop(names(cor_args)))
    cor_args[[i]] <- input[[paste0("cor_", i)]]
  cor_args
})

cor_sum_args <- as.list(if (exists("summary.correlation")) {
  formals(summary.correlation)
} else {
  formals(radiant.basics::summary.correlation)
} )

## list of function inputs selected by user
cor_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(cor_sum_args))
    cor_sum_args[[i]] <- input[[paste0("cor_", i)]]
  cor_sum_args
})

output$ui_cor_vars <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- varnames()
    isChar <- .get_class() %in% c("character", "factor") %>%
      set_names(vars)
    tlv <- two_level_vars()
    if (length(tlv) > 0) isChar[tlv] <- FALSE
    vars <- vars[!isChar]
  })
  if (length(vars) == 0) return()
  selectInput(
    inputId = "cor_vars", label = "Select variables:",
    choices = vars,
    selected = state_multiple("cor_vars", vars, isolate(input$cor_vars)),
    multiple = TRUE,
    size = min(10, length(vars)),
    selectize = FALSE
  )
})

output$ui_cor_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "cor_nrobs", "Number of data points plotted:", 
    choices = choices,
    selected = state_single("cor_nrobs", choices, 1000)
  )
})

output$ui_correlation <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      uiOutput("ui_cor_vars"),
      selectInput(
        "cor_method", "Method:",
        choices = cor_method,
        selected = state_single("cor_method", cor_method, "pearson"),
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.tabs_correlation == 'Summary'",
        numericInput(
          "cor_cutoff", "Correlation cutoff:",
          min = 0, max = 1, step = 0.05,
          value = state_init("cor_cutoff", 0)
        ),
        checkboxInput(
          "cor_covar", "Show covariance matrix",
          value = state_init("cor_covar", FALSE)
        )
      ),
      conditionalPanel(
        condition = "input.tabs_correlation == 'Plot'",
        uiOutput("ui_cor_nrobs")
      )
    ),
    help_and_report(
      modal_title = "Correlation",
      fun_name = "correlation",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/correlation.md"))
    )
  )
})

cor_plot <- reactive({
  max(2, length(input$cor_vars)) %>% {
    list(plot_width = 400 + 75 * ., plot_height = 400 + 75 * .)
  }
})

cor_plot_width <- function()
  cor_plot() %>% {
    if (is.list(.)) .$plot_width else 650
  }

cor_plot_height <- function()
  cor_plot() %>% {
    if (is.list(.)) .$plot_height else 650
  }

## output is called from the main radiant ui.R
output$correlation <- renderUI({
  register_print_output("summary_correlation", ".summary_correlation")
  register_plot_output(
    "plot_correlation", ".plot_correlation",
    height_fun = "cor_plot_height",
    width_fun = "cor_plot_width"
  )

  ## two separate tabs
  cor_output_panels <- tabsetPanel(
    id = "tabs_correlation",
    tabPanel("Summary", verbatimTextOutput("summary_correlation")),
    tabPanel("Plot",
      download_link("dlp_correlation"),
      plotOutput(
        "plot_correlation",
        width = "100%",
        height = "100%"
      )
    )
  )

  stat_tab_panel(
    menu = "Basics > Tables",
    tool = "Correlation",
    tool_ui = "ui_correlation",
    output_panels = cor_output_panels
  )
})

cor_available <- reactive({
  if (not_available(input$cor_vars) || length(input$cor_vars) < 2) {
    return("This analysis requires two or more variables or type numeric,\ninteger,or date. If these variable types are not available\nplease select another dataset.\n\n" %>% suggest_data("salary"))
  }
  "available"
})

.correlation <- reactive({
  validate(
    need(
      input$cor_cutoff >= 0 && input$cor_cutoff <= 1,
      "Provide a correlation cutoff value in the range from 0 to 1"
    )
  )
  do.call(correlation, cor_inputs())
})

.summary_correlation <- reactive({
  if (cor_available() != "available") return(cor_available())
  do.call(summary, c(list(object = .correlation()), cor_sum_inputs()))
})

.plot_correlation <- reactive({
  if (cor_available() != "available") return(cor_available())
  req(input$cor_nrobs)
  capture_plot(plot(.correlation(), nrobs = input$cor_nrobs))
})

observeEvent(input$correlation_report, {
  if (length(input$cor_vars) < 2) return(invisible())
  inp_out <- list("", "")
  nrobs <- ifelse(is_empty(input$cor_nrobs), 1000, as_integer(input$cor_nrobs))
  inp_out[[1]] <- clean_args(cor_sum_inputs(), cor_sum_args[-1])
  inp_out[[2]] <- list(nrobs = nrobs)
  update_report(
    inp_main = clean_args(cor_inputs(), cor_args),
    fun_name = "correlation",
    inp_out = inp_out,
    fig.width = cor_plot_width(),
    fig.height = cor_plot_height()
  )
})

download_handler(
  id = "dlp_correlation", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_correlation"),
  type = "png",
  caption = "Save correlation plot",
  plot = .plot_correlation,
  width = cor_plot_width,
  height = cor_plot_height
)
