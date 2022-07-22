## alternative hypothesis options
ct_check <- c(
  "Observed" = "observed", "Expected" = "expected",
  "Chi-squared" = "chi_sq", "Deviation std." = "dev_std",
  "Row percentages" = "row_perc",
  "Column percentages" = "col_perc",
  "Table percentages" = "perc"
)

## list of function arguments
ct_args <- as.list(formals(cross_tabs))

## list of function inputs selected by user
ct_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ct_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ct_args$dataset <- input$dataset
  for (i in r_drop(names(ct_args)))
    ct_args[[i]] <- input[[paste0("ct_", i)]]
  ct_args
})

###############################
# Cross-tabs
###############################
output$ui_ct_var1 <- renderUI({
  vars <- c("None" = "", groupable_vars())
  selectInput(
    inputId = "ct_var1", label = "Select a categorical variable:",
    choices = vars, selected = state_single("ct_var1", vars), multiple = FALSE
  )
})

output$ui_ct_var2 <- renderUI({
  if (not_available(input$ct_var1)) return()
  vars <- c("None" = "", groupable_vars())

  if (length(vars) > 0) vars <- vars[-which(vars == input$ct_var1)]
  selectInput(
    inputId = "ct_var2", label = "Select a categorical variable:",
    selected = state_single("ct_var2", vars),
    choices = vars, multiple = FALSE
  )
})

output$ui_cross_tabs <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_cross_tabs == 'Summary'",
        uiOutput("ui_ct_var1"),
        uiOutput("ui_ct_var2")
      ),
      checkboxGroupInput(
        "ct_check", NULL,
        choices = ct_check,
        selected = state_group("ct_check"),
        inline = FALSE
      )
    ),
    help_and_report(
      modal_title = "Cross-tabs",
      fun_name = "cross_tabs",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/cross_tabs.md"))
    )
  )
})

ct_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * max(length(input$ct_check), 1))
})

ct_plot_width <- function()
  ct_plot() %>% {if (is.list(.)) .$plot_width else 650}

ct_plot_height <- function()
  ct_plot() %>% {if (is.list(.)) .$plot_height else 400}

## output is called from the main radiant ui.R
output$cross_tabs <- renderUI({
  register_print_output("summary_cross_tabs", ".summary_cross_tabs")
  register_plot_output(
    "plot_cross_tabs", ".plot_cross_tabs",
    height_fun = "ct_plot_height",
    width_fun = "ct_plot_width"
  )

  ## two separate tabs
  ct_output_panels <- tabsetPanel(
    id = "tabs_cross_tabs",
    tabPanel("Summary", verbatimTextOutput("summary_cross_tabs")),
    tabPanel(
      "Plot",
      download_link("dlp_cross_tabs"),
      plotOutput("plot_cross_tabs", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Tables",
    tool = "Cross-tabs",
    tool_ui = "ui_cross_tabs",
    output_panels = ct_output_panels
  )
})

ct_available <- reactive({
  if (not_available(input$ct_var1) || not_available(input$ct_var2)) {
    "This analysis requires two categorical variables. Both must have two or more levels.\nIf these variable types are not available please select another dataset.\n\n" %>% 
      suggest_data("newspaper")
  } else {
    "available"
  }
})

.cross_tabs <- reactive({
  cti <- ct_inputs()
  cti$envir <- r_data
  do.call(cross_tabs, cti)
})

.summary_cross_tabs <- reactive({
  if (ct_available() != "available") return(ct_available())
  summary(.cross_tabs(), check = input$ct_check)
})

.plot_cross_tabs <- reactive({
  if (ct_available() != "available") return(ct_available())
  validate(need(input$ct_check, "\n\n\n           Nothing to plot. Please select a plot type"))
  withProgress(message = "Generating plots", value = 1, {
    plot(.cross_tabs(), check = input$ct_check, shiny = TRUE)
  })
})

cross_tabs_report <- function() {
  if (radiant.data::is_empty(input$ct_var1) || radiant.data::is_empty(input$ct_var2)) return(invisible())
  inp_out <- list("", "")
  if (length(input$ct_check) > 0) {
    outputs <- c("summary", "plot")
    inp_out[[1]] <- list(check = input$ct_check)
    inp_out[[2]] <- list(check = input$ct_check, custom = FALSE)
    figs <- TRUE
  } else {
    outputs <- "summary"
    inp_out[[1]] <- list(check = "")
    figs <- FALSE
  }

  update_report(
    inp_main = clean_args(ct_inputs(), ct_args),
    inp_out = inp_out,
    fun_name = "cross_tabs",
    outputs = outputs,
    figs = figs,
    fig.width = ct_plot_width(),
    fig.height = ct_plot_height()
  )
}

download_handler(
  id = "dlp_cross_tabs", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_cross_tabs"),
  type = "png",
  caption = "Save cross-tabs plot",
  plot = .plot_cross_tabs,
  width = ct_plot_width,
  height = ct_plot_height
)

observeEvent(input$cross_tabs_report, {
  r_info[["latest_screenshot"]] <- NULL
  cross_tabs_report()
})

observeEvent(input$cross_tabs_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_cross_tabs_screenshot")
})

observeEvent(input$modal_cross_tabs_screenshot, {
  cross_tabs_report()
  removeModal() ## remove shiny modal after save
})