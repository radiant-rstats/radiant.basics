###############################
# Single proportion - ui
###############################

## alternative hypothesis options
sp_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
sp_plots <- c("Bar" = "bar", "Simulate" = "simulate")

## list of function arguments
sp_args <- as.list(formals(single_prop))

## list of function inputs selected by user
sp_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  sp_args$data_filter <- if (input$show_filter) input$data_filter else ""
  sp_args$dataset <- input$dataset
  for (i in r_drop(names(sp_args)))
    sp_args[[i]] <- input[[paste0("sp_", i)]]
  sp_args
})

output$ui_sp_var <- renderUI({
  vars <- c("None" = "", groupable_vars())
  selectInput(
    inputId = "sp_var", label = "Variable (select one):",
    choices = vars,
    selected = state_single("sp_var", vars),
    multiple = FALSE
  )
})

output$up_sp_lev <- renderUI({
  req(available(input$sp_var))
  levs <- .get_data()[[input$sp_var]] %>% as.factor() %>% levels()

  selectInput(
    "sp_lev", "Choose level:",
    choices = levs,
    selected = state_single("sp_lev", levs),
    multiple = FALSE
  )
})

output$ui_single_prop <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_single_prop == 'Summary'",
        uiOutput("ui_sp_var"),
        uiOutput("up_sp_lev"),
        selectInput(
          "sp_alternative", "Alternative hypothesis:",
          choices = sp_alt,
          selected = state_single("sp_alternative", sp_alt, sp_args$alternative),
          multiple = FALSE
        ),
        sliderInput(
          "sp_conf_lev", "Confidence level:",
          min = 0.85, max = 0.99, step = 0.01,
          value = state_init("sp_conf_lev", sp_args$conf_lev)
        ),
        numericInput(
          "sp_comp_value", "Comparison value:",
          value = state_init("sp_comp_value", sp_args$comp_value),
          min = 0.01, max = 0.99, step = 0.01
        )
        # radioButtons("sp_type", label = "Test:", c("Binomial" = "binom", "Chi-square" = "chisq"),
        #     selected = state_init("sp_type", "binom"),
        #     inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.tabs_single_prop == 'Plot'",
        selectizeInput(
          "sp_plots", "Select plots:",
          choices = sp_plots,
          selected = state_multiple("sp_plots", sp_plots, "bar"),
          multiple = TRUE,
          options = list(placeholder = "Select plots", plugins = list("remove_button", "drag_drop"))
        )
      )
    ),
    help_and_report(
      modal_title = "Single proportion",
      fun_name = "single_prop",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/single_prop.md"))
    )
  )
})

sp_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * max(length(input$sp_plots), 1))
})

sp_plot_width <- function()
  sp_plot() %>% {if (is.list(.)) .$plot_width else 650}

sp_plot_height <- function()
  sp_plot() %>% {if (is.list(.)) .$plot_height else 400}

## output is called from the main radiant ui.R
output$single_prop <- renderUI({
  register_print_output("summary_single_prop", ".summary_single_prop")
  register_plot_output(
    "plot_single_prop", ".plot_single_prop",
    height_fun = "sp_plot_height"
  )

  ## two separate tabs
  sp_output_panels <- tabsetPanel(
    id = "tabs_single_prop",
    tabPanel("Summary", verbatimTextOutput("summary_single_prop")),
    tabPanel(
      "Plot",
      download_link("dlp_single_prop"),
      plotOutput("plot_single_prop", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Proportions",
    tool = "Single proportion",
    tool_ui = "ui_single_prop",
    output_panels = sp_output_panels
  )
})

sp_available <- reactive({
  if (not_available(input$sp_var)) {
    "This analysis requires a categorical variable. In none are available\nplease select another dataset.\n\n" %>% suggest_data("consider")
  } else if (input$sp_comp_value %>% {is.na(.) | . > 1 | . <= 0}) {
    "Please choose a comparison value between 0 and 1"
  } else {
    "available"
  } 
})

.single_prop <- reactive({
  do.call(single_prop, sp_inputs())
})

.summary_single_prop <- reactive({
  if (sp_available() != "available") return(sp_available())
  summary(.single_prop())
})

.plot_single_prop <- reactive({
  if (sp_available() != "available") return(sp_available())
  validate(need(input$sp_plots, "\n\n\n           Nothing to plot. Please select a plot type"))
  withProgress(message = "Generating plots", value = 1, {
    plot(.single_prop(), plots = input$sp_plots, shiny = TRUE)
  })
})

observeEvent(input$single_prop_report, {
  if (is_empty(input$sp_var)) return(invisible())
  if (length(input$sp_plots) == 0) {
    figs <- FALSE
    outputs <- c("summary")
    inp_out <- list("", "")
  } else {
    outputs <- c("summary", "plot")
    inp_out <- list("", list(plots = input$sp_plots, custom = FALSE))
    figs <- TRUE
  }
  update_report(
    inp_main = clean_args(sp_inputs(), sp_args),
    fun_name = "single_prop", inp_out = inp_out,
    outputs = outputs, figs = figs,
    fig.width = sp_plot_width(),
    fig.height = sp_plot_height()
  )
})

download_handler(
  id = "dlp_single_prop", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_single_prop"),
  type = "png",
  caption = "Save single proportion plot",
  plot = .plot_single_prop,
  width = sp_plot_width,
  height = sp_plot_height
)
