## choice lists for compare means
cp_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
cp_adjust <- c("None" = "none", "Bonferroni" = "bonf")
# cp_plots <- c("Proportions" = "props", "Relative" = "counts")
cp_plots <- c("Bar" = "bar", "Dodge" = "dodge")

## list of function arguments
cp_args <- as.list(formals(compare_props))

## list of function inputs selected by user
cp_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  cp_args$data_filter <- if (input$show_filter) input$data_filter else ""
  cp_args$dataset <- input$dataset
  for (i in r_drop(names(cp_args)))
    cp_args[[i]] <- input[[paste0("cp_", i)]]
  cp_args
})

###############################
# Compare proportions
###############################
output$ui_cp_var1 <- renderUI({
  vars <- c("None" = "", groupable_vars())
  selectInput(
    "cp_var1", "Select a grouping variable:",
    choices = vars,
    selected = state_single("cp_var1", vars),
    multiple = FALSE
  )
})

output$ui_cp_var2 <- renderUI({
  vars <- two_level_vars()
  if (not_available(input$cp_var1)) return()
  if (input$cp_var1 %in% vars) vars <- vars[-which(vars == input$cp_var1)]

  vars <- c("None" = "", vars)
  selectInput(
    inputId = "cp_var2", "Variable (select one):",
    selected = state_single("cp_var2", vars),
    choices = vars,
    multiple = FALSE
  )
})

output$ui_cp_levs <- renderUI({
  if (not_available(input$cp_var2)) {
    return()
  } else {
    levs <- .get_data()[[input$cp_var2]] %>% as.factor() %>% levels()
  }

  selectInput(
    inputId = "cp_levs", "Choose level:",
    choices = levs,
    selected = state_single("cp_levs", levs),
    multiple = FALSE
  )
})

output$ui_cp_comb <- renderUI({
  if (not_available(input$cp_var1)) return()

  levs <- .get_data()[[input$cp_var1]] %>% as.factor() %>% levels()
  alevs <- .get_data()[[input$cp_var1]] %>% unique()
  levs <- levs[levs %in% alevs]

  if (length(levs) > 2) {
    cmb <- combn(levs, 2) %>% apply(2, paste, collapse = ":")
  } else {
    return()
  }

  selectizeInput(
    "cp_comb", "Choose combinations:",
    choices = cmb,
    selected = state_multiple("cp_comb", cmb, cmb[1]),
    multiple = TRUE,
    options = list(placeholder = "Evaluate all combinations", plugins = list("remove_button", "drag_drop"))
  )
})


output$ui_compare_props <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_compare_props == 'Summary'",
        uiOutput("ui_cp_var1"),
        uiOutput("ui_cp_var2"),
        uiOutput("ui_cp_levs"),
        uiOutput("ui_cp_comb"),
        selectInput(
          inputId = "cp_alternative", "Alternative hypothesis:",
          choices = cp_alt,
          selected = state_single("cp_alternative", cp_alt, cp_args$alternative)
        ),
        checkboxInput(
          "cp_show", "Show additional statistics",
          value = state_init("cp_show", FALSE)
        ),
        sliderInput(
          "cp_conf_lev", "Confidence level:",
          min = 0.85, max = 0.99, step = 0.01,
          value = state_init("cp_conf_lev", cp_args$conf_lev)
        ),
        radioButtons(
          inputId = "cp_adjust", "Multiple comp. adjustment:",
          cp_adjust,
          selected = state_init("cp_adjust", cp_args$adjust),
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.tabs_compare_props == 'Plot'",
        selectizeInput(
          inputId = "cp_plots", label = "Select plots:",
          choices = cp_plots,
          selected = state_multiple("cp_plots", cp_plots, "bar"),
          multiple = TRUE,
          options = list(placeholder = "Select plots", plugins = list("remove_button", "drag_drop"))
        )
      )
    ),
    help_and_report(
      modal_title = "Compare proportions",
      fun_name = "compare_props",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/compare_props.md"))
    )
  )
})

cp_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * max(length(input$cp_plots), 1))
})

cp_plot_width <- function()
  cp_plot() %>% {if (is.list(.)) .$plot_width else 650}

cp_plot_height <- function()
  cp_plot() %>% {if (is.list(.)) .$plot_height else 400}

# output is called from the main radiant ui.R
output$compare_props <- renderUI({
  register_print_output("summary_compare_props", ".summary_compare_props", )
  register_plot_output(
    "plot_compare_props", ".plot_compare_props",
    height_fun = "cp_plot_height"
  )

  # two separate tabs
  cp_output_panels <- tabsetPanel(
    id = "tabs_compare_props",
    tabPanel("Summary", verbatimTextOutput("summary_compare_props")),
    tabPanel(
      "Plot",
      download_link("dlp_compare_props"),
      plotOutput("plot_compare_props", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Proportions",
    tool = "Compare proportions",
    tool_ui = "ui_compare_props",
    output_panels = cp_output_panels
  )
})

cp_available <- reactive({
  if (not_available(input$cp_var1) || not_available(input$cp_var2)) {
    "This analysis requires two categorical variables. The first must have\ntwo or more levels. The second can have only two levels. If these\nvariable types are not available please select another dataset.\n\n" %>% suggest_data("titanic")
  } else if (input$cp_var1 %in% input$cp_var2) {
    " "
  } else {
    "available"
  }
})

.compare_props <- reactive({
  do.call(compare_props, cp_inputs())
})

.summary_compare_props <- reactive({
  if (cp_available() != "available") return(cp_available())
  if (input$cp_show) summary(.compare_props(), show = TRUE) else summary(.compare_props())
})

.plot_compare_props <- reactive({
  if (cp_available() != "available") return(cp_available())
  validate(need(input$cp_plots, "\n\n\n           Nothing to plot. Please select a plot type"))
  withProgress(message = "Generating plots", value = 1, {
    plot(.compare_props(), plots = input$cp_plots, shiny = TRUE)
  })
})

observeEvent(input$compare_props_report, {
  if (is_empty(input$cp_var1) || is_empty(input$cp_var2)) return(invisible())
  figs <- FALSE
  outputs <- c("summary")
  inp_out <- list(list(show = input$cp_show), "")
  if (length(input$cp_plots) > 0) {
    outputs <- c("summary", "plot")
    inp_out[[2]] <- list(plots = input$cp_plots, custom = FALSE)
    figs <- TRUE
  }

  update_report(
    inp_main = clean_args(cp_inputs(), cp_args),
    fun_name = "compare_props",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = cp_plot_width(),
    fig.height = cp_plot_height()
  )
})

download_handler(
  id = "dlp_compare_props", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_compare_props"),
  type = "png",
  caption = "Save compare proportions plot",
  plot = .plot_compare_props,
  width = cp_plot_width,
  height = cp_plot_height
)
