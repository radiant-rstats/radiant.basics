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
  for (i in r_drop(names(cor_args))) {
    cor_args[[i]] <- input[[paste0("cor_", i)]]
  }
  cor_args
})

output$ui_cor_method <- renderUI({
  if (isTRUE(input$cor_hcor)) cor_method <- c("Pearson" = "pearson")
  selectInput(
    "cor_method", "Method:",
    choices = cor_method,
    selected = state_single("cor_method", cor_method, "pearson"),
    multiple = FALSE
  )
})

cor_sum_args <- as.list(if (exists("summary.correlation")) {
  formals(summary.correlation)
} else {
  formals(radiant.basics::summary.correlation)
})

## list of function inputs selected by user
cor_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(cor_sum_args)) {
    cor_sum_args[[i]] <- input[[paste0("cor_", i)]]
  }
  cor_sum_args
})

output$ui_cor_vars <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- varnames()
    toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
    vars <- vars[toSelect]
  })
  if (length(vars) == 0) {
    return()
  }
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

output$ui_cor_name <- renderUI({
  req(input$dataset)
  textInput("cor_name", "Store as data.frame:", "", placeholder = "Provide a name")
})

## add a spinning refresh icon if correlations need to be (re)calculated
run_refresh(cor_args, "cor", init = "vars", tabs = "tabs_correlation", label = "Calculate correlation", relabel = "Re-calculate correlations")

output$ui_correlation <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_correlation == 'Summary'",
      wellPanel(
        actionButton("cor_run", "Calculate correlation", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_correlation == 'Summary'",
        uiOutput("ui_cor_vars"),
        uiOutput("ui_cor_method"),
        checkboxInput("cor_hcor", "Adjust for {factor} variables", value = state_init("cor_hcor", FALSE)),
        conditionalPanel(
          condition = "input.cor_hcor == true",
          checkboxInput("cor_hcor_se", "Calculate adjusted p.values", value = state_init("cor_hcor_se", FALSE))
        ),
        numericInput(
          "cor_cutoff", "Correlation cutoff:",
          min = 0, max = 1, step = 0.05,
          value = state_init("cor_cutoff", 0)
        ),
        conditionalPanel(
          condition = "input.cor_hcor == false",
          checkboxInput(
            "cor_covar", "Show covariance matrix",
            value = state_init("cor_covar", FALSE)
          )
        ),
      ),
      conditionalPanel(
        condition = "input.tabs_correlation == 'Plot'",
        uiOutput("ui_cor_nrobs")
      )
    ),
    conditionalPanel(
      condition = "input.tabs_correlation == 'Summary'",
      wellPanel(
        tags$table(
          tags$td(uiOutput("ui_cor_name")),
          tags$td(actionButton("cor_store", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
        )
      )
    ),
    help_and_report(
      modal_title = "Correlation",
      fun_name = "correlation",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/correlation.md"))
    )
  )
})

observeEvent(input$cor_hcor, {
  if (input$cor_hcor == FALSE) {
    updateCheckboxInput(session, "cor_hcor_se", value = FALSE)
  } else {
    updateCheckboxInput(session, "cor_covar", value = FALSE)
  }
})

cor_plot <- reactive({
  max(2, length(input$cor_vars)) %>%
    (function(x) list(plot_width = 400 + 75 * x, plot_height = 400 + 75 * x))
})

cor_plot_width <- function() {
  cor_plot() %>%
    (function(x) if (is.list(x)) x$plot_width else 650)
}

cor_plot_height <- function() {
  cor_plot() %>%
    (function(x) if (is.list(x)) x$plot_height else 650)
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
    tabPanel(
      "Plot",
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

# .correlation <- reactive({
.correlation <- eventReactive(input$cor_run, {
  cori <- cor_inputs()
  cori$envir <- r_data
  do.call(correlation, cori)
})

.summary_correlation <- reactive({
  if (cor_available() != "available") {
    return(cor_available())
  }
  if (not_pressed(input$cor_run)) {
    return("** Press the Calculate correlation button to generate output **")
  }
  validate(
    need(
      input$cor_cutoff >= 0 && input$cor_cutoff <= 1,
      "Provide a correlation cutoff value in the range from 0 to 1"
    )
  )
  withProgress(message = "Calculating correlations", value = 0.5, {
    do.call(summary, c(list(object = .correlation()), cor_sum_inputs()))
  })
})

.plot_correlation <- reactive({
  if (cor_available() != "available") {
    return(cor_available())
  }
  if (not_pressed(input$cor_run)) {
    return("** Press the Calculate correlation button to generate output **")
  }
  req(input$cor_nrobs)
  withProgress(message = "Generating correlation plot", value = 0.5, {
    capture_plot(plot(.correlation(), nrobs = input$cor_nrobs))
  })
})

correlation_report <- function() {
  if (length(input$cor_vars) < 2) {
    return(invisible())
  }
  inp_out <- list("", "")
  nrobs <- ifelse(is.empty(input$cor_nrobs), 1000, as_integer(input$cor_nrobs))
  inp_out[[1]] <- clean_args(cor_sum_inputs(), cor_sum_args[-1])
  inp_out[[2]] <- list(nrobs = nrobs)

  if (!is.empty(input$cor_name)) {
    dataset <- fix_names(input$cor_name)
    if (input$cor_name != dataset) {
      updateTextInput(session, inputId = "cor_name", value = dataset)
    }
    xcmd <- paste0(dataset, " <- cor2df(result)\nregister(\"", dataset, "\", descr = result$descr)")
  } else {
    xcmd <- ""
  }

  update_report(
    inp_main = clean_args(cor_inputs(), cor_args),
    fun_name = "correlation",
    inp_out = inp_out,
    fig.width = cor_plot_width(),
    fig.height = cor_plot_height(),
    xcmd = xcmd
  )
}

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

observeEvent(input$cor_store, {
  req(input$cor_name)
  cmat <- try(.correlation(), silent = TRUE)
  if (inherits(cmat, "try-error") || is.null(cmat)) {
    return()
  }

  dataset <- fix_names(input$cor_name)
  updateTextInput(session, inputId = "cor_name", value = dataset)
  r_data[[dataset]] <- cor2df(cmat)
  register(dataset, descr = cmat$descr)
  updateSelectInput(session, "dataset", selected = input$dataset)

  ## See https://shiny.posit.co//reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Data Stored",
      span(
        paste0("Dataset '", dataset, "' was successfully added to the
                datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the results by clicking the
                report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "s",
      easyClose = TRUE
    )
  )
})

observeEvent(input$correlation_report, {
  r_info[["latest_screenshot"]] <- NULL
  correlation_report()
})

observeEvent(input$correlation_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_correlation_screenshot")
})

observeEvent(input$modal_correlation_screenshot, {
  correlation_report()
  removeModal() ## remove shiny modal after save
})
