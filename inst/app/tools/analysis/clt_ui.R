###############################
# Central Limit Theorem
###############################
clt_dist <- c(
  "Normal" = "Normal", 
  "Binomial" = "Binomial", 
  "Uniform" = "Uniform", 
  "Exponential" = "Exponential"
)
clt_stat <- c("Sum" = "sum", "Mean" = "mean")
clt_args <- as.list(formals(clt))

clt_inputs <- reactive({
  for (i in names(clt_args))
    clt_args[[i]] <- input[[paste0("clt_", i)]]
  clt_args
})

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(clt_args, "clt", init = "dist", label = "Run simulation", relabel = "Re-run simulation", data = FALSE)

output$ui_clt <- renderUI({
  tagList(
    wellPanel(
      actionButton("clt_run", "Run simulation", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      selectInput(
        "clt_dist", "Distribution:",
        choices = clt_dist,
        selected = state_single("clt_dist", clt_dist),
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.clt_dist == 'Uniform'",
        div(
          class = "row",
          div(
            class = "col-xs-6",
            numericInput(
              "clt_unif_min", "Min:",
              value = state_init("clt_unif_min", 0)
            )
          ),
          div(
            class = "col-xs-6",
            numericInput(
              "clt_unif_max", "Max:",
              value = state_init("clt_unif_max", 1)
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.clt_dist == 'Normal'",
        div(
          class = "row",
          div(
            class = "col-xs-6",
            numericInput(
              "clt_norm_mean", "Mean:",
              value = state_init("clt_norm_mean", 0)
            )
          ),
          div(
            class = "col-xs-6",
            numericInput(
              "clt_norm_sd", "SD:",
              value = state_init("clt_norm_sd", 1),
              min = 0.1, step = 0.1
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.clt_dist == 'Exponential'",
        numericInput(
          "clt_expo_rate", "Rate:",
          value = state_init("clt_expo_rate", 1),
          min = 1, step = 1
        )
      ),
      conditionalPanel(
        condition = "input.clt_dist == 'Binomial'",
        div(
          class = "row",
          div(
            class = "col-xs-6",
            numericInput(
              "clt_binom_size", "Size:",
              value = state_init("clt_binom_size", 10),
              min = 1, step = 1
            )
          ),
          div(
            class = "col-xs-6",
            numericInput(
              "clt_binom_prob", "Prob:",
              value = state_init("clt_binom_prob", 0.2),
              min = 0, max = 1, step = .1
            )
          )
        )
      ),
      div(
        class = "row",
        div(
          class = "col-xs-6",
          numericInput(
            "clt_n", "Sample size:",
            value = state_init("clt_n", 100),
            min = 2, step = 1
          )
        ),
        div(
          class = "col-xs-6",
          numericInput(
            "clt_m", "# of samples:",
            value = state_init("clt_m", 100),
            min = 2, step = 1
          )
        )
      ),
      sliderInput(
        "clt_bins", label = "Number of bins:",
        min = 1, max = 50, step = 1,
        value = state_init("clt_bins", 15),
      ),
      radioButtons(
        "clt_stat", NULL,
        choices = clt_stat,
        selected = state_init("clt_stat", "sum"),
        inline = TRUE
      )
    ),
    help_and_report(
      modal_title = "Central Limit Theorem", fun_name = "clt",
      help_file = inclRmd(file.path(getOption("radiant.path.basics"), "app/tools/help/clt.md"))
    )
  )
})

clt_plot_width <- function() 700
clt_plot_height <- function() 700

## output is called from the main radiant ui.R
output$clt <- renderUI({
  register_plot_output(
    "plot_clt", ".plot_clt",
    height_fun = "clt_plot_height",
    width_fun = "clt_plot_width"
  )

  ## two separate tabs
  clt_output_panels <- tagList(
    tabPanel(
      "Plot",
      download_link("dlp_clt"),
      plotOutput("plot_clt", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Probability",
    tool = "Central Limit Theorem",
    data = NULL,
    tool_ui = "ui_clt",
    output_panels = clt_output_panels
  )
})

.clt <- eventReactive(input$clt_run, {
  ## avoiding input errors
  ret <- ""
  if (is.na(input$clt_n) || input$clt_n < 2) {
    ret <- "Please choose a sample size larger than 2"
  } else if (is.na(input$clt_m) || input$clt_m < 2) {
    ret <- "Please choose 2 or more samples"
  } else if (input$clt_dist == "Uniform") {
    if (is.na(input$clt_unif_min)) {
      ret <- "Please choose a minimum value for the uniform distribution"
    } else if (is.na(input$clt_unif_max)) {
      ret <- "Please choose a maximum value for the uniform distribution"
    } else if (input$clt_unif_max <= input$clt_unif_min) {
      ret <- "The maximum value for the uniform distribution\nmust be larger than the minimum value"
    }
  } else if (input$clt_dist == "Normal") {
    if (is.na(input$clt_norm_mean)) {
      ret <- "Please choose a mean value for the normal distribution"
    } else if (is.na(input$clt_norm_sd) || input$clt_norm_sd < .001) {
      ret <- "Please choose a non-zero standard deviation for the normal distribution"
    }
  } else if (input$clt_dist == "Exponential") {
    if (is.na(input$clt_expo_rate) || input$clt_expo_rate < 1) {
      ret <- "Please choose a rate larger than 1 for the exponential distribution"
    }
  } else if (input$clt_dist == "Binomial") {
    if (is.na(input$clt_binom_size) || input$clt_binom_size < 1) {
      ret <- "Please choose a size parameter larger than 1 for the binomial distribution"
    } else if (is.na(input$clt_binom_prob) || input$clt_binom_prob < 0.01) {
      ret <- "Please choose a probability between 0 and 1 for the binomial distribution"
    }
  }
  if (is_empty(ret)) {
    do.call(clt, clt_inputs())
  } else {
    ret
  }
})

.plot_clt <- reactive({
  if (not_pressed(input$clt_run)) return("** Press the Run simulation button to simulate data **")
  clt <- .clt()
  validate(need(!is.character(clt), paste0("\n\n\n         ", clt)))
  withProgress(message = "Generating plots", value = 1, {
    plot(clt, stat = input$clt_stat, bins = input$clt_bins)
  })
})

observeEvent(input$clt_report, {
  outputs <- c("plot")
  inp_out <- list(list(stat = input$clt_stat, bins = input$clt_bins))
  inp <- clt_inputs() 
  inp3 <- inp[!grepl("_", names(inp))]
  if (input$clt_dist == "Normal") {
    inp <- c(inp3, inp[grepl("norm_", names(inp))])
  } else if (input$clt_dist == "Uniform") {
    inp <- c(inp3, inp[grepl("unif", names(inp))])
  } else if (input$clt_dist == "Binomial") {
    inp <- c(inp3, inp[grepl("binom_", names(inp))])
  } else if (input$clt_dist == "Exponential") {
    inp <- c(inp3, inp[grepl("expo_", names(inp))])
  }

  update_report(
    inp_main = clean_args(inp, clt_args),
    fun_name = "clt", 
    inp_out = inp_out,
    outputs = outputs, 
    figs = TRUE,
    fig.width = clt_plot_width(),
    fig.height = clt_plot_height()
  )
})

download_handler(
  id = "dlp_clt", 
  fun = download_handler_plot, 
  fn = function() paste0(tolower(input$clt_dist), "_clt"),
  type = "png",
  caption = "Save central limit theorem plot",
  plot = .plot_clt,
  width = clt_plot_width,
  height = clt_plot_height
)
