pc_dist <- c(
  "Binomial" = "binom", "Chi-squared" = "chisq", "Discrete" = "disc",
  "Exponential" = "expo", "F" = "fdist", "Log normal" = "lnorm", 
  "Normal" = "norm", "Poisson" = "pois", "t" = "tdist", "Uniform" = "unif"
)

pc_type <- c("Values" = "values", "Probabilities" = "probs")

make_pc_values_input <- function(lb, lb_init = NA, ub, ub_init = 0) {
  if(!radiant.data::is_empty(r_state[[lb]])) ub_init <- NA
  if(!radiant.data::is_empty(r_state[[ub]])) lb_init <- NA
  tags$table(
    tags$td(numericInput(lb, "Lower bound:", value = state_init(lb, lb_init))),
    tags$td(numericInput(ub, "Upper bound:", value = state_init(ub, ub_init)))
  )
}

make_side_by_side <- function(a, b) {
  tags$table(
    tags$td(a, width="50%"),
    tags$td(b, width="50%"),
    width="100%"
  )
}

make_pc_prob_input <- function(lb, lb_init = NA, ub, ub_init = 0.95) {
  if(!radiant.data::is_empty(r_state[[lb]])) ub_init <- NA
  if(!radiant.data::is_empty(r_state[[ub]])) lb_init <- NA
  make_side_by_side(
    numericInput(
      lb, "Lower bound:", value = state_init(lb, lb_init),
      min = 0, max = 1, step = .005
    ),
    numericInput( 
      ub, "Upper bound:", value = state_init(ub, ub_init),
      min = 0, max = 1, step = .005
    )
  )
}

output$ui_pc_pois <- renderUI({
  numericInput(
    "pcp_lambda", "Lambda:",
    value = state_init("pcp_lambda", 1),
    min = 1
  )
})

output$ui_pc_input_pois <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcp_lb", lb_init = NA, "pcp_ub", ub_init = 3)
  } else {
    make_pc_prob_input("pcp_plb", lb_init = NA, "pcp_pub", ub_init = 0.95)
  }
})

output$ui_pc_expo <- renderUI({
  numericInput(
    "pce_rate", "Rate:",
    value = state_init("pce_rate", 1),
    min = 0
  )
})

output$ui_pc_input_expo <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pce_lb", lb_init = NA, "pce_ub", ub_init = 2.996)
  } else {
    make_pc_prob_input("pce_plb", lb_init = NA, "pce_pub", ub_init = 0.95)
  }
})

output$ui_pc_disc <- renderUI({
  tagList(
    returnTextInput(
      "pcd_v", "Values:",
      value = state_init("pcd_v", "1 2 3 4 5 6")
    ),
    returnTextInput(
      "pcd_p", "Probabilities:",
      value = state_init("pcd_p", "1/6")
    )
  )
})

output$ui_pc_input_disc <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcd_lb", lb_init = NA, "pcd_ub", ub_init = 3)
  } else {
    make_pc_prob_input("pcd_plb", lb_init = NA, "pcd_pub", ub_init = 0.95)
  }
})

output$ui_pc_fdist <- renderUI({
  tagList(
    numericInput(
      "pcf_df1", "Degrees of freedom 1:",
      value = state_init("pcf_df1", 10),
      min = 1
    ),
    numericInput(
      "pcf_df2", "Degrees of freedom 2:",
      value = state_init("pcf_df2", 10),
      min = 5
    )
  )
})

output$ui_pc_input_fdist <- renderUI({
  if (input$pc_type == "values")  {
    make_pc_values_input("pcf_lb", lb_init = NA, "pcf_ub", ub_init = 2.978)
  } else {
    make_pc_prob_input("pcf_plb", lb_init = NA, "pcf_pub", ub_init = 0.95)
  }
})

output$ui_pc_chisq <- renderUI({
  numericInput(
    "pcc_df", "Degrees of freedom:",
    value = state_init("pcc_df", 1),
    min = 1
  )
})

output$ui_pc_input_chisq <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcc_lb", lb_init = NA, "pcc_ub", ub_init = 3.841)
  } else {
    make_pc_prob_input("pcc_plb", lb_init = NA, "pcc_pub", ub_init = 0.95)
  }
})

output$ui_pc_tdist <- renderUI({
  numericInput(
    "pct_df", "Degrees of freedom:",
    value = state_init("pct_df", 10),
    min = 3
  )
})

output$ui_pc_input_tdist <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pct_lb", lb_init = -Inf, "pct_ub", ub_init = 2.228)
  } else {
    make_pc_prob_input("pct_plb", lb_init = 0.025, "pct_pub", ub_init = 0.975)
  }
})

output$ui_pc_norm <- renderUI({
  make_side_by_side(
    numericInput(
      "pc_mean", "Mean:",
      value = state_init("pc_mean", 0)
    ),
    numericInput(
      "pc_stdev", "St. dev:",
      min = 0,
      value = state_init("pc_stdev", 1)
    )
  )
})

output$ui_pc_input_norm <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pc_lb", lb_init = -Inf, "pc_ub", ub_init = 0)
  } else {
    make_pc_prob_input("pc_plb", lb_init = 0.025, "pc_pub", ub_init = 0.975)
  }
})

output$ui_pc_lnorm <- renderUI({
  make_side_by_side(
    numericInput(
      "pcln_meanlog", "Mean log:",
      value = state_init("pcln_meanlog", 0)
    ),
    numericInput(
      "pcln_sdlog", "St. dev log:",
      min = 0,
      value = state_init("pcln_sdlog", 1)
    )
  )
})

output$ui_pc_input_lnorm <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcln_lb", lb_init = 0, "pcln_ub", ub_init = 1)
  } else {
    make_pc_prob_input("pcln_plb", lb_init = 0.025, "pcln_pub", ub_init = 0.975)
  }
})

output$ui_pc_binom <- renderUI({
  make_side_by_side(
    numericInput(
      "pcb_n", label = "n:",
      value = state_init("pcb_n", 10), min = 0
    ),
    numericInput(
      "pcb_p", "p:",
      min = 0, max = 1, step = .005,
      value = state_init("pcb_p", .2)
    )
  )
})

output$ui_pc_input_binom <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcb_lb", lb_init = NA, "pcb_ub", ub_init = 3)
  } else {
    make_pc_prob_input("pcb_plb", lb_init = NA, "pcb_pub", ub_init = 0.3)
  }
})

output$ui_pc_unif <- renderUI({
  make_side_by_side(
    numericInput(
      "pcu_min", "Min:",
      value = state_init("pcu_min", 0)
    ),
    numericInput(
      "pcu_max", "Max:",
      value = state_init("pcu_max", 1)
    )
  )
})

output$ui_pc_input_unif <- renderUI({
  if (input$pc_type == "values") {
    make_pc_values_input("pcu_lb", lb_init = NA, "pcu_ub", ub_init = 0.3)
  } else {
    make_pc_prob_input("pcu_plb", lb_init = NA, "pcu_pub", ub_init = 0.3)
  }
})

output$ui_prob_calc <- renderUI({
  tagList(
    wellPanel(
      selectInput(
        "pc_dist", label = "Distribution:",
        choices = pc_dist,
        selected = state_init("pc_dist", "norm"),
        multiple = FALSE
      ),
      conditionalPanel(
        "input.pc_dist == 'norm'",
        uiOutput("ui_pc_norm")
      ),
      conditionalPanel(
        "input.pc_dist == 'lnorm'",
        uiOutput("ui_pc_lnorm")
      ),
      conditionalPanel(
        "input.pc_dist == 'binom'",
        uiOutput("ui_pc_binom")
      ),
      conditionalPanel(
        "input.pc_dist == 'unif'",
        uiOutput("ui_pc_unif")
      ),
      conditionalPanel(
        "input.pc_dist == 'tdist'",
        uiOutput("ui_pc_tdist")
      ),
      conditionalPanel(
        "input.pc_dist == 'fdist'",
        uiOutput("ui_pc_fdist")
      ),
      conditionalPanel(
        "input.pc_dist == 'chisq'",
        uiOutput("ui_pc_chisq")
      ),
      conditionalPanel(
        "input.pc_dist == 'disc'",
        uiOutput("ui_pc_disc")
      ),
      conditionalPanel(
        "input.pc_dist == 'expo'",
        uiOutput("ui_pc_expo")
      ),
      conditionalPanel(
        "input.pc_dist == 'pois'",
        uiOutput("ui_pc_pois")
      )
    ),
    wellPanel(
      radioButtons(
        "pc_type", "Input type:",
        choices = pc_type,
        selected = state_init("pc_type", "values"),
        inline = TRUE
      ),
      conditionalPanel(
        "input.pc_dist == 'norm'",
        uiOutput("ui_pc_input_norm")
      ),
      conditionalPanel(
        "input.pc_dist == 'lnorm'",
        uiOutput("ui_pc_input_lnorm")
      ),
      conditionalPanel(
        "input.pc_dist == 'binom'",
        uiOutput("ui_pc_input_binom")
      ),
      conditionalPanel(
        "input.pc_dist == 'unif'",
        uiOutput("ui_pc_input_unif")
      ),
      conditionalPanel(
        "input.pc_dist == 'tdist'",
        uiOutput("ui_pc_input_tdist")
      ),
      conditionalPanel(
        "input.pc_dist == 'fdist'",
        uiOutput("ui_pc_input_fdist")
      ),
      conditionalPanel(
        "input.pc_dist == 'chisq'",
        uiOutput("ui_pc_input_chisq")
      ),
      conditionalPanel(
        "input.pc_dist == 'disc'",
        uiOutput("ui_pc_input_disc")
      ),
      conditionalPanel(
        "input.pc_dist == 'expo'",
        uiOutput("ui_pc_input_expo")
      ),
      conditionalPanel(
        "input.pc_dist == 'pois'",
        uiOutput("ui_pc_input_pois")
      ),
      numericInput(
        "pc_dec", "Decimals:",
        value = state_init("pc_dec", 3),
        min = 0
      )
    ),
    help_and_report(
      modal_title = "Probability calculator",
      fun_name = "prob_calc",
      help_file = inclMD(file.path(getOption("radiant.path.basics"), "app/tools/help/prob_calc.md"))
    )
  )
})

pc_plot_width <- function()
  if (!is.null(input$viz_plot_width)) input$viz_plot_width else 650

pc_plot_height <- function() 400

pc_args <- reactive({
  pc_dist <- input$pc_dist
  if (radiant.data::is_empty(pc_dist) || pc_dist == "norm") {
    as.list(formals(prob_norm))
  } else if (pc_dist == "lnorm") {
    as.list(formals(prob_lnorm))
  } else if (pc_dist == "binom") {
    as.list(formals(prob_binom))
  } else if (pc_dist == "unif") {
    as.list(formals(prob_unif))
  } else if (pc_dist == "tdist") {
    as.list(formals(prob_tdist))
  } else if (pc_dist == "fdist") {
    as.list(formals(prob_fdist))
  } else if (pc_dist == "chisq") {
    as.list(formals(prob_chisq))
  } else if (pc_dist == "disc") {
    as.list(formals(prob_disc))
  } else if (pc_dist == "expo") {
    as.list(formals(prob_expo))
  } else if (pc_dist == "pois") {
    as.list(formals(prob_pois))
  }
})

## list of function inputs selected by user
pc_inputs <- reactive({
  pc_dist <- input$pc_dist
  if (radiant.data::is_empty(pc_dist) || pc_dist == "norm") {
    pre <- "pc_"
  } else if (pc_dist == "lnorm") {
    pre <- "pcln_"
  } else if (pc_dist == "binom") {
    pre <- "pcb_"
  } else if (pc_dist == "unif") {
    pre <- "pcu_"
  } else if (pc_dist == "tdist") {
    pre <- "pct_"
  } else if (pc_dist == "fdist") {
    pre <- "pcf_"
  } else if (pc_dist == "chisq") {
    pre <- "pcc_"
  } else if (pc_dist == "disc") {
    pre <- "pcd_"
  } else if (pc_dist == "expo") {
    pre <- "pce_"
  } else if (pc_dist == "pois") {
    pre <- "pcp_"
  }

  # loop needed because reactive values don't allow single bracket indexing
  args <- pc_args()
  for (i in names(args))
    args[[i]] <- input[[paste0(pre, i)]]

  validate(
    need(
      input$pc_dec,
      "Provide an integer value for the number of decimal places"
    )
  )

  args[["dec"]] <- input$pc_dec
  args
})

## output is called from the main radiant ui.R
output$prob_calc <- renderUI({
  register_print_output("summary_prob_calc", ".summary_prob_calc")
  register_plot_output(
    "plot_prob_calc", ".plot_prob_calc",
    height_fun = "pc_plot_height",
    width_fun = "pc_plot_width"
  )

  ## two separate tabs
  pc_output_panels <- tagList(
    tabPanel("Summary", verbatimTextOutput("summary_prob_calc")),
    tabPanel(
      "Plot",
      download_link("dlp_prob_calc"),
      plotOutput("plot_prob_calc", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Basics > Probability",
    tool = "Probability calculator",
    data = NULL,
    tool_ui = "ui_prob_calc",
    output_panels = pc_output_panels
  )
})

pc_available <- reactive({
  if (radiant.data::is_empty(input$pc_dist) || radiant.data::is_empty(input$pc_type)) {
    ""
  } else {
    a <- "available"
    if (input$pc_dist == "norm") {
      if (is_not(input$pc_mean) || is_not(input$pc_stdev) || input$pc_stdev <= 0) {
        a <- "Please provide a mean and standard deviation (> 0)"
      }
    } else if (input$pc_dist == "lnorm") {
      if (is_not(input$pcln_meanlog) || is_not(input$pcln_sdlog) || input$pcln_sdlog <= 0) {
        a <- "Please provide a mean and standard deviation (> 0)"
      }
    } else if (input$pc_dist == "binom") {
      if (is_not(input$pcb_n) || input$pcb_n < 0 || is_not(input$pcb_p) || input$pcb_p < 0) {
        a <- "Please provide a value for n (number of trials) and p (probability of success)"
      }
    } else if (input$pc_dist == "unif") {
      if (is_not(input$pcu_min) || is_not(input$pcu_max)) {
        a <- "Please provide a minimum and a maximum value"
      }
    } else if (input$pc_dist == "tdist") {
      if (is_not(input$pct_df)) {
        a <- "Please provide a value for the degrees of freedom (> 0)"
      }
    } else if (input$pc_dist == "fdist") {
      if (is_not(input$pcf_df1) || is_not(input$pcf_df2) || input$pcf_df1 < 1 || input$pcf_df2 < 5) {
        a <- "Please provide a value for Degrees of freedom 1 (> 0)\nand for Degrees of freedom 2 (> 4)"
      }
    } else if (input$pc_dist == "chisq") {
      if (is_not(input$pcc_df)) {
        a <- "Please provide a value for the degrees of freedom (> 0)"
      }
    } else if (input$pc_dist == "disc") {
      if (radiant.data::is_empty(input$pcd_v) || radiant.data::is_empty(input$pcd_p)) {
        a <- "Please provide a set of values and probabilities.\nSeparate numbers using spaces (e.g., 1/2 1/2)"
      }
    } else if (input$pc_dist == "expo") {
      if (is_not(input$pce_rate) || input$pce_rate <= 0) {
        a <- "Please provide a value for the rate (> 0)"
      }
    } else if (input$pc_dist == "pois") {
      if (is_not(input$pcp_lambda) || input$pcp_lambda <= 0) {
        a <- "Please provide a value for lambda (> 0)"
      }
    } else {
      a <- ""
    }
    a
  }
})

.prob_calc <- reactive({
  validate(
    need(pc_available() == "available", pc_available())
  )
  do.call(get(paste0("prob_", input$pc_dist)), pc_inputs())
})

.summary_prob_calc <- reactive({
  type <- if (radiant.data::is_empty(input$pc_type)) "values" else input$pc_type
  summary(.prob_calc(), type = type)
})

.plot_prob_calc <- reactive({
  req(pc_available() == "available")
  type <- if (radiant.data::is_empty(input$pc_type)) "values" else input$pc_type
  plot(.prob_calc(), type = type)
})

prob_calc_report <- function() {
  req(input$pc_dist)
  type <- input$pc_type
  inp <- pc_inputs()
  if (!is.null(type) && type == "probs") {
    inp_out <- list(type = type) %>% list(., .)
    inp[["ub"]] <- inp[["lb"]] <- NA
  } else {
    inp_out <- list("", "")
    inp[["pub"]] <- inp[["plb"]] <- NA
  }

  if (input$pc_dist == "disc") {
    inp$v <- radiant.data::make_vec(inp$v)
    inp$p <- radiant.data::make_vec(inp$p)
  }

  outputs <- c("summary", "plot")
  update_report(
    inp_main = clean_args(inp, pc_args()),
    fun_name = paste0("prob_", input$pc_dist),
    inp_out = inp_out,
    outputs = outputs,
    figs = TRUE,
    fig.width = pc_plot_width(),
    fig.height = pc_plot_height()
  )
}

download_handler(
  id = "dlp_prob_calc", 
  fun = download_handler_plot, 
  fn = function() paste0(input$pc_dist, "_prob_calc"),
  type = "png",
  caption = "Save probability calculator plot",
  plot = .plot_prob_calc,
  width = pc_plot_width,
  height = pc_plot_height
)

observeEvent(input$prob_calc_report, {
  r_info[["latest_screenshot"]] <- NULL
  prob_calc_report()
})

observeEvent(input$prob_calc_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_prob_calc_screenshot")
})

observeEvent(input$modal_prob_calc_screenshot, {
  prob_calc_report()
  removeModal() ## remove shiny modal after save
})