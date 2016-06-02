help_basic <- c("Probability calculator" = "prob_calc.md", "Central limit theorem" = "clt.md",
                "Single mean" = "single_mean.md", "Compare means" = "compare_means.md",
                "Single proportion" = "single_prop.md", "Compare proportions" = "compare_props.md",
                "Goodness of fit" = "goodness.md", "Cross-tabs" = "cross_tabs.md",
                "Correlation" = "correlation.md")

output$help_basic <- reactive(append_help("help_basic", file.path(getOption("radiant.path.basic"),"app/tools/help"), Rmd = TRUE))

observeEvent(input$help_basic_all, {help_switch(input$help_basic_all, "help_basic")})
observeEvent(input$help_basic_none,{help_switch(input$help_basic_none, "help_basic", help_on = FALSE)})

help_basic_panel <- tagList(
  wellPanel(
    HTML("<label>Basic menu: <i id='help_basic_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_basic_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_basic", NULL, help_basic,
       selected = state_init("help_basic"), inline = TRUE)
  )
)
