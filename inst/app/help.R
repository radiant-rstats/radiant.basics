help_basics <- c("Probability calculator" = "prob_calc.md", "Central limit theorem" = "clt.md",
                "Single mean" = "single_mean.md", "Compare means" = "compare_means.md",
                "Single proportion" = "single_prop.md", "Compare proportions" = "compare_props.md",
                "Goodness of fit" = "goodness.md", "Cross-tabs" = "cross_tabs.md",
                "Correlation" = "correlation.md")

output$help_basics <- reactive(append_help("help_basics", file.path(getOption("radiant.path.basics"),"app/tools/help"), Rmd = TRUE))

observeEvent(input$help_basics_all, {help_switch(input$help_basics_all, "help_basics")})
observeEvent(input$help_basics_none,{help_switch(input$help_basics_none, "help_basics", help_on = FALSE)})

help_basics_panel <- tagList(
  wellPanel(
    HTML("<label>Basics menu: <i id='help_basics_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_basics_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_basics", NULL, help_basics,
       selected = state_group("help_basics"), inline = TRUE)
  )
)
