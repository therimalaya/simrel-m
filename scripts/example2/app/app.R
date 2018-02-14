library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(pls)
library(simrel)
source("00-function.r")
load("design.Rdata")
load("final-data.Rdata")

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("Multivariate Simulation"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, numericInput("seed", "Seed", 123, min = -9999, max = 9999, width = "100%")),
          column(6, numericInput("n", "Number of observation", 100, min = 10, width = "100%")),
          column(6, numericInput("p", "Number of predictors", 500, min = 2, width = "100%")),
          column(6, numericInput("m", "Number of responses", 5, min = 2, width = "100%")),
          column(6, textInput("q", "Number of relevant predictors", value = "250, 250", width = "100%")),
          column(6, textInput("relpos", "Position of Relevant components", value = "2; 3", width = '100%')),
          column(6, textInput("ypos", "Position of response components", value = "1, 2, 3, 4, 5", width = '100%')),
          column(6, numericInput("ntest", "Number of Test Obs.", 500, min = 10, width = "100%")),
          fluidRow(
            column(
              12, 
              column(6, sliderInput("gamma", "Decay factor of eigenvector of predictors", 
                                    value = 0.6, min = 0, max = 6, width = '100%', step = 0.01)),
              column(6, sliderInput("eta", "Decay factor of eigenvector of response", 
                                    value = 0.1, min = 0, max = 1, width = '100%', step = 0.01)))
          ),
          column(12, textInput("R2", "Coefficient of Determination", value = "0.8, 0.7", width = '100%')),
          fluidRow(
            column(
              12, 
              column(6, actionLink("simulate", "Simulate Now", icon = icon("random"), class = "btn btn-primary")),
              column(6, bookmarkButton())
            )
          )
        ),
        br(),
        fluidRow(column(12, uiOutput("avg_plot_respUI")))
      ),
      mainPanel(
        fluidRow(column(12, plotOutput("rmsep_plot"))),
        fluidRow(column(12, uiOutput("avg_plotUI"))),
        fluidRow(column(12, uiOutput("this_plotUI")))
      )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  plot_title <- reactive(paste("RMSEP plot for simulated data using seed", input$seed))
  observe({
    dgn_idx <- NULL
    design <- design %>% 
      mutate_if(is_list, map_chr, list2chr)
    rep_chr <- formatC(1:20, width = 2, flag = "0", format = "d")
    if (input$seed %in% paste0("1", rep_chr)) dgn_idx <- 1
    if (input$seed %in% paste0("2", rep_chr)) dgn_idx <- 2
    if (input$seed %in% paste0("3", rep_chr)) dgn_idx <- 3
    if (input$seed %in% paste0("4", rep_chr)) dgn_idx <- 4
    if (!is.null(dgn_idx)) {
      rep_idx <- as.integer(str_sub(formatC(input$seed, width = 4, flag = "0", format = "d"), 3, 4))
      dgn <- design %>% slice(dgn_idx)
      updateNumericInput(session, "n", value = dgn$n)
      updateNumericInput(session, "p", value = dgn$p)
      updateNumericInput(session, "m", value = dgn$m)
      updateTextInput(session, "relpos", value = dgn$relpos)
      updateTextInput(session, "ypos", value = paste(eval(parse(text = dgn$ypos)), collapse = ", "))
      updateTextInput(session, "q", value = gsub(";", ",", dgn$q))
      updateTextInput(session, "R2", value = gsub(";", ",", dgn$R2))
      updateNumericInput(session, "ntest", value = dgn$ntest)
      updateSliderInput(session, "gamma", value = dgn$gamma)
      updateSliderInput(session, "eta", value = dgn$eta)
      
      ## My Plot ----
      output$avg_plot <- renderPlot({
        plt <- function(dgn, rep) {
          myData %>% 
            filter(ErrorType == "test",  Model != "PCR") %>% 
            filter(replicate %in% rep, design %in% dgn) %>% 
            ggplot(aes(comp, RMSEP, color = Model, 
                       group = Model, fill = Model)) +
            stat_summary(fun.y = mean, geom = "line") +
            stat_summary(fun.y = mean, geom = "point", 
                         size = 1, shape = 21, color = "black", stroke = 0.2) +
            facet_grid(design + eta + relpos + R2 ~ Response, 
                       scales = 'free_y', labeller = label_both) +
            scale_x_continuous(breaks = seq(0, 10, 2)) +
            labs(x = "Components", y = "RMSEP", 
                 fill = "Method", color = "Method") +
            theme(legend.position = "top") +
            ggtitle(paste("Average of", min(rep), "-", max(rep), 
                          "replicate of design", min(dgn), "-", max(dgn), "from my analysis"))
        }
        return(plt(dgn_idx, 1:20))
      }, res = 110)
      output$avg_plotUI <- renderUI({
        fluidRow(
          fluidRow(column(12, plotOutput("avg_plot")))
        )
      })
      
      output$this_plot <- renderPlot({
        plt <- function(d_idx, r_idx) {
          myData %>% 
            filter(ErrorType == "test",  Model != "PCR") %>% 
            filter(replicate %in% r_idx, design %in% d_idx) %>% 
            ggplot(aes(comp, RMSEP, color = Model, 
                       group = Model, fill = Model)) +
            stat_summary(fun.y = mean, geom = "line") +
            stat_summary(fun.y = mean, geom = "point", 
                         size = 1, shape = 21, color = "black", stroke = 0.2) +
            facet_grid(design + eta + relpos + R2 ~ Response, 
                       scales = 'free_y', labeller = label_both) +
            scale_x_continuous(breaks = seq(0, 10, 2)) +
            labs(x = "Components", y = "RMSEP", 
                 fill = "Method", color = "Method") +
            theme(legend.position = "top") +
            ggtitle(paste("RMSEP plot for design", dgn_idx, "and replicate", rep_idx, "from my analysis"))
        }
        return(plt(dgn_idx, rep_idx))
      }, res = 110)
      output$this_plotUI <- renderUI({
        fluidRow(
          fluidRow(column(12, plotOutput("this_plot")))
        )
      })
      
      output$avg_plot_resp <- renderPlot({
        plt <- function(dgn, rep) {
          myData %>% 
            filter(ErrorType == "test",  Model != "PCR") %>% 
            filter(replicate %in% rep, design %in% dgn) %>% 
            ggplot(aes(comp, RMSEP, color = Model, 
                       group = Model, fill = Model)) +
            stat_summary(fun.y = mean, geom = "line") +
            stat_summary(fun.y = mean, geom = "point", 
                         size = 1, shape = 21, color = "black", stroke = 0.2) +
            facet_grid(design + eta + relpos + R2 ~ ., 
                       scales = 'free_y', labeller = label_both) +
            scale_x_continuous(breaks = seq(0, 10, 2)) +
            labs(x = "Components", y = "RMSEP", 
                 fill = "Method", color = "Method") +
            theme(legend.position = "top") +
            ggtitle(paste("RMSEP plot from my analysis"),
                    subtitle = paste("For design", dgn_idx, "and replicate", rep_idx, "averaged over response and replicates."))
        }
        return(plt(dgn_idx, 1:6))
      }, res = 110)
      output$avg_plot_respUI <- renderUI({
        fluidRow(
          fluidRow(column(12, plotOutput("avg_plot_resp")))
        )
      })
      
      plot_title <<- reactive(paste("RMSEP plot for design", dgn_idx, "and replicate", rep_idx, "from this simulation"))
    }
  })
  
  opts <- reactive({
    need_list <- is.numeric(parse_parm(input$ypos)) & 
      !is.list(parse_parm(input$relpos))
    lst <- list(
      n = input$n,
      p = input$p,
      q = parse_parm(input$q),
      relpos = parse_parm(input$relpos),
      gamma = input$gamma,
      eta = input$eta,
      R2 = parse_parm(input$R2),
      ypos = parse_parm(input$ypos),
      m = input$m,
      ntest = input$ntest,
      type = "multivariate"
    )
    if (need_list) {
      lst <- within(lst, {
        relpos <- list(relpos)
        ypos <- list(ypos)
      })
    } else {
      if (is.numeric(lst$ypos)) lst$ypos <- list(lst$ypos)
    }
    return(lst)
  })
  sobj <- eventReactive(input$simulate, {
    sobj <- withProgress(message = "Simulating Data", {
      set.seed(input$seed)
      do.call(simrel, opts())
    })
    return(sobj)
  })
  dta <- eventReactive(sobj(), {
    data.frame(x = I(sobj()[["X"]]), y = I(sobj()[["Y"]]))
  })
  test <- eventReactive(sobj(), {
    data.frame(x = I(sobj()[["testX"]]), y = I(sobj()[["testY"]]))
  })
  pls2_mdl <- eventReactive(dta(), {
    dta <- dta()
    mdl <- plsr(y ~ x, data = dta, ncomp = min(10, input$p, input$n))
    return(mdl)
  })
  pcr_mdl <- eventReactive(dta(), {
    dta <- dta()
    mdl <- pcr(y ~ x, data = dta, ncomp = min(10, input$p, input$n))
    return(mdl)
  })
  pls1_mdl <- eventReactive(dta(), {
    dta <- dta()
    mdl <- lapply(1:input$m, function(yi){
      dta$y <- unclass(dta$y)[, yi]
      plsr(y ~ x, data = dta, ncomp = min(10, input$p, input$n))
    })
  })
  
  output$rmsep_plot <- renderPlot({
    withProgress(message = "Computing Error", {
      pls2_err <- get_rmsep_df(pls2_mdl(), test())
      pcr_err <- get_rmsep_df(pcr_mdl(), test())
      pls1_err <- get_rmsep_df(pls1_mdl(), test())
      err_dt <- bind_rows(list(PLS2 = pls2_err, PCR = pcr_err, PLS1 = pls1_err), .id = "Method")
      names(err_dt) <- c("Method", "ErrorType", "Response", "Comp", "RMSEP")
      err_dt <- err_dt %>% 
        mutate(Comp = as.numeric(gsub("[A-Z a-z()]", "", Comp)),
               Comp = ifelse(is.na(Comp), 0, Comp))
    })
    withProgress(message = "Making Plot", {
      ggplot(err_dt %>% filter(ErrorType == "test", Method != "PCR"),
             aes(Comp, RMSEP, group = Method, color = Method)) +
        geom_point(size = 1, shape = 21, color = "black", stroke = 0.2) +
        geom_line() +
        facet_grid(.~Response, labeller = label_both) +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(0, 10, 2)) +
        ggtitle(plot_title())
    })
  }, height = 400, res = 110)
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

