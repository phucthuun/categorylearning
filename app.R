# rm(list=ls())

{
  require(shinydashboard)
  
  library(shiny)
  library(shinyjs)
  library(DT)
  library(shinydashboard)
  library(shinyThings)
  library(shinythemes)
  library(bslib)
  library(shinycssloaders)
  library(shinyWidgets)
  
  library(dplyr)
  library(tidyverse)
  library(tidyr)
  library(reshape2)
  library(readxl)
  
  library(ggraph)
  library(plotly)
  
  
  library(ggplot2)
  library(lemon)
  library(ggpubr)
  library(plotly)
  library(cowplot)
  library(grid)
  
}

{ # retrieve palettes
  # 12 areas
  Palette_AreaAbs <- c('#363285', '#1e9dca', '#9fd6e5', '#da3893', '#f6897e', '#e32526',
                       '#036b2d', '#3bb54f', '#90f3ab', '#f2de42', '#fd9727', '#8e5412')
  }


{ # retrieve data
  
  df <- read.csv("sharingbystimlevelSHINY.csv")%>%
    mutate(Pattern=case_when(count=='1=unique'~Pattern,
                             count!='1=unique'~'shared'),
           AreaAbs = factor(AreaAbs, levels =  c('V1','TO','AT','PF[L]','PM[L]','M1[L]','A1','AB','PB','PF[i]','PM[i]','M1[i]')))
  
  
  
  a <- t(array(1:625, dim=c(25,25))) %>% as.data.frame() 
  names(a) <- c(1:25)
  a <- a %>%
    rownames_to_column(var = "y")
  b <- melt(a, id.vars = "y", variable.name = "x", value.name = "CAcell") %>%
    mutate(x = as.numeric(x),  y = as.numeric(y))
  
  df <- left_join(df,b,by="CAcell")
  
  rm(a,b)
  
}

# ui ----
ui = navbarPage(id = 'wholePage',
                # App Title
                # title=div(img(src="LOGO_BLL_long_high-removebg-preview.png", height = "110px",width = "372px")),
                title = '',
                theme = shinytheme("cosmo"),
                
                ## ABOUT ----
                tabPanel('ABOUT', id='about',
                         # WHAT
                         fluidRow(
                           column(3),
                           column(6, align="center", 
                                  includeHTML(("intro_text.html")),
                                  hr(),
                                  
                                  img(src='426_2021_1591_Fig2_HTML.png', style='height: 488px; width: 726px'),
                                  p("Image taken from Henningsen-Schomers"),
                                  hr(),
                                  h2(strong(style="text-align: center;", 'Design')),
                                  p(style="text-align: left;","A. TRAINING. The neural network model will be trained on 10 categories, each containing 3 training instances. The training instances of the same category will be labelled either by the same category label or by their distinct proper names."),
                                  p(style="text-align: left;","B. TEST. For each of the 10 categories we will present to the neural network 3 training instances together with 3 novel instances."),
                                  img(src='Design.jpg', style='height: 313px; width: 1089px'),
                                  
                                  
                                  
                                  
                                  includeHTML(("end_text.html"))
                                  ),
                           column(3)
                         )),
                
                ## EXPLORE ----
                tabPanel('EXPLORE',
                         fluidRow(class='explore',
                           column(1), 
                           column(10, 
                                  p(),
                                  
                                  dashboardPage(
                                    dashboardHeader(disable=T),
                                    dashboardSidebar(disable=T,collapsed=T,width=0.00001),
                                    dashboardBody(
                                      
                                      fluidRow(class='exploreSearch',
                                        column(3),
                                        column(6,
                                          box(title = NULL, solidHeader = TRUE, status = 'warning', width = NULL,
                                              includeHTML(("explore_text.html")),
                                              # selectizeInput("model_id", "Select model", choices = unique(df$Model), multiple = F),
                                            sliderTextInput("model_id","Select model" , 
                                                            choices = unique(df$Model), 
                                                                        # selected = c("January", "February", "March", "April"), #if you want any default values 
                                                                        animate = FALSE, grid = FALSE, 
                                                                        hide_min_max = FALSE, from_fixed = FALSE,
                                                                        to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                        to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                                        post = NULL, dragRange = TRUE),
                                            sliderTextInput("concept_id","Select category" , 
                                                            choices = unique(df$Concept), 
                                                            # selected = c("January", "February", "March", "April"), #if you want any default values 
                                                            animate = FALSE, grid = FALSE, 
                                                            hide_min_max = FALSE, from_fixed = FALSE,
                                                            to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                            to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                                            post = NULL, dragRange = TRUE),
                                            # selectizeInput("concept_id", "Select category", choices = unique(df$Concept), multiple = F)
                                            )),
                                        column(3)
                                        ),
                                      
                                      
                                      ### Plot CL ====
                                      fluidRow(
                                        box(title = NULL, solidHeader = T, status = 'warning', width = NULL,
                                            column(6,
                                                   strong("CATEGORY LABEL", style = "color: #428BCA"),
                                                   plotOutput("illustration_CL")%>% withSpinner(type=4,color="#428BCA")),
                                            column(6,
                                                   strong("PROPER NAME", style = "color: #428BCA"),
                                                   plotOutput("illustration_PN")%>% withSpinner(type=4,color="#428BCA"))
                                            )
                                      )
                                      )
                                    )
                                  ),
                           
                           column(1)
                           )
                         ),
                  tags$head(
                  tags$style(
                    HTML('
            #about {background-color: #DEDEDE;}
            .navbar {background-color: #428BCA !important;}
            .navbar-default .navbar-brand{background-color: #428BCA; color: white;}
            .navbar-default .navbar-nav > .active > a, 
            .navbar-default .navbar-nav > .active > a:focus,
            .navbar-default .navbar-nav > .active > a:hover {background-color: #00548D;}
            
            .navbar-header { width:100% }
            .navbar-nav > li > a, 
            .navbar-brand {padding-top:4px !important; padding-bottom:0px !important; height: 60px; width: 100%; text-align: right;font-size: 20px;  }
            
            .skin-blue .main-sidebar {background-color:  #eeeeee;}
            .overview{background-color: #eeeeee;}
            .explore{background-color: #eeeeee;}
            
            /* body */
            .content-wrapper, .right-side {background-color: #eeeeee;}
            .content-wrapper, .left-side {background-color: #eeeeee;}
            .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #eeeeee;
                        }
             
            .overview .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .overview .box.box-solid{background: #ffffff; border-bottom-color:#428BCA; border-left-color:#428BCA; border-right-color:#428BCA; border-top-color:#428BCA;}
            
            .explore .box.box-solid.box-warning>.box-header {color:#000000; background:#ffff;}
            .explore .box.box-solid{background: #ffffff; border-bottom-color:#428BCA; border-left-color:#428BCA; border-right-color:#428BCA; border-top-color:#428BCA;}
            
            .exploreSearch .box.box-solid{background: #ffffff !important; border-bottom-color:#ffffff !important; border-left-color:#ffffff !important; border-right-color:#ffffff !important; border-top-color:#ffffff !important;}
            
            
            /* collapse button*/
            .fa, .fas {color: black;}
            
            /*notification/
            .shiny-notification-error {background-color:#428BCA; color: #ffffff} 
            
                 ')))
                
                
                
)


# server ----
server <- function(input, output, session) { 
  
  # Preprocess outputs----
  # Make reactive to store filter criteria
  
  # model_id input
  input.model_id <- reactive({input$model_id})

  # concept_id input
  input.concept_id <- reactive({input$concept_id})
  
  # Make binary df
  df.new.plot <- reactive({ 
    
    df %>% filter(Model == input.model_id() & Concept == input.concept_id())
    
  })
  
  
  
  
  ## PN ----
  ### Display output
  
  # Plot
  output$illustration_PN <- renderPlot({
    
    # CL
    pB=ggplot(df.new.plot() %>% filter(Type=='Proper Name'))+
      geom_point(aes(x=x,y=y, color=Pattern, text=Pattern), size = 2.5)+
      scale_color_manual(name = 'CAcells', values = c('#FF6F91','#1CB755','#569CFF','black'), drop=F)+
      scale_y_reverse(minor_breaks = seq(0 , 25, 1), breaks = seq(0, 25, 1))+
      scale_x_continuous(minor_breaks = seq(0 , 25, 1), breaks = seq(0, 25, 1))+
      facet_wrap(~AreaAbs, nrow = 2, labeller=label_parsed)+
      theme(aspect.ratio = 1,
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=25), 
            legend.text=element_text(size=20),
            strip.text.x = element_text(size = 15),
            strip.text.y = element_text(size = 15))
    
    gB <- ggplot_gtable(ggplot_build(pB))
    stripr <- which(grepl('strip-t', gB$layout$name))
    fills <- Palette_AreaAbs
    k <- 1
    for (i in stripr) {
      j <- which(grepl('rect', gB$grobs[[i]]$grobs[[1]]$childrenOrder))
      gB$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }
    
    grid.draw(gB)
    
    
  })
  
  
  
  
  ## CL ----
  output$illustration_CL <- renderPlot({
    
    # CL
    pA=ggplot(df.new.plot() %>% filter(Type=='Category Label'))+
      geom_point(aes(x=x,y=y, color=Pattern, text=Pattern), size = 2.5)+
      scale_color_manual(name = 'CAcells', values = c('#FF6F91','#1CB755','#569CFF','black'), drop=F)+
      scale_y_reverse(minor_breaks = seq(0 , 25, 1), breaks = seq(0, 25, 1))+
      scale_x_continuous(minor_breaks = seq(0 , 25, 1), breaks = seq(0, 25, 1))+
      facet_wrap(~AreaAbs, nrow = 2, labeller=label_parsed)+
      theme(aspect.ratio = 1,
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=25), 
            legend.text=element_text(size=20),
            strip.text.x = element_text(size = 15),
            strip.text.y = element_text(size = 15))
    
    gA <- ggplot_gtable(ggplot_build(pA))
    stripr <- which(grepl('strip-t', gA$layout$name))
    fills <- Palette_AreaAbs
    k <- 1
    for (i in stripr) {
      j <- which(grepl('rect', gA$grobs[[i]]$grobs[[1]]$childrenOrder))
      gA$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }
    
    grid.draw(gA)
    
  })
  
  
}


shinyApp(ui, server)
