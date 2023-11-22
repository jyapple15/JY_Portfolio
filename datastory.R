library(shiny)
library(shinyglide)
library(slickR)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(shinycssloaders)

# Colour scheme (23 colours needed for palette)
colour_palette <- c("#a9a9f9","#009699","maroon","red","orange","yellow","palegreen","green","lightblue","blue","purple","pink","green","lightblue","blue","purple","pink","pink","green","lightblue","blue","purple","pink")
colour_gradient <- c("#009699","royalblue","#a9a9f9") #Low to high conc
colour_bkg <- c("#a9a9f9","#009699") #contrasting w each other and w palette

# Spinner Options
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Importing Cleaned Datasets
labelled_movies <- c("Buddy","Hobbit","Machete","Mitty","Paranormal","Hunger")

# Importing Snipped Datasets
for(i in c("ms_data","screen_times")){
  assign(i,read_csv(sprintf("./src/data/cleaned/snipped_%s.csv",i)))
}

ms_data <- ms_data %>%
  pivot_longer(
    cols = matches("^\\d"),
    names_to = "cmpd",
    values_to = "conc"
  ) %>% 
  mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind])


# Labelled ms_data (Only 6 diff movies)
labelled_ms <- ms_data %>% 
  dplyr::filter(!is.na(label))
  

exp_labelled_ms <- labelled_ms %>% #To separate labels
  separate_longer_delim(
    cols = label,
    delim = "; "
  )

fear_labels <- grep("death|suspense|horror|murder|aggressive|violence|shock", 
                    distinct(exp_labelled_ms,label)$label, 
                    value=TRUE)

# Unlabelled ms_data (Excluding the buffer time before labelled movies start)
unlabelled_ms <- ms_data %>% 
  dplyr::filter(is.na(label))

# Any pics etc. need to be in www folder
ui <- fluidPage(
  shinythemes::themeSelector(),
  tags$link(rel = "stylesheet", href = "styles.css"), #In www folder
  titlePanel("Is the smell of fear real?"),
  
  glide(
    id = "description",
    controls_position = "bottom",
    previous_label = "Huh?",
    next_label = "What now?",
    
    #Screen 1
    screen(
      div(
        p("This is the 1st chunk of words that I shall potentially write, look forward to it \n\n\n\n\n heyhey"),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_1"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          sliderInput(inputId = "graph_1_rank",
                      label = "Expected Fear Rank",
                      value = c(1,25),
                      min = 1,
                      max = length(distinct(ms_data,cmpd)$cmpd),
                      step = 1,
                      ticks = FALSE),
        )
      )
    ),
    
    #Screen 2
    screen(
      div(
        p("This is the 2nd chunk of words that I shall potentially write, look forward to it \n\n\n\n\n heyhey"),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_2"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          uiOutput("select_screen_2"),
          
          checkboxInput(inputId = "graph_2_checkbox",
                        label = "Exclude zeros",
                        value = FALSE),
        )
      )
    ),
    
    #Screen 3
    screen(
      div(
        p("This is the 3rd chunk of words that I shall potentially write, look forward to it \n\n\n\n\n heyhey"),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = uiOutput("graph_3"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          radioButtons(
            inputId = "graph_3_movie",
            label = "Select a movie:",
            choices = distinct(labelled_ms,movie)$movie
          ),
          sliderInput(inputId = "graph_3_rank",
                      label = "Expected Fear Rank",
                      value = c(1,25),
                      min = 1,
                      max = length(distinct(ms_data,cmpd)$cmpd),
                      step = 1,
                      ticks = FALSE)
        )
      )
    ),
    
    #Screen 4
    screen(
      div(
        p("This is the 4th chunk of words that I shall potentially write, look forward to it \n\n\n\n\n heyhey"),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_4"),
            image = "loading_ghost.gif",
            image.width = "400px"
          ),
          uiOutput("checkbox_screen_4") #TODO
        ),
        sidebarPanel(
          radioButtons(
            inputId = "graph_4_movie",
            label = "Select a movie:",
            choices = distinct(labelled_ms,movie)$movie
          ),
          uiOutput("select_screen_4")
        )
      )
    ),
    
    #Screen 5
    screen(
      div(
        p("This is the 5th chunk of words that I shall potentially write, look forward to it \n\n\n\n\n heyhey"),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_5"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          sliderInput(inputId = "graph_5_rank",
                      label = "Expected Fear Rank",
                      value = c(1,25),
                      min = 1,
                      max = length(distinct(ms_data,cmpd)$cmpd),
                      step = 1,
                      ticks = FALSE),
        )
      )
    )
  ),
  wellPanel(
    selectInput(inputId = "whats_fear", 
                label = "\"Fear\" labels", 
                choices = distinct(exp_labelled_ms,label)$label, 
                multiple = TRUE, 
                selected = fear_labels,),
  )
)

#Tabset panel for tabs (Example 6) runExample("06_tabsets")

server <- function(input, output){
  #https://mastering-shiny.org/action-graphics.html
  
  tidied_labelled_ms <- reactive(exp_labelled_ms %>%
                                   mutate(is.fear = if_else(grepl(paste(fear_labels, collapse="|"),label), "Fear","Others")))

  #Screen 1
  graph_1 <- reactive(tidied_labelled_ms() %>% 
    group_by(cmpd) %>%
    mutate(Tconc = sum(conc)) %>%
    group_by(cmpd,is.fear) %>%
    mutate(fraction = sum(conc)/Tconc) %>%
    distinct(cmpd,is.fear,fraction) %>%
    arrange(is.fear, desc(fraction)))
  
  # For use in interactive UI and others
  fear_cmpd_ranked <- reactive(graph_1()$cmpd[1:(length(graph_1()$cmpd)/2)])
  
  # Interactive UI (Screen 2)
  output$select_screen_2 <- renderUI({
    selectInput(inputId = "graph_2_cmpd",
                label = "Choose a compound:",
                choices = fear_cmpd_ranked())
  })
  
  # Interactive UI (Screen 4)
  output$select_screen_4 <- renderUI({
    selectInput(inputId = "graph_4_cmpd",
                label = "Choose a compound:",
                choices = fear_cmpd_ranked())
  })
  
  #For later stuff
  fear_cmpd_ranked <- reactive(graph_1()$cmpd[1:(length(graph_1()$cmpd)/2)])
  
  #For user input in shiny 
  user_start_rank_1 <- reactive(input$graph_1_rank[1])
  user_end_rank_1 <- reactive(input$graph_1_rank[2])
  
  select_to_plot_1 <- reactive(graph_1()[c(user_start_rank_1():user_end_rank_1(), (length(graph_1()$cmpd)-user_start_rank_1()+1):(length(graph_1()$cmpd)-user_end_rank_1()+1)),])
  
  output$graph_1 <- renderPlot({
    select_to_plot_1() %>%
      group_by(is.fear) %>%
      mutate(cmpd = fct_reorder(as.character(cmpd), fraction)) %>%
      ggplot(aes(y = cmpd, x = fraction, fill = fct_reorder(is.fear, -fraction))) +
      geom_col() + 
      labs(title = paste("Top",user_start_rank_1(),"to",user_end_rank_1(),"Most Frequently Emitted Compounds when in Fear"),
           subtitle = "Identified by distribution across different types of scenes",
           y = "m/z of Compound", 
           x = "Fraction of the Total Concentration", 
           fill = "Type of Scene") + 
      scale_fill_manual(values=colour_palette) +
      theme(panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.title.x = element_text(margin = margin(t = 10)),
            plot.subtitle = element_text(margin = margin(b = 10))
      ) + 
      scale_x_continuous(expand = c(0, 0))
  })
  
  #Screen 2
  #For user input in shiny
  user_cmpd_2 <- reactive(input$graph_2_cmpd)
  zeros <- reactive(input$graph_2_checkbox) #FALSE means do not exclude zeros
  
  graph_2 <- reactive(tidied_labelled_ms() %>% 
                        dplyr::filter(cmpd == user_cmpd_2()))
  graph_2t <- reactive(graph_2())
  
  output$graph_2 <- renderPlot({
    if(zeros() == TRUE){
      graph_2t <- reactive(graph_2() %>% dplyr::filter(conc > 0))
    } 
    graph_2t() %>% 
      ggplot(aes(x=conc_perpax, y=label)) + 
      geom_hex() +
      labs(title = paste("Distribution of compound with m/z =",user_cmpd_2(),"across different scenes"),
           subtitle = paste(if_else(zeros() == FALSE, "Excludes", "Includes"), "data points where compound is absent"),
           y = "Type of Scene", 
           x = "Concentration per Pax per Scene") + 
      scale_fill_gradientn(colours = colour_gradient)
    })
  
  #Screen 3
  #For user input in Shiny
  user_movie_3 <- reactive(input$graph_3_movie)
  user_top_cmpd_3 <- reactive(input$graph_3_rank[1]) #A range selected by user
  user_bottom_cmpd_3 <- reactive(input$graph_3_rank[2])
  user_count_cmpd_3 <- reactive(user_bottom_cmpd_3() - user_top_cmpd_3() + 1)
  
  #Vector of selected compounds
  selected_cmpd_3 <- reactive(fear_cmpd_ranked()[user_top_cmpd_3():user_bottom_cmpd_3()])
  
  graph_3t <- reactive(labelled_ms %>% 
                         dplyr::filter(movie == user_movie_3()) %>%
                         mutate(is.fear = if_else(grepl(paste(fear_labels, collapse="|"),label), "Fear","Others")) %>%
                         dplyr::filter(cmpd %in% selected_cmpd_3()) %>% #Select top cmpd
                         group_by(counter,cmpd) %>% #To add an ave conc common to similar movies
                         mutate(average = mean(conc_perpax)) %>% 
                         group_by(cmpd) %>% 
                         mutate(sum = sum(conc)) %>% ungroup())

  # Vector of absent cmpd (Complete Absence)
  removed_cmpd <- reactive(dplyr::filter(distinct(graph_3t(), cmpd, sum), sum == 0)$cmpd)

  graph_3 <- reactive(graph_3t() %>% 
                        dplyr::filter(sum != 0) %>% 
                        distinct(counter,cmpd, .keep_all = TRUE) %>% #To remove movie duplicates
                        transform(cmpd=factor(cmpd, levels=selected_cmpd_3())) %>%
                        arrange(cmpd, desc(is.fear)))
  
  # Plot
  output$graph_3_no <- renderText({"All compounds selected were absent throughout the movie selected."})
  
  output$graph_3_yes <- renderPlot({
    graph_3() %>% #NOTE: IF PLOT CONC, A FEW COMPLETE ABSENCE
      ggplot(aes(x=counter/2, y=average, colour=fct_inorder(is.fear))) + 
      geom_point(alpha=0.5)+ 
      facet_wrap(~ cmpd, 
                 ncol = ceiling(sqrt(user_count_cmpd_3()-length(removed_cmpd()))), 
                 scales="free", 
                 labeller = as_labeller(~ paste0("No. ", user_top_cmpd_3() + which(selected_cmpd_3() %in% .x) - 1,":\n", .x))) + 
      guides(color = guide_legend(reverse = TRUE)) +
      labs(title = paste("Distribution of Top", 
                         user_top_cmpd_3(), 
                         "to", 
                         user_bottom_cmpd_3(), 
                         "Fear Compounds in", 
                         user_movie_3()),
           subtitle = "Concentrations are averaged",
           y = "Concentration per Scene", 
           x = "Duration of Movie",
           caption = str_wrap(paste(if_else(length(removed_cmpd())>1, "Plots", "Plot"),
                                    "for",
                                    if_else(length(removed_cmpd())>1, "compounds", "compound"),
                                    "with m/z =",
                                    paste(removed_cmpd(), collapse=", "), 
                                    if_else(length(removed_cmpd())>1, "have", "has"),
                                    "been removed due to absence throughout movie."), 150),
           color = "Type of Scene") + 
      theme(axis.text.y = element_text(size=5),
            axis.text.x = element_text(size=5),
            strip.text = element_text(size=6, margin=margin(t=1,b=2)),
            strip.background.x = element_rect(fill=colour_palette),
            panel.spacing = unit(1, "lines"),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, linewidth = 0.1, color = "grey"),
            plot.caption = element_text(hjust=0)) +
      scale_color_manual(values = colour_palette)
  })
  
  output$graph_3 <- renderUI({
    switch(if_else(length(graph_3()$cmpd) != 0, 1, 2),
           withSpinner(
             ui_element = plotOutput("graph_3_yes"),
             image = "loading_ghost.gif",
             image.width = "400px"),
           verbatimTextOutput("graph_3_no"))
  }) #SWITCH() https://stackoverflow.com/questions/70348671/how-to-display-outputs-based-on-radio-button-in-r-shiny
  
  # Screen 4
  #For user input in Shiny
  user_movie_4 <- reactive(input$graph_4_movie)
  user_cmpd_4 <- reactive(input$graph_4_cmpd)
  
  # Interactive UI (Screen 4) TODO Select all: https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click
  # NOTE: Mistake -> needa filter user_movie_4() --> BUT, when did that, error --> should try reactive({}) or filter through screen_times next
  user_movie_4_indices <- reactive(distinct(labelled_ms,user_movie_4(),movie_F_ind)$movie_F_ind) 
  
  ui_4_checkbox <- reactive({
    list <- tagList()
    for(i in user_movie_4_indices()){
      list <- append(list, div(
        p(paste("Screening No.:", i)),
        p(paste("Screening No.:", i)),
        p(paste("Screening No.:", i))
        ))
    }
  })
  
  output$checkbox_screen_4 <- renderUI({ 
    checkboxGroupInput(inputId = "graph_4_details",
                       label = "Select desired screenings",
                       selected = user_movie_4_indices(),
                       choiceNames = user_movie_4_indices(),
                       choiceValues = user_movie_4_indices())  
  })
  
  graph_4 <- reactive(tidied_labelled_ms() %>% arrange(is.fear) %>%
                        distinct(Time, cmpd, .keep_all = TRUE) %>% #To remove expanded labels
                        arrange(counter, cmpd) %>% 
                        dplyr::filter(cmpd == user_cmpd_4()) %>% #Select top cmpd
                        dplyr::filter(movie==user_movie_4()) %>% #Select 1 movie
                        mutate(movie_F_ind = fct_reorder2(as.character(movie_F_ind), counter, conc_perpax)) %>% 
                        arrange(movie_F_ind) %>% 
                        dplyr::filter(movie_F_ind %in% input$graph_4_details)) #JY: To filter screenings (All else run well)
  
  Others_4 <- reactive(graph_4() %>% mutate(conc_perpax=(max(conc_perpax)+min(conc_perpax))/2) %>% dplyr::filter(is.fear == "Others", movie_F_ind==graph_4()$movie_F_ind[1]))
  Fear_4 <- reactive(graph_4() %>% mutate(conc_perpax=(max(conc_perpax)+min(conc_perpax))/2) %>% dplyr::filter(is.fear == "Fear", movie_F_ind==graph_4()$movie_F_ind[1]))
  
  output$graph_4 <- renderPlot({
    graph_4() %>%
      ggplot(aes(x=counter/2, y=conc_perpax)) + 
      geom_tile(data = Fear_4(),
                height=max(graph_4()$conc_perpax)-min(graph_4()$conc_perpax),
                aes(fill="Fear"),linetype=0,alpha=0.3) +
      geom_tile(data = Others_4(), 
                height=max(graph_4()$conc_perpax)-min(graph_4()$conc_perpax), 
                aes(fill="Others"),linetype=0,alpha=0.1) +
      geom_line(aes(group = movie_F_ind, color = movie_F_ind)) +
      labs(title = paste("Distribution of compound with m/z =", 
                         user_cmpd_4(), 
                         "across duration of", 
                         user_movie_4()),
           subtitle = "Each screening is represented by a line graph.",
           y = "Concentration per Scene", 
           x = "Duration of Movie (min)",
           color = "Screening",
           fill = "Type of Scene") + 
      theme(panel.background = element_blank()) + 
      scale_fill_manual(values = colour_bkg) +
      scale_color_manual(values = colour_palette) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0))
  })
  
  # Print Screening Details (By clicking on label) (TODO)
  #graph_4 %>% distinct(movie_F_ind) %>% arrange(movie_F_ind)
  # movie A 
  #graph_4 %>% distinct(movie_F_ind) %>% arrange(movie_F_ind) # %>% ...
  # No. of people, Time of day, Date (Maybe got spoilers so less scared haha)
  
}

shinyApp(ui = ui, server = server)