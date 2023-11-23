library(shiny)
library(shinyglide)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(shinycssloaders)
library(rlist)

# Colour scheme (23 colours needed for palette)
colour_palette <- c("#a9a9f9","#009699","maroon","red","orange","yellow","palegreen","green","lightblue","blue","purple","pink","green","lightblue","blue","purple","pink","pink","green","lightblue","blue","purple","pink")
colour_gradient <- c("#009699","royalblue","#a9a9f9") #Low to high conc
colour_bkg <- c("#a9a9f9","#009699") #contrasting w each other and w palette

# Spinner Options
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Importing Snipped Datasets
for(i in c("ms_data","screen_times","label_set")){
  assign(i,read_csv(sprintf("./src/data/cleaned/snipped_%s.csv",i)))
}

fear_labels <- grep("death|suspense|horror|murder|aggressive|violence|shock", 
                    label_set$label, 
                    value=TRUE)

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
                      max = length(select(ms_data, matches("^\\d"))),
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
            choices = distinct(dplyr::filter(ms_data,!is.na(label)),movie)$movie
          ),
          sliderInput(inputId = "graph_3_rank",
                      label = "Expected Fear Rank",
                      value = c(1,25),
                      min = 1,
                      max = length(select(ms_data, matches("^\\d"))),
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
            choices = distinct(dplyr::filter(ms_data,!is.na(label)),movie)$movie
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
            ui_element = plotOutput("graph_5a"),
            image = "loading_ghost.gif",
            image.width = "400px"
          ),
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_5b"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          uiOutput("select_screen_5"),
          
          checkboxInput(inputId = "graph_5_checkbox",
                        label = "Exclude zeros",
                        value = FALSE),
        )
      )
    )
  ),
  wellPanel(
    selectInput(inputId = "whats_fear", 
                label = "\"Fear\" labels", 
                choices = label_set$label, 
                multiple = TRUE, 
                selected = fear_labels,),
  )
)

#Tabset panel for tabs (Example 6) runExample("06_tabsets")

server <- function(input, output){
  #https://mastering-shiny.org/action-graphics.html
  
  fear_labels <- reactive(input$whats_fear)

  #Screen 1
  fear_cmpd_ranked <- reactive(
    ms_data %>% 
      dplyr::filter(!is.na(label)) %>%
      mutate(is.fear = if_else(grepl(paste(fear_labels(), collapse="|"),label),"Fear","Others")) %>%
      group_by(is.fear)%>%
      reframe(across(matches("^\\d"), sum)) %>%
      reframe(across(matches("^\\d"),function(x){x/sum(x)})) %>%
      mutate(is.fear=c("Fear","Others")[row_number(`14.0028`)]) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "fraction"
      ) %>% 
      dplyr::filter(is.fear == "Fear") %>%
      arrange(desc(fraction)))
  
  # Interactive UI (Screen 2)
  output$select_screen_2 <- renderUI({
    selectInput(inputId = "graph_2_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = fear_cmpd_ranked()$cmpd)
  })
  
  # Interactive UI (Screen 4)
  output$select_screen_4 <- renderUI({
    selectInput(inputId = "graph_4_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = fear_cmpd_ranked()$cmpd)
  })
  
  # Interactive UI (Screen 5)
  output$select_screen_5 <- renderUI({
    selectInput(inputId = "graph_5_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = fear_cmpd_ranked()$cmpd)
  })
  
  #For user input in shiny 
  user_start_rank_1 <- reactive(input$graph_1_rank[1])
  user_end_rank_1 <- reactive(input$graph_1_rank[2])
  
  select_to_plot_1 <- reactive(graph_1()[c(user_start_rank_1():user_end_rank_1(), (length(graph_1()$cmpd)-user_start_rank_1()+1):(length(graph_1()$cmpd)-user_end_rank_1()+1)),])
  
  output$graph_1 <- renderPlot({
    fear_cmpd_ranked() %>%
      slice(user_start_rank_1():user_end_rank_1()) %>%
      ggplot(aes(y = fct_reorder(cmpd,fraction), x = fraction)) +
      geom_col(fill = colour_palette[1]) + 
      labs(title = paste("Top",user_start_rank_1(),"to",user_end_rank_1(),"Most Frequently Emitted Compounds when in Fear"),
           subtitle = "Identified by distribution across different types of scenes",
           y = "m/z of Compound", 
           x = "Fraction of the Total Concentration") + 
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
  
  output$graph_2 <- renderPlot({
    ms_data %>% 
      dplyr::filter(!is.na(label)) %>%
      select(user_cmpd_2(),movie_F_ind, label) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>% 
      { if(zeros()) dplyr::filter(.,conc>0) else(.)} %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind]) %>%
      select(-conc, -movie_F_ind, -cmpd) %>%
      separate_longer_delim(
        cols = label,
        delim = "; "
      ) %>% 
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
  selected_cmpd_3 <- reactive(fear_cmpd_ranked()$cmpd[user_top_cmpd_3():user_bottom_cmpd_3()])

  # Vector of absent cmpd (Complete Absence)
  removed_cmpd <- reactive(
    ms_data %>% 
      dplyr::filter(movie == user_movie_3()) %>%
      select(all_of(selected_cmpd_3())) %>%
      select_if(~all(.==0)) %>%
      colnames()
  )
  
  # Plot
  output$graph_3_no <- renderText({"All compounds selected were absent throughout the movie selected."})
  
  output$graph_3_yes <- renderPlot({
    ms_data %>% 
      dplyr::filter(!is.na(label),
                    movie == user_movie_3()) %>%
      select(all_of(selected_cmpd_3()[!selected_cmpd_3() %in% removed_cmpd()]),counter,label) %>%
      group_by(counter,label) %>%
      reframe(across(matches("^\\d"), mean)) %>%
      mutate(is.fear = if_else(grepl(paste(fear_labels(), collapse="|"),label), "Fear","Others")) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "average"
      )  %>% #To remove movie duplicates
      transform(cmpd=factor(cmpd, levels=selected_cmpd_3())) %>%
      arrange(cmpd, desc(is.fear)) %>% #NOTE: IF PLOT CONC, A FEW COMPLETE ABSENCE
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
    switch(if_else(length(removed_cmpd()) != length(selected_cmpd_3()), 1, 2),
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
  user_movie_4_indices <- reactive(ms_data %>% 
      dplyr::filter(movie == user_movie_4()) %>%
      distinct(movie_F_ind) %>% '[['("movie_F_ind"))
  
  ui_4_screens <- reactive({
    list <- tagList()
    for(i in user_movie_4_indices()){
      list <- list.append(list, div(
        p(paste("Screening:",i)),
        p(paste("Scheduled for",screen_times$scheduled[i])),
        p(paste("Occupied by",screen_times$number.visitors[i], "people")),
        p(paste(screen_times$filled..[i],"% filled", sep=""))
      ))
    }
    return(list)
  })
  
  output$checkbox_screen_4 <- renderUI({ 
    checkboxGroupInput(inputId = "graph_4_screenings",
                       label = "Select desired screenings:",
                       selected = user_movie_4_indices(),
                       choiceNames = ui_4_screens(),
                       choiceValues = user_movie_4_indices(),
                       inline = TRUE)  
  })
  
  selected_screenings <- reactive(input$graph_4_screenings)
  
  graph_4_info <- reactive(
    ms_data %>% 
      dplyr::filter(!is.na(label),
                    movie == user_movie_4(),
                    movie_F_ind %in% selected_screenings()) %>%
      select(user_cmpd_4(),counter,label,movie_F_ind) %>%
      group_by(movie_F_ind) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>%
      ungroup() %>%
      mutate(conc_perpax = (max(conc/screen_times$number.visitors[movie_F_ind])-min(conc/screen_times$number.visitors[movie_F_ind]))) %>%
      distinct(counter,.keep_all = TRUE) %>%
      mutate(is.fear = if_else(grepl(paste(fear_labels(), collapse="|"),label),"Fear","Others")) %>%
      select(counter, is.fear,conc_perpax)
  )
  
  graph_4_height_info <- reactive(
    ms_data %>%
      dplyr::filter(!is.na(label),
                  movie == user_movie_4(),
                  movie_F_ind %in% selected_screenings()) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>%
      ungroup() %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind]) %>% 
      '[['(user_cmpd_4())
  )
  
  output$graph_4 <- renderPlot({
    ms_data %>% 
      dplyr::filter(!is.na(label),
                    movie == user_movie_4(),
                    movie_F_ind %in% selected_screenings()) %>%
      select(user_cmpd_4(),counter,label,movie_F_ind) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind]) %>%
      mutate(movie_F_ind = fct_reorder2(as.character(movie_F_ind), counter, conc_perpax)) %>%
      arrange(movie_F_ind) %>%
      ggplot(aes(x=counter/2, y=conc_perpax)) + 
      geom_tile(data = graph_4_info() %>% dplyr::filter(is.fear == "Fear"),
                height=max(graph_4_height_info())-min(graph_4_height_info()),
                aes(fill="Fear"),linetype=0,alpha=0.3) +
      geom_tile(data = graph_4_info() %>% dplyr::filter(is.fear == "Others"), 
                height=max(graph_4_height_info())-min(graph_4_height_info()), 
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
  
  # Screen 5
  user_cmpd_5 <- reactive(input$graph_5_cmpd) #SELECT from really narrowed few (TODO)
  
  output$graph_5a <- renderPlot({
    ms_data %>%
      select(user_cmpd_5(),counter,label,movie,movie_F_ind) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>%
      {if(input$graph_5_checkbox) dplyr::filter(., conc>0) else(.)} %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind]) %>% 
      ggplot(aes(x = conc_perpax, y = movie)) +
      geom_boxplot() +
      labs(x = "Concentration per Pax per Scene", 
           y = "Movie",
           title = paste("Distribution of compound with m/z =", user_cmpd_5()))
  })
  
  output$graph_5b <- renderPlot({
    ms_data %>%
      select(user_cmpd_5(),counter,label,movie,movie_F_ind) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>%
      group_by(movie) %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind],
             overall_ave_conc_perpax = mean(conc_perpax)) %>%
      ungroup() %>%
      distinct(movie, .keep_all = TRUE) %>%
      mutate(fear_rating = screen_times$fear_rating[movie_F_ind]) %>%
      distinct(movie, overall_ave_conc_perpax, .keep_all = TRUE) %>% 
      mutate(fear_rating = screen_times$fear_rating[movie_F_ind]) %>%
      ggplot(aes(x = fear_rating, y = overall_ave_conc_perpax)) +
      geom_point() + geom_line() + 
      labs(x = "Online Fear Ratings",
           y = "Average Concentration per Pax per Scene",
           title = "Comparison of Actual and Expected Fear Ratings", 
           subtitle = paste("Based on compound with m/z =",user_cmpd_5()), 
           #caption = "Fear ratings are an average of rating obtained from Reel Scary and Common Sense Media",
           caption=a("ggplot2 Package", href = "https://ggplot2.tidyverse.org/"))
  })
    
}

shinyApp(ui = ui, server = server)