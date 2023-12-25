library(shiny)
library(shinyglide)
library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(rlist)
library(viridisLite)

# Colour scheme (23 colours needed for palette)
colour_palette <- plasma(23)
colour_gradient <- plasma(3) #Low to high conc
colour_bkg <- mako(2) #contrasting w each other and w palette

# Importing Snipped Datasets
for(i in c("ms_data","screen_times","label_set")){
  assign(i,read_csv(sprintf("./src/data/cleaned/snipped_%s.csv",i)))
}

fear_labels <- grep("death|suspense|horror|murder|aggressive|violence|shock", 
                    label_set$label, 
                    value=TRUE)

# Any pics etc. need to be in www folder
ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "styles.css"), #In www folder
  titlePanel("Is the smell of fear real?"),
  
  glide(
    id = "description",
    controls_position = "bottom",
    previous_label = "Huh?",
    next_label = "What now?",
    
    #Screen 0
    screen(
      div(
        p("???The smell of fear??? ??? a common phrase used to imply that one???s fear can be detected."),
        br(),
        p("Some think of it as a metaphor of sorts. Meanwhile, some say that other living things
          such as bears and dogs can smell your fear. Have you ever wondered if this is true? Does
          the ???smell of fear??? actually exist? Well, let???s break it down."),
        tags$table(
          tags$tbody(
            tags$td(
              p("A smell of fear can possibly come about from chemicals excreted by our bodies. These are
                then detected by the olfactory receptors such as those in our noses, leading to what we 
                perceive as ???smell???. Thus, a smell of fear would be detectable by analysing the gas around
                a person in fear. Using this concept, a group of researchers sampled the gas emitted in the
                Cinestar Cinema in Mainz, Germany over 1.5 months every 30s (Wicker et al., 2015). These 
                samples were then input into a mass spectrometer (PTR-MS-ToF) to obtain the", actionLink(inputId = "mz_def", label="mass to charge 
                ratio (m/z)"), "of compounds in the gas. Now, let's look for trends by comparing the concentrations of those compounds with 
                the pre-labelled scenes!")),
            tags$td(
              tags$image(src = "https://d1ymz67w5raq8g.cloudfront.net/Pictures/480xAny/3/8/6/114386_0117EiC_EducationinChemistry_January2017-28.jpg", height="190px")
            )
          )
        ),
        p("*Masses such as 18.9921 and 18.9995 are considered as non-identical. Mass of compounds are not 
          unique for chemicals with the same composition.")
      )
    ),
    
    #Screen 1
    screen(
      div(
        p("Firstly, we need to decide what we count as a fear-inducing scene. When we define those as the scenes involving those at the bottom, 
          we see that the top ranked ???fear compounds??? are as follows. Try modifying the fear-inducing scenes at the bottom of this screen!"),
        br()
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
                      ticks = FALSE)
        )
      ),
      div(
        br(),
        p("Here, we can see that some compounds are more present during fear-inducing 
          scenes than other scenes. So, the smell of fear does exist? Well, there's a catch."), 
        
        p("Unfortunately, this plot is skewed as some compounds are relatively absent in our data set. 
          This means that the fraction is calculated from few data points and may 
          not be an accurate representation of the compound???s distribution across 
          scenes. In addition, scenes labelled with both ???fear??? and ???non-fear labels??? 
          are overall considered as ???fear scenes???. This might lead to an overrepresentation of ???fear scenes???.")
      )
    ),
    
    #Screen 2
    screen(
      div(
        p("Nonetheless, we now have a better idea of what the ranking should look like. 
          Considering that the top data might be skewed, the compounds a little lower in 
          rank are likely to give a better idea of the true ranking."),
        p("Here, we can see the distribution of a compound over different scene labels. 
          A high number of points can be seen concentrated at the lower concentrations, 
          suggesting lots of data points where compounds have concentrations at noise level. 
          In addition, most compounds can be seen having low concentrations at a large number 
          of conversation scenes. This could be due to movies having a large number of 
          conversation scenes, resulting in an underrepresentation of other scenes."),
        br()
      ),
      sidebarLayout(
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Main", 
                               withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
                                 ui_element = plotOutput("graph_2a"),
                                 image = "loading_ghost.gif",
                                 image.width = "400px"
                               )),
                      tabPanel("Distributions of scenes", 
                               withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
                                 ui_element = plotOutput("graph_2b"),
                                 image = "loading_ghost.gif",
                                 image.width = "400px"
                               )))
        ),
        sidebarPanel(
          uiOutput("select_screen_2"),
          
          checkboxInput(inputId = "graph_2_noise",
                        label = "Remove possible noise",
                        value = FALSE),
          
          checkboxInput(inputId = "graph_2_conversation",
                        label = tags$section("Remove labels: ", tags$ul(tags$li("conversation general"), tags$li("conversation: conversation main actor"))),
                        value = FALSE)
        )
      ),
      div(br(),
          p("Upon removing those, many compounds are noted to have their concentrations split in aggregated columns. This suggests that another variable is affecting gas emissions per person. This could be the  factors such as the time of screening and number of people in the vicinity."))
    ),
    
    #Screen 3
    screen(
      div(
        p("Let???s try to keep one other variable constant ??? the movie."),
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
            choices = distinct(dplyr::filter(ms_data,!is.na(label)),movie)$movie,
            selected = "Paranormal Activity: The Marked Ones"
          ),
          sliderInput(inputId = "graph_3_rank",
                      label = "Expected Fear Rank",
                      value = c(1,10),
                      min = 1,
                      max = length(select(ms_data, matches("^\\d"))),
                      step = 1,
                      ticks = FALSE)
        )
      ),
      div(
        br(),
        p("From the plots, we can see that there is no outstanding trend between the fear-labelled 
            scenes and the concentration of most compounds. However, when comparing the overall compound concentration 
            over time, most plots reveal relatively constant gas compound concentrations over 
            time. This could be due to those gas molecules being present in the regular atmosphere 
            instead of a contribution from us. It could also be due to our gas emissions that are 
            unaffected by the movie scenes being played."), 
        p("On the other hand, certain compounds 
            are noted to have a generally decreasing or increasing trend from the start to the end of a 
            movie. These could be due to events happening over the course of the movie. When focusing on 
            the scene labels, a general trend is noted whereby the fear scenes cluster at the end. This 
            aligns with our expectations of movies becoming more exciting at the end."),
        p("For certain movies, in particular the horror movies, such compounds could be ???fear compounds??? 
            which increased as the suspense grew near the end of the movie. In this list, the most probable 
            movies to incite fear would be the horror movie ??? ???Paranormal Activity: The Marked Ones???. Thus, 
            probable ???fear compounds??? include m/z = 31.0178, 39, 42.0338, 44.9998, 45.0335, 47.0491, 48.0542, 
            49.0578, 57.0699, 65.0603, 75.0478, and 79.0542 and more. A trend can be observed in the m/z value 
            and could potentially suggest a link."),
        p("Across the different compounds, there are also a few similar patterns in certain compounds??? 
            evolution over time. These could be due to the compounds coming from the same source ??? a larger 
            gas molecule which they fragmented from, due to the features of mass spectrometry. One example of such 
            trends is the evolution of compounds with m/z 57.0699 and 79.0542 over the duration of ???Paranormal Activity: 
            The Marked Ones???.")
      )
    ),
    
    #Screen 4
    screen(
      div(
        p("To get a better idea regarding the distribution of those identified compounds, we can look into the different screenings of the same movie."),
      ),
      sidebarLayout(
        mainPanel(
          withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
            ui_element = plotOutput("graph_4"),
            image = "loading_ghost.gif",
            image.width = "400px"
          )
        ),
        sidebarPanel(
          radioButtons(
            inputId = "graph_4_movie",
            label = "Select a movie:",
            choices = distinct(dplyr::filter(ms_data,!is.na(label)),movie)$movie,
            selected = "Paranormal Activity: The Marked Ones"
          ),
          actionButton(inputId = "graph_4_modify", 
                       label = "Modify Screenings"),
          uiOutput("select_screen_4")
        )
      ),
      div(
        p("Upon doing so, we can see a difference in the screenings. For some, a higher average is 
        observed. One example is in the distribution of most compounds over the duration of screening 
        no. 100 of ???Paranormal Activity: The Marked Ones???. Upon closer investigation, it is noted that 
        that screening stood out as having significantly fewer cinema goers. This suggests that the lower 
        occupancy of the room could have influenced each person unconsciously exhale more. Since this 
        trend can also be found for the other movies, it is unlikely to suggest higher induced fear. 
        Instead, it suggests a different social influence."),
        p("Moreover, when analysing the previously identified compounds ??? such as that with m/z = 42.0338, 
        a pattern can be observed whereby its concentration increases within the fear bands. This suggests
        a gradual increase in those emissions as fear is gradually introduced in fear-inducing scenes and 
        supports the identification of those compounds as fear compounds.")
      )
    ),
    
    #Screen 5
    screen(
      div(
        p("From the previous data, we can see that the smell of fear definitely exists. Some possible 
          ???fear compounds??? include those of m/z = 42.0338 and 44.9998. Now, how does this compare to 
          other movies? In the data set used, information was collected for 16 different movies. However, 
          scene labels were only recorded for the 6 movies we have been looking at. ")
      ),
      sidebarLayout(
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Distribution", 
                               withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
                                 ui_element = plotOutput("graph_5a"),
                                 image = "loading_ghost.gif",
                                 image.width = "400px"
                               )),
                      tabPanel("vs Fear Ratings", 
                               withSpinner( #https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
                                 ui_element = plotOutput("graph_5b"),
                                 image = "loading_ghost.gif",
                                 image.width = "400px"
                               )))
        ),
        sidebarPanel(
          uiOutput("select_screen_5"),
          
          checkboxInput(inputId = "graph_5_checkbox",
                        label = "Exclude zeros",
                        value = FALSE),
        )
      ),
      div(
        br(),
        p("Taking m/z = 42.0338 as a reference, we can see some disagreements with our expectations
          as we compare the distributions across different movies. Some non-horror movies are noted
          to have relatively high concentrations of this compound per scene. However, this could be 
          due to a vagueness in scene labelling, as well as a difference number of screenings sampled. 
          From our previous observations, these concentrations could also be higher in non-horror movies 
          due to a difference in occupancy."),
        p("Nonetheless, the horror movie ???Carrie??? is noted to have an average well above the other movies. 
        This trend can also be observed in the plot against their fear ratings, suggesting that 42.03338 is a 
        ???fear compound???."),
        p("Overall, it is clear that a ???smell of fear??? exists. However, considering that fear comes in a large 
          variety of forms, more research is needed to correctly assign each chemical to a specific fear. Now, 
          do you agree? Try adjusting the parameters to come to your own conclusion!")
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

server <- function(input, output, session){
  #https://mastering-shiny.org/action-graphics.html
  
  observeEvent(input$mz_def, {
    showModal(
      modalDialog(
        p("m/z can be generally regarded as the mass since most compounds reaching the
          detector have a charge of 1"), 
        footer = modalButton("Done")
      )
    )
  })
  
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
  
  screen_2_fear_cmpd_ranked <- reactive(
    ms_data %>% 
      dplyr::filter(!is.na(label)) %>%
      {if(input$graph_2_conversation) dplyr::filter(.,grepl(paste(label_set$label[!label_set$label %in% c("conversation: general", "conversation: conversation main actor")], collapse="|"),label)) else(.)} %>%
      select(matches("^\\d")) %>% 
      {if(input$graph_2_noise) select_if(., where(~any(.x>0.5))) else(.)} %>%
      colnames() %>%
      factor(levels = fear_cmpd_ranked()$cmpd, ordered=TRUE) %>%
      sort())
  
  # Interactive UI (Screen 2)
  output$select_screen_2 <- renderUI({
    selectInput(inputId = "graph_2_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = screen_2_fear_cmpd_ranked(),
                selected = "123.1168")
  })
  
  # Interactive UI (Screen 4)
  output$select_screen_4 <- renderUI({
    selectInput(inputId = "graph_4_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = fear_cmpd_ranked()$cmpd,
                selected = "42.0338")
  })
  
  # Interactive UI (Screen 5)
  output$select_screen_5 <- renderUI({
    selectInput(inputId = "graph_5_cmpd",
                label = "Choose a compound:\n(Arranged by expected fear rank)",
                choices = fear_cmpd_ranked()$cmpd,
                selected = "42.0338")
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
           x = "Fraction of Concentration in Fear Scenes") + 
      theme(panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt'))),
            plot.subtitle = element_text(margin = margin(b = 10))
      ) + 
      scale_x_continuous(expand = c(0, 0))
  })
  
  #Screen 2
  #For user input in shiny
  user_cmpd_2 <- reactive(input$graph_2_cmpd)
  
  output$graph_2a <- renderPlot({
    ms_data %>% 
      dplyr::filter(!is.na(label)) %>%
      select(user_cmpd_2(),movie_F_ind, label) %>%
      pivot_longer(
        cols = matches("^\\d"),
        names_to = "cmpd",
        values_to = "conc"
      ) %>% 
      {if(input$graph_2_noise) dplyr::filter(.,conc>0.5) else(.)} %>%
      mutate(conc_perpax = conc/screen_times$number.visitors[movie_F_ind]) %>%
      select(-conc, -movie_F_ind, -cmpd) %>%
      separate_longer_delim(
        cols = label,
        delim = "; "
      ) %>% 
      {if(input$graph_2_conversation) dplyr::filter(., label != "conversation: general", label != "conversation: conversation main actor") else(.)} %>% 
      ggplot(aes(x=conc_perpax, y=label)) + 
      geom_hex() +
      labs(title = paste("Distribution of compound with m/z =",user_cmpd_2(),"across different scenes"),
           subtitle = paste(if_else(input$graph_2_conversation, "Excludes", "Includes"), "data points where compound concentration < 1"),
           y = "Type of Scene", 
           x = "Concentration per Pax per Scene") + 
      scale_fill_gradientn(colours = colour_gradient) + 
      theme(axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt'))))
  })
  
  output$graph_2b <- renderPlot({
    ms_data %>% 
      dplyr::filter(!is.na(label)) %>%
      select(label) %>%
      separate_longer_delim(
        cols = label,
        delim = "; "
      ) %>%
      ggplot(aes(y=label)) +
      geom_bar(fill = colour_palette[1]) +
      labs(title = paste("Distribution of scenes"),
           y = "Type of Scene", 
           x = "Count") + 
      theme(axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt'))),
            panel.background = element_blank())
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
            axis.line = element_line(linewidth = 0.1, arrow = arrow(type='closed', length = unit(2,'pt'))),
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
  
  observeEvent(input$graph_4_modify, {
    showModal(
      modalDialog(
        checkboxGroupInput(inputId = "graph_4_screenings",
                           label = "Select desired screenings:",
                           selected = selected_screenings(),
                           choiceNames = ui_4_screens(),
                           choiceValues = user_movie_4_indices(),
                           inline = TRUE), #TODO
        footer = modalButton("Done")
      )
    )
  })
  #TODO: Set min for checkbox
  #observe({
  # if(length(input$SelecetedVars) < my_min){
  #  updateCheckboxGroupInput(session, "SelecetedVars", selected= "a1")
  #}
  #})
  
  selected_screenings <- reactive(input$graph_4_screenings)
  
  graph_4_info <- reactive(
    ms_data %>% 
      dplyr::filter(!is.na(label),
                    movie == user_movie_4()) %>% #ISSUE IS THAT screen_times$movie[...] gives c(NA,NA,NA,NA,NA,NA)
      {if(length(selected_screenings() != 0)) {if(all(selected_screenings() %in% which(screen_times$movie == user_movie_4()))) dplyr::filter(., movie_F_ind %in% selected_screenings()) else(.)} else(.)} %>%
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
                    movie == user_movie_4()) %>%
      {if(length(selected_screenings() != 0)) {if(all(selected_screenings() %in% which(screen_times$movie == user_movie_4()))) dplyr::filter(., movie_F_ind %in% selected_screenings()) else(.)} else(.)} %>%
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
                    movie == user_movie_4()) %>%
      {if(length(selected_screenings() != 0)) {if(all(selected_screenings() %in% which(screen_times$movie == user_movie_4()))) dplyr::filter(., movie_F_ind %in% selected_screenings()) else(.)} else(.)} %>%
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
           y = "Concentration per Pax per Scene", 
           x = "Duration of Movie (min)",
           color = "Screening",
           fill = "Type of Scene") + 
      theme(panel.background = element_blank(),
            axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt')))) + 
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
           title = paste("Distribution of compound with m/z =", user_cmpd_5())) +
      theme(axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt'))))
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
           caption = "Fear ratings are an average of rating obtained from Reel Scary and Common Sense Media") + 
      theme(plot.caption = element_text(hjust=0),
            axis.line = element_line(linewidth = 0.3, arrow = arrow(type='closed', length = unit(5,'pt'))))
  })
  
}

shinyApp(ui = ui, server = server)