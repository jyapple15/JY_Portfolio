library(shiny)
library(shinyglide)
library(slickR)
library(ggplot2)

# Any pics etc. need to be in www folder

ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "styles.css"),
  titlePanel("Is the smell of fear real?"),
  glide(
    id = "description",
    custom_controls = div(glideControls(
      prevButton(class="left-centered"),
      nextButton(class="right-centered")
    )),
    controls_position = "bottom",
    previous_label = "<",
    next_label = ">",
    
    screen(
      div(class="glidecontent",
        p("This is the"),
        tags$button("first"),
        p("screen")
      )
    ),
    screen(
      div(class="glidecontent",
          p("This is the second and final screen.")
      )
    )
  ),
  mainPanel(
    slickROutput(outputId="maintextbar", width = "100%")
  )
  #Tabset panel for tabs (Example 6) runExample("06_tabsets")
)

server <- function(input, output){
  output$maintextbar <- renderSlickR({
    text <- c("I like books", "books are fun")
    slickR(text)
  })
}

shinyApp(ui = ui, server = server)