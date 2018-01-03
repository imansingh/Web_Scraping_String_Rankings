shinyUI(dashboardPage(
  dashboardHeader(title = "Stringforum Data"),
  dashboardSidebar(
    # sidebarUserPanel("Stringforum Project",
    #                  image = "imansingh_headshot.jpg"),
    sidebarMenu(
      menuItem("String Selector", tabName = "strings", icon = icon("map")),
      menuItem("Reviews by Racquet", tabName = "racquet", icon = icon("bar-chart")),
      menuItem("Reviews by Tester", tabName = "tester", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "strings", h2("Find the Right String Based on Your Criteria"),
              fluidRow(h3("Select how important these factors are to you, on a scale of 1-10")),
              fluidRow(
                box(sliderInput("string_comfort", "Comfort", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_control", "Control", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_durability", "Durability", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_feel", "Feel", min = 1, max = 10, value = 5), width = 3)),
              fluidRow(
                box(sliderInput("string_power", "Power", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_spin", "Spin", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_tension_stability", "Tension Stability", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_tester_satisfaction", "Overall User Satisfaction", min = 1, max = 10, value = 5), width = 3)),
                fluidRow(h3("Only Include Strings with Multiple Reviews? Select Minimum Number of Reviews")),
                fluidRow(
                  box(sliderInput("string_minimum_reviews", "Minimum # of Reviews", min = 1, max = 256, value = 1), width = 3)),
                fluidRow(h3("Top Strings Matching Your Selections")),
                fluidRow(
                       box(DT::dataTableOutput("string_table"), width = 12))
      ),
      tabItem(tabName = "racquet", h2("Average Ratings and Detailed Reviews Based on Racquet Selection"),
        #fluidRow(h3("Only Include Racquets with Multiple Reviews? Select Minimum Number of Reviews")),
        #fluidRow(
        #  box(sliderInput("racquet_minimum_reviews", "Minimum # of Reviews", min = 1, max = 289, value = 1), width = 3)),
        fluidRow(box(selectizeInput("selected_racquet", "Select Racquet", racquet_means_clean$tester_racquet))),
        fluidRow(box(DT::dataTableOutput("racquet_means_table"), width = 12)),
        fluidRow(box(DT::dataTableOutput("racquet_details_table"), width = 12))
        ),
      tabItem(tabName = "tester", h2("Average Ratings and Detailed Reviews Based on Reviewer Name"),
              #fluidRow(h3("Do You Want To Include Only Racquets with Multiple Reviews? If So, Select Minimum Number of Reviews")),
              #fluidRow(
              #  box(sliderInput("racquet_minimum_reviews", "Minimum # of Reviews", min = 1, max = 289, value = 1), width = 3)),
              fluidRow(box(selectizeInput("selected_reviewer", "Select Reviewer", tester_means_clean$tester_name))),
              fluidRow(box(DT::dataTableOutput("tester_means_table"), width = 12)),
              fluidRow(box(DT::dataTableOutput("tester_details_table"), width = 12))
      )
    )
  )
))