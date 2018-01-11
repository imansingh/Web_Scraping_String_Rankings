shinyUI(dashboardPage(
  dashboardHeader(title = "Stringforum Project"),
  dashboardSidebar(
    sidebarUserPanel('Iman Singh',
                     subtitle = 'NYC Data Science Academy',
                     image = 'imansingh_headshot.jpg'
                     ),
    sidebarMenu(
      menuItem('String Selector', tabName = 'selector', 
               icon = icon('map')
               ),
      menuItem('Reviews by Racquet', tabName = 'racquet', 
               icon = icon('bar-chart')
               ),
      menuItem('Reviews by Tester', tabName = 'tester', 
               icon = icon('line-chart')
               ),
      br(),
      menuItem('Contact Iman Singh'),
      menuItem('LinkedIn', icon = icon('linkedin-square'), 
               href = 'https://www.linkedin.com/in/imansingh/'
               ),
      menuItem('GitHub', icon = icon('github'), 
               href = 'https://github.com/imansingh/Scraping-Project'
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'selector', 
              h2('Find the Right String Based on Your Criteria'),
              fluidRow(h3('Select your String Preferences, Tester Criteria, 
                          Tester Racquet, and General Criteria')),
              fluidRow(
                tabBox(
                  #title = 'First tabBox', 
                  id = 'selectorInput', 
                  height = '500px',
                  width = 12,
                  selected = 'String Preferences',
                  tabPanel('String Preferences',
                           h3('Select how important these factors are to you, 
                              on a scale of 1-10'),
                           fluidRow(
                             box(sliderInput('string_comfort', 'Comfort', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3),
                             box(sliderInput('string_control', 'Control', 
                                             min = 1, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_durability', 'Durability', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3),
                             box(sliderInput('string_feel', 'Feel', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3)
                             ),
                           fluidRow(
                             box(sliderInput('string_power', 'Power', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3),
                             box(sliderInput('string_spin', 'Spin', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3),
                             box(sliderInput('string_tension_stability', 
                                             'Tension Stability', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3),
                             box(sliderInput('string_tester_satisfaction', 
                                             'Overall User Satisfaction', 
                                             min = 1, max = 10, value = 5), 
                                 width = 3)
                             )
                           ),
                  tabPanel("Tester Criteria",
                           h3('Only include reviews by testers with these 
                              characteristics:'),
                           fluidRow(
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_gender', 
                                      'Tester Gender',
                                      choices = c('Male', 'Female', 
                                                  'None Selected'),
                                      selected = c('Male','Female', 
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_level', 
                                      'Tester Level',
                                      choices = c('Beginner', 'Recreational', 
                                                  'Lower League', 'Upper League',
                                                  'National Tournament',
                                                  'International Tournament', 
                                                  'None Selected'),
                                      selected = c('Beginner', 'Recreational', 
                                                   'Lower League', 'Upper League',
                                                   'National Tournament',
                                                   'International Tournament',
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_age', 
                                      'Tester Age',
                                      choices = c('Junior', 'Adult', 
                                                  'Young Senior', 'Senior',
                                                  'None Selected'),
                                      selected = c('Junior', 'Adult', 
                                                   'Young Senior', 'Senior',
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_strokes', 
                                      'Tester Swing Speed',
                                      choices = c('Fast', 'Medium', 'Slow',
                                                  'None Selected'),
                                      selected = c('Fast', 'Medium', 'Slow',
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_style', 
                                      'Tester Playing Style',
                                      choices = c('All-Around',
                                                  'Defensive Baseline',
                                                  'Offensive Baseline',
                                                  'Serve & Volley',
                                                  'None Selected'),
                                      selected = c('All-Around',
                                                   'Defensive Baseline',
                                                   'Offensive Baseline',
                                                   'Serve & Volley',
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_spin', 
                                      'Tester Spin Level',
                                      choices = c('Heavy', 'Moderate', 'Low',
                                                  'None Selected'),
                                      selected = c('Heavy', 'Moderate', 'Low', 
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    )
                             )
                           ),
                  tabPanel("Tester Racquet Criteria",
                           # Tester Racquet Info: racquet_manufacturer, racquet_model, frame_size, 
                           # string_pattern
                           # Tester Racquet Tension: main_tension, cross_tension
                           # String Info: string_material, string_construction, string_features
                           # String Gauge: string_gauge
                           # Price: price_adjusted
                           # Review Adjectives: review_adjectives_split
                           
                           h3('Only include reviews by testers using these 
                              types of racquets:'),
                           fluidRow(
                             column(width = 4,
                                    box(selectizeInput(
                                      'racquet_manufacturer', 
                                      'Tester Racquet Manufacturer(s)', 
                                      choices = string_data$racquet_manufacturer,
                                      multiple = TRUE,
                                      options = list(placeholder = 
                                                       '(choose one or more)')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(selectizeInput(
                                      'racquet_model',
                                      'Tester Racquet Model(s)',
                                      choices = string_data$racquet_model,
                                      multiple = TRUE,
                                      options = list(placeholder = 
                                                       '(choose one or more)',
                                                     maxOptions = 2000)),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'frame_size', 
                                      'Tester Frame Size',
                                      choices = c('Midsize (93 in\u00B2, 
                                                  600 cm\u00B2)',
                                                  'MidPlus (,95 in\u00B2,
                                                  630-660 cm\u00B2)',
                                                  'Oversize (110 in\u00B2,
                                                  >660 cm\u00B2)',
                                                  'None Selected'),
                                      selected = c('Midsize (93 in\u00B2, 
                                                  600 cm\u00B2)',
                                                   'MidPlus (95 in\u00B2,
                                                   630-660 cm\u00B2)',
                                                   'Oversize (110 in\u00B2,
                                                   >660 cm\u00B2)',
                                                   'None Selected')),
                                      actionLink('selectall',"Select All"),
                                      "  |  ",
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(selectizeInput(
                                      'string_pattern',
                                      'Tester String Pattern',
                                      choices = string_data1$string_pattern,
                                      multiple = TRUE,
                                      options = list(placeholder = 
                                                       '(choose one or more)')),
                                      # actionLink('selectall',"Select All"),
                                      # "  |  ",
                                      # actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(sliderInput('main_tension', 'Tension - Main Strings', 
                                                    min = 35, max = 75, 
                                                    value = c(35,75),
                                                    round = TRUE),
                                        width = NULL),
                                    box(sliderInput('cross_tension', 'Tension - Cross Strings', 
                                                    min = 35, max = 75, 
                                                    value = c(35,75),
                                                    round = TRUE),
                                        width = NULL)
                                    )
                             )
                           )
                  )
                ),
              fluidRow(
                box(sliderInput("string_comfort", "Comfort", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_control", "Control", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_durability", "Durability", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_feel", "Feel", min = 1, max = 10, value = 5), width = 3)
                ),
              fluidRow(
                box(sliderInput("string_power", "Power", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_spin", "Spin", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_tension_stability", "Tension Stability", min = 1, max = 10, value = 5), width = 3),
                box(sliderInput("string_tester_satisfaction", "Overall User Satisfaction", min = 1, max = 10, value = 5), width = 3)
                ),
                fluidRow(h3("Only Include Strings with Multiple Reviews? Select Minimum Number of Reviews")),
                fluidRow(
                  box(sliderInput("string_minimum_reviews", "Minimum # of Reviews", min = 1, max = 256, value = 1), width = 3)
                  ),
                fluidRow(h3("Top Strings Matching Your Selections")),
                fluidRow(
                       box(DT::dataTableOutput("string_table"), width = 12)
                  )
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
