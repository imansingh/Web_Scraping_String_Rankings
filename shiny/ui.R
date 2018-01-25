shinyUI(dashboardPage(
  dashboardHeader(title = "Stringforum Project"),
  dashboardSidebar(
    sidebarUserPanel('Iman Singh',
                     subtitle = 'NYC Data Science Academy',
                     image = 'imansingh_headshot.jpg'
                     ),
    sidebarMenu(
      menuItem('Search Criteria', tabName = 'criteria', 
               icon = icon('map')
               ),
      menuItem('String Selector', tabName = 'selector', 
               icon = icon('bar-chart')
               ),
      menuItem('String Profiles', tabName = 'profile', 
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
      tabItem(tabName = 'criteria', 
              h2('Filter Which String Reviews are Fed Into String Selector'),
              fluidRow(column(width = 12, style = 'padding:15px',
                              h3('Select your String Criteria, Tester Criteria, 
                                 and Tester Racquet Criteria:'),
                              h4('The table below will update based on your 
                                 choices')
                              )
                       ),
              fluidRow(
                tabBox(
                  #title = 'First tabBox', 
                  id = 'criteriaInput', 
                  #height = '500px',
                  width = 12,
                  selected = 'String Criteria',
                  tabPanel('String Criteria',
                           h3('Only include strings with these 
                              characteristics:'),
                           fluidRow(
                             box(sliderInput('string_minimum_reviews', 
                                             'Minimum # of Reviews', 
                                             min = min(string_data1$num_ratings, 
                                                       na.rm = TRUE), 
                                             max = max(string_data1$num_ratings, 
                                                       na.rm = TRUE), 
                                             value = 1), 
                                 width = 6),
                             box(sliderInput(
                               'string_price', 
                               'Price Range ($)',
                               min = min(string_data1$price_adjusted, 
                                         na.rm = TRUE),
                               max = max(string_data1$price_adjusted,
                                         na.rm = TRUE),
                               value=c(min(string_data1$price_adjusted,
                                           na.rm = TRUE),
                                       max(string_data1$price_adjusted,
                                           na.rm = TRUE))),
                               checkboxInput('price_missing', 
                                             'Include strings with 
                                             no price listed',
                                             value = TRUE),
                               width = 6)
                             ),
                           fluidRow(
                             box(checkboxGroupInput(
                               'string_material', 'String Material',
                               choices = c(sort(unique(unlist(
                                 string_data1[["string_material"]]
                                 [!is.na(string_data1[["string_material"]]
                                 )]))),
                                 none_text),
                               selected = c(sort(unique(unlist(
                                 string_data1[["string_material"]]
                                 [!is.na(string_data1[["string_material"]]
                                 )]))),
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4),
                             box(checkboxGroupInput(
                               'string_construction', 
                               'String Construction',
                               choices = c(sort(unique(unlist(
                                 string_data1[["string_construction"]]
                                 [!is.na(string_data1[["string_construction"]]
                                 )]))),
                                 none_text),
                               selected = c(sort(unique(unlist(
                                 string_data1[["string_construction"]]
                                 [!is.na(string_data1[["string_construction"]]
                                 )]))),
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4),
                             box(checkboxGroupInput(
                               'string_features', 
                               'String Features',
                               choices = c(sort(unique(unlist(
                                 string_data1[["string_features"]]
                                 [!is.na(string_data1[["string_features"]]
                                 )]))),
                                 none_text),
                               selected = c(sort(unique(unlist(
                                 string_data1[["string_features"]]
                                 [!is.na(string_data1[["string_features"]]
                                 )]))),
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4)
                             ),
                           fluidRow(
                             column(width = 4,
                                    h4('String Gauge:'), 'Make your selection on 
                                    either the Metric or the US scale'),
                             column(width = 4,
                                    box(sliderInput(
                                      'string_gauge_metric', 
                                      'String Gauge - Metric (mm)',
                                      min = min(string_data1$
                                                  string_gauge_metric,
                                                na.rm = TRUE),
                                      max = max(string_data1$
                                                  string_gauge_metric,
                                                na.rm = TRUE),
                                      value=c(min(string_data1$
                                                    string_gauge_metric,
                                                  na.rm = TRUE),
                                              max(string_data1$
                                                    string_gauge_metric,
                                                  na.rm = TRUE))),
                                      checkboxInput('gauge_metric_missing', 
                                                    'Include strings with no 
                                                    gauge listed',
                                                    value = TRUE),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(sliderInput(
                                      'string_gauge_us', 
                                      'String Gauge - US',
                                      min = min(string_data1$string_gauge_us,
                                                na.rm = TRUE),
                                      max = max(string_data1$string_gauge_us,
                                                na.rm = TRUE),
                                      value=c(min(string_data1$string_gauge_us,
                                                  na.rm = TRUE),
                                              max(string_data1$string_gauge_us,
                                                  na.rm = TRUE))),
                                      checkboxInput('gauge_us_missing', 
                                                    'Include strings with no 
                                                    gauge listed',
                                                    value = TRUE),
                                      width = NULL)
                                    )
                             ),
                           fluidRow(
                             column(width = 4,
                                    h4('String Adjectives (Positive):'),
                                    'Only include strings where one or more
                                    reviewers listed these adjectives'),
                             column(width = 8,
                                    box(selectizeInput(
                                      'string_adjectives_positive',
                                      'Filter by Adjectives (Positive)',
                                      choices = sort(unlist(
                                        string_data1$string_adjectives)),
                                      multiple = TRUE,
                                      options = list(placeholder =
                                                       '(choose one or more)')),
                                      checkboxInput('adjectives_positive_missing', 
                                                    'Include strings with no 
                                                    adjectives listed',
                                                    value = TRUE),
                                      width = NULL)
                                    )
                             ),
                           fluidRow(
                             column(width = 4,
                                    h4('String Adjectives (Negative):'),
                                    'Do not include any strings where a reviewer
                                    listed these adjectives'),
                             column(width = 8,
                                    box(selectizeInput(
                                      'string_adjectives_negative',
                                      'Filter by Adjectives (Negative)',
                                      choices = sort(unlist(
                                        string_data1$string_adjectives)),
                                      multiple = TRUE,
                                      options = list(placeholder =
                                                       '(choose one or more)')),
                                      checkboxInput('adjectives_negative_missing', 
                                                    'Include strings with no 
                                                    adjectives listed',
                                                    value = TRUE),
                                      width = NULL)
                                    )
                             )
                           ),
                  tabPanel("Tester Criteria",
                           h3('Only include reviews by testers with these 
                              characteristics:'),
                           fluidRow(
                             box(sliderInput(
                               'tester_minimum_reviews', 
                               'Tester Minimum # of Reviews', 
                               min = min(string_data1$tester_reviews), 
                               max = max(string_data1$tester_reviews), 
                               value = 1),
                               width = 8)
                             ),
                           fluidRow(
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_gender', 
                                      'Tester Gender',
                                      choices = c('Male', 
                                                  'Female', 
                                                  'None Listed'),
                                      selected = c('Male',
                                                   'Female',
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_level', 
                                      'Tester Level',
                                      choices = c('Beginner', 'Recreational', 
                                                  'Lower League', 'Upper League',
                                                  'National Tournament',
                                                  'International Tournament', 
                                                  'None Listed'),
                                      selected = c('Beginner', 'Recreational', 
                                                   'Lower League', 'Upper League',
                                                   'National Tournament',
                                                   'International Tournament',
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_age', 
                                      'Tester Age',
                                      choices = c('Junior', 'Adult', 
                                                  'Young Senior', 'Senior',
                                                  'None Listed'),
                                      selected = c('Junior', 'Adult', 
                                                   'Young Senior', 'Senior',
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_strokes', 
                                      'Tester Swing Speed',
                                      choices = c('Slow', 'Medium', 'Fast',
                                                  'None Listed'),
                                      selected = c('Slow', 'Medium', 'Fast',
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    ),
                             column(width = 4,
                                    box(checkboxGroupInput(
                                      'tester_playstyle', 
                                      'Tester Playing Style',
                                      choices = c('All-Around',
                                                  'Defensive Baseline',
                                                  'Offensive Baseline',
                                                  'Serve & Volley',
                                                  'None Listed'),
                                      selected = c('All-Around',
                                                   'Defensive Baseline',
                                                   'Offensive Baseline',
                                                   'Serve & Volley',
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL),
                                    box(checkboxGroupInput(
                                      'tester_spin', 
                                      'Tester Spin Level',
                                      choices = c('Low', 'Moderate', 'Heavy',
                                                  'None Listed'),
                                      selected = c('Low', 'Moderate', 'Heavy', 
                                                   'None Listed')),
                                      actionLink('selectall',"Select All"),
                                      HTML('&ensp;'), '|', HTML('&ensp;'),
                                      actionLink('clearall', "Clear All"),
                                      width = NULL)
                                    )
                             )
                           ),
                  tabPanel("Tester Racquet Criteria",
                           h3('Only include reviews by testers using these 
                              types of racquets:'),
                           fluidRow( 
                             box(selectizeInput(
                               'racquet_manufacturer', 
                               'Tester Racquet Manufacturer(s)', 
                               choices = sort(string_data1$racquet_manufacturer
                                              [string_data1$racquet_manufacturer != '']),
                               multiple = TRUE,
                               options = list(placeholder = 
                                                '(choose one or more)')),
                               checkboxInput('manufacturer_missing', 
                                             'Include reviews for racquets with 
                                              no manufacturer listed',
                                             value = TRUE),
                               width = 4),
                             box(selectizeInput(
                               'racquet_model',
                               'Tester Racquet Model(s)',
                               choices = models_by_manufacturer,
                               # choices = string_data$racquet_model,
                               multiple = TRUE,
                               options = list(placeholder = 
                                                '(choose one or more)',
                                              maxOptions = 2000)),
                               checkboxInput('model_missing', 
                                             'Include reviews for racquets with 
                                              no model listed',
                                             value = TRUE),
                               width = 4),
                             box(selectizeInput(
                               'string_pattern',
                               'Tester String Pattern',
                               choices = sort(string_data1$string_pattern),
                               multiple = TRUE,
                               options = list(placeholder = 
                                                '(choose one or more)')),
                               checkboxInput('pattern_missing', 
                                             'Include reviews for racquets with 
                                              no string pattern listed',
                                             value = TRUE),
                               width = 4)
                             ),
                           fluidRow(
                             box(checkboxGroupInput(
                               'frame_size', 
                               'Tester Frame Size',
                               choices = c('Midsize ( >93 in\u00B2, 
                                           >593 cm\u00B2 )',
                                           'MidPlus ( 93-105 in\u00B2,
                                           594-677 cm\u00B2 )',
                                           'Oversize ( >106 in\u00B2,
                                           >678 cm\u00B2 )',
                                           'None Listed'),
                               selected = c('Midsize ( >93 in\u00B2, 
                                           >593 cm\u00B2 )',
                                            'MidPlus ( 93-105 in\u00B2,
                                           594-677 cm\u00B2 )',
                                            'Oversize ( >106 in\u00B2,
                                           >678 cm\u00B2 )',
                                            'None Listed')),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4),
                             box(sliderInput(
                               'main_tension', 
                               'Tension - Main Strings', 
                               min = 35, max = 75, value = c(35,75),
                               round = TRUE),
                               checkboxInput('main_missing', 
                                             'Include reviews for racquets with 
                                             no main tension listed',
                                             value = TRUE),
                               width = 4),
                             box(sliderInput(
                               'cross_tension', 
                               'Tension - Cross Strings', 
                               min = 35, max = 75, value = c(35,75),
                               round = TRUE),
                               checkboxInput('cross_missing', 
                                             'Include reviews for racquets with 
                                              no cross tension listed',
                                             value = TRUE),
                               width = 4)
                             )
                           )
                  )
                ),
              # fluidRow(
              #   box(sliderInput("string_comfort", "Comfort", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_control", "Control", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_durability", "Durability", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_feel", "Feel", min = 1, max = 10, value = 5), width = 3)
              #   ),
              # fluidRow(
              #   box(sliderInput("string_power", "Power", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_spin", "Spin", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_tension_stability", "Tension Stability", min = 1, max = 10, value = 5), width = 3),
              #   box(sliderInput("string_tester_satisfaction", "Overall User Satisfaction", min = 1, max = 10, value = 5), width = 3)
              #   ),
              #   fluidRow(h3("Only Include Strings with Multiple Reviews? Select Minimum Number of Reviews")),
              #   fluidRow(
              #     box(sliderInput("string_minimum_reviews", "Minimum # of Reviews", min = 1, max = 256, value = 1), width = 3)
              #     ),
                fluidRow(h3('[insert count] strings meet your criteria'))
                ,fluidRow(
                       box(DT::dataTableOutput("criteria_table"), width = 12)
                  )
              )
    )
  )
))
  #     tabItem(tabName = "selector", 
  #             h2('Find the Right String Based on Your Criteria'),
  #             fluidRow(h3('Select based either on String Preferences, 
  #                         or String Adjectives')),
  #             fluidRow(
  #               tabBox(
  #                 #title = 'First tabBox', 
  #                 id = 'selectorInput', 
  #                 height = '500px',
  #                 width = 12,
  #                 selected = 'String Preferences',
  #                 tabPanel('String Preferences',
  #                          h3('Select how important these factors are to you, 
  #                             on a scale of 1-10:'),
  #                          fluidRow(
  #                            box(sliderInput('string_comfort', 'Comfort', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3),
  #                            box(sliderInput('string_control', 'Control', 
  #                                            min = 1, max = 10, value = 5),
  #                                width = 3),
  #                            box(sliderInput('string_durability', 'Durability', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3),
  #                            box(sliderInput('string_feel', 'Feel', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3)
  #                          ),
  #                          fluidRow(
  #                            box(sliderInput('string_power', 'Power', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3),
  #                            box(sliderInput('string_spin', 'Spin', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3),
  #                            box(sliderInput('string_tension_stability', 
  #                                            'Tension Stability', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3),
  #                            box(sliderInput('string_tester_satisfaction', 
  #                                            'Overall User Satisfaction', 
  #                                            min = 1, max = 10, value = 5), 
  #                                width = 3)
  #                          )
  #                          )
  #               )
  #             ),
  #             #  tabPanel("String Adjectives",
  #             #          h3('Select whether you like or dislike these types 
  #             #             of strings:'),
  #             #          'Scale:', 
  #             #          HTML('&emsp;'), '+2 = Strongly Like,', 
  #             #          HTML('&emsp;'), '+1 = Like,', 
  #             #          HTML('&emsp;'), '0 = Neutral,', 
  #             #          HTML('&emsp;'), '-1 = Dislike,',
  #             #          HTML('&emsp;'), '-2 = Strongly Dislike',
  #             #          fluidRow(
  #             #            box(sliderInput('soft', 'Soft', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('comfortable', 'Comfortable', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('resilient', 'Resilient', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('flexible', 'Flexible', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('elastic', 'Elastic', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('lively', 'Lively', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2)
  #             #            ),
  #             #          fluidRow(
  #             #            box(sliderInput('explosive', 'Explosive', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('stretchy', 'Stretchy', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('spongy', 'Spongy', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('springy', 'Springy', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('stiff', 'Stiff', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('wire-like', 'Wire-like', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2)
  #             #            ),
  #             #          fluidRow(
  #             #            box(sliderInput('solid', 'Solid', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('crispy', 'Crispy', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('precise', 'Precise', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('dull', 'Dull', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('sluggish', 'Sluggish', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('boring', 'Boring', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2)
  #             #            ),
  #             #          fluidRow(
  #             #            box(sliderInput('rough', 'Rough', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('innovative', 'Innovative', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('outdated', 'Outdated', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2),
  #             #            box(sliderInput('unique', 'Unique', 
  #             #                            min = -2, max = 2, value = 0), 
  #             #                width = 2)
  #             #            )
  #             #          ),
  #             
  #             
  #       #fluidRow(h3("Only Include Racquets with Multiple Reviews? Select Minimum Number of Reviews")),
  #       #fluidRow(
  #       #  box(sliderInput("racquet_minimum_reviews", "Minimum # of Reviews", min = 1, max = 289, value = 1), width = 3)),
  #       fluidRow(box(selectizeInput("selected_racquet", "Select Racquet", racquet_means_clean$tester_racquet))),
  #       fluidRow(box(DT::dataTableOutput("racquet_means_table"), width = 12)),
  #       fluidRow(box(DT::dataTableOutput("racquet_details_table"), width = 12))
  #             ),
  #     tabItem(tabName = "profile", h2("Average Ratings and Detailed Reviews Based on Reviewer Name"),
  #             #fluidRow(h3("Do You Want To Include Only Racquets with Multiple Reviews? If So, Select Minimum Number of Reviews")),
  #             #fluidRow(
  #             #  box(sliderInput("racquet_minimum_reviews", "Minimum # of Reviews", min = 1, max = 289, value = 1), width = 3)),
  #             fluidRow(box(selectizeInput("selected_reviewer", "Select Reviewer", tester_means_clean$tester_name))),
  #             fluidRow(box(DT::dataTableOutput("tester_means_table"), width = 12)),
  #             fluidRow(box(DT::dataTableOutput("tester_details_table"), width = 12))
  #             )
  #     )
  #   )
  # ))
