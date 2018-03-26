shinyUI(dashboardPage(
  dashboardHeader(title = "Tennis String Finder"),
  dashboardSidebar(
    sidebarUserPanel('Iman Singh',
                     image = 'imansingh_headshot.jpg'),
    sidebarMenu(
      menuItem('App Info', tabName = 'info',
               icon = icon('info-circle')),
      menuItem('Review Criteria', tabName = 'criteria', 
               icon = icon('filter')),
      menuItem('String Rankings', tabName = 'selector', 
               icon = icon('sort')),
      menuItem('String Profiles', tabName = 'string_profile', 
               icon = icon('search-plus')),
      br(),
      br(),
      br(),
      menuItem('About Iman:'),
      menuItem('Blog', icon = icon('wordpress'), 
               href = 'https://nycdatascience.com/blog/author/imansingh/'),
      menuItem('LinkedIn', icon = icon('linkedin-square'), 
               href = 'https://www.linkedin.com/in/imansingh/'),
      menuItem('GitHub', icon = icon('github'), 
               href = 'https://github.com/imansingh/Scraping-Project')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'info',
              h1('Tennis String Finder'),
              HTML('<h3>An App for Helping  Tennis Players Find the Perfect 
                        String</h3>
                   <br> <h4>
                   Three steps to finding the right string: 
                   <br><br>
                   <ol>
                   <li>Use "Review Criteria" sidebar item to filter reviews, 
                       keeping only the ones relevant to you. As you make 
                       selections in the three input panels, the output table 
                       will update. This filtered output table will be used for 
                       rankings, so be sure to leave enough data for an accurate 
                       analysis! </li>
                   <br>
                   <li>Use "String Rankings" sidebar item to rank strings based
                       on your preferred weights for string characteristics and 
                       adjectives.</li>
                   <br>
                   <li>Use "String Profile" sidebar item to view detailed review 
                       information about the top ranked strings, in order to 
                       select ones to test (and, after testing, write a review 
                       on stringforum with your evaluation).</li>
                   </ol> </h4>
                   <br> <br>
                   For more information about this app and the string finder 
                   concept, see my 
                   <a href="https://nycdatascience.com/blog/student-works/web-scraping-building-an-app-to-find-the-perfect-tennis-string/">
                      blog post</a>.
                   <br>
                   Data for this app were scraped from 
                   <a href="https://www.stringforum.net/stringsearch.php">stringforum.net</a>')
              
        
        ),
      tabItem(tabName = 'criteria', 
              h2('Filter Which Reviews are Used for String Rankings and Profiles'),
              fluidRow(style = 'padding:15px',
                       h3('Select Your String Criteria, Tester Criteria, and 
                          Tester Racquet Criteria:'),
                       h4('The table below will update based on your choices')),
              fluidRow(
                tabBox(
                  id = 'criteriaInput', 
                  width = 12,
                  selected = 'String Criteria',
                  tabPanel('String Criteria',
                           h4('Only include strings with these 
                              attributes:'),
                           fluidRow(
                             box(sliderInput('string_minimum_reviews', 
                                             'Minimum # of Total Reviews', 
                                             min = min(string_data$num_ratings, 
                                                       na.rm = TRUE), 
                                             max = max(string_data$num_ratings, 
                                                       na.rm = TRUE), 
                                             value = 1), 
                                 width = 6),
                             box(sliderInput(
                               'string_price', 
                               'Price Range ($)',
                               min = min(string_data$price_adjusted, 
                                         na.rm = TRUE),
                               max = max(string_data$price_adjusted,
                                         na.rm = TRUE),
                               value=c(min(string_data$price_adjusted,
                                           na.rm = TRUE),
                                       max(string_data$price_adjusted,
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
                               choices = c(string_material_list,
                                 none_text),
                               selected = c(string_material_list,
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4),
                             box(checkboxGroupInput(
                               'string_construction', 
                               'String Construction',
                               choices = c(string_construction_list,
                                 none_text),
                               selected = c(string_construction_list,
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4),
                             box(checkboxGroupInput(
                               'string_features', 
                               'String Features',
                               choices = c(string_features_list,
                                 none_text),
                               selected = c(string_features_list,
                                 none_text)),
                               actionLink('selectall',"Select All"),
                               HTML('&ensp;'), '|', HTML('&ensp;'),
                               actionLink('clearall', "Clear All"),
                               width = 4)
                             ),
                           fluidRow(
                             column(width = 4,
                                    h4('String Gauge:')),
                             column(width = 4,
                                    box(radioButtons(
                                      'gauge_choice',
                                      'Do you want to use Metric or US scale?',
                                      choices = list(
                                        'Metric Scale' = 'metric',
                                        'US Scale' = 'US'),
                                        selected = 'metric'),
                                      width = NULL)),
                             column(width = 4,
                                    conditionalPanel(
                                      'input.gauge_choice == "metric"',
                                      box(sliderInput(
                                        'string_gauge_metric',
                                        'String Gauge - Metric (mm)',
                                        min = min(string_data$
                                                    string_gauge_metric,
                                                  na.rm = TRUE),
                                        max = max(string_data$
                                                    string_gauge_metric,
                                                  na.rm = TRUE),
                                       value=c(min(string_data$
                                                     string_gauge_metric,
                                                   na.rm = TRUE),
                                                       max(string_data$
                                                             string_gauge_metric,
                                                           na.rm = TRUE))),
                                               checkboxInput('gauge_metric_missing',
                                                             'Include strings with no
                                                             gauge listed',
                                                             value = TRUE),
                                       width = NULL)),
                                    conditionalPanel(
                                      'input.gauge_choice == "US"',
                                      box(sliderInput(
                                       'string_gauge_us', 
                                       'String Gauge - US',
                                       min = min(string_data$string_gauge_us,
                                                 na.rm = TRUE),
                                       max = max(string_data$string_gauge_us,
                                                 na.rm = TRUE),
                                       value=c(min(string_data$string_gauge_us,
                                                   na.rm = TRUE),
                                               max(string_data$string_gauge_us,
                                                   na.rm = TRUE))),
                                       checkboxInput('gauge_us_missing',
                                                     'Include strings with no
                                                     gauge listed',
                                                     value = TRUE),
                                       width = NULL)))
                             )
                  #          fluidRow(
                  #            column(width = 4,
                  #                   h4('String Adjectives (Include):'),
                  #                   'Only include strings where one or more
                  #                   reviewers listed these adjectives'),
                  #            column(width = 8,
                  #                   box(selectizeInput(
                  #                     'string_adjectives_positive',
                  #                     'Filter by Adjectives (Positive)',
                  #                     choices = adjectives_list,
                  #                     multiple = TRUE,
                  #                     options = list(placeholder =
                  #                                      '(choose one or more)')),
                  #                     checkboxInput('adjectives_positive_missing', 
                  #                                   'Include strings with no 
                  #                                   adjectives listed',
                  #                                   value = TRUE),
                  #                     width = NULL))
                  #            ),
                  #          fluidRow(
                  #            column(width = 4,
                  #                   h4('String Adjectives (Exclude):'),
                  #                   'Do not include any strings where a reviewer
                  #                   listed these adjectives'),
                  #            column(width = 8,
                  #                   box(selectizeInput(
                  #                     'string_adjectives_negative',
                  #                     'Filter by Adjectives (Negative)',
                  #                     choices = adjectives_list,
                  #                     multiple = TRUE,
                  #                     options = list(placeholder =
                  #                                      '(choose one or more)')),
                  #                     checkboxInput('adjectives_negative_missing', 
                  #                                   'Include strings with no 
                  #                                   adjectives listed',
                  #                                   value = TRUE),
                  #                     width = NULL))
                  #            )
                            ),
                  tabPanel("Tester Criteria",
                           h4('Only include reviews by testers with these 
                              attributes:'),
                           fluidRow(
                             box(sliderInput(
                               'tester_minimum_reviews', 
                               'Minimum # of Reviews Written by Tester', 
                               min = min(string_data$tester_reviews), 
                               max = max(string_data$tester_reviews), 
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
                           h4('Only include reviews by testers using these 
                              types of racquets:'),
                           fluidRow( 
                             box(selectizeInput(
                               'racquet_manufacturer', 
                               'Tester Racquet Manufacturer(s)', 
                               choices = sort(string_data$racquet_manufacturer
                                              [string_data$racquet_manufacturer != '']),
                               multiple = TRUE,
                               options = list(placeholder = 
                                                '(choose one or more)')),
                               checkboxInput('manufacturer_missing', 
                                             'Include reviews for racquets with 
                                              no manufacturer listed',
                                             value = TRUE),
                               width = 4),
                             box(uiOutput('racquet_model_list'),
                               checkboxInput('model_missing',
                                             'Include reviews for racquets with
                                              no model listed',
                                             value = TRUE),
                               width = 4),
                             box(selectizeInput(
                               'string_pattern',
                               'Tester String Pattern',
                               choices = sort(string_data$string_pattern),
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
                               'Tension - Main Strings (lbs)', 
                               min = 35, max = 75, value = c(35,75),
                               round = TRUE),
                               checkboxInput('main_missing', 
                                             'Include reviews for racquets with 
                                             no main tension listed',
                                             value = TRUE),
                               width = 4),
                             box(sliderInput(
                               'cross_tension', 
                               'Tension - Cross Strings (lbs)', 
                               min = 35, max = 75, value = c(35,75),
                               round = TRUE),
                               checkboxInput('cross_missing', 
                                             'Include reviews for racquets with 
                                              no cross tension listed',
                                             value = TRUE),
                               width = 4)
                             )
                           ),
                  tabPanel("Reference",
                           HTML('<ul>
                                    <li><strong>Price:</strong> prices listed in 
                                        Euros were converted to US Dollars at 
                                        exchange rate of 1 EUR to 1.24 USD
                                    </li> <br>
                                    <li><strong>Gauge:</strong> 
                                        US/Metric coversions made 
                                        according to the official USRSA chart on
                                        <a href="http://www.tennis-warehouse.com/LC/StringReference.html">this page</a>
                                        Note that the ranges overlap. When converting from US 
                                        to metric, mean value of range was used. <br> <br>
                                        Because of the overlap, I could not use
                                        the chart as when converting from 
                                        metric to US. Instead, I used these
                                        ranges:
                                        <ul>
                                            <li>US 15 = Metric >= 1.34 & <= 1.49)</li>
                                            <li>US 16 = Metric >= 1.23 & <= 1.33)</li>
                                            <li>US 17 = Metric >= 1.16 & <= 1.22)</li>
                                            <li>US 18 = Metric >= 1.06 & <= 1.15)</li>
                                            <li>US 19 = Metric >= .9 & <= 1.05)</li>
                                        </ul> <br>
                                        To make US gauges into numeric values,
                                        compatible with the slider input, I 
                                        dropped the "L" when it was present.
                                    </li> <br>
                                    <li><strong>Tester Attributes:</strong> All
                                        tester attributes are self-reported by
                                        reviewers, and there are no guidelines 
                                        given on the website
                                    </li>
                                </ul>'
                           )
                    )
                  )
                
                ),
              fluidRow(style = 'padding:15px',
                       h3(textOutput('criteria_table_title'))),
              fluidRow(box(DT::dataTableOutput('criteria_table'), width = 12))
              ),
      tabItem(tabName = "selector",
              h2('Find Top Ranked Strings from Filtered Reviews'),
              fluidRow(style = 'padding:15px',
                       h3('Select Weights for String Characteristics and/or 
                          String Adjectives:'),
                       h4('The table below will update based on your choices')
                       ),
              fluidRow(tabBox(
                  id = 'selectorInput',
                  width = 12,
                  selected = 'String Characteristics',
                  tabPanel('String Characteristics',
                           h4('Select how important these characteristics are to you,
                              on a scale of 0-10:'),
                           fluidRow(
                             box(sliderInput('string_comfort', 'Comfort',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_control', 'Control',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_durability', 'Durability',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_feel', 'Feel',
                                             min = 0, max = 10, value = 5),
                                 width = 3)
                           ),
                           fluidRow(
                             box(sliderInput('string_power', 'Power',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_spin', 'Spin',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_tension_stability',
                                             'Tension Stability',
                                             min = 0, max = 10, value = 5),
                                 width = 3),
                             box(sliderInput('string_overall_satisfaction',
                                             'Overall User Satisfaction',
                                             min = 0, max = 10, value = 5),
                                 width = 3)
                             )
                           ),
                  tabPanel("String Adjectives",
                           h4('Select whether you like or dislike these types
                              of strings:'),
                           'Scale:',
                           HTML('&emsp;'), '+2 = Strongly Like,',
                           HTML('&emsp;'), '+1 = Like,',
                           HTML('&emsp;'), '0 = Neutral,',
                           HTML('&emsp;'), '-1 = Dislike,',
                           HTML('&emsp;'), '-2 = Strongly Dislike',
                           fluidRow(
                             box(sliderInput('soft', 'Soft',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('comfortable', 'Comfortable',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('resilient', 'Resilient',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('flexible', 'Flexible',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('elastic', 'Elastic',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('lively', 'Lively',
                                             min = -2, max = 2, value = 0),
                                 width = 2)
                           ),
                           fluidRow(
                             box(sliderInput('explosive', 'Explosive',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('stretchy', 'Stretchy',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('spongy', 'Spongy',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('springy', 'Springy',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('stiff', 'Stiff',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('wire_like', 'Wire-like',
                                             min = -2, max = 2, value = 0),
                                 width = 2)
                           ),
                           fluidRow(
                             box(sliderInput('solid', 'Solid',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('crispy', 'Crispy',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('precise', 'Precise',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('dull', 'Dull',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('sluggish', 'Sluggish',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('boring', 'Boring',
                                             min = -2, max = 2, value = 0),
                                 width = 2)
                           ),
                           fluidRow(
                             box(sliderInput('rough', 'Rough',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('innovative', 'Innovative',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('outdated', 'Outdated',
                                             min = -2, max = 2, value = 0),
                                 width = 2),
                             box(sliderInput('unique', 'Unique',
                                             min = -2, max = 2, value = 0),
                                 width = 2)
                             )
                           ),
                  tabPanel("Reference",
                           HTML('<ul>
                                 <li> <strong> # Reviews (ch): </strong> 
                                    Number of reviews for the string with at
                                    least one characteristic rated
                                </li> <br>
                                <li> <strong> Characteristics Scores: </strong> 
                                    % of maximum possible score for that characteristic 
                                    <br>
                                    A score of 50 can be interpreted 
                                    as: "Taking into account only the
                                    reviews where the string was rated for this 
                                    characteristic, the string received exactly 
                                    50% of the maximum possible rating"
                                </li> <br>
                                <li> <strong> Overall Characteristics Score: </strong> 
                                    The weighted average of
                                    all the individual characteristics scores,
                                    with weights input by user. By default, all
                                    characteristics get equal weight (5)
                                </li> <br>
                                <li> <strong> # Reviews (adj): </strong> 
                                    Number of reviews for the string with at
                                    least one adjective selected
                                </li> <br>
                                <li> <strong> Adjectives Scores: </strong> 
                                    % of reviews selecting the adjective 
                                    <br> 
                                    A score of 50 can be interpreted as: "Taking 
                                    into account only the reviews where at least 
                                    one adjective was selected, exactly 50% of 
                                    the reviews for the string included this 
                                    adjective."
                                </li> <br>
                                <li> <strong> Overall Adjectives Score: </strong> 
                                    The weighted average of
                                    all the individual adjectives scores,
                                    with weights input by user. No adjectives 
                                    count toward overall score unless user 
                                    changes the weight (default weights are 0)
                                </li> <br>
                                <li> <strong> Cell Background Colors: </strong>
                                    Colored according to 
                                    percentile within filtered dataset, 
                                    in 5% increments. If the score is above the 
                                    55th percentile for a metric, its cell will 
                                    be green. If it’s below the 45th percentile, 
                                    its cell will be red. The middle percentiles 
                                    are white, and shades of color get darker 
                                    toward the extremes
                                </li> <br>
                                </ul>')
                           )
                  )),
              fluidRow(style = 'padding:15px',
                       h3('Table Options')
                       ),
              fluidRow(box(radioButtons('table_choice',
                           'Do you want to rank strings based on
                           String Characteristics, String Adjectives, or Both?',
                           choices = list('String Characteristics' = 
                                            'characteristics',
                                          'String Adjectives' =
                                            'adjectives',
                                          'Both (Weighted Average)' = 'both'),
                           selected = 'characteristics'),
                           conditionalPanel('input.table_choice == "both"',
                                            'Select your preferred weights:',
                                            fluidRow(
                                              box(sliderInput(
                                                'c_weight',
                                                'String Characteristics Weight',
                                                min = 0,
                                                max = 10,
                                                value = 5),
                                                width = 6),
                                              box(sliderInput(
                                                'a_weight',
                                                'String Adjectives Weight',
                                                min = 0,
                                                max = 10,
                                                value = 5),
                                                width = 6)
                                            )
                           ),
                width=6)),
              fluidRow(style = 'padding:15px',
                       h3(textOutput('selector_table_title'))),
              fluidRow(box(DT::dataTableOutput("selector_table"), width = 12))
              ),
      tabItem(tabName = "string_profile",
              h2('Get Review Information about a Specific String'),
              fluidRow(style = 'padding:15px',
                       h3('Select a string:'),
                       h4('The information below will update based on your 
                          choice')
                       ),
              fluidRow(
                box(uiOutput('filtered_strings'),
                  width = 4)
                ),
              fluidRow(
                tabBox(
                  id = 'string_profile_output',
                  width = 12,
                  selected = 'Read Reviews',
                  tabPanel('Read Reviews',
                           box(DT::dataTableOutput("review_table"), 
                               width = NULL)
                           ),
                  tabPanel('Review Word Cloud',
                           box(width = NULL,
                             column(width = 4,
                                    sliderInput("freq",
                                                "Minimum Frequency:",
                                                min = 1,  max = 50, value = 15),
                                    sliderInput("max",
                                                "Maximum Number of Words Displayed:",
                                                min = 1,  max = 300,  value = 100)
                                    ),
                             column(width = 8,
                                    box(plotOutput("wordcloud"),
                                        width = NULL)
                                    )
                             )
                           ),
                  tabPanel('Characteristics Analysis',
                           box(DT::dataTableOutput('characteristics_analysis_table'), 
                               width = NULL)
                           ),
                  # tabPanel('Characteristics Plot',
                  #          h3('Select Characteristics to Plot'),
                  #          fluidRow(
                  #            column(width = 6,
                  #                   box(selectizeInput(
                  #                     'x_var_char',
                  #                     'X-axis Variable', 
                  #                     choices = characteristics_list,
                  #                     multiple = FALSE,
                  #                     options = list(placeholder = 
                  #                                      '(choose x variable)')))),
                  #            column(width = 6,
                  #                    box(selectizeInput(
                  #                      'y_var_char',
                  #                      'Y-axis Variable', 
                  #                      choices = characteristics_list,
                  #                      multiple = FALSE,
                  #                      options = list(placeholder = 
                  #                                       '(choose y variable)'))))
                  #          ),
                  #          fluidRow(
                  #            column(width = 6, 
                  #                   box(DT::dataTableOutput('characteristics_plot_table'), 
                  #                       width = NULL)),
                  #            column(6, box(plotOutput('characteristics_plot', height = 500), 
                  #                          width = NULL))
                  #            )
                  #          ),
                  tabPanel('Adjectives Analysis',
                           box(DT::dataTableOutput('adjectives_analysis_table'), 
                               width = NULL)
                           ),
                  # tabPanel('Adjectives Plot',
                  #          box('Adjectives Plot'), width = NULL
                  #          )
                  tabPanel("Reference",
                           HTML('<ul>
                                <li> <strong> Characteristics Scores: </strong> 
                                % of maximum possible score for that characteristic 
                                <br>
                                A score of 50 can be interpreted 
                                as: "Taking into account only the
                                reviews that contain ratings for this 
                                characteristic, strings in the sample received 
                                exactly 50% of the maximum possible rating"
                                </li> <br>
                                <li> <strong> Adjectives Scores: </strong> 
                                % of reviews selecting the adjective 
                                <br> 
                                A score of 50 can be interpreted as: "Taking 
                                into account only the reviews where at least 
                                one adjective was selected, exactly 50% of 
                                reviews in the sample included this 
                                adjective."
                                </li> <br>
                                </ul>')
                           )
                  )
                )
              )
    )
  )
))
     
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
