shinyServer(function(input, output){
  #options(DT.extensions = list('ColReorder'), DT.options = list(scrollX=TRUE, scrollY=TRUE, colReorder = TRUE))
  # show strings data.table
  
  ## Produce filtered data table based on user criteria
  output$criteria_table = DT::renderDataTable({
    
    # Filter based on string criteria
    # minimum_reviews
    string_criteria_filtered = string_data1 %>%
      filter(!(num_ratings < input$string_minimum_reviews))
    
    # string_price
    if(input$price_missing){
      string_criteria_filtered = string_criteria_filtered %>%
        filter(price_adjusted >= input$string_price[1] |
                 is.na(price_adjusted)) %>%
        filter(price_adjusted <= input$string_price[2] |
                 is.na(price_adjusted))    
        } else {
          string_criteria_filtered = string_criteria_filtered %>%
            filter(price_adjusted >= input$string_price[1])  %>%
            filter(price_adjusted <= input$string_price[2])
        }
    
    #string_gauge_metric
    if(input$gauge_metric_missing){
      string_criteria_filtered = string_criteria_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1] |
                 is.na(string_gauge_metric)) %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2] |
                 is.na(string_gauge_metric))    
    } else {
      string_criteria_filtered = string_criteria_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1])  %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2])
    }
    
    #string_gauge_us
    if(input$gauge_us_missing){
      string_criteria_filtered = string_criteria_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1] |
                 is.na(string_gauge_us)) %>%
        filter(string_gauge_us <= input$string_gauge_us[2] |
                 is.na(string_gauge_us))    
    } else {
      string_criteria_filtered = string_criteria_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1])  %>%
        filter(string_gauge_us <= input$string_gauge_us[2])
    }
    
    #string_adjectives_positive
    if(!(is.null(input$string_adjectives_positive))){
      matrix = sapply(input$string_adjectives_positive,
                      function(string) 
                        grepl(string, 
                              string_criteria_filtered$string_adjectives))
      if(input$adjectives_positive_missing){
        matrix = cbind(matrix, 
                       is.na(string_criteria_filtered$string_adjectives))
      }
      string_criteria_filtered = string_criteria_filtered[rowSums(matrix) > 0,]
    }
    
    #string_adjectives_negative
    if(!(is.null(input$string_adjectives_negative))){
      matrix = sapply(input$string_adjectives_negative,
                      function(string) 
                        grepl(string, 
                              string_criteria_filtered$string_adjectives))
      if(!input$adjectives_negative_missing){
        matrix = cbind(matrix, 
                       is.na(string_criteria_filtered$string_adjectives))
      }
      string_criteria_filtered = string_criteria_filtered[rowSums(matrix) < 1,]
    }
    
    
    # if(!input$price_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'price_adjusted']),]
    # }
    
    # if(!input$gauge_metric_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_gauge_metric']),]
    # }
    # 
    # if(!input$gauge_metric_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_gauge_metric']),]
    # }
    # 
    # if(!input$adjectives_positive_missing | !input$adjectives_negative_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_adjectives']),]
    # }
    # 
    # if(!input$material_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_material']),]
    # }
    # 
    # if(!input$construction_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_construction']),]
    # }
    # 
    # if(!input$features_missing){
    #   string_criteria_filtered = string_criteria_filtered[
    #     na.omit(string_criteria_filtered[ , 'string_features']),]
    # }
    
    # string_criteria_filtered = string_data1 %>%
    #   filter(!(num_ratings < input$string_minimum_reviews)) %>%
    #   filter(!(price_adjusted < input$string_price[1])) %>%
    #   filter(!(price_adjusted > input$string_price[2])) %>%
    #   filter(!(string_gauge_metric < input$string_gauge_metric[1])) %>%
    #   filter(!(string_gauge_metric > input$string_gauge_metric[2])) %>%
    #   filter(!(string_gauge_us < input$string_gauge_us[1])) %>%
    #   filter(!(string_gauge_us <= input$string_gauge_us[2]))

    # if(!is.null(input$string_adjectives_positive) &
    #    input$string_adjectives_positive != ''){
    #   print(input$string_adjectives_positive)
    #   matrix = sapply(input$string_adjectives_positive,
    #                   function(string)
    #                     grepl(string,
    #                           string_data1$string_adjectives))
    #   print(matrix)
    #   string_criteria_filtered = string_criteria_filtered[rowSums(matrix) > 0,]
    # }
    # 
    # if(!is.null(input$string_adjectives_negative) &
    #   input$string_adjectives_negative != ''){
    #   matrix = sapply(input$string_adjectives_negative,
    #                   function(string)
    #                     grepl(string,
    #                           string_criteria_filtered$string_adjectives))
    #   string_criteria_filtered = string_criteria_filtered[rowSums(matrix) < 1,]
    # }

    # if(!is.null(input$string_material)){
    #   matrix = sapply(input$string_material,
    #                   function(string)
    #                     grepl(string,
    #                           string_criteria_filtered$string_material))
    #   string_criteria_filtered = string_criteria_filtered[rowSums(matrix) > 0,]
    # }
    # 
    # if(!is.null(input$string_construction)){
    #   matrix = sapply(input$string_construction,
    #                   function(string)
    #                     grepl(string,
    #                           string_criteria_filtered$string_construction))
    #   string_criteria_filtered = string_criteria_filtered[rowSums(matrix) > 0,]
    # }
    # 
    # if(!is.null(input$string_features)){
    #   matrix = sapply(input$string_features,
    #                   function(string)
    #                     grepl(string,
    #                           string_criteria_filtered$string_features))
    #   string_criteria_filtered = string_criteria_filtered[rowSums(matrix) > 0,]
    # }
    
    string_criteria_filtered$string_adjectives = 
      sapply(string_criteria_filtered$string_adjectives, 
             function(vec) paste(vec, collapse = ', '))
    
    string_criteria_filtered$string_material = 
      sapply(string_criteria_filtered$string_material, 
             function(vec) paste(vec, collapse = ', '))
    
    string_criteria_filtered$string_construction = 
      sapply(string_criteria_filtered$string_construction, 
             function(vec) paste(vec, collapse = ', '))
    
    string_criteria_filtered$string_features = 
      sapply(string_criteria_filtered$string_features, 
             function(vec) paste(vec, collapse = ', '))

    datatable(string_criteria_filtered, rownames=TRUE, 
              extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE,
                              fixedColumns = TRUE, autoWidth = TRUE)))
    # %>%
      #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
      # formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
      #                         'power', 'spin', 'tension_stab', 'satisfaction'), 
      #             digits = 4)

    
    
    # string_means_weighted <- string_means_selected %>% 
    #   arrange(desc(comfort * input$string_comfort + 
    #                  control * input$string_control + 
    #                  durability * input$string_durability + 
    #                  feel * input$string_feel + 
    #                  power * input$string_power + 
    #                  spin * input$string_spin + 
    #                  tension_stab * input$string_tension_stability + 
    #                  satisfaction * input$string_tester_satisfaction))
    # datatable(string_means_weighted, rownames=TRUE, 
    #           extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
    #           options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
    #                           fixedColumns = TRUE, autoWidth = TRUE))) %>%
    #   #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
    #   formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
    #                           'power', 'spin', 'tension_stab', 'satisfaction'), 
    #               digits = 4)
  })
  
  # output$string_table <- DT::renderDataTable({
  #   string_means_selected <- string_means %>% 
  #     filter(reviews >= input$string_minimum_reviews)
  #   string_means_weighted <- string_means_selected %>% 
  #     arrange(desc(comfort * input$string_comfort + 
  #                    control * input$string_control + 
  #                    durability * input$string_durability + 
  #                    feel * input$string_feel + 
  #                    power * input$string_power + 
  #                    spin * input$string_spin + 
  #                    tension_stab * input$string_tension_stability + 
  #                    satisfaction * input$string_tester_satisfaction))
  #   datatable(string_means_weighted, rownames=TRUE, 
  #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
  #             options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
  #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
  #   #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
  #                 digits = 4)
  # })
  # output$racquet_means_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   racquet_selected_means <- racquet_means_clean %>% filter(tester_racquet == input$selected_racquet)
  #   datatable(racquet_selected_means, rownames=TRUE,
  #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
  #             options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
  #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                           'power', 'spin', 'tension_stab', 'satisfaction'), 
  #               digits = 4)
  # })
  # 
  # output$racquet_details_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   racquet_selected_details <- detailed_reviews %>% filter(tester_racquet == input$selected_racquet)
  #   datatable(racquet_selected_details, rownames = TRUE)
  # })
  # 
  # output$tester_means_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   tester_selected_means <- tester_means_clean %>% filter(tester_name == input$selected_reviewer)
  #   datatable(tester_selected_means, rownames=TRUE) %>%
  #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
  #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
  #                 digits = 4)
  # })
  # 
  # output$tester_details_table <- DT::renderDataTable({
  #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
  #   tester_selected_details <- detailed_reviews %>% filter(tester_name == input$selected_reviewer)
  #   datatable(tester_selected_details, rownames = TRUE)
  # })
})
 
  
