shinyServer(function(input, output){
  #options(DT.extensions = list('ColReorder'), DT.options = list(scrollX=TRUE, scrollY=TRUE, colReorder = TRUE))
  # show strings data.table
  
  
  
  ## racquet_model_list
  # Produce updated list of racquet models based on user selection
  # for racquet manufacturer
  output$racquet_model_list = renderUI({
    filtered_model_list = sapply(input$racquet_manufacturer, 
                                 function(string) 
                                   unname(models_by_manufacturer[string]))
    
    selectizeInput(
      'racquet_model',
      'Tester Racquet Model(s)',
      choices = filtered_model_list,
      multiple = TRUE,
      options = list(placeholder = 
                       '(choose one or more)',
                     maxOptions = 2000))
  })
  
  
  output$filtered_strings = renderUI({
    selectizeInput(
      'string_selected', 
      'Select String', 
      choices = sort(get_string_data_filtered()$string_name),
      options = list(placeholder = 
                       '(type part of the name)'))
  })
    
  ## get string_data_filtered()
  # reactive expression to store datatable filtered based on user criteria
  # to be used in criteria_table and selector_table output
  
  get_string_data_filtered = reactive({
    # Filter based on string criteria
    # create string_data_filtered and filter by string_minimum_reviews
    string_data_filtered = string_data_wrangled %>%
      filter(num_ratings >= input$string_minimum_reviews)
    
    # string_price
    if(input$price_missing){
      string_data_filtered = string_data_filtered %>%
        filter(price_adjusted >= input$string_price[1] |
                 is.na(price_adjusted)) %>%
        filter(price_adjusted <= input$string_price[2] |
                 is.na(price_adjusted))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(price_adjusted >= input$string_price[1])  %>%
        filter(price_adjusted <= input$string_price[2])
    }
    
    #string_material
    if(!(is.null(input$string_material))){
      matrix = sapply(input$string_material,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_material))
      if('None Listed' %in% input$string_material){
        matrix = cbind(matrix, 
                       string_data_filtered$string_material == '')
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_construction
    if(!(is.null(input$string_construction))){
      matrix = sapply(input$string_construction,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_construction))
      if('None Listed' %in% input$string_construction){
        matrix = cbind(matrix, 
                       string_data_filtered$string_construction == '')
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_features
    if(!(is.null(input$string_features))){
      matrix = sapply(input$string_features,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_features))
      if('None Listed' %in% input$string_features){
        matrix = cbind(matrix, 
                       string_data_filtered$string_features == '')
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_gauge_metric
    if(input$gauge_metric_missing){
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1] |
                 is.na(string_gauge_metric)) %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2] |
                 is.na(string_gauge_metric))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_metric >= input$string_gauge_metric[1])  %>%
        filter(string_gauge_metric <= input$string_gauge_metric[2])
    }
    
    #string_gauge_us
    if(input$gauge_us_missing){
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1] |
                 is.na(string_gauge_us)) %>%
        filter(string_gauge_us <= input$string_gauge_us[2] |
                 is.na(string_gauge_us))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(string_gauge_us >= input$string_gauge_us[1])  %>%
        filter(string_gauge_us <= input$string_gauge_us[2])
    }
    
    #string_adjectives_positive
    if(!(is.null(input$string_adjectives_positive))){
      matrix = sapply(input$string_adjectives_positive,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_adjectives))
      if(input$adjectives_positive_missing){
        matrix = cbind(matrix, 
                       string_data_filtered$string_adjectives == '')
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    #string_adjectives_negative
    if(!(is.null(input$string_adjectives_negative))){
      matrix = sapply(input$string_adjectives_negative,
                      function(string) 
                        grepl(string, 
                              string_data_filtered$string_adjectives))
      if(!input$adjectives_negative_missing){
        matrix = cbind(matrix, 
                       string_data_filtered$string_adjectives == '')
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) < 1,]
    }
    
    # Filter based on tester criteria
    # tester_minimum_reviews
    string_data_filtered = string_data_filtered %>%
      filter(tester_reviews >= input$tester_minimum_reviews)
    
    # tester_gender
    if(!(is.null(input$tester_gender))){
      matrix = sapply(input$tester_gender,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_gender))
      if('None Listed' %in% input$tester_gender){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_gender))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    } 
    
    # tester_age
    if(!(is.null(input$tester_age))){
      matrix = sapply(input$tester_age,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_age))
      if('None Listed' %in% input$tester_age){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_age))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_style
    if(!(is.null(input$tester_playstyle))){
      matrix = sapply(input$tester_playstyle,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_playstyle))
      if('None Listed' %in% input$tester_playstyle){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_playstyle))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_level
    if(!(is.null(input$tester_level))){
      matrix = sapply(input$tester_level,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_level))
      if('None Listed' %in% input$tester_level){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_level))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_swing_speed
    if(!(is.null(input$tester_strokes))){
      matrix = sapply(input$tester_strokes,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_strokes))
      if('None Listed' %in% input$tester_strokes){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_strokes))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_spin_level
    if(!(is.null(input$tester_spin))){
      matrix = sapply(input$tester_spin,
                      function(string)
                        grepl(string,
                              string_data_filtered$tester_spin))
      if('None Listed' %in% input$tester_spin){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$tester_spin))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # Filter based on tester racquet criteria
    # tester_racquet_manufacturer
    if(!(is.null(input$racquet_manufacturer))){
      matrix = sapply(input$racquet_manufacturer,
                      function(string)
                        grepl(string,
                              string_data_filtered$racquet_manufacturer))
      if(input$manufacturer_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$racquet_manufacturer))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # if(input$manufacturer_missing){
    #   matrix = matrix(is.na(string_data_filtered$racquet_manufacturer))
    #   if(!(is.null(input$racquet_manufacturer))){
    #     matrix = cbind(matrix, 
    #                    sapply(input$racquet_manufacturer, function(string) 
    #                      grepl(string, 
    #                            string_data_filtered$racquet_manufacturer)))
    #   }
    #   string_data_filtered = string_data_filtered[rowSums(matrix) > 0 , ]
    # } else {
    #   if(!(is.null(input$racquet_manufacturer))){
    #     matrix = cbind(matrix, 
    #                    sapply(input$racquet_manufacturer, function(string) 
    #                      grepl(string, 
    #                            string_data_filtered$racquet_manufacturer)))
    #     string_data_filtered = string_data_filtered[rowSums(matrix) > 0 , ]
    #     }
    #   }
    
    # tester_racquet_model
    if(!(is.null(input$racquet_model))){
      matrix = sapply(input$racquet_model,
                      function(string)
                        grepl(string,
                              string_data_filtered$racquet_model))
      if(input$model_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$racquet_model))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_string_pattern
    if(!(is.null(input$string_pattern))){
      matrix = sapply(input$string_pattern,
                      function(string)
                        grepl(string,
                              string_data_filtered$string_pattern))
      if(input$pattern_missing){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$string_pattern))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_frame_size
    size_vec = unname(sapply(input$frame_size, 
                             function(string) 
                               strsplit(string, split = ' ')[[1]][1]))
    
    if(!(is.null(size_vec))){
      matrix = sapply(size_vec,
                      function(string)
                        grepl(string,
                              string_data_filtered$frame_size))
      if('None' %in% size_vec){
        matrix = cbind(matrix,
                       is.na(string_data_filtered$frame_size))
      }
      string_data_filtered = string_data_filtered[rowSums(matrix) > 0,]
    }
    
    # tester_main_tension
    if(input$main_missing){
      string_data_filtered = string_data_filtered %>%
        filter(main_tension >= input$main_tension[1] |
                 main_tension < 35 | main_tension > 75 |
                 is.na(main_tension)) %>%
        filter(main_tension <= input$main_tension[2] |
                 main_tension < 35 | main_tension > 75 |
                 is.na(main_tension))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(main_tension >= input$main_tension[1])  %>%
        filter(main_tension <= input$main_tension[2])
    }
    
    # tester_cross_tension
    if(input$cross_missing){
      string_data_filtered = string_data_filtered %>%
        filter(cross_tension >= input$cross_tension[1] |
                 cross_tension < 35 | cross_tension > 75 |
                 is.na(cross_tension)) %>%
        filter(cross_tension <= input$cross_tension[2] |
                 cross_tension < 35 | cross_tension > 75 |
                 is.na(cross_tension))    
    } else {
      string_data_filtered = string_data_filtered %>%
        filter(cross_tension >= input$cross_tension[1])  %>%
        filter(cross_tension <= input$cross_tension[2])
    }
    
    ## return filtered datatable
    return(string_data_filtered)
  })
  
  
  ## criteria_table_title
  # Produce text above criteria table using data from string_data_filtered()
  output$criteria_table_title = renderText({
    paste0(nrow(get_string_data_filtered()), ' reviews meet your criteria:')
  })
  
  
  ## criteria_table
  # Produce data table using data from string_data_filtered()
  output$criteria_table = DT::renderDataTable({
    
    # we don't need to display characteristics, adjectives or review text since 
    # they are not options related to these in string criteria
    criteria_table_data = get_string_data_filtered() %>%
      select(string_name, num_ratings, price_adjusted, string_material, 
             string_construction, string_features, string_gauge_metric,
             string_gauge_us, string_adjectives, tester_name, tester_reviews,
             tester_gender, tester_age, tester_level, tester_playstyle, 
             tester_strokes, tester_spin, racquet_manufacturer, racquet_model,
             string_pattern, frame_size, main_tension, cross_tension)
    
    # # paste all the vectors of strings into single strings for display
    # criteria_table_data$string_adjectives =
    #   sapply(criteria_table_data$string_adjectives,
    #          function(vec) paste(vec, collapse = ', '))
    # 
    # criteria_table_data$string_material =
    #   sapply(criteria_table_data$string_material,
    #          function(vec) paste(vec, collapse = ', '))
    # 
    # criteria_table_data$string_construction =
    #   sapply(criteria_table_data$string_construction,
    #          function(vec) paste(vec, collapse = ', '))
    # 
    # criteria_table_data$string_features =
    #   sapply(criteria_table_data$string_features,
    #          function(vec) paste(vec, collapse = ', '))
    
    # create datatable
    datatable(criteria_table_data, rownames = FALSE, 
              colnames = c('string name', '# reviews', 'price',
                           'string material', 'string construction',
                           'string features', 'string gauge (mm)',
                           'string gauge (US)', 'string adjectives',
                           'tester name', '# tester reviews', 'tester gender', 
                           'tester age', 'tester level', 'tester playstyle',
                           'tester swing speed', 'tester spin level',
                           'racquet manufacturer', 'racquet model',
                           'string pattern', 'frame size', 'main tension',
                           'cross tension'),
              #extensions = list('ColReorder'), # , 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
                              # fixedColumns = TRUE, 
                              autoWidth = TRUE))) %>%
      formatCurrency('price_adjusted')
  })
  
  ## selector_table_title
  # Produce text above selector table using data from string_data_filtered()
  output$selector_table_title = renderText({
    paste0(length(unique(get_string_data_filtered()$string_name)),
           ' strings meet your criteria:')
  })
  
  ## selector_table
  # Produce sorted/ranked table based on 
  output$selector_table = DT::renderDataTable({
    
    selector_table_characteristics = get_string_data_filtered() %>%
      # group filtered data frame by string names
      group_by(string_name) %>%
      # get the mean scores for each string
      summarise(num_char_reviews = n(), 
                comfort = mean(comfort, na.rm=TRUE), 
                control = mean(control, na.rm=TRUE), 
                durability = mean(durability, na.rm=TRUE), 
                feel = mean(feel, na.rm=TRUE), 
                power = mean(power, na.rm=TRUE), 
                spin = mean(spin, na.rm=TRUE), 
                tension_stab = mean(tension_stability, na.rm=TRUE), 
                satisfaction = mean(overall_satisfaction, na.rm=TRUE)) %>%
      # multiply means by input to get weighted means, then sum to get c_score
      mutate(characteristics_score = 
               comfort * input$string_comfort +
               control * input$string_control +
               durability * input$string_durability +
               feel * input$string_feel +
               power * input$string_power +
               spin * input$string_spin +
               tension_stab * input$string_tension_stability +
               satisfaction * input$string_overall_satisfaction) %>%
      # arrange by c_score, descennding
      arrange(desc(characteristics_score))
    
    
    # convert characteristics_score to percentile - scale for z-stat, then pnorm
    selector_table_characteristics = 
      selector_table_characteristics %>%
      mutate(characteristics_score = 
               pnorm(scale(selector_table_characteristics$
                             characteristics_score)) * 
               100)

     selector_table_adjectives = get_string_data_filtered() %>%
       # remove reviews with no adjectives listed
       filter(!(string_adjectives == '')) %>%
       # for each review get percentage of adjectives listed matching adjective
       mutate(soft = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'soft')) %>%
       mutate(comfortable = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'comfortable')) %>%
       mutate(flexible = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'flexible')) %>%
       mutate(precise = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'precise')) %>%
       mutate(resilient = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'resilient')) %>%
       mutate(explosive = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'explosive')) %>%
       mutate(innovative = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'innovative')) %>%
       mutate(unique = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'unique')) %>%
       mutate(spongy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'spongy')) %>%
       mutate(stiff = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'stiff')) %>%
       mutate(dull = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'dull')) %>%
       mutate(lively = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'lively')) %>%
       mutate(stretchy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'stretchy')) %>%
       mutate(crispy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'crispy')) %>%
       mutate(boring = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'boring')) %>%
       mutate(elastic = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'elastic')) %>%
       mutate(solid = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'solid')) %>%
       mutate(rough = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'rough')) %>%
       mutate(wire_like = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'wire_like')) %>%
       mutate(springy = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'springy')) %>%
       mutate(sluggish = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'sluggish')) %>%
       mutate(outdated = get_adjective_pct(
         get_string_data_filtered()$string_adjectives, 'outdated')) %>%
       # group reviews by string name
       group_by(string_name) %>%
       # for reviews grouped by string name get mean % for each adjective
       summarise(num_adjec_reviews = n(),
                 soft = mean(soft),
                 comfortable = mean(comfortable),
                 flexible = mean(flexible),
                 precise = mean (precise),
                 resilient = mean(resilient),
                 explosive = mean(explosive),
                 innovative = mean(innovative),
                 unique = mean(unique),
                 spongy = mean(spongy),
                 stiff = mean(stiff),
                 dull = mean(dull),
                 lively = mean(lively),
                 stretchy  = mean(stretchy),
                 crispy = mean(crispy),
                 boring = mean(boring),
                 elastic = mean(elastic),
                 solid = mean(solid),
                 rough = mean(rough),
                 wire_like = mean(wire_like),
                 springy = mean(springy),
                 sluggish = mean(sluggish),
                 outdated = mean(outdated)) %>%
       # multiply means by user input to get weighted means, then sum for score 
       mutate(adjectives_score = 
                soft * input$soft +
                comfortable * input$comfortable +
                flexible * input$flexible +
                precise * input$precise +
                resilient * input$resilient +
                explosive * input$explosive +
                innovative * input$innovative +
                unique * input$unique +
                spongy * input$spongy +
                stiff * input$stiff +
                dull * input$dull +
                lively * input$lively +
                stretchy  * input$stretchy +
                crispy * input$crispy +
                boring * input$boring +
                elastic * input$elastic +
                solid * input$solid +
                rough * input$rough +
                wire_like * input$wire_like +
                springy * input$springy +
                sluggish * input$sluggish +
                outdated * input$outdated) %>%
       # arrange by adjectives_score descending
       arrange(desc(adjectives_score))
     
     # convert adjectives_score to percentile - scale for z-stat, then pnorm
     if(sum(selector_table_adjectives$adjectives_score) > 0){
       selector_table_adjectives =
         selector_table_adjectives %>%
         mutate(adjectives_score = 
                  pnorm(scale(selector_table_adjectives$adjectives_score)) * 
                  100)
     }
     
     selector_table_both = selector_table_characteristics %>%
       full_join(selector_table_adjectives, by = 'string_name') %>%
       mutate(combined_score = 
         (characteristics_score * input$c_weight +
            adjectives_score * input$a_weight) /
           sum(input$c_weight, input$a_weight)) %>%
       arrange(desc(combined_score[,]))
     
     # convert combined_score to percentile - scale for z-stat, then pnorm
     selector_table_both =
       selector_table_both %>%
       mutate(combined_score =
                pnorm(scale(selector_table_both$combined_score)) *
                100)
    
     brks_comfort = quantile(selector_table_characteristics$comfort, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
     
     brks_control = quantile(selector_table_characteristics$control, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
  
     brks_durability = quantile(selector_table_characteristics$durability, 
                                 probs = seq(.05, .95, .05), 
                                 na.rm = TRUE)
     
     brks_feel = quantile(selector_table_characteristics$feel, 
                          probs = seq(.05, .95, .05), 
                          na.rm = TRUE)
     
     brks_power = quantile(selector_table_characteristics$power, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_spin = quantile(selector_table_characteristics$spin, 
                          probs = seq(.05, .95, .05), 
                          na.rm = TRUE)
     
     brks_tension = quantile(selector_table_characteristics$tension_stab, 
                              probs = seq(.05, .95, .05), 
                              na.rm = TRUE)
     
     brks_satisfaction = quantile(selector_table_characteristics$satisfaction, 
                                   probs = seq(.05, .95, .05), 
                                   na.rm = TRUE)
     
     brks_characteristics = quantile(selector_table_characteristics$
                                        characteristics_score, 
                              probs = seq(.05, .95, .05), 
                              na.rm = TRUE)
     
     brks_soft = quantile(selector_table_adjectives$soft, 
                                      probs = seq(.05, .95, .05), 
                                      na.rm = TRUE)
     
     brks_comfortable = quantile(selector_table_adjectives$comfortable, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_flexible = quantile(selector_table_adjectives$flexible, 
                                 probs = seq(.05, .95, .05), 
                                 na.rm = TRUE)
     
     brks_precise = quantile(selector_table_adjectives$precise, 
                              probs = seq(.05, .95, .05), 
                              na.rm = TRUE)
     
     brks_resilient = quantile(selector_table_adjectives$resilient, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
     
     brks_explosive = quantile(selector_table_adjectives$explosive, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
     
     brks_innovative = quantile(selector_table_adjectives$innovative, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
     
     brks_unique = quantile(selector_table_adjectives$unique, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
     
     brks_spongy = quantile(selector_table_adjectives$spongy, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_stiff = quantile(selector_table_adjectives$stiff, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_dull = quantile(selector_table_adjectives$dull, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_lively = quantile(selector_table_adjectives$lively, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_stretchy = quantile(selector_table_adjectives$stretchy, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_crispy = quantile(selector_table_adjectives$crispy, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_boring = quantile(selector_table_adjectives$boring, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_elastic = quantile(selector_table_adjectives$elastic, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_solid = quantile(selector_table_adjectives$solid, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)
     
     brks_rough = quantile(selector_table_adjectives$rough, 
                            probs = seq(.05, .95, .05), 
                            na.rm = TRUE)

     brks_wire_like = quantile(selector_table_adjectives$wire_like, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_springy = quantile(selector_table_adjectives$springy, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_sluggish = quantile(selector_table_adjectives$sluggish, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_outdated = quantile(selector_table_adjectives$outdated, 
                           probs = seq(.05, .95, .05), 
                           na.rm = TRUE)
     
     brks_adjectives = quantile(selector_table_adjectives$adjectives_score, 
                                      probs = seq(.05, .95, .05), 
                                      na.rm = TRUE)
     
     brks_combined = quantile(selector_table_both$combined_score, 
                             probs = seq(.05, .95, .05), 
                             na.rm = TRUE)
  

     if(input$table_choice == 'characteristics'){
       datatable(selector_table_characteristics, rownames = TRUE,
                 colnames = c('string name', '# reviews (ch)', 'comfort', 
                              'control', 'durability', 'feel', 
                              'power', 'spin', 'tension stab', 
                              'satisfaction', 'characteristics score'),
                 #extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
                                 # fixedColumns = TRUE, 
                                 autoWidth = TRUE))) %>%
         formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
                                 'power', 'spin', 'tension_stab', 
                                 'satisfaction', 'characteristics_score'),
                     digits = 1) %>%
         formatStyle(columns = 'comfort', 
                     backgroundColor = styleInterval(brks_comfort, clrs)) %>%
         formatStyle(columns = 'control', 
                     backgroundColor = styleInterval(brks_control, clrs)) %>%
         formatStyle(columns = 'durability', 
                     backgroundColor = styleInterval(brks_durability, clrs)) %>%
         formatStyle(columns = 'feel', 
                     backgroundColor = styleInterval(brks_feel, clrs)) %>%
         formatStyle(columns = 'power', 
                     backgroundColor = styleInterval(brks_power, clrs)) %>%
         formatStyle(columns = 'spin', 
                     backgroundColor = styleInterval(brks_spin, clrs)) %>%
         formatStyle(columns = 'tension_stab', 
                     backgroundColor = styleInterval(brks_tension, clrs)) %>%
         formatStyle(columns = 'satisfaction', 
                     backgroundColor = styleInterval(brks_satisfaction, clrs)) %>%
         formatStyle(columns = 'characteristics_score',
                     backgroundColor = styleInterval(brks_characteristics, clrs))
     } else if(input$table_choice == 'adjectives'){
       datatable(selector_table_adjectives, rownames=TRUE,
                 # extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
                 colnames = c('string name', '# reviews (adj)','soft', 'comfortable', 'flexible', 'precise', 
                              'resilient', 'explosive', 'innovative', 'unique', 
                              'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
                              'crispy', 'boring', 'elastic', 'solid', 'rough', 
                              'wire-like', 'springy', 'sluggish', 'outdated', 
                              'adjectives score'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
                                 # fixedColumns = TRUE, 
                                 autoWidth = TRUE))) %>%
       formatRound(columns = c('soft', 'comfortable', 'flexible', 'precise', 
                               'resilient', 'explosive', 'innovative', 'unique', 
                               'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
                               'crispy', 'boring', 'elastic', 'solid', 'rough', 
                               'wire_like', 'springy', 'sluggish', 'outdated', 
                               'adjectives_score'),
                   digits = 1) %>%
         formatStyle(columns = 'soft',
                     backgroundColor = styleInterval(brks_soft, clrs)) %>%
         formatStyle(columns = 'comfortable',
                     backgroundColor = styleInterval(brks_comfortable, clrs)) %>%
         formatStyle(columns = 'flexible',
                     backgroundColor = styleInterval(brks_flexible, clrs)) %>%
         formatStyle(columns = 'precise',
                     backgroundColor = styleInterval(brks_precise, clrs)) %>%
         formatStyle(columns = 'resilient',
                     backgroundColor = styleInterval(brks_resilient, clrs)) %>%
         formatStyle(columns = 'explosive',
                     backgroundColor = styleInterval(brks_explosive, clrs)) %>%
         formatStyle(columns = 'innovative',
                     backgroundColor = styleInterval(brks_innovative, clrs)) %>%
         formatStyle(columns = 'unique',
                     backgroundColor = styleInterval(brks_unique, clrs)) %>%
         formatStyle(columns = 'spongy',
                     backgroundColor = styleInterval(brks_spongy, clrs)) %>%
         formatStyle(columns = 'stiff',
                     backgroundColor = styleInterval(brks_stiff, clrs)) %>%
         formatStyle(columns = 'dull',
                     backgroundColor = styleInterval(brks_dull, clrs)) %>%
         formatStyle(columns = 'lively',
                     backgroundColor = styleInterval(brks_lively, clrs)) %>%
         formatStyle(columns = 'stretchy',
                     backgroundColor = styleInterval(brks_stretchy, clrs)) %>%
         formatStyle(columns = 'crispy',
                     backgroundColor = styleInterval(brks_crispy, clrs)) %>%
         formatStyle(columns = 'boring',
                     backgroundColor = styleInterval(brks_boring, clrs)) %>%
         formatStyle(columns = 'elastic',
                     backgroundColor = styleInterval(brks_elastic, clrs)) %>%
         formatStyle(columns = 'solid',
                     backgroundColor = styleInterval(brks_solid, clrs)) %>%
         formatStyle(columns = 'rough',
                     backgroundColor = styleInterval(brks_rough, clrs)) %>%
         formatStyle(columns = 'wire_like',
                     backgroundColor = styleInterval(brks_wire_like, clrs)) %>%
         formatStyle(columns = 'springy',
                     backgroundColor = styleInterval(brks_springy, clrs)) %>%
         formatStyle(columns = 'sluggish',
                     backgroundColor = styleInterval(brks_sluggish, clrs)) %>%
         formatStyle(columns = 'outdated',
                     backgroundColor = styleInterval(brks_outdated, clrs)) %>%
         formatStyle(columns = 'adjectives_score', 
                     backgroundColor = styleInterval(brks_adjectives, clrs)
                     # ifelse(
                     #   length(selector_table_adjectives$adjectives_score) == 0,
                     #   backgroundColor = NULL,
                     #   backgroundColor = styleInterval(
                     #     quantile(selector_table_adjectives$adjectives_score, 
                     #              probs = seq(.05, .95, .05)), 
                     #     clrs)
                     #   )
                     )
       } else if(input$table_choice == 'both'){
       datatable(selector_table_both, rownames=TRUE,
                 colnames = c('string name', '# reviews (ch)','comfort', 
                              'control', 'durability', 'feel',
                              'power', 'spin', 'tension stab', 
                              'satisfaction', 'characteristics score', 
                              '# reviews (adj)',
                              'soft', 'comfortable', 'flexible', 'precise', 
                              'resilient', 'explosive', 'innovative', 'unique', 
                              'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
                              'crispy', 'boring', 'elastic', 'solid', 'rough', 
                              'wire-like', 'springy', 'sluggish', 'outdated', 
                              'adjectives score', 'combined score'),
                 #extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
                 options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
                                 # fixedColumns = TRUE, 
                                 autoWidth = TRUE))) %>%
           formatRound(columns = c('comfort', 'control', 'durability', 'feel',
                                   'power', 'spin', 'tension_stab', 
                                   'satisfaction', 'characteristics_score',
                                   'soft', 'comfortable', 'flexible', 'precise', 
                                   'resilient', 'explosive', 'innovative', 'unique', 
                                   'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
                                   'crispy', 'boring', 'elastic', 'solid', 'rough', 
                                   'wire_like', 'springy', 'sluggish', 'outdated', 
                                   'adjectives_score', 'combined_score'),
                       digits = 1) %>%
           formatStyle(columns = 'comfort', 
                       backgroundColor = styleInterval(brks_comfort, clrs)) %>%
           formatStyle(columns = 'control', 
                       backgroundColor = styleInterval(brks_control, clrs)) %>%
           formatStyle(columns = 'durability', 
                       backgroundColor = styleInterval(brks_durability, clrs)) %>%
           formatStyle(columns = 'feel', 
                       backgroundColor = styleInterval(brks_feel, clrs)) %>%
           formatStyle(columns = 'power', 
                       backgroundColor = styleInterval(brks_power, clrs)) %>%
           formatStyle(columns = 'spin', 
                       backgroundColor = styleInterval(brks_spin, clrs)) %>%
           formatStyle(columns = 'tension_stab', 
                       backgroundColor = styleInterval(brks_tension, clrs)) %>%
           formatStyle(columns = 'satisfaction', 
                       backgroundColor = styleInterval(brks_satisfaction, clrs)) %>%
           formatStyle(columns = 'characteristics_score',
                       backgroundColor = styleInterval(brks_characteristics, clrs)) %>%
           formatStyle(columns = 'soft',
                       backgroundColor = styleInterval(brks_soft, clrs)) %>%
           formatStyle(columns = 'comfortable',
                       backgroundColor = styleInterval(brks_comfortable, clrs)) %>%
           formatStyle(columns = 'flexible',
                       backgroundColor = styleInterval(brks_flexible, clrs)) %>%
           formatStyle(columns = 'precise',
                       backgroundColor = styleInterval(brks_precise, clrs)) %>%
           formatStyle(columns = 'resilient',
                       backgroundColor = styleInterval(brks_resilient, clrs)) %>%
           formatStyle(columns = 'explosive',
                       backgroundColor = styleInterval(brks_explosive, clrs)) %>%
           formatStyle(columns = 'innovative',
                       backgroundColor = styleInterval(brks_innovative, clrs)) %>%
           formatStyle(columns = 'unique',
                       backgroundColor = styleInterval(brks_unique, clrs)) %>%
           formatStyle(columns = 'spongy',
                       backgroundColor = styleInterval(brks_spongy, clrs)) %>%
           formatStyle(columns = 'stiff',
                       backgroundColor = styleInterval(brks_stiff, clrs)) %>%
           formatStyle(columns = 'dull',
                       backgroundColor = styleInterval(brks_dull, clrs)) %>%
           formatStyle(columns = 'lively',
                       backgroundColor = styleInterval(brks_lively, clrs)) %>%
           formatStyle(columns = 'stretchy',
                       backgroundColor = styleInterval(brks_stretchy, clrs)) %>%
           formatStyle(columns = 'crispy',
                       backgroundColor = styleInterval(brks_crispy, clrs)) %>%
           formatStyle(columns = 'boring',
                       backgroundColor = styleInterval(brks_boring, clrs)) %>%
           formatStyle(columns = 'elastic',
                       backgroundColor = styleInterval(brks_elastic, clrs)) %>%
           formatStyle(columns = 'solid',
                       backgroundColor = styleInterval(brks_solid, clrs)) %>%
           formatStyle(columns = 'rough',
                       backgroundColor = styleInterval(brks_rough, clrs)) %>%
           formatStyle(columns = 'wire_like',
                       backgroundColor = styleInterval(brks_wire_like, clrs)) %>%
           formatStyle(columns = 'springy',
                       backgroundColor = styleInterval(brks_springy, clrs)) %>%
           formatStyle(columns = 'sluggish',
                       backgroundColor = styleInterval(brks_sluggish, clrs)) %>%
           formatStyle(columns = 'outdated',
                       backgroundColor = styleInterval(brks_outdated, clrs)) %>%
           formatStyle(columns = 'adjectives_score', 
                       backgroundColor = styleInterval(brks_adjectives, clrs)) %>%
           formatStyle(columns = 'combined_score', 
                       backgroundColor = styleInterval(brks_combined, clrs))
     }
     
    
    # 
    #%>%
      #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>%
      # formatRound(columns = c('comfort', 'control', 'durability', 'feel',
      #                         'power', 'spin', 'tension_stab', 'satisfaction'),
      #            digits = 4)
    })
  
  ## reviews_table
  # Produce data table using data from string_data_filtered()
  
  get_string_data_specific = reactive({
    get_string_data_filtered() %>%
      filter(string_name == input$string_selected) 
    })
  
  output$review_table = DT::renderDataTable({
    # paste all the vectors of strings into single strings for display
    req(input$string_selected)
    
    review_table_data = get_string_data_specific() %>%
      select(string_name, tester_name , review_text, string_adjectives, 
             overall_satisfaction, comfort, control, durability, feel, power, 
             spin, tension_stability, price_adjusted,
             string_material, string_construction, string_features,
             tester_reviews, tester_gender, tester_age, tester_level,
             tester_playstyle, tester_strokes, tester_spin,
             racquet_manufacturer, racquet_model, string_pattern, frame_size,
             main_tension, cross_tension)
    
    review_table_data$string_adjectives =
      sapply(review_table_data$string_adjectives,
             function(vec) paste(vec, collapse = ', '))
    
    review_table_data$string_material = 
      sapply(review_table_data$string_material, 
             function(vec) paste(vec, collapse = ', '))
    
    review_table_data$string_construction = 
      sapply(review_table_data$string_construction, 
             function(vec) paste(vec, collapse = ', '))
    
    review_table_data$string_features = 
      sapply(review_table_data$string_features, 
             function(vec) paste(vec, collapse = ', '))
      
    
    # create datatable
    datatable(review_table_data, rownames = FALSE,
              colnames = c('string name', 'tester name', 'review text',
                           'string adjectives', 'overall satisfaction',
                           'comfort', 'control', 'durability', 'feel',
                           'power', 'spin', 'tension stability',
                           'price', 'string material', 'string construction',
                           'string features', '# tester reviews', 'tester gender',
                           'tester age', 'tester level', 'tester playstyle',
                           'tester swing speed', 'tester spin level',
                           'racquet manufacturer', 'racquet model',
                           'string pattern', 'frame size', 'main tension',
                           'cross tension'),
              #extensions = list('ColReorder'), # , 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
                              # fixedColumns = TRUE, 
                              autoWidth = TRUE,
                              columnDefs = list(list(width = '400px',
                                                     targets = 2))
                              ))) %>% 
      formatCurrency('price_adjusted') %>%
      formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
                              'power', 'spin', 'tension_stability', 
                              'overall_satisfaction'),
                  digits = 1)
    
    # Put # reviews, price, material, features, construction up top
  })
  

  terms = reactive({
    filtered_review_text = get_string_data_specific() %>%
      pull(review_text) %>%
      paste(collapse = ' ')
    
      
    getTermMatrix = memoise(function(text) {
      # Careful not to let just any name slip in here; a
      # malicious user could manipulate this value.
      # if (!(book %in% books))
      #   stop("Unknown book")
      # 
      # text <- readLines(sprintf("./%s.txt.gz", book),
      #                   encoding="UTF-8")
      
      myCorpus = Corpus(VectorSource(text))
      myCorpus = tm_map(myCorpus, content_transformer(tolower))
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,
                        c(stopwords("SMART"), "the", "and", "but", 'string', 
                          'strings', 'hour', 'hours', 'day', 'days', 'time',
                          'dont', 'didnt', 'play','player', 'played', 'tennis',
                          'court', 'ball', 'shot', 'shots', 'test', 'tested'))
      
      myDTM = TermDocumentMatrix(myCorpus,
                                 control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      sort(rowSums(m), decreasing = TRUE)
    })
    

    withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(filtered_review_text)
    })

  })
  
  wordcloud_rep = repeatable(wordcloud)
  
  output$wordcloud = renderPlot({
    req(input$string_selected)
    v = terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors = brewer.pal(8, "Dark2"))
  })
  

  output$characteristics_analysis_table = DT::renderDataTable({
    req(input$string_selected)
    
    test = string_data[1,]
    test %>% group_by(string_name) %>% summarise(mean(comfort))
    
    # returns a dataframe grouped by string name with means of characteristics
    # get_characteristics_means = function(df){
    #   df %>% 
    #     group_by(string_name) %>%
    #     summarise(mean(comfort), mean(control), mean(durability), mean(feel),
    #               mean(power), mean(spin), mean(tension_stability), 
    #               mean(overall_satisfaction), sd(comfort))#, #quantile(comfort))
    # }
    
    characteristics_string_means = 
      c(mean(get_string_data_specific()$comfort, na.rm = TRUE),
        mean(get_string_data_specific()$control, na.rm = TRUE),
        mean(get_string_data_specific()$durability, na.rm = TRUE),
        mean(get_string_data_specific()$feel, na.rm = TRUE),
        mean(get_string_data_specific()$power, na.rm = TRUE),
        mean(get_string_data_specific()$spin, na.rm = TRUE),
        mean(get_string_data_specific()$tension_stability, na.rm = TRUE),
        mean(get_string_data_specific()$overall_satisfaction, na.rm = TRUE)
      )
    
    characteristics_sample_means = 
      c(mean(get_string_data_filtered()$comfort, na.rm = TRUE),
        mean(get_string_data_filtered()$control, na.rm = TRUE),
        mean(get_string_data_filtered()$durability, na.rm = TRUE),
        mean(get_string_data_filtered()$feel, na.rm = TRUE),
        mean(get_string_data_filtered()$power, na.rm = TRUE),
        mean(get_string_data_filtered()$spin, na.rm = TRUE),
        mean(get_string_data_filtered()$tension_stability, na.rm = TRUE),
        mean(get_string_data_filtered()$overall_satisfaction, na.rm = TRUE)
      )
    
    characteristics_full_means = 
      c(mean(string_data_wrangled$comfort, na.rm = TRUE),
        mean(string_data_wrangled$control, na.rm = TRUE),
        mean(string_data_wrangled$durability, na.rm = TRUE),
        mean(string_data_wrangled$feel, na.rm = TRUE),
        mean(string_data_wrangled$power, na.rm = TRUE),
        mean(string_data_wrangled$spin, na.rm = TRUE),
        mean(string_data_wrangled$tension_stability, na.rm = TRUE),
        mean(string_data_wrangled$overall_satisfaction, na.rm = TRUE)
      )

    # characteristics_sample_percentile = c(
    #   ecdf(get_string_data_filtered()$comfort)(characteristics_string_means[1]),
    #   ecdf(get_string_data_filtered()$control)(characteristics_string_means[2]),
    #   ecdf(get_string_data_filtered()$durability)(characteristics_string_means[3]),
    #   ecdf(get_string_data_filtered()$feel)(characteristics_string_means[4]),
    #   ecdf(get_string_data_filtered()$power)(characteristics_string_means[5]),
    #   ecdf(get_string_data_filtered()$spin)(characteristics_string_means[6]),
    #   ecdf(get_string_data_filtered()$tension_stability)(characteristics_string_means[7]),
    #   ecdf(get_string_data_filtered()$overall_satisfaction)(characteristics_string_means[8]))
    # 
    # characteristics_full_percentile = c(
    #   ecdf(string_data_wrangled$comfort)(characteristics_string_means[1]),
    #   ecdf(string_data_wrangled$control)(characteristics_string_means[2]),
    #   ecdf(string_data_wrangled$durability)(characteristics_string_means[3]),
    #   ecdf(string_data_wrangled$feel)(characteristics_string_means[4]),
    #   ecdf(string_data_wrangled$power)(characteristics_string_means[5]),
    #   ecdf(string_data_wrangled$spin)(characteristics_string_means[6]),
    #   ecdf(string_data_wrangled$tension_stability)(characteristics_string_means[7]),
    #   ecdf(string_data_wrangled$overall_satisfaction)(characteristics_string_means[8]))
    
    characteristics_sample_z = c(
      (characteristics_string_means[1] - characteristics_sample_means[1]) /
        sd(get_string_data_filtered()$comfort, na.rm = TRUE),
      (characteristics_string_means[2] - characteristics_sample_means[2]) /
        sd(get_string_data_filtered()$control, na.rm = TRUE),
      (characteristics_string_means[3] - characteristics_sample_means[3]) /
        sd(get_string_data_filtered()$durability, na.rm = TRUE),
      (characteristics_string_means[4] - characteristics_sample_means[4]) /
        sd(get_string_data_filtered()$feel, na.rm = TRUE),
      (characteristics_string_means[5] - characteristics_sample_means[5]) /
        sd(get_string_data_filtered()$power, na.rm = TRUE),
      (characteristics_string_means[6] - characteristics_sample_means[6]) /
        sd(get_string_data_filtered()$spin, na.rm = TRUE),
      (characteristics_string_means[7] - characteristics_sample_means[7]) /
        sd(get_string_data_filtered()$tension_stability, na.rm = TRUE),
      (characteristics_string_means[8] - characteristics_sample_means[8]) /
        sd(get_string_data_filtered()$overall_satisfaction, na.rm = TRUE))
      
    characteristics_full_z = c(
      (characteristics_string_means[1] - characteristics_full_means[1]) /
        sd(string_data_wrangled$comfort, na.rm = TRUE),
      (characteristics_string_means[2] - characteristics_full_means[2]) /
        sd(string_data_wrangled$control, na.rm = TRUE),
      (characteristics_string_means[3] - characteristics_full_means[3]) /
        sd(string_data_wrangled$durability, na.rm = TRUE),
      (characteristics_string_means[4] - characteristics_full_means[4]) /
        sd(string_data_wrangled$feel, na.rm = TRUE),
      (characteristics_string_means[5] - characteristics_full_means[5]) /
        sd(string_data_wrangled$power, na.rm = TRUE),
      (characteristics_string_means[6] - characteristics_full_means[6]) /
        sd(string_data_wrangled$spin, na.rm = TRUE),
      (characteristics_string_means[7] - characteristics_full_means[7]) /
        sd(string_data_wrangled$tension_stability, na.rm = TRUE),
      (characteristics_string_means[8] - characteristics_full_means[8]) /
        sd(string_data_wrangled$overall_satisfaction, na.rm = TRUE))
    
    characteristics_sample_percentile = pnorm(characteristics_sample_z) * 100
    
    characteristics_full_percentile = pnorm(characteristics_full_z) * 100
 
    # 
    # brks_char_sample_percentile = quantile(characteristics_sample_percentile, 
    #                             probs = seq(.05, .95, .05))
    # 
    # brks_char_full_percentile = quantile(characteristics_full_percentile, 
    #                           probs = seq(.05, .95, .05))
    
    # brks_char_sample_z = quantile(characteristics_sample_z, 
    #                                   probs = seq(.05, .95, .05))
    # 
    # brks_char_full_z = quantile(characteristics_full_z, 
    #                                 probs = seq(.05, .95, .05))
  
    
    characteristics_analysis_df = 
      data.frame(characteristics_list,
                 characteristics_string_means = 
                   round(characteristics_string_means, 1),
                 characteristics_sample_means =
                   round(characteristics_sample_means, 1),
                                    characteristics_sample_percentile =
                                      round(characteristics_sample_percentile, 1),
                                    characteristics_sample_z =
                                      round(characteristics_sample_z, 2),
                                    characteristics_full_means =
                                      round(characteristics_full_means, 1),
                                    characteristics_full_percentile =
                                      round(characteristics_full_percentile, 1),
                                    characteristics_full_z =
                                      round(characteristics_full_z, 2)) 
    
    datatable(characteristics_analysis_df, rownames=FALSE,
              caption = 'selected sample = what is selected in search criteria',
              colnames = c('characteristics', 'avg score - selected string', 
                           'avg score - selected sample', 'percentile - string within sample',
                           'z score - string within sample', 'avg score - all reviews',
                           'percentile - all reviews', 'z score - all reviews'),
              #extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, paging = FALSE,
                              #colReorder = TRUE,
                              # fixedColumns = TRUE, 
                              autoWidth = TRUE)))  %>%
      # formatRound(columns = c('comfort', 'control', 'durability', 'feel',
      #                         'power', 'spin', 'tension_stab', 
      #                         'satisfaction', 'characteristics_score',
      #                         'soft', 'comfortable', 'flexible', 'precise', 
      #                         'resilient', 'explosive', 'innovative', 'unique', 
      #                         'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
      #                         'crispy', 'boring', 'elastic', 'solid', 'rough', 
      #                         'wire_like', 'springy', 'sluggish', 'outdated', 
      #                         'adjectives_score'),
      #             digits = 4) %>%
      formatStyle(columns = 'characteristics_sample_percentile',
                  backgroundColor = styleInterval(brks_percentile, clrs)) %>%
      formatStyle(columns = 'characteristics_sample_z',
                  backgroundColor = styleInterval(brks_z, clrs)) %>%
      formatStyle(columns = 'characteristics_full_percentile',
                  backgroundColor = styleInterval(brks_percentile, clrs)) %>%
      formatStyle(columns = 'characteristics_full_z',
                  backgroundColor = styleInterval(brks_z, clrs))
  })
  
  # # reactive dataframe for characteristics plot
  # get_characteristics_plot_df = reactive({
  #   get_string_data_filtered()[, c(input$x_var_char, input$y_var_char)]
  # })
  # 
  # # render the characteristics table (with row names)
  # output$characteristics_plot_table = DT::renderDataTable({
  #   
  #   datatable(get_characteristics_plot_df())
  #   
  #   })
  # 
  # # a scatterplot with certain points highlighted
  # output$characteristics_plot = renderPlot({
  #   
  #   s1 = input$characteristics_plot_table_rows_current  # rows on the current page
  #   s2 = input$characteristics_plot_table_rows_all      # rows on all pages (after being filtered)
  #   
  #   par(mar = c(4, 4, 1, .1))
  #   plot(get_characteristics_plot_df(), pch = 21)
  #   
  #   # solid dots (pch = 19) for current page
  #   if (length(s1)) {
  #     points(get_characteristics_plot_df()[s1, , drop = FALSE], pch = 19, cex = 2)
  #   }
  #   
  #   # show red circles when performing searching
  #   if (length(s2) > 0 && length(s2) < nrow(get_characteristics_plot_df())) {
  #     points(get_characteristics_plot_df()[s2, , drop = FALSE], pch = 21, cex = 3, col = 'red')
  #   }
  #   
  #   # dynamically change the legend text
  #   s = input$characteristics_plot_table_search
  #   txt = if (is.null(s) || s == '') 'Filtered data' else {
  #     sprintf('Data matching "%s"', s)
  #   }
  #   
  #   legend(
  #     'topright', c('Original data', 'Data on current page', txt),
  #     pch = c(21, 19, 21), pt.cex = c(1, 2, 3), col = c(1, 1, 2),
  #     y.intersp = 2, bty = 'n'
  #   )
  #   
  # })
  # 
  output$adjectives_analysis_table = DT::renderDataTable({
    req(input$string_selected)
    
    # all_adjectives_string = unlist(
    #   get_string_data_specific() %>%
    #     pull(string_adjectives))
    # 
    # all_adjectives_sample = unlist(get_string_data_filtered()$string_adjectives) 
    # 
    # all_adjectives_full = unlist(string_data_wrangled$string_adjectives)
    
    
    # get_adjective_pct2 = function(str, vec, df){
    #   (sum(vec == str, na.rm = TRUE) / nrow(df)) * 100
    # }
    
    # test = string_data_wrangled %>%
    #   mutate(dull_pct = 
    #            sum(unlist(string_data_wrangled %>%
    #                         filter(string_name == string_data_wrangled$string_name) %>% 
    #                         pull(string_adjectives)) == 'dull', na.rm = TRUE) / 
    #            nrow(string_data_wrangled %>%
    #                   filter(string_name == 'Luxilon Big Banger Alu Power 16L')) * 100)
    # test
    
    adjective_pct_string_mtx = 
      sapply(adjectives_list, get_adjective_pct,
             string_list = get_string_data_specific()$string_adjectives)
    
    adjective_pct_sample_mtx = 
      sapply(adjectives_list, get_adjective_pct,
             string_list = get_string_data_filtered()$string_adjectives)
    
    adjective_pct_full_mtx = 
      sapply(adjectives_list, get_adjective_pct,
             string_list = string_data_wrangled$string_adjectives)
    
    
    
    
    adjective_pct_string_means = colMeans(adjective_pct_string_mtx)
    adjective_pct_sample_means = colMeans(adjective_pct_sample_mtx)
    adjective_pct_full_means = colMeans(adjective_pct_full_mtx)
    
    # # create empty vectors for efficiency
    # adjective_pct_sample_percentile = 
    #   vector(mode = "double", length = length(adjective_pct_string_means))
    # adjective_pct_full_percentile = 
    #   vector(mode = "double", length = length(adjective_pct_string_means))
    # 
    # # get percentile for each column
    # # note: since the matrixes are not grouped by string, these are not really 
    # #       percentile of a string within a larger group of strings
    # #       we are actually computing percentile of a string (mean value 
    # #       of all the reviews) within the larger group of all reviews. 
    # for(i in seq_along(adjective_pct_string_means)){
    #   adjective_pct_sample_percentile[i] = 
    #     ecdf(adjective_pct_sample_mtx[ ,i])(adjective_pct_string_means[i]) * 100
    #   adjective_pct_full_percentile[i] = 
    #     ecdf(adjective_pct_full_mtx[, i])(adjective_pct_string_means[i]) * 100
    # }
    
    #    adjective_pct_string_sd = apply(adjective_pct_string_mtx, 2, sd)
    adjective_pct_sample_sd = apply(adjective_pct_sample_mtx, 2, sd)
    adjective_pct_full_sd = apply(adjective_pct_full_mtx, 2, sd)
    
    adjective_pct_sample_sd = apply(adjective_pct_sample_mtx, 2, sd)
    adjective_pct_full_sd = apply(adjective_pct_full_mtx, 2, sd)
    
    adjective_pct_sample_z = 
      (adjective_pct_string_means - adjective_pct_sample_means) /
      adjective_pct_sample_sd
    
    adjective_pct_full_z = 
      (adjective_pct_string_means - adjective_pct_full_means) /
      adjective_pct_full_sd
    
    adjective_pct_sample_percentile = pnorm(adjective_pct_sample_z) * 100
    
    adjective_pct_full_percentile = pnorm(adjective_pct_full_z) * 100
    
    # adjective_pct_sample_z = 
    #   (adjective_pct_string_means - adjective_pct_sample_means) /
    #   adjective_pct_sample_sd
    # 
    # adjective_pct_full_z = 
    #   (adjective_pct_string_means - adjective_pct_full_means) /
    #   adjective_pct_full_sd
    #   
    
    
    # adjective_pct_string_vec = sapply(adjectives_list, get_adjective_pct2,
    #                                   vec = all_adjectives_string,
    #                                   df = get_string_data_specific())
    # 
    # adjective_pct_sample_vec = sapply(adjectives_list, get_adjective_pct2,
    #                                   vec = all_adjectives_sample,
    #                                   df = get_string_data_filtered())
    # 
    # adjective_pct_full_vec = sapply(adjectives_list, get_adjective_pct2,
    #                                   vec = all_adjectives_full,
    #                                   df = string_data_wrangled)
    
    # brks_adj_string_means = quantile(adjective_pct_string_means, 
    #                                  probs = seq(.05, .95, .05))
    
    # brks_adj_sample_means = quantile(adjective_pct_sample_means, 
    #                                  probs = seq(.05, .95, .05))
    
    # brks_adj_full_means = quantile(adjective_pct_full_means, 
    #                                probs = seq(.05, .95, .05))
    
    brks_adj_sample_z = quantile(adjective_pct_sample_z, 
                                 probs = seq(.05, .95, .05))
    
    brks_adj_full_z = quantile(adjective_pct_full_z, 
                               probs = seq(.05, .95, .05))
    
    
    adjectives_analysis_df = 
      data.frame(adjectives_list,
                 adjective_pct_string_means = adjective_pct_string_means / 100,
                 adjective_pct_sample_means = adjective_pct_sample_means / 100,
                 adjective_pct_sample_percentile = 
                   round(adjective_pct_sample_percentile, 1),
                 adjective_pct_sample_z = round(adjective_pct_sample_z, 2),
                 adjective_pct_full_means = adjective_pct_full_means / 100,
                 adjective_pct_full_percentile = 
                   round(adjective_pct_full_percentile, 1),
                 adjective_pct_full_z = round(adjective_pct_full_z, 2)) %>%
      arrange(desc(adjective_pct_string_means))
    
    datatable(adjectives_analysis_df, rownames=FALSE,
              caption = 'selected sample = what is selected in search criteria',
              colnames = c('adjectives', 'prevalence - selected string', 
                           'prevalence - selected sample', 'percentile - string within sample',
                           'z score - string within sample', 'prevalence - all reviews',
                           'percentile - all reviews', 'z score - all reviews'),
              #extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
              options = (list(scrollX = TRUE, scrollY=TRUE, paging = FALSE,
                              #colReorder = TRUE,
                              # fixedColumns = TRUE, 
                              autoWidth = TRUE)))  %>%
      formatPercentage(columns = c('adjective_pct_string_means', 
                                   'adjective_pct_sample_means', 
                                   'adjective_pct_full_means'), digits = 1) %>%
      # formatStyle(columns = 'adjective_pct_string_means',
      #             backgroundColor = styleInterval(brks_adj_string_means, clrs)) %>%
      # formatStyle(columns = 'adjective_pct_sample_means',
      #             backgroundColor = styleInterval(brks_adj_sample_means, clrs)) %>%
      formatStyle(columns = 'adjective_pct_sample_percentile',
                  backgroundColor = styleInterval(brks_percentile, clrs)) %>%
      formatStyle(columns = 'adjective_pct_sample_z',
                  backgroundColor = styleInterval(brks_z, clrs)) %>%
      # formatStyle(columns = 'adjective_pct_full_means',
      #             backgroundColor = styleInterval(brks_adj_full_means, clrs)) %>%
      formatStyle(columns = 'adjective_pct_full_percentile',
                  backgroundColor = styleInterval(brks_percentile, clrs)) %>%
      formatStyle(columns = 'adjective_pct_full_z',
                  backgroundColor = styleInterval(brks_z, clrs))
  })
#   output$adjectives_rank_table = DT::renderDataTable({
#     req(input$string_selected)
#     
#     all_adjectives_string = unlist(
#       get_string_data_specific() %>%
#         pull(string_adjectives))
#     
#     all_adjectives_sample = unlist(get_string_data_filtered()$string_adjectives) 
#     
#     all_adjectives_full = unlist(string_data_wrangled$string_adjectives)
#     
#     
#     get_adjective_rank = function(str, vec, df){
#       (sum(vec == str, na.rm = TRUE) / nrow(df)) * 100
#     }
#     
#     adjective_pct_string_vec = 
#       sapply(adjectives_list, get_adjective_pct,
#              string_list = get_string_data_specific()$string_adjectives)
#     
#     adjective_pct_sample_vec = 
#       sapply(adjectives_list, get_adjective_pct,
#              string_list = get_string_data_filtered()$string_adjectives)
#     
#     adjective_pct_full_vec = 
#       sapply(adjectives_list, get_adjective_pct,
#              string_list = string_data_wrangled$string_adjectives)
#     
# # 
# #     adjective_pct_string_vec = sapply(adjectives_list, get_adjective_pct2, 
# #                                       vec = all_adjectives_string, 
# #                                       df = get_string_data_specific())
# #     
# #     adjective_pct_sample_vec = sapply(adjectives_list, get_adjective_pct2, 
# #                                       vec = all_adjectives_sample, 
# #                                       df = get_string_data_filtered())
# #     
# #     adjective_pct_full_vec = sapply(adjectives_list, get_adjective_pct2, 
# #                                     vec = all_adjectives_full, 
# #                                     df = string_data_wrangled)
# #     
#     
#     brks_string = quantile(colMeans(adjective_pct_string_vec), 
#                            probs = seq(.05, .95, .05), 
#                            na.rm = TRUE)
#     
#     brks_sample = quantile(colMeans(adjective_pct_sample_vec), 
#                            probs = seq(.05, .95, .05), 
#                            na.rm = TRUE)
#     
#     brks_full = quantile(colMeans(adjective_pct_full_vec), 
#                          probs = seq(.05, .95, .05), 
#                          na.rm = TRUE)
#     
#     adjective_count_df = data.frame(adjectives_list,
#                                     adjective_pct_string_vec, 
#                                     adjective_pct_sample_vec,
#                                     adjective_pct_full_vec)
#     
#     datatable(adjective_count_df, rownames=FALSE,
#               #extensions = list('ColReorder'), #, 'FixedColumns', 'Responsive'),
#               options = (list(scrollX = TRUE, scrollY=TRUE, #colReorder = TRUE,
#                               # fixedColumns = TRUE, 
#                               autoWidth = TRUE)))  %>%
#       # formatRound(columns = c('comfort', 'control', 'durability', 'feel',
#       #                         'power', 'spin', 'tension_stab', 
#       #                         'satisfaction', 'characteristics_score',
#       #                         'soft', 'comfortable', 'flexible', 'precise', 
#       #                         'resilient', 'explosive', 'innovative', 'unique', 
#       #                         'spongy', 'stiff', 'dull', 'lively', 'stretchy', 
#       #                         'crispy', 'boring', 'elastic', 'solid', 'rough', 
#       #                         'wire_like', 'springy', 'sluggish', 'outdated', 
#       #                         'adjectives_score'),
#       #             digits = 4) %>%
#       formatStyle(columns = 'adjective_pct_string_vec',
#                   backgroundColor = styleInterval(brks_string, clrs)) %>%
#       formatStyle(columns = 'adjective_pct_sample_vec',
#                   backgroundColor = styleInterval(brks_sample, clrs)) %>%
#       formatStyle(columns = 'adjective_pct_full_vec',
#                   backgroundColor = styleInterval(brks_full, clrs))
#   
#     
#   })
# 
#   ##
#   # for ranking: 
#   #   selector_table_adjectives = get_string_data_filtered() %>%
#   #   # remove reviews with no adjectives listed
#   #   filter(!(is.na(string_adjectives))) %>%
#   #   # for each review get percentage of adjectives listed matching adjective
#   #   mutate(soft = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'soft')) %>%
#   #   mutate(comfortable = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'comfortable')) %>%
#   #   mutate(flexible = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'flexible')) %>%
#   #   mutate(precise = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'precise')) %>%
#   #   mutate(resilient = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'resilient')) %>%
#   #   mutate(explosive = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'explosive')) %>%
#   #   mutate(innovative = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'innovative')) %>%
#   #   mutate(unique = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'unique')) %>%
#   #   mutate(spongy = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'spongy')) %>%
#   #   mutate(stiff = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'stiff')) %>%
#   #   mutate(dull = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'dull')) %>%
#   #   mutate(lively = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'lively')) %>%
#   #   mutate(stretchy = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'stretchy')) %>%
#   #   mutate(crispy = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'crispy')) %>%
#   #   mutate(boring = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'boring')) %>%
#   #   mutate(elastic = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'elastic')) %>%
#   #   mutate(solid = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'solid')) %>%
#   #   mutate(rough = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'rough')) %>%
#   #   mutate(wire_like = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'wire_like')) %>%
#   #   mutate(springy = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'springy')) %>%
#   #   mutate(sluggish = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'sluggish')) %>%
#   #   mutate(outdated = get_adjective_pct(
#   #     get_string_data_filtered()$string_adjectives, 'outdated')) %>%
#   #   # group reviews by string name
#   #   group_by(string_name) %>%
#   #   # for reviews grouped by string name get mean % for each adjective
#   #   summarise(num_adjec_reviews = n(),
#   #             soft = mean(soft),
#   #             comfortable = mean(comfortable),
#   #             flexible = mean(flexible),
#   #             precise = mean (precise),
#   #             resilient = mean(resilient),
#   #             explosive = mean(explosive),
#   #             innovative = mean(innovative),
#   #             unique = mean(unique),
#   #             spongy = mean(spongy),
#   #             stiff = mean(stiff),
#   #             dull = mean(dull),
#   #             lively = mean(lively),
#   #             stretchy  = mean(stretchy),
#   #             crispy = mean(crispy),
#   #             boring = mean(boring),
#   #             elastic = mean(elastic),
#   #             solid = mean(solid),
#   #             rough = mean(rough),
#   #             wire_like = mean(wire_like),
#   #             springy = mean(springy),
#   #             sluggish = mean(sluggish),
#   #             outdated = mean(outdated)) %>%
#     
#   
#  
#   # output$string_table <- DT::renderDataTable({
#   #   string_means_selected <- string_means %>% 
#   #     filter(reviews >= input$string_minimum_reviews)
#   #   string_means_weighted <- string_means_selected %>% 
#   #     arrange(desc(comfort * input$string_comfort + 
#   #                    control * input$string_control + 
#   #                    durability * input$string_durability + 
#   #                    feel * input$string_feel + 
#   #                    power * input$string_power + 
#   #                    spin * input$string_spin + 
#   #                    tension_stab * input$string_tension_stability + 
#   #                    satisfaction * input$string_overall_satisfaction))
#   #   datatable(string_means_weighted, rownames=TRUE, 
#   #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
#   #             options = (list(sc=rollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
#   #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
#   #   #columnDefs = list(list(width = '200px', targets= c(7,8)))) %>% 
#   #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
#   #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
#   #                 digits = 4)
#   # })
#   # output$racquet_means_table <- DT::renderDataTable({
#   #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
#   #   racquet_selected_means <- racquet_means_clean %>% filter(tester_racquet == input$selected_racquet)
#   #   datatable(racquet_selected_means, rownames=TRUE,
#   #             extensions = list('ColReorder', 'FixedColumns', 'Responsive'),
#   #             options = (list(scrollX = TRUE, scrollY=TRUE, colReorder = TRUE, 
#   #                             fixedColumns = TRUE, autoWidth = TRUE))) %>%
#   #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
#   #                           'power', 'spin', 'tension_stab', 'satisfaction'), 
#   #               digits = 4)
#   # })
#   # 
#   # output$racquet_details_table <- DT::renderDataTable({
#   #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
#   #   racquet_selected_details <- detailed_reviews %>% filter(tester_racquet == input$selected_racquet)
#   #   datatable(racquet_selected_details, rownames = TRUE)
#   # })
#   # 
#   # output$tester_means_table <- DT::renderDataTable({
#   #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
#   #   tester_selected_means <- tester_means_clean %>% filter(tester_name == input$selected_reviewer)
#   #   datatable(tester_selected_means, rownames=TRUE) %>%
#   #     formatRound(columns = c('comfort', 'control', 'durability', 'feel', 
#   #                             'power', 'spin', 'tension_stab', 'satisfaction'), 
#   #                 digits = 4)
#   # })
#   # 
#   # output$tester_details_table <- DT::renderDataTable({
#   #   #racquet_means_selected <- racquet_means_clean %>% filter(reviews >= input$racquet_minimum_reviews)
#   #   tester_selected_details <- detailed_reviews %>% filter(tester_name == input$selected_reviewer)
#   #   datatable(tester_selected_details, rownames = TRUE)
#   # })
 })
 
  
