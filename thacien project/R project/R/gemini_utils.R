# R/gemini_utils.R
# Enhanced AI Integration with Gemini API

#' Initialize Gemini Client
#'
#' @param api_key Your Gemini API key (optional, can be set via environment variable)
#' @return A Gemini client object
initialize_gemini_client <- function(api_key = NULL) {
  # Hardcoded API key for immediate use
  hardcoded_api_key <- "AIzaSyDKcdDI8VTB7iudAtX33QmHEjsBxY5SnVU"
  
  # Get API key from parameter, environment variable, or use hardcoded
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (api_key == "") {
      api_key <- getOption("gemini.api_key")
      if (is.null(api_key)) {
        api_key <- hardcoded_api_key  # Use hardcoded key as fallback
      }
    }
  }
  
  if (api_key == "" || is.null(api_key)) {
    warning("No Gemini API key provided. Using mock responses.")
    return(list(
      api_key = NULL,
      mock_mode = TRUE,
      base_url = "https://generativelanguage.googleapis.com/v1beta"
    ))
  }
  
  return(list(
    api_key = api_key,
    mock_mode = FALSE,
    base_url = "https://generativelanguage.googleapis.com/v1beta"
  ))
}

#' Generate AI Policy Recommendation
#'
#' @param client A Gemini client object
#' @param context A list of context variables for the prompt
#' @return A character string with a policy recommendation
generate_policy_recommendation <- function(client, context) {
  # Basic validation
  if (!is.list(context) || is.null(client)) {
    return("Invalid client or context.")
  }
  
  # If no API key, use enhanced mock responses
  if (client$mock_mode) {
    return(generate_mock_recommendation(context))
  }
  
  # Make actual API call to Gemini
  tryCatch({
    return(make_gemini_api_call(client, context))
  }, error = function(e) {
    warning("API call failed, falling back to mock response: ", e$message)
    return(generate_mock_recommendation(context))
  })
}

#' Make actual Gemini API call
#'
#' @param client A Gemini client object
#' @param context A list of context variables
#' @return A character string with AI-generated recommendation
make_gemini_api_call <- function(client, context) {
  # Construct the prompt
  prompt <- build_policy_prompt(context)
  
  # Prepare API request
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = list(
      temperature = 0.7,
      topK = 40,
      topP = 0.95,
      maxOutputTokens = 1024
    )
  )
  
  # Make HTTP request
  response <- httr::POST(
    url = paste0(client$base_url, "/models/gemini-pro:generateContent?key=", client$api_key),
    httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
    httr::timeout(30)
  )
  
  # Parse response
  if (httr::status_code(response) == 200) {
    result <- httr::content(response, "parsed")
    recommendation <- result$candidates[[1]]$content$parts[[1]]$text
    return(format_ai_recommendation(recommendation, context))
  } else {
    stop("API request failed with status: ", httr::status_code(response))
  }
}

#' Build intelligent prompt for Gemini
#'
#' @param context A list of context variables
#' @return A formatted prompt string
build_policy_prompt <- function(context) {
  district <- context$district %||% "the selected region"
  year <- context$year %||% "current"
  focus_area <- context$focus_area %||% "nutrition improvement"
  target <- context$target %||% "general population"
  budget <- context$budget %||% 1000000
  timeline <- context$timeline %||% 12
  
  lang_name <- if (context$lang %||% "en" == "rw") "Kinyarwanda" else "English"
  # Get additional context from data
  stunting_rate <- context$stunting_rate %||% "unknown"
  wasting_rate <- context$wasting_rate %||% "unknown"
  anemia_rate <- context$anemia_rate %||% "unknown"
  poverty_rate <- context$poverty_rate %||% "unknown"
  
  prompt <- paste0(
    "You are a nutrition policy expert analyzing data for ", district, " in ", year, ". ",
    "Generate a comprehensive, evidence-based policy recommendation with the following context:\n\n",
    
    "NUTRITION INDICATORS:\n",
    "- Stunting rate: ", stunting_rate, "%\n",
    "- Wasting rate: ", wasting_rate, "%\n", 
    "- Anemia rate: ", anemia_rate, "%\n",
    "- Poverty rate: ", poverty_rate, "%\n\n",
    
    "POLICY PARAMETERS:\n",
    "- Focus area: ", focus_area, "\n",
    "- Target population: ", target, "\n",
    "- Available budget: $", format(budget, big.mark = ","), "\n",
    "- Implementation timeline: ", timeline, " months\n\n",
    
    "Please provide a structured policy recommendation that includes:\n",
    "1. Executive Summary (2-3 sentences)\n",
    "2. Key Interventions (3-5 specific actions)\n",
    "3. Implementation Strategy (timeline and resources)\n",
    "4. Expected Outcomes (measurable targets)\n",
    "5. Risk Assessment (potential challenges)\n",
    "6. Success Metrics (how to measure progress)\n\n",
    
    "Base recommendations on evidence-based nutrition interventions and global best practices."
  )
  
  return(prompt)
}

#' Generate enhanced mock recommendation
#'
#' @param context A list of context variables
#' @return A character string with mock recommendation
generate_mock_recommendation <- function(context) {
  district <- context$district %||% "the selected region"
  year <- context$year %||% "current"
  focus_area <- context$focus_area %||% "nutrition improvement"
  target <- context$target %||% "general population"
  budget <- context$budget %||% 1000000
  timeline <- context$timeline %||% 12
  lang <- context$lang %||% "en"
  
  # Analyze context to provide intelligent mock responses
  interventions <- c(
    "Community-based nutrition education programs",
    "Iron and folic acid supplementation for women of reproductive age",
    "School feeding programs with locally-sourced foods",
    "Water, sanitation, and hygiene (WASH) improvements",
    "Agricultural diversification and food security initiatives"
  )
  
  # Select interventions based on focus area
  selected_interventions <- switch(
    tolower(focus_area),
    "stunting reduction" = interventions[1:3],
    "anemia prevention" = interventions[c(2, 4, 5)],
    "wasting prevention" = interventions[c(1, 3, 5)],
    "poverty reduction" = interventions[c(4, 5, 1)],
    interventions[1:3]  # default
  )
  
  # Calculate expected outcomes based on budget
  expected_reduction <- min(15, max(5, budget / 100000))
  
  # Bilingual text
  texts <- list(
    en = list(
      title = "AI Policy Recommendation",
      exec_summary_title = "Executive Summary",
      exec_summary_text = "Based on current nutrition indicators, we recommend a comprehensive {tolower(focus_area)} strategy targeting {target} with a budget allocation of ${format(budget, big.mark = ',')} over {timeline} months.",
      interventions_title = "Key Interventions",
      impl_strategy_title = "Implementation Strategy",
      phase1 = "Phase 1 (Months 1-3): Program design and stakeholder engagement",
      phase2 = "Phase 2 (Months 4-9): Pilot implementation in selected areas",
      phase3 = "Phase 3 (Months 10-{timeline}): Full-scale rollout and monitoring",
      outcomes_title = "Expected Outcomes",
      outcome1 = "{round(expected_reduction)}% reduction in stunting rates",
      outcome2 = "{round(expected_reduction * 0.8)}% improvement in anemia prevalence",
      outcome3 = "Increased community awareness and behavior change",
      outcome4 = "Strengthened health system capacity",
      risk_title = "Risk Assessment",
      risk1 = "Community resistance to behavior change",
      risk2 = "Seasonal variations affecting implementation",
      risk3 = "Resource constraints and budget limitations",
      risk4 = "Coordination challenges among stakeholders",
      metrics_title = "Success Metrics",
      metric1 = "Monthly monitoring of nutrition indicators",
      metric2 = "Community satisfaction surveys",
      metric3 = "Program coverage and participation rates",
      metric4 = "Cost-effectiveness analysis",
      footer = "This is an AI-generated recommendation. For real-time analysis, please configure your Gemini API key."
    ),
    rw = list(
      title = "Icyifuzo cy'Ingamba cya AI",
      exec_summary_title = "Incamake Nyobozi",
      exec_summary_text = "Dushingiye ku bipimo by'imirire bihari, turasaba ingamba yuzuye yo {tolower(focus_area)} igenewe {target} hakoreshejwe ingengo y'imari ya ${format(budget, big.mark = ',')} mu gihe cy'amezi {timeline}.",
      interventions_title = "Ibikorwa by'ingenzi",
      impl_strategy_title = "Ingamba zo Gushyira mu Bikorwa",
      phase1 = "Icyiciro cya 1 (Amezi 1-3): Gutegura gahunda no kugisha inama abafatanyabikorwa",
      phase2 = "Icyiciro cya 2 (Amezi 4-9): Igerageza rya gahunda ahatoranyijwe",
      phase3 = "Icyiciro cya 3 (Amezi 10-{timeline}): Gushyira mu bikorwa hose no gukurikirana",
      outcomes_title = "Ibiteganijwe Kugerwaho",
      outcome1 = "Kugabanuka kwa {round(expected_reduction)}% mu gwingira",
      outcome2 = "Kugabanuka kwa {round(expected_reduction * 0.8)}% mu burwayi bwa anemiya",
      outcome3 = "Ubumenyi bwiyongereye mu baturage n'impinduka mu myitwarire",
      outcome4 = "Gushimangira ubushobozi bwa sisitemu y'ubuzima",
      risk_title = "Isesengura ry'Ibyago",
      risk1 = "Kutakira impinduka mu myitwarire mu baturage",
      risk2 = "Ihindagurika ry'ibihe rishobora kugira ingaruka ku ishyirwa mu bikorwa",
      risk3 = "Amikoro ahagije n'ingengo y'imari nkeya",
      risk4 = "Ingorane mu guhuza ibikorwa by'abafatanyabikorwa",
      metrics_title = "Ibipimo by'Intsinzi",
      metric1 = "Gukurikirana ibipimo by'imirire buri kwezi",
      metric2 = "Isuzuma ry'uko abaturage bishimiye serivisi",
      metric3 = "Igipimo cy'abagezweho na gahunda n'abitabira",
      metric4 = "Isesengura ry'ikiguzi n'inyungu",
      footer = "Iki ni icyifuzo cyakozwe na AI. Kugira ngo ubone isesengura nyaryo, shyiramo urufunguzo rwa Gemini API."
    )
  )
  
  t <- texts[[lang]]
  
  recommendation <- paste0(
    "<div class='ai-recommendation'>",
    "<h4><i class='fa fa-robot'></i> ", t$title, " for ", district, " (", year, ")</h4>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-lightbulb'></i> ", t$exec_summary_title, "</h5>",
    "<p>", glue::glue(t$exec_summary_text), "</p>",
    "</div>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-tasks'></i> ", t$interventions_title, "</h5>",
    "<ul>", paste0("<li>", selected_interventions, "</li>", collapse = ""), "</ul>",
    "</div>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-calendar-alt'></i> ", t$impl_strategy_title, "</h5>",
    "<ul>",
    "<li><strong>", glue::glue(t$phase1), "</strong></li>",
    "<li><strong>", glue::glue(t$phase2), "</strong></li>",
    "<li><strong>", glue::glue(t$phase3), "</strong></li>",
    "</ul>",
    "</div>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-chart-line'></i> ", t$outcomes_title, "</h5>",
    "<ul>",
    "<li>", glue::glue(t$outcome1), "</li>",
    "<li>", glue::glue(t$outcome2), "</li>",
    "<li>", t$outcome3, "</li>",
    "<li>", t$outcome4, "</li>",
    "</ul>",
    "</div>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-exclamation-triangle'></i> ", t$risk_title, "</h5>",
    "<ul>", "<li>", t$risk1, "</li>", "<li>", t$risk2, "</li>", "<li>", t$risk3, "</li>", "<li>", t$risk4, "</li>", "</ul>",
    "</div>",
    
    "<div class='recommendation-section'>",
    "<h5><i class='fa fa-check-circle'></i> ", t$metrics_title, "</h5>",
    "<ul>", "<li>", t$metric1, "</li>", "<li>", t$metric2, "</li>", "<li>", t$metric3, "</li>", "<li>", t$metric4, "</li>", "</ul>",
    "</div>",
    
    "<div class='ai-footer'><small>", t$footer, "</small></div>",
    "</div>"
  )
  return(recommendation)
}

#' Format AI recommendation with styling
#' @param recommendation Raw AI recommendation text (as a single string)
#' @param context A list of context variables
format_ai_recommendation <- function(recommendation, context) {
  # This function now correctly formats the raw text from the Gemini API
  # into the styled HTML structure used by the mock function.
  
  # Split the raw text into sections based on the numbered list format
  sections <- strsplit(recommendation, "\n\\d\\.\\s*")[[1]]
  
  # Helper to extract content from a section
  extract_section <- function(text, title) {
    pattern <- paste0("(?i)", title, ":?\\s*\\n?(.*)")
    match <- regmatches(text, regexec(pattern, text, perl = TRUE))
    if (length(match[[1]]) > 1) {
      return(trimws(match[[1]][2]))
    }
    return(text) # Return full text if title not found
  }
  
  # Extract content for each part of the recommendation
  exec_summary <- extract_section(sections[1], "Executive Summary")
  interventions <- extract_section(sections[2], "Key Interventions")
  impl_strategy <- extract_section(sections[3], "Implementation Strategy")
  outcomes <- extract_section(sections[4], "Expected Outcomes")
  risks <- extract_section(sections[5], "Risk Assessment")
  metrics <- extract_section(sections[6], "Success Metrics")
  
  # Convert bullet points (like '*' or '-') into HTML list items
  to_html_list <- function(text) {
    if (is.null(text) || nchar(text) == 0) return("")
    items <- strsplit(text, "\n\\s*[-*]\\s*")[[1]]
    items <- items[nchar(items) > 0] # Remove empty items
    if (length(items) == 0) return(paste0("<p>", text, "</p>"))
    paste0("<ul>", paste0("<li>", items, "</li>", collapse = ""), "</ul>")
  }
  
  # Build the final HTML
  paste0(
    "<div class='ai-recommendation'>",
      "<h4><i class='fa fa-robot'></i> AI Policy Recommendation for ", context$district, " (", context$year, ")</h4>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-lightbulb'></i> Executive Summary</h5>",
        "<p>", exec_summary, "</p>",
      "</div>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-tasks'></i> Key Interventions</h5>",
        to_html_list(interventions),
      "</div>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-calendar-alt'></i> Implementation Strategy</h5>",
        to_html_list(impl_strategy),
      "</div>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-chart-line'></i> Expected Outcomes</h5>",
        to_html_list(outcomes),
      "</div>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-exclamation-triangle'></i> Risk Assessment</h5>",
        to_html_list(risks),
      "</div>",
      
      "<div class='recommendation-section'>",
        "<h5><i class='fa fa-check-circle'></i> Success Metrics</h5>",
        to_html_list(metrics),
      "</div>",
      
      "<div class='ai-footer'><small><i class='fa fa-check-circle'></i> Generated by Gemini AI</small></div>",
    "</div>"
  )
}

#' Generate Policy Recommendation (Asynchronous)
#'
#' @param client A Gemini client object
#' @param context A list of context variables for the prompt
#' @return A promise that will resolve to a character string with a policy recommendation
generate_policy_recommendation_async <- function(client, context) {
  if (requireNamespace("future", quietly = TRUE)) {
    future::future({
      generate_policy_recommendation(client, context)
    })
  } else {
    # Fallback to synchronous if future is not available
    return(generate_policy_recommendation(client, context))
  }
}

#' Generate AI insights for dashboard
#'
#' @param data A data frame with nutrition data
#' @param client A Gemini client object
#' @return A list of AI insights
generate_dashboard_insights <- function(data, client) {
  if (nrow(data) == 0) {
    return(list(
      summary = "No data available for analysis",
      trends = "Unable to identify trends",
      recommendations = "Please ensure data is loaded"
    ))
  }
  
  # Calculate key metrics
  avg_stunting <- mean(data$stunting, na.rm = TRUE)
  avg_wasting <- mean(data$wasting, na.rm = TRUE)
  avg_anemia <- mean(data$anemia, na.rm = TRUE)
  
  # Generate contextual insights
  if (client$mock_mode) {
    insights <- list(
      summary = paste0(
        "Nutrition analysis shows stunting at ", round(avg_stunting, 1), "%, ",
        "wasting at ", round(avg_wasting, 1), "%, and anemia at ", round(avg_anemia, 1), "%. ",
        "These indicators suggest ", 
        ifelse(avg_stunting > 30, "critical", ifelse(avg_stunting > 20, "moderate", "good")),
        " nutritional status."
      ),
      trends = paste0(
        "Data trends indicate ", 
        ifelse(avg_stunting > 25, "high priority areas requiring immediate intervention", 
               "areas with potential for improvement through targeted programs")
      ),
      recommendations = paste0(
        "Key recommendations include: (1) Community nutrition education, ",
        "(2) Iron supplementation programs, (3) Water and sanitation improvements"
      )
    )
  } else {
    # Use AI for real insights
    context <- list(
      stunting_rate = avg_stunting,
      wasting_rate = avg_wasting,
      anemia_rate = avg_anemia,
      data_points = nrow(data),
      focus_area = "dashboard insights"
    )
    
    insights <- list(
      summary = generate_policy_recommendation(client, context),
      trends = "AI-generated trend analysis",
      recommendations = "AI-generated recommendations"
    )
  }
  
  return(insights)
}


# Function to merge multiple files
multmerge <- function(all_files, sep = ",") {
  # Read all files, ensuring strings are not treated as factors
  data_list <- lapply(all_files, function(file) {
    read.csv(file, sep = sep, check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  # Row-bind all data frames together.
  # This requires all files to have the same column names.
  combined_data <- do.call(rbind, data_list)
  
  # Remove duplicate keys, keeping the *last* occurrence.
  # This ensures that translations from temp files overwrite existing ones.
  merged_data <- combined_data[!duplicated(combined_data$key, fromLast = TRUE), ]
  
  return(merged_data)
}
