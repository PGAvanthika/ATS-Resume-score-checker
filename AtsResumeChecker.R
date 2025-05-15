# Load required libraries
library(shiny)
library(pdftools)
library(officer)
library(readr)
library(tm)
library(tidytext)
library(ggplot2)
library(stringr)
library(reticulate)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

# Set up Python environment
transformers <- import("transformers")
torch <- import("torch")

# Function to extract text
extract_text <- function(file_path, ext) {
  if (ext == "pdf") {
    return(paste(pdf_text(file_path), collapse = " "))
  } else if (ext == "docx") {
    doc <- read_docx(file_path)
    text <- docx_summary(doc)$text
    return(paste(text, collapse = " "))
  } else if (ext == "txt") {
    return(paste(readLines(file_path), collapse = " "))
  } else {
    return(NULL)
  }
}

# Clean and preprocess text
clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  return(text)
}

# BERT-based keyword matching
match_keywords_bert <- function(resume_text, job_desc) {
  resume_cleaned <- clean_text(resume_text)
  job_desc_cleaned <- clean_text(job_desc)
  
  tokenizer <- transformers$AutoTokenizer$from_pretrained("bert-base-uncased")
  model <- transformers$AutoModel$from_pretrained("bert-base-uncased")
  
  inputs_resume <- tokenizer$encode_plus(resume_cleaned, return_tensors='pt', padding=TRUE, truncation=TRUE, max_length=512L)
  inputs_job_desc <- tokenizer$encode_plus(job_desc_cleaned, return_tensors='pt', padding=TRUE, truncation=TRUE, max_length=512L)
  
  with(torch$no_grad(), {
    outputs_resume <- model(inputs_resume$input_ids)
    outputs_job_desc <- model(inputs_job_desc$input_ids)
  })
  
  resume_embedding <- outputs_resume$last_hidden_state[0, 0, ]
  job_desc_embedding <- outputs_job_desc$last_hidden_state[0, 0, ]
  
  cosine_sim <- torch$nn$functional$cosine_similarity(resume_embedding, job_desc_embedding, dim=0L)
  
  return(round(cosine_sim$item() * 100, 2))
}

# Find missing keywords
find_missing_keywords <- function(resume_text, job_desc) {
  resume_tokens <- unlist(strsplit(clean_text(resume_text), "\\s+"))
  job_tokens <- unlist(strsplit(clean_text(job_desc), "\\s+"))
  
  resume_tokens <- unique(resume_tokens[!resume_tokens %in% stopwords("en")])
  job_tokens <- unique(job_tokens[!job_tokens %in% stopwords("en")])
  
  missing <- setdiff(job_tokens, resume_tokens)
  return(missing)
}

# UI Layout with custom color scheme
ui <- fluidPage(
  # Custom CSS for the color scheme
  tags$head(
    tags$style(HTML("
      /* Main color palette */
      :root {
        --primary-color: rgb(115, 72, 17);
        --accent-color: #c9ae6f;
        --light-accent: #f0e7d3;
        --dark-text: #333333;
        --light-text: #ffffff;
      }
      
      /* Background and text colors */
      body {
        background-color: #f9f7f3;
        color: var(--dark-text);
        font-family: 'Open Sans', sans-serif;
      }
      
      /* Header styling */
      .main-header {
        background-color: var(--primary-color);
        color: var(--light-text);
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      .main-header h1 {
        margin: 0;
        font-weight: 300;
        letter-spacing: 1px;
      }
      
      /* Panel styling */
      .panel {
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      
      /* Button styling */
      .btn-primary {
        background-color: var(--primary-color);
        border-color: var(--primary-color);
        color: var(--light-text);
      }
      
      .btn-primary:hover {
        background-color: rgb(95, 60, 14);
        border-color: rgb(95, 60, 14);
      }
      
      .btn-secondary {
        background-color: var(--accent-color);
        border-color: var(--accent-color);
        color: var(--dark-text);
      }
      
      .btn-secondary:hover {
        background-color: #b9a059;
        border-color: #b9a059;
      }
      
      /* Input field styling */
      .form-control {
        border: 1px solid #ddd;
        border-radius: 3px;
      }
      
      .form-control:focus {
        border-color: var(--accent-color);
        box-shadow: 0 0 0 0.2rem rgba(201, 174, 111, 0.25);
      }
      
      /* Labels */
      label {
        font-weight: 600;
        color: var(--primary-color);
      }
      
      /* Result panel */
      .result-panel {
        background-color: var(--primary-color);
        color: var(--light-text);
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
      }
      
      .result-panel h3 {
        border-bottom: 1px solid var(--accent-color);
        padding-bottom: 10px;
        margin-top: 0;
      }
      
      /* Keywords styling */
      .keyword-chip {
        background-color: var(--accent-color);
        color: var(--dark-text);
        border-radius: 16px;
        padding: 5px 12px;
        margin: 3px;
        display: inline-block;
        font-size: 14px;
      }
      
      /* Footer */
      .footer {
        background-color: var(--primary-color);
        color: var(--light-text);
        text-align: center;
        padding: 10px;
        margin-top: 20px;
        border-radius: 5px;
      }
      
      /* Score display */
      .score-display {
        text-align: center;
        padding: 10px;
        margin-bottom: 20px;
      }
      
      .score-label {
        font-size: 18px;
        font-weight: 600;
        margin-top: 10px;
        color: var(--primary-color);
      }
      
      /* Navbar styling */
      .navbar {
        background-color: var(--primary-color);
        border-color: var(--primary-color);
      }
      
      .navbar-default .navbar-brand {
        color: var(--light-text);
      }
      
      .navbar-default .navbar-nav > li > a {
        color: var(--light-text);
      }
      
      .navbar-default .navbar-nav > .active > a, 
      .navbar-default .navbar-nav > .active > a:focus, 
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: var(--accent-color);
        color: var(--dark-text);
      }
    "))
  ),
  
  # Navigation bar
  navbarPage(
    title = div(img(src = "https://img.icons8.com/ios-filled/50/ffffff/resume.png", height = "30px"), "ATS Resume Checker"),
    id = "nav",
    theme = shinytheme("flatly"),
    
    # Main tab
    tabPanel(
      "Check Resume",
      div(class = "main-header", h1("Optimize Your Resume for ATS")),
      
      fluidRow(
        # Left panel - Input
        column(
          width = 4,
          div(class = "panel",
              h3("Upload Documents", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
              fileInput("resume", "Upload Resume", 
                        accept = c(".pdf", ".docx", ".txt"),
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              textAreaInput("job_desc", "Paste Job Description", "", rows = 6),
              div(style = "text-align: center; margin-top: 20px;",
                  actionButton("analyze", "Analyze Resume", class = "btn-primary", style = "width: 100%; padding: 10px;"),
                  br(), br(),
                  downloadButton("report", "Download Report", class = "btn-secondary", style = "width: 100%; padding: 10px;")
              )
          )
        ),
        
        # Right panel - Results
        column(
          width = 8,
          uiOutput("results_panel")
        )
      ),
      
      # Footer
      div(class = "footer", 
          p("ATS Resume Checker © 2025", style = "margin: 0;")
      )
    ),
    
    # Info tab
    tabPanel(
      "How It Works",
      div(class = "main-header", h1("How Our ATS Resume Checker Works")),
      div(class = "panel",
          h3("Understanding ATS Systems", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
          p("Applicant Tracking Systems (ATS) are used by employers to manage job applications and screen resumes. These systems scan resumes for relevant keywords and qualifications."),
          h3("Our Technology", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
          p("Our ATS Resume Checker uses advanced AI technology to analyze your resume against job descriptions:"),
          tags$ul(
            tags$li("BERT-based semantic matching to understand context, not just keywords"),
            tags$li("Intelligent keyword extraction from job descriptions"),
            tags$li("Detailed analysis of missing keywords and phrases"),
            tags$li("Visual score representation showing match percentage")
          ),
          h3("How to Improve Your Score", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
          p("To improve your ATS score:"),
          tags$ul(
            tags$li("Add missing keywords identified in our analysis"),
            tags$li("Use industry-standard terminology"),
            tags$li("Format your resume properly with clear sections"),
            tags$li("Quantify achievements where possible")
          )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  resume_text <- reactive({
    req(input$resume)
    ext <- tools::file_ext(input$resume$name)
    extract_text(input$resume$datapath, ext)
  })
  
  analysis <- eventReactive(input$analyze, {
    req(resume_text(), input$job_desc)
    score <- match_keywords_bert(resume_text(), input$job_desc)
    missing <- find_missing_keywords(resume_text(), input$job_desc)
    return(list(score = score, missing = missing))
  })
  
  # Dynamic label selection
  score_label <- reactive({
    req(analysis())
    score <- analysis()$score
    
    set.seed(Sys.time())  # ensures randomness
    
    if (score < 50) {
      sample(c("Needs Improvement", "Poor Match", "Not Suitable", "Weak Fit"), 1)
    } else if (score < 75) {
      sample(c("Fair Match", "Moderate Match", "Partial Fit", "Somewhat Suitable"), 1)
    } else if (score < 90) {
      sample(c("Strong Match", "Well-Aligned", "Good Fit", "Qualified"), 1)
    } else {
      sample(c("Excellent Match", "Top Fit", "Outstanding Candidate", "ATS-Optimized Resume"), 1)
    }
  })
  
  # Dynamic color based on score
  score_color <- reactive({
    req(analysis())
    score <- analysis()$score
    
    if (score < 50) {
      "#e74c3c"  # Red
    } else if (score < 75) {
      "#f39c12"  # Orange
    } else if (score < 90) {
      "#2ecc71"  # Green
    } else {
      "#27ae60"  # Dark Green
    }
  })
  
  # Generate missing keywords as chips
  missing_keywords_ui <- reactive({
    req(analysis())
    missing <- analysis()$missing
    
    if (length(missing) == 0) {
      return(p("No missing keywords. Great match!", style = "color: #2ecc71; font-weight: bold;"))
    } else {
      keyword_chips <- lapply(missing, function(keyword) {
        div(class = "keyword-chip", keyword)
      })
      return(div(style = "margin-top: 10px;", keyword_chips))
    }
  })
  
  # Results panel UI
  output$results_panel <- renderUI({
    if (!is.null(input$resume) && !is.null(input$analyze) && input$analyze > 0) {
      div(
        # Upload status
        div(class = "panel", style = "border-left: 4px solid #c9ae6f;",
            h4("Uploaded File", style = "margin-top: 0; color: rgb(115, 72, 17);"),
            p(icon("file"), input$resume$name)
        ),
        
        # Score visualization
        div(class = "panel",
            h3("ATS Match Score", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
            div(class = "score-display",
                plotlyOutput("score_plot", height = "300px"),
                div(class = "score-label", 
                    textOutput("score_label_text")
                )
            )
        ),
        
        # Missing keywords
        div(class = "panel",
            h3("Missing Keywords", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
            p("These keywords were found in the job description but not in your resume:"),
            uiOutput("missing_keywords_display")
        ),
        
        # Recommendations
        div(class = "panel",
            h3("Recommendations", style = "color: rgb(115, 72, 17); border-bottom: 2px solid #c9ae6f; padding-bottom: 10px;"),
            p("Based on our analysis, here are some recommendations to improve your resume:"),
            tags$ul(
              tags$li("Add the missing keywords highlighted above"),
              tags$li("Ensure your resume format is ATS-friendly"),
              tags$li("Quantify your accomplishments where possible"),
              tags$li("Use industry-standard terminology")
            )
        )
      )
    } else {
      div(class = "panel", style = "text-align: center; padding: 40px;",
          img(src = "https://img.icons8.com/ios/100/c9ae6f/resume.png", height = "100px"),
          h3("Upload your resume and paste a job description to begin", style = "color: rgb(115, 72, 17); margin-top: 20px;"),
          p("Our AI-powered tool will analyze your resume against the job description to help you optimize for ATS systems.")
      )
    }
  })
  
  # Score label text
  output$score_label_text <- renderText({
    req(analysis(), score_label())
    paste(score_label(), "-", analysis()$score, "%")
  })
  
  # Missing keywords display
  output$missing_keywords_display <- renderUI({
    missing_keywords_ui()
  })
  
  # Score plot with donut chart
  output$score_plot <- renderPlotly({
    req(analysis())
    score <- analysis()$score
    remaining <- 100 - score
    
    plot_ly(
      values = c(score, remaining),
      labels = c("Match", "Remaining"),
      marker = list(colors = c(score_color(), "#e5e5e5")),
      textinfo = "none",
      hole = 0.7,
      type = "pie",
      sort = FALSE,
      direction = "clockwise",
      rotation = 90
    ) %>%
      layout(
        showlegend = FALSE,
        annotations = list(
          list(
            x = 0.5, y = 0.5,
            text = paste0(score, "%"),
            font = list(size = 30, color = score_color()),
            showarrow = FALSE
          )
        ),
        margin = list(l = 20, r = 20, b = 20, t = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Download report
  output$report <- downloadHandler(
    filename = function() { 
      paste0("ATS_Report_", format(Sys.Date(), "%Y%m%d"), ".txt") 
    },
    content = function(file) {
      req(analysis())
      writeLines(c(
        "===== ATS RESUME ANALYSIS REPORT =====",
        "",
        paste("Resume File:", input$resume$name),
        paste("Analysis Date:", format(Sys.Date(), "%B %d, %Y")),
        "",
        paste("ATS MATCH SCORE:", analysis()$score, "%"),
        paste("Match Classification:", score_label()),
        "",
        "MISSING KEYWORDS:",
        if (length(analysis()$missing) == 0) {
          "No missing keywords. Great match!"
        } else {
          paste("- ", analysis()$missing)
        },
        "",
        "RECOMMENDATIONS:",
        "- Add the missing keywords to your resume",
        "- Ensure your resume format is ATS-friendly",
        "- Quantify your accomplishments where possible",
        "- Use industry-standard terminology",
        "",
        "===== END OF REPORT ====="
      ), file)
    }
  )
}

# Run the App
shinyApp(ui, server)