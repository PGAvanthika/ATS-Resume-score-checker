ATS-Resume-score-checker
The ATS Resume Checker is a user-friendly web application built with R Shiny and Python's BERT (transformers) model. It allows users to upload a resume and a job description to analyze the semantic match between them. The tool provides a match score and highlights missing keywords to help users tailor their resumes more effectively.
  
  Uses Python’s BERT model (via transformers) for semantic matching

  Processes resume and job description texts into vectors and computes cosine similarity

  Extracts keywords using text mining to identify gaps
  

🚀 Features

📄 Supports PDF, DOCX, and TXT resume formats

🤖 BERT-based semantic similarity scoring using Python (via reticulate)

🧠 Keyword extraction to find missing skills or terms from the job description

📈 Visual feedback on ATS compatibility

🎨 Custom aesthetic UI with a modern brown-and-gold theme

📥 Option to download a report of the results

🛠 Requirements

R version 4.1 or higher
Python 3.6+ with transformers and torch libraries installed

Install R Packages:
install.packages(c(
  "shiny", "shinythemes", "reticulate", "tm", "wordcloud", 
  "text2vec", "stringr", "pdftools", "readtext", "text", 
  "DT", "shinydashboard", "ggplot2", "dplyr"
))

Install Python Packages:

Use a Python environment accessible to R (reticulate):
pip install transformers torch


▶️ How to Run

Open app.R in RStudio.

Ensure Python environment is configured correctly with reticulate.

Click "Run App".

<img width="1439" alt="Screenshot 2025-04-29 at 8 26 05 PM" src="https://github.com/user-attachments/assets/e8dd9fbe-c65e-404b-9697-56a80fb93fc0" />


