#Lisa O'Connell
#Purpose: To process the ABSVignette.Rmd to a README.md file to display a GitHub Page


rmarkdown::render(input="ABS_API_Vignette.Rmd",  
                  output_file = "README.md",
                  output_format = "github_document"
                  )


