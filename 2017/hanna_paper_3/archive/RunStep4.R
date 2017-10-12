RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/hanna_paper_3/",
  RAW = "/analyses/data_raw/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/hanna_paper_3",
  BAKED = "/analyses/results_baked/hanna_paper_3/",
  FINAL = "/analyses/results_final/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/")

rmarkdown::render(input = "MarkdownStep4.Rmd", output_file = "MarkdownStep4.pdf", 
                  output_dir = RAWmisc::PROJ$SHARED_TODAY, output_format = rmarkdown::pdf_document())

