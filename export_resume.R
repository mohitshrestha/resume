pagedown::chrome_print("resume.html")
pdftools::pdf_subset(input = "resume.pdf", pages =  c(1,2))
fs::file_copy(path = "resume_output.pdf", new_path = "resume.pdf", overwrite = TRUE)
