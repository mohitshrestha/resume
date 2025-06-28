library(pagedown)
library(pdftools)
library(fs)

# Step 1: Convert HTML to PDF
pagedown::chrome_print("docs/resume.html")  # produces docs/resume.pdf

# Step 2: Get total pages in the PDF
pdf_path <- "docs/resume.pdf"
total_pages <- pdf_info(pdf_path)$pages

# # Step 3: Decide how many pages to keep (up to 4)
# max_pages <- 4
# pages_to_keep <- seq_len(min(total_pages, max_pages))

pages_to_keep <- seq_len(total_pages)  # All pages

# Step 4: Subset the PDF to just those pages and overwrite the original
trimmed_pdf <- pdf_subset(pdf_path, pages = pages_to_keep)

# Step 5: Overwrite original PDF with trimmed version (ensure no extra file is created)
result <- file.rename(trimmed_pdf, pdf_path)  # Overwrite directly without creating a second file

invisible(result) # suppress TRUE in console
