# Function to handle column formatting: replaces "N/A|NA" values with empty strings
col_br <- function(col){
    dplyr::if_else(
        !is.na(col) & col != "N/A|NA", 
        paste0(col, ""),  # Return the column value if not "N/A|NA"
        ""  # Otherwise, return an empty string
    )
}

# Function to handle location formatting with an icon
col_br_location <- function(col){
    dplyr::if_else(
        !is.na(col) & col != "N/A|NA", 
        paste0('<br><i class="fa-solid fa-location-dot"></i> ', col),  # Add location icon if valid
        ""  # Return empty string if not a valid location
    )
}

# Function to create a section (e.g., work experience) in the CV
create_section <- function(cv_data, section_name){
    cv_data |>
        dplyr::mutate(
            # Ensure 'in_resume' is a character and fill missing 'end' dates with today's date
            in_resume = as.character(in_resume),
            end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
            # Replace "Online" locations with NA
            location = dplyr::if_else(location == "Online", NA_character_, location)
        ) |>
        # Filter for rows where 'in_resume' is TRUE and the section matches
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        # Select relevant columns
        dplyr::select(section:description_5) |>
        # Sort by 'end' and 'start' dates in descending order
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            # Create a formatted date string based on the 'end' and 'start' dates
            date = dplyr::case_when(
                end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                end == start ~ glue::glue("{format(end, '%b %Y')}"),
                TRUE ~ ""
            ),
            .before = everything()  # Place 'date' at the beginning of the dataframe
        ) |>
        # Create the main header for the project (combining title and institution)
        dplyr::mutate(
            main_header = dplyr::if_else(
                ({col_br(institution)} == ""),
                glue::glue("**{title}**"),
                glue::glue("**{title}** <br> *{col_br(institution)}*")
            ),
            .after = date # Place 'main_text' after 'date'
        ) |>
        # Create the main text for the CV section, combining title, institution, and descriptions
        dplyr::mutate(
            main_description = dplyr::if_else(
                ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != "" & {col_br(description_4)} != "" & {col_br(description_5)} != ""), 
                glue::glue("<br> 
                            - {col_br(description_1)}
                            - {col_br(description_2)} 
                            - {col_br(description_3)} 
                            - {col_br(description_4)} 
                            - {col_br(description_5)}"),
                dplyr::if_else(
                    ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != "" & {col_br(description_4)} != ""), 
                    glue::glue("<br> 
                                - {col_br(description_1)}
                                - {col_br(description_2)} 
                                - {col_br(description_3)}
                                - {col_br(description_4)}"),
                    dplyr::if_else(
                        ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != ""), 
                        glue::glue("<br> 
                                    - {col_br(description_1)}
                                    - {col_br(description_2)} 
                                    - {col_br(description_3)}"),
                        dplyr::if_else(
                            ({col_br(description_1)} != "" & {col_br(description_2)} != ""), 
                            glue::glue("<br> 
                                        - {col_br(description_1)}
                                        - {col_br(description_2)}"),
                            glue::glue("<br> 
                                        - {col_br(description_1)}")
                        )
                    )
                )
            ),
            .after = date # Place 'main_text' after 'date'
        ) |>
        # Combine the header and description into 'main_text'
        dplyr::mutate(
            main_text = glue::glue("{main_header} {main_description}"),
            .after = date
        ) |>
        # Remove unnecessary columns
        dplyr::select(-c(start, end, section, title, institution, location, description_1, description_2, description_3, description_4, description_5, main_header, main_description)) |>
        # Create a GT table and apply markdown formatting
        gt::gt() |>
        gt::fmt_markdown(columns = c(date, main_text)) |>
        gt::tab_options(
            column_labels.hidden = TRUE, 
            table.width = gt::pct(100),
            table.border.top.style = "hidden",
            table.border.bottom.style = "hidden"
        ) |>
        gt::cols_align(align = "left", columns = main_text)  # Align the main text to the left
}

# Function to create an education section in the CV
create_education_section <- function(cv_data, section_name){
    cv_data |>
        dplyr::mutate(
            # Ensure 'in_resume' is a character and fill missing 'end' dates with today's date
            in_resume = as.character(in_resume),
            end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
            # Replace "Online" locations with NA
            location = dplyr::if_else(location == "Online", NA_character_, location)
        ) |>
        # Filter for rows where 'in_resume' is TRUE and the section matches
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        # Select relevant columns
        dplyr::select(section:description_5) |>
        # Sort by 'end' and 'start' dates in descending order
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            # Create a formatted date string based on the 'end' and 'start' dates
            date = dplyr::case_when(
                end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                end == start ~ glue::glue("{format(end, '%b %Y')}"),
                TRUE ~ ""
            ),
            .before = everything()  # Place 'date' at the beginning of the dataframe
        ) |>
        # Create the main text for the CV section, combining title, institution, and description
        dplyr::mutate(
            main_text = glue::glue(
                "**{title}** <br> *{col_br(institution)}* {col_br_location(location)} <br> {col_br(description_1)}"
            ),
            .after = date  # Place 'main_text' after 'date'
        ) |>
        # Remove unnecessary columns
        dplyr::select(-c(start, end, section, title, institution, location, description_1, description_2, description_3, description_4, description_5)) |>
        # Create a GT table and apply markdown formatting
        gt::gt() |>
        gt::fmt_markdown(columns = c(date, main_text)) |>
        gt::tab_options(
            column_labels.hidden = TRUE, 
            table.width = gt::pct(100), 
            table.border.top.style = "hidden",
            table.border.bottom.style = "hidden"
        ) |>
        gt::cols_align(align = "left", columns = main_text)  # Align the main text to the left
}

# Function to create a projects section in the CV
create_projects_section <- function(cv_data, section_name) {
    cv_data |>
        dplyr::mutate(
            # Ensure 'in_resume' is a character and fill missing 'end' dates with today's date
            in_resume = as.character(in_resume),
            end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
            # Replace "Online" locations with NA
            location = dplyr::if_else(location == "Online", NA_character_, location)
        ) |>
        # Filter for rows where 'in_resume' is TRUE and the section matches
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        # Select relevant columns
        dplyr::select(section:description_5) |>
        # Sort by 'end' and 'start' dates in descending order
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            # Create a formatted date string based on the 'end' and 'start' dates
            date = dplyr::case_when(
                end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                end == start ~ glue::glue("{format(end, '%b %Y')}"),
                TRUE ~ ""
            ),
            .before = everything()  # Place 'date' at the beginning of the dataframe
        ) |>
        # Create the main header for the project (combining title and institution)
        dplyr::mutate(
            main_header = dplyr::if_else(
                ({col_br(institution)} == ""),
                glue::glue("**{title}**"),
                glue::glue("**{title}** <br> *{col_br(institution)}*")
            ),
            .after = date
        ) |>
        # Create the main description of the project
        dplyr::mutate(
            main_description = dplyr::if_else(
                ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != "" & {col_br(description_4)} != "" & {col_br(description_5)} != ""), 
                glue::glue("<br> 
                            - {col_br(description_1)}
                            - {col_br(description_2)} 
                            - {col_br(description_3)} 
                            - {col_br(description_4)} 
                            - {col_br(description_5)}"),
                dplyr::if_else(
                    ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != "" & {col_br(description_4)} != ""), 
                    glue::glue("<br> 
                                - {col_br(description_1)}
                                - {col_br(description_2)} 
                                - {col_br(description_3)}
                                - {col_br(description_4)}"),
                    dplyr::if_else(
                        ({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != ""), 
                        glue::glue("<br> 
                                    - {col_br(description_1)}
                                    - {col_br(description_2)} 
                                    - {col_br(description_3)}"),
                        dplyr::if_else(
                            ({col_br(description_1)} != "" & {col_br(description_2)} != ""), 
                            glue::glue("<br> 
                                        - {col_br(description_1)}
                                        - {col_br(description_2)}"),
                            glue::glue("<br> 
                                        - {col_br(description_1)}")
                        )
                    )
                )
            ),
            .after = date # Place 'main_text' after 'date'
        ) |>
        # Combine the header and description into 'main_text'
        dplyr::mutate(
            main_text = glue::glue("{main_header} {main_description}"),
            .after = date
        ) |>
        # Remove unnecessary columns
        dplyr::select(-c(start, end, date, section, title, institution, location, description_1, description_2, description_3, description_4, description_5, main_header, main_description)) |>
        # Create a GT table and apply markdown formatting
        gt::gt() |>
        gt::fmt_markdown(columns = c(main_text)) |>
        gt::tab_options(
            column_labels.hidden = TRUE, 
            table.width = gt::pct(100),
            table.border.top.style = "hidden",
            table.border.bottom.style = "hidden"
        ) |>
        gt::cols_align(align = "left", columns = main_text)  # Align the main text to the left
}