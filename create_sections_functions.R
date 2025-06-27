col_br <- function(col){
    dplyr::if_else(
        !is.na(col) & col != "N/A|NA", 
        paste0(col, ""),
        ""
    )
}

col_br_location <- function(col){
    dplyr::if_else(
        !is.na(col) & col != "N/A|NA", 
        paste0('<br><i class="fa-solid fa-location-dot"></i> ', col),
        ""
    )
}

create_section <- function(cv_data, section_name){
    cv_data |>
        dplyr::mutate(in_resume = as.character(in_resume),
                      end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
                      location = dplyr::if_else(location == "Online", NA_character_, location)) |>
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        dplyr::select(section:description_3) |>
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            date =
                dplyr::case_when(
                    end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                    end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                    end == start ~ glue::glue("{format(end, '%b %Y')}"),
                    TRUE ~ ""
                ) ,
            .before = everything()
        ) |>
        dplyr::mutate(
            main_text =
                glue::glue(
                    "**{title}** <br> *{col_br(institution)}* {col_br_location(location)}
          - {col_br(description_1)}
          - {col_br(description_2)} 
          - {col_br(description_3)}"),
            .after = date
        ) |>
        dplyr::select(-c(start, end, section, title, institution, location, description_1, description_2, description_3)) |>
        gt::gt() |>
        gt::fmt_markdown(columns = c(date, main_text)) |> 
        gt::tab_options(column_labels.hidden = TRUE, table.width = gt::pct(100),
                        table.border.top.style = "hidden",
                        table.border.bottom.style = "hidden") |> 
        gt::cols_align(align = "left", columns = main_text)
}

create_education_section <- function(cv_data, section_name){
    cv_data |>
        dplyr::mutate(in_resume = as.character(in_resume),
                      end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
                      location = dplyr::if_else(location == "Online", NA_character_, location)) |>
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        dplyr::select(section:description_3) |>
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            date =
                dplyr::case_when(
                    end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                    end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                    end == start ~ glue::glue("{format(end, '%b %Y')}"),
                    TRUE ~ ""
                ) ,
            .before = everything()
        ) |>
        dplyr::mutate(
            main_text =
                glue::glue(
                    "**{title}** <br> *{col_br(institution)}* {col_br_location(location)} <br> {col_br(description_1)}"),
            .after = date
        ) |>
        dplyr::select(-c(start, end, section, title, institution, location, description_1, description_2, description_3)) |>
        gt::gt() |>
        gt::fmt_markdown(columns = c(date, main_text)) |>
        gt::tab_options(column_labels.hidden = TRUE, 
                        table.width = gt::pct(100), 
                        table.border.top.style = "hidden",
                        table.border.bottom.style = "hidden"
        ) |> 
        gt::cols_align(align = "left", columns = main_text)
}

create_projects_section <- function(cv_data, section_name){
    cv_data |>
        dplyr::mutate(in_resume = as.character(in_resume),
                      end = tidyr::replace_na(end, (lubridate::ymd(Sys.Date()))),
                      location = dplyr::if_else(location == "Online", NA_character_, location)) |>
        dplyr::filter(in_resume %in% c("TRUE"), section == section_name) |>
        dplyr::select(section:description_3) |>
        dplyr::arrange(desc(end), desc(start)) |>
        dplyr::mutate(
            date =
                dplyr::case_when(
                    end == (lubridate::ymd(Sys.Date())) ~ glue::glue("Current <br> | <br> {format(start, '%b %Y')}"),
                    end != start ~ glue::glue("{format(end, '%b %Y')} <br> | <br> {format(start, '%b %Y')}"),
                    end == start ~ glue::glue("{format(end, '%b %Y')}"),
                    TRUE ~ ""
                ) ,
            .before = everything()
        ) |>
        dplyr::mutate(
            main_header = dplyr::if_else(({col_br(institution)} == ""),
                                         glue::glue("**{title}**"),
                                         glue::glue("**{title}** <br> *{col_br(institution)}*")),
            .after = date) |>
        dplyr::mutate(
            main_description = dplyr::if_else(({col_br(description_1)} != "" & {col_br(description_2)} != "" & {col_br(description_3)} != ""), 
                                              glue::glue("<br> 
          - {col_br(description_1)}
          - {col_br(description_2)} 
          - {col_br(description_3)}"),
                                              dplyr::if_else(({col_br(description_1)} != "" & {col_br(description_2)} != ""), 
                                                             glue::glue("<br> 
          - {col_br(description_1)}
          - {col_br(description_2)}"),
                                                             glue::glue("<br> 
          - {col_br(description_1)}"))),
            .after = date) |>
        dplyr::mutate(
            main_text = glue::glue("{main_header} {main_description}"),
            .after = date) |> 
        dplyr::select(-c(start, end, date, section, title, institution, location, description_1, description_2, description_3, main_header, main_description)) |>
        gt::gt() |>
        #gt::fmt_markdown(columns = c(date, main_text)) |> 
        gt::fmt_markdown(columns = c(main_text)) |> 
        gt::tab_options(column_labels.hidden = TRUE, table.width = gt::pct(100),
                        table.border.top.style = "hidden",
                        table.border.bottom.style = "hidden") |> 
        gt::cols_align(align = "left", columns = main_text)
}