################################################################################
## 01. 사용자 정의 함수 - R Markdown 삽입을 위한 코드 생성
################################################################################
##==============================================================================
## 01.01. 수치형 변수 집계
##==============================================================================
create_summary_numeric <- function(id_dataset, variables = NULL, 
                                   statistics = NULL, quantiles = NULL,
                                   digits = 3, group_flag = FALSE, group_variable = NULL, 
                                   plot = TRUE,
                                   output_dir = glue::glue("{getwd()}/www/report"), 
                                   output_file = "num_summary") {
  source_rmd  <- "summary_numeric.Rmd"
  target_rmd <- glue::glue("{output_dir}/{output_file}.Rmd")
  target_html <- glue::glue("{output_dir}/{output_file}.html")
  
  rmd_file <- file.path(system.file(package = "BitStat"), 
                        "app", "report", "descriptive", source_rmd)
  flag <- file.copy(from = rmd_file, to = target_rmd)
  
  variables <- variables %>% 
    paste(collapse = ",")
  
  statistics <- statistics %>% 
    paste(collapse = ",")
  
  quantiles <- quantiles %>% 
    paste(collapse = ",")
  
  group_variable <- group_variable %>% 
    paste(collapse = ",")
  
  #--Store parameters ----------------------------------------------------------  
  # id_dataset
  rmd_content <- sub("\\$id_dataset\\$", id_dataset, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # variables
  rmd_content <- sub("\\$variables\\$", variables, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # statistics
  rmd_content <- sub("\\$statistics\\$", statistics, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # quantiles
  rmd_content <- sub("\\$quantiles\\$", quantiles, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # digits
  rmd_content <- sub("\\$digits\\$", digits, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of group_by
  rmd_content <- sub("\\$group_flag\\$", group_flag, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # group variable name
  rmd_content <- sub("\\$group_variable\\$", group_variable, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of plot
  rmd_content <- sub("\\$plot_flag\\$", plot, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # for non-ASCII in MS-Windows 
  if (grepl("^mingw", R.version$os)) { 
    writeLines(iconv(readLines(target_rmd), from = "CP949", to = "UTF8"), 
               file(target_rmd, encoding="UTF-8"))
  }
  
  rmarkdown::render(
    target_rmd,
    output_dir = output_dir,
    output_file = target_html,
    envir = new.env(parent = globalenv())
  )
  
  # Rmd 파일을 삭제하지 않으면, 데이터셋(Rmd 파일의 내용)을 바꾸더라도 ifrmae의 html 컨텐츠가 바뀌지 않음
  unlink(target_rmd)
}


##==============================================================================
## 01.02. 범주형 변수 집계
##==============================================================================
create_summary_category <- function(id_dataset, variables = NULL, 
                                    plot = TRUE,
                                    output_dir = glue::glue("{getwd()}/www/report"), 
                                    output_file = "cat_summary") {
  source_rmd  <- "summary_category.Rmd"
  target_rmd <- glue::glue("{output_dir}/{output_file}.Rmd")
  target_html <- glue::glue("{output_dir}/{output_file}.html")
  
  rmd_file <- file.path(system.file(package = "BitStat"), 
                        "app", "report", "descriptive", source_rmd)
  flag <- file.copy(from = rmd_file, to = target_rmd)
  
  variables <- variables %>% 
    paste(collapse = ",")
  
  #--Store parameters ----------------------------------------------------------  
  # id_dataset
  rmd_content <- sub("\\$id_dataset\\$", id_dataset, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # variables
  rmd_content <- sub("\\$variables\\$", variables, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of plot
  rmd_content <- sub("\\$plot_flag\\$", plot, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # for non-ASCII in MS-Windows 
  if (grepl("^mingw", R.version$os)) { 
    writeLines(iconv(readLines(target_rmd), from = "CP949", to = "UTF8"), 
               file(target_rmd, encoding="UTF-8"))
  }
  
  rmarkdown::render(
    target_rmd,
    output_dir = output_dir,
    output_file = target_html,
    envir = new.env(parent = globalenv())
  )
  
  # Rmd 파일을 삭제하지 않으면, 데이터셋(Rmd 파일의 내용)을 바꾸더라도 ifrmae의 html 컨텐츠가 바뀌지 않음
  unlink(target_rmd)
}


##==============================================================================
## 01.03. 범주형 변수 분할표
##==============================================================================
create_summary_contingency <- function(id_dataset, 
                                       variable_row = NULL, 
                                       variable_col = NULL, 
                                       marginal = FALSE,
                                       marginal_type = "sum",
                                       plot = TRUE,
                                       output_dir = glue::glue("{getwd()}/www/report"), 
                                       output_file = "cat_contingency") {
  source_rmd  <- "summary_contingency.Rmd"
  target_rmd <- glue::glue("{output_dir}/{output_file}.Rmd")
  target_html <- glue::glue("{output_dir}/{output_file}.html")
  
  rmd_file <- file.path(system.file(package = "BitStat"), 
                        "app", "report", "descriptive", source_rmd)
  flag <- file.copy(from = rmd_file, to = target_rmd)
  
  #--Store parameters ----------------------------------------------------------  
  # id_dataset
  rmd_content <- sub("\\$id_dataset\\$", id_dataset, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # variable_row
  rmd_content <- sub("\\$variable_row\\$", variable_row, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # variable_col
  rmd_content <- sub("\\$variable_col\\$", variable_col, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # marginal
  rmd_content <- sub("\\$marginal\\$", marginal, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # marginal_type
  rmd_content <- sub("\\$marginal_type\\$", marginal_type, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # flag of plot
  rmd_content <- sub("\\$plot_flag\\$", plot, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # for non-ASCII in MS-Windows 
  if (grepl("^mingw", R.version$os)) { 
    writeLines(iconv(readLines(target_rmd), from = "CP949", to = "UTF8"), 
               file(target_rmd, encoding="UTF-8"))
  }
  
  rmarkdown::render(
    target_rmd,
    output_dir = output_dir,
    output_file = target_html,
    envir = new.env(parent = globalenv())
  )
  
  # Rmd 파일을 삭제하지 않으면, 데이터셋(Rmd 파일의 내용)을 바꾸더라도 ifrmae의 html 컨텐츠가 바뀌지 않음
  unlink(target_rmd)
}


##==============================================================================
## 01.04. 상관행렬
##==============================================================================
create_mat_corr <- function(id_dataset, variables = NULL, 
                            method = c("pearson", "kendall", "spearman"),
                            digits = 3, group_flag = FALSE, group_variable = NULL, 
                            plot = TRUE,
                            output_dir = glue::glue("{getwd()}/www/report"), 
                            output_file = "corr_mat") {
  method <- match.arg(method)
  
  source_rmd  <- "correlation_matrix.Rmd"
  target_rmd <- glue::glue("{output_dir}/{output_file}.Rmd")
  target_html <- glue::glue("{output_dir}/{output_file}.html")
  
  rmd_file <- file.path(system.file(package = "BitStat"), 
                        "app", "report", "descriptive", source_rmd)
  flag <- file.copy(from = rmd_file, to = target_rmd)
  
  variables <- variables %>% 
    paste(collapse = ",")
  
  group_variable <- group_variable %>% 
    paste(collapse = ",")
  
  #--Store parameters ----------------------------------------------------------  
  # id_dataset
  rmd_content <- sub("\\$id_dataset\\$", id_dataset, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # variables
  rmd_content <- sub("\\$variables\\$", variables, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # method
  rmd_content <- sub("\\$method\\$", method, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # digits
  rmd_content <- sub("\\$digits\\$", digits, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of group_by
  rmd_content <- sub("\\$group_flag\\$", group_flag, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # group variable name
  rmd_content <- sub("\\$group_variable\\$", group_variable, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of plot
  rmd_content <- sub("\\$plot_flag\\$", plot, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # for non-ASCII in MS-Windows 
  if (grepl("^mingw", R.version$os)) { 
    writeLines(iconv(readLines(target_rmd), from = "CP949", to = "UTF8"), 
               file(target_rmd, encoding="UTF-8"))
  }
  
  rmarkdown::render(
    target_rmd,
    output_dir = output_dir,
    output_file = target_html,
    envir = new.env(parent = globalenv())
  )
  
  # Rmd 파일을 삭제하지 않으면, 데이터셋(Rmd 파일의 내용)을 바꾸더라도 ifrmae의 html 컨텐츠가 바뀌지 않음
  unlink(target_rmd)
}


##==============================================================================
## 01.05. 상관 검정
##==============================================================================
create_mat_test <- function(id_dataset, variables = NULL, 
                            method = c("pearson", "kendall", "spearman"),
                            alternative = c("two.sided", "less", "greater"),
                            group_flag = FALSE, group_variable = NULL, plot = TRUE,
                            output_dir = glue::glue("{getwd()}/www/report"), 
                            output_file = "corr_test") {
  method <- match.arg(method)
  
  source_rmd  <- "correlation_test.Rmd"
  target_rmd <- glue::glue("{output_dir}/{output_file}.Rmd")
  target_html <- glue::glue("{output_dir}/{output_file}.html")
  
  rmd_file <- file.path(system.file(package = "BitStat"), 
                        "app", "report", "descriptive", source_rmd)
  flag <- file.copy(from = rmd_file, to = target_rmd)
  
  variables <- variables %>% 
    paste(collapse = ",")
  
  group_variable <- group_variable %>% 
    paste(collapse = ",")
  
  #--Store parameters ----------------------------------------------------------  
  # id_dataset
  rmd_content <- sub("\\$id_dataset\\$", id_dataset, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")

  # variables
  rmd_content <- sub("\\$variables\\$", variables, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # method
  rmd_content <- sub("\\$method\\$", method, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # alternative
  rmd_content <- sub("\\$alternative\\$", alternative, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # flag of group_by
  rmd_content <- sub("\\$group_flag\\$", group_flag, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # group variable name
  rmd_content <- sub("\\$group_variable\\$", group_variable, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")
  
  # flag of plot
  rmd_content <- sub("\\$plot_flag\\$", plot, 
                     readLines(target_rmd))
  cat(rmd_content, file = target_rmd, sep = "\n")  
  
  # for non-ASCII in MS-Windows 
  if (grepl("^mingw", R.version$os)) { 
    writeLines(iconv(readLines(target_rmd), from = "CP949", to = "UTF8"), 
               file(target_rmd, encoding="UTF-8"))
  }
  
  rmarkdown::render(
    target_rmd,
    output_dir = output_dir,
    output_file = target_html,
    envir = new.env(parent = globalenv())
  )
  
  # Rmd 파일을 삭제하지 않으면, 데이터셋(Rmd 파일의 내용)을 바꾸더라도 ifrmae의 html 컨텐츠가 바뀌지 않음
  unlink(target_rmd)
}
