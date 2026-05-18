library(openxlsx)

set.seed(20260518)

# -----------------------------
# Long COVID-like synthetic data
# -----------------------------

generate_synthetic_covid <- function(
    n,
    m = 27,
    scale_min = 1,
    scale_max = 10,
    output_dir = "./synthetic_data/covid"
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  dataset <- as.data.frame(
    matrix(
      sample(scale_min:scale_max, n * m, replace = TRUE),
      nrow = n,
      ncol = m
    )
  )
  
  colnames(dataset) <- sprintf("SY09_%02d", seq_len(m))
  
  # Fixed group indicator used by the application
  dataset$Group_A <- 1
  
  # Binary subgroup label
  dataset$gender <- sample(c(1, 2), n, replace = TRUE)
  
  output_path <- file.path(
    output_dir,
    paste0("synthetic_covid_n", n, "_m", m, ".xlsx")
  )
  
  write.xlsx(dataset, output_path, rowNames = FALSE)
  message("Saved: ", output_path)
  
  invisible(dataset)
}

# -----------------------------
# Rare disease-like synthetic data
# -----------------------------

generate_synthetic_rare <- function(
    n,
    m = 46,
    scale_min = 1,
    scale_max = 5,
    output_dir = "./synthetic_data/rare"
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  dataset <- as.data.frame(
    matrix(
      sample(scale_min:scale_max, n * m, replace = TRUE),
      nrow = n,
      ncol = m
    )
  )
  
  colnames(dataset) <- paste0("q", seq_len(m))
  
  # Six synthetic diagnosis groups
  dataset$diagnosis <- sample(1:6, n, replace = TRUE)
  
  output_path <- file.path(
    output_dir,
    paste0("synthetic_rare_n", n, "_m", m, ".xlsx")
  )
  
  write.xlsx(dataset, output_path, rowNames = FALSE)
  message("Saved: ", output_path)
  
  invisible(dataset)
}

# -----------------------------
# Generate validation datasets
# -----------------------------

covid_settings <- data.frame(
  scenario = c("C1", "C2", "C3"),
  n = c(500, 1000, 3000),
  m = c(27, 27, 27)
)

rare_settings <- data.frame(
  scenario = c("R1", "R2"),
  n = c(500, 1000),
  m = c(46, 46)
)

for (i in seq_len(nrow(covid_settings))) {
  generate_synthetic_covid(
    n = covid_settings$n[i],
    m = covid_settings$m[i]
  )
}

for (i in seq_len(nrow(rare_settings))) {
  generate_synthetic_rare(
    n = rare_settings$n[i],
    m = rare_settings$m[i]
  )
}