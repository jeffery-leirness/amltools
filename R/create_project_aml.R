#' Create a project for Azure Machine Learning Studio
#'
#' @param path The path for the project directory. If it exists, it is used.
#' If it does not exist, it is created, provided that the parent path exists.
#' This value is passed the `path` argument of `usethis::create_project()`.
#' @param use_renv Whether or not to initialize the project to use the [renv]
#' (https://rstudio.github.io/renv/index.html) package. If `TRUE`, the
#' `renv.lock` file will be created within the project at `env/renv.lock` to
#' ensure any relevant Dockerfiles within the `env` directory have access.
#' @param use_python Whether or not to initialize [renv]
#' (https://rstudio.github.io/renv/index.html) python integration. This only
#' happens if `use_renv` is also `TRUE`.
#' @param use_targets Whether or not to initialize the project to use the
#' [targets](https://docs.ropensci.org/targets/) package.
#' @param use_git Whether or not to initialize a Git repository. If `TRUE`,
#' calls `usethis::use_git()`.
#' @param use_github Whether or not to connect the Git repository with GitHub.
#' If `TRUE` and `use_git` is also `TRUE`, calls `usethis::use_github()`.
#' @param github_private Whether or not the GitHub repository should be private.
#' This value is passed to the `private` argument of `usethis::use_github()`.
#' This argument is only evaluated if both `use_git` and `use_github` are `TRUE`.
#' @param r_version Declare a specific version of R to use for [renv]
#' (https://rstudio.github.io/renv/index.html) integration and/or Dockerfile
#' creation. If `NULL`, this will be set to the R version of the user's session.
#' @param py_version Declare a specific version of Python to be associated with
#' [renv](https://rstudio.github.io/renv/index.html). This argument is only
#' evaluated if both `use_renv` and `use_python` are `TRUE`.
#' @param dockerfile Whether or not to create a generic Dockerfile within the
#' project at `env/Dockerfile` for use when submitting R jobs to Azure Machine
#' Learning Studio compute clusters. If `use_rev` is `TRUE`, the Dockerfile will
#' include code to install [renv](https://rstudio.github.io/renv/index.html) and
#' restore packages from the `renv.lock` file.
#'
#' @return
#' @export
#'
#' @examples
create_project_aml <- function(path, use_renv = TRUE, use_python = TRUE,
                               use_targets = TRUE, use_git = TRUE,
                               use_github = FALSE, github_private = TRUE,
                               r_version = NULL, py_version = NULL,
                               dockerfile = TRUE) {

  # determine r version to use for renv and dockerfile
  if (any(use_renv, dockerfile)) {
    rver <- if (!is.null(r_version)) r_version else paste(R.version$major, R.version$minor, sep = ".")
  }

  # create and activate R project
  usethis::create_project(path, open = FALSE)
  setwd(file.path(getwd(), path))

  # create project directories
  dir.create("data-raw", recursive = TRUE)
  dir.create("data", recursive = TRUE)
  dir.create("env", recursive = TRUE)
  dir.create("notes", recursive = TRUE)
  dir.create("scripts", recursive = TRUE)

  # use MIT + U.S. Department of Commerce license with project
  usethis::use_mit_license()
  license_txt <- c(
    "Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. \u00a7105). ",
    "The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries ",
    "other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the ",
    "Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create ",
    "derivative works of the Software outside of the United States."
  )
  if (file.exists("LICENSE")) {
    usethis::write_union("LICENSE", c("", license_txt))
  } else {
    usethis::write_over("LICENSE", license_txt)
  }

  # add generic README.md file to project
  # include U.S. Department of Commerce disclaimer and license
  readme_txt <- c(
    "## Disclaimer",
    "",
    "This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.",
    "",
    "## License",
    "",
    paste(license_txt, collapse = "")
  )
  usethis::write_over("README.md", readme_txt)

  # create generic Dockerfile for use in compute clusters
  if (dockerfile) {
    docker_txt <- c(
      "# Install specific R version",
      paste0("FROM rocker/geospatial:", rver),
      "",
      "# Install python",
      "RUN apt-get update -qq && \\",
      " apt-get install -y python3-pip tcl tk libz-dev libpng-dev",
      "RUN ln -f /usr/bin/python3 /usr/bin/python",
      "RUN ln -f /usr/bin/pip3 /usr/bin/pip",
      "RUN pip install -U pip",
      "",
      "# Install azureml-mlflow",
      "RUN pip install azureml-mlflow",
      "",
      "# Install renv",
      "RUN R -e \"install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))\"",
      "",
      "# Copy the renv.lock lockfile to the container",
      "WORKDIR /project\nCOPY renv.lock renv.lock",
      "",
      "# Set renv library path",
      "ENV RENV_PATHS_LIBRARY renv/library",
      "",
      "# Restore packages from renv lockfile",
      "RUN R -e \"renv::restore()\""
    )
    if (!use_renv) {
      docker_txt <- docker_txt[1:(length(docker_txt) - 12)]
    }
    usethis::write_over("env/Dockerfile", docker_txt)
  }

  # initialize targets package
  if (use_targets) {
    targets::use_targets(open = FALSE)
  }

  # renv setup ------------------------------------------------------------
  if (use_renv) {

    # renv settings:
    # change the lockfile location (must be in the same directory as the Dockerfile)
    Sys.setenv(
      RENV_PATHS_LOCKFILE = "env/renv.lock"
    )

    # write renv settings to .Renviron file so they persist for all sessions
    renviron_txt <- Sys.getenv("RENV_PATHS_LOCKFILE", names = TRUE)
    renviron_txt <- paste(names(renviron_txt), renviron_txt, sep = " = ", collapse = "\n")
    usethis::write_over(".Renviron", renviron_txt)

    # create .amlignore file
    # this is necessary when using renv and submitting jobs to a compute
    amlignore_txt <- c(
      ".ipynb_aml_checkpoints/",
      "*.amltmp",
      "*.amltemp",
      "renv/",
      ".Rprofile"
    )
    usethis::write_over(".amlignore", amlignore_txt)

    # initialize renv
    renv::init(settings = list(r.version = rver), load = FALSE, restart = FALSE)

    # activate renv python integration
    # if (use_python) {
    #   renv::install("reticulate", prompt = FALSE)
    #   reticulate::install_python()
    #   renv::use_python(type = "virtualenv")
    #   reticulate::py_install(packages = c("azure-ai-ml", "azure-identity", "azureml", "azureml-core", "azureml-fsspec",
    #                                       "azure-storage-blob", "mltable"))
    #   renv::snapshot(prompt = FALSE)
    # }
  }

  # initialize git repository & add custom lines to .gitignore file
  if (use_git) {

    # initialize git repository
    usethis::use_git()

    # add various entries to .gitignore
    usethis::use_git_ignore(ignores = c("*.amltmp", "*.amltemp", "notes/"))
    if (use_targets) {
      usethis::use_git_ignore(ignores = "_targets/")
    }
    usethis::use_git_ignore(ignores = c("*", "!.gitignore"), directory = "data")
    usethis::use_git_ignore(ignores = c("*", "!.gitignore"), directory = "data-raw")
    usethis::use_git()

    # initialize `git-secrets`
    system("git secrets --install")
    system("git secrets --scan -r")

    # add project directory as safe directory within git
    # system(paste("git config --global --add safe.directory", usethis::proj_path()))

    # optionally connect to github
    if (use_github) {

      # create github repository and configure as git remote
      usethis::use_github(private = github_private)

    }

  }

  # activate the project and open new RStudio session if using
  usethis::proj_activate(usethis::proj_get())

}
