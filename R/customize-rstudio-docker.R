#' Customize RStudio docker container running in Azure Machine Learning Studio
#'
#' Applies a set of standard configurations to an RStudio Server docker
#' container running in Azure Machine Learning Studio, including RStudio
#' preferences, Azure CLI installation, and Git settings.
#'
#' @param username A character string specifying the user-specific username for
#'   the Azure Machine Learning Studio environment. This should match the
#'   user-specific directory nested directly under `path_users`.
#' @param path_users A character string specifying the path for the Users
#'   directory on Azure Machine Learning Studio.
#' @param git_setup A logical value indicating whether to apply Git settings.
#'   If `TRUE`, configures the Git user name, email, default branch, and
#'   credential helper, and prompts to set Git credentials.
#' @param git_name A character string specifying the Git user name to configure.
#' @param git_email A character string specifying the Git user email to
#'   configure.
#'
#' @return Called for its side effects; returns invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' customize_rstudio_docker(
#'   username = "jane.doe",
#'   git_name = "Jane Doe",
#'   git_email = "jane.doe@noaa.gov"
#' )
#' }
customize_rstudio_docker <- function(
  username,
  path_users = "~/cloudfiles/code/Users",
  git_setup = TRUE,
  git_name,
  git_email
) {
  # set user-specific directory path
  .path_user <- fs::path(path_users, username)

  # set specific rstudio preferences --------------------------------------
  rstudio.prefs::use_rstudio_prefs(
    save_workspace = "never",
    load_workspace = FALSE,
    initial_working_directory = .path_user,
    default_open_project_location = .path_user,
    always_save_history = FALSE,
    insert_native_pipe_operator = TRUE,
    soft_wrap_r_files = TRUE,
    rainbow_parentheses = TRUE,
    posix_terminal_shell = "bash",
    editor_theme = "Cobalt",
    restore_last_project = FALSE
  )

  # install azure cli
  system("curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash")

  # configure git ---------------------------------------------------------
  usethis::use_git_config(
    "user",
    user.name = git_name,
    user.email = git_email,
    core.editor = "code --wait"
  )
  usethis::git_default_branch_configure()
  usethis::use_git_config(
    "user",
    safe.directory = "*",
    credential.helper = "cache --timeout=7776000"
  )
  # usethis::use_git_config("user", secrets.patterns = "password\s*=\s*.+",
  #                         secrets.patterns = "Password\s*=\s*.+",
  #                         secrets.patterns = "PASSWORD\s*=\s*.+",
  #                         secrets.patterns = "user\s*=\s*.+",
  #                         secrets.patterns = "User\s*=\s*.+",
  #                         secrets.patterns = "USER\s*=\s*.+")

  # # install git secrets -----------------------------------------------------
  # if (!fs::dir_exists(fs::path(.path_user, "git-secrets"))) {
  #   usethis::create_from_github("awslabs/git-secrets", destdir = .path_user,
  #                               fork = FALSE, rstudio = FALSE, open = FALSE)
  # }
  # cmd <- paste("sudo make -C", fs::path(.path_user, "git-secrets"), "install")
  # system(cmd)
  #
  # # configure git secrets ---------------------------------------------------
  # system("git secrets --register-aws --global")
  # system("git secrets --install ~/.git-templates/git-secrets")
  # system("git config --global init.templateDir ~/.git-templates/git-secrets")
  #
  # # delete git-secrets directory
  # fs::dir_delete(file.path(.path_user, "git-secrets"))

  # set git credentials
  gitcreds::gitcreds_set()

  # alternatively, use ssh for github connection
  # cmd <- paste0("ssh-keygen -t ed25519 -C '", username, "'")
  # system(cmd)
  # system("ssh-add ~/.ssh/id_ed25519")
  # next:
  # 1. copy public key (from file ~/.ssh/id_ed25519.pub)
  # 2. register public key with github
  # 3. ensure remote repository is set to use ssh (`git remote set-url origin <ssh command to clone repository>`)
}
