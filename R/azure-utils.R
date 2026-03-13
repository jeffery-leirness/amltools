#' Get an Azure authentication token
#'
#' Attempts to obtain an Azure authentication token using managed identity
#' first, then falls back to interactive authentication if managed identity
#' is unavailable.
#'
#' @param resource A character string specifying the Azure resource URL to
#'   authenticate against.
#' @param tenant A character string specifying the Azure Active Directory tenant
#'   ID. Defaults to `"common"`.
#' @param app A character string specifying the Azure application (client) ID.
#'   If `NULL`, attempts to retrieve from an existing Azure login.
#' @param auth_type A character string specifying the authentication type to use
#'   as a fallback. Passed to [AzureRMR::get_azure_token()].
#'
#' @return An Azure token object.
azure_auth_token <- function(
  resource = "https://storage.azure.com",
  tenant = "common",
  app = NULL,
  auth_type = "device_code"
) {
  tryCatch(
    AzureAuth::get_managed_token(resource),
    error = function(err) {
      if (is.null(app)) {
        app <- try(
          AzureRMR::get_azure_login()$token$client$client_id,
          silent = TRUE
        )
        if (inherits(app, "try-error")) {
          AzureRMR::create_azure_login()
          app <- AzureRMR::get_azure_login()$token$client$client_id
        }
      }
      tokens <- AzureRMR::list_azure_tokens()
      resources <- purrr::map(tokens, \(x) x$resource)
      token_use <- match(resource, resources)[1]
      if (!is.na(token_use)) {
        tokens[[token_use]]
      } else {
        AzureRMR::get_azure_token(
          resource,
          tenant = tenant,
          app = app,
          auth_type = auth_type
        )
      }
    }
  )
}

#' Upload a targets object to Azure Blob Storage
#'
#' Uploads a file to Azure Blob Storage for use as a content-addressable
#' storage (CAS) backend with the
#' [targets](https://docs.ropensci.org/targets/) package. Uses environment
#' variables `TARGETS_ENDPOINT`, `TARGETS_CONTAINER`, and `TARGETS_AUTH_TOKEN`
#' for storage configuration.
#'
#' @param key A character string specifying the content hash key for the object.
#' @param path A character string specifying the local file path to upload.
#'   Directory paths are not supported.
#'
#' @return The result of [AzureStor::upload_to_url()], returned invisibly.
#'
#' @noRd
azure_upload_targets <- function(key, path) {
  if (fs::is_dir(path)) {
    stop("This CAS repository does not support directory outputs.")
  }
  AzureStor::upload_to_url(
    path,
    dest = paste(
      Sys.getenv("TARGETS_ENDPOINT"),
      Sys.getenv("TARGETS_CONTAINER"),
      targets::tar_path_store(),
      key,
      sep = "/"
    ),
    token = Sys.getenv("TARGETS_AUTH_TOKEN")
  )
}

#' Download a targets object from Azure Blob Storage
#'
#' Downloads a file from Azure Blob Storage used as a content-addressable
#' storage (CAS) backend with the
#' [targets](https://docs.ropensci.org/targets/) package. Uses environment
#' variables `TARGETS_ENDPOINT`, `TARGETS_CONTAINER`, and `TARGETS_AUTH_TOKEN`
#' for storage configuration.
#'
#' @param key A character string specifying the content hash key for the object.
#' @param path A character string specifying the local file path to save the
#'   downloaded object to.
#'
#' @return The result of [AzureStor::download_from_url()], returned invisibly.
#'
#' @noRd
azure_download_targets <- function(key, path) {
  AzureStor::download_from_url(
    paste(
      Sys.getenv("TARGETS_ENDPOINT"),
      Sys.getenv("TARGETS_CONTAINER"),
      targets::tar_path_store(),
      key,
      sep = "/"
    ),
    dest = path,
    token = Sys.getenv("TARGETS_AUTH_TOKEN"),
    overwrite = TRUE
  )
}

#' Check if a targets object exists in Azure Blob Storage
#'
#' Checks whether a file exists in Azure Blob Storage used as a
#' content-addressable storage (CAS) backend with the
#' [targets](https://docs.ropensci.org/targets/) package. Uses environment
#' variables `TARGETS_ENDPOINT`, `TARGETS_CONTAINER`, and `TARGETS_AUTH_TOKEN`
#' for storage configuration.
#'
#' @param key A character string specifying the content hash key for the object.
#'
#' @return A logical value: `TRUE` if the object exists, `FALSE` otherwise.
#'
#' @noRd
azure_exists_targets <- function(key) {
  AzureStor::storage_endpoint(
    Sys.getenv("TARGETS_ENDPOINT"),
    token = Sys.getenv("TARGETS_AUTH_TOKEN")
  ) |>
    AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
    AzureStor::storage_file_exists(fs::path(targets::tar_path_store(), key))
}
