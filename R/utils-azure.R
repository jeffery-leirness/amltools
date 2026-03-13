# Semi-automated tests of Azure Storage integration live in tests/azure/. # nolint
# These tests should not be fully automated because they automatically create
# Azure storage containers (i.e., "buckets") and upload data, which could put an
# unexpected and unfair burden on external contributors from the open source
# community.
# nocov start

#' Get storage properties for an Azure blob
#'
#' Retrieves the storage properties (metadata) for a blob in an Azure Storage
#' container.
#'
#' @param key A character string specifying the blob name (path within the
#'   container).
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param version Not currently implemented.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return A list of storage properties, or `NULL` if the blob does not exist
#'   or if a 400 HTTP error occurs.
# TODO: implement "version" optional argument
azure_head <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  tryCatch(
    AzureStor::storage_endpoint(
      endpoint,
      key = auth_key,
      token = auth_token,
      sas = auth_sas
    ) |>
      AzureStor::storage_container(name = bucket) |>
      AzureStor::get_storage_properties(key),
    http_400 = function(condition) NULL
  )
}

#' Check if a blob exists in Azure Storage
#'
#' Checks whether a blob exists in an Azure Storage container.
#'
#' @param key A character string specifying the blob name (path within the
#'   container).
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param version Not currently implemented.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return A logical value: `TRUE` if the blob exists, `FALSE` otherwise.
#'
#' @noRd
# TODO: implement "version" optional argument
azure_exists <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  AzureStor::storage_endpoint(
    endpoint,
    key = auth_key,
    token = auth_token,
    sas = auth_sas
  ) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_file_exists(key)
}

#' List ETags for blobs in an Azure Storage container
#'
#' Retrieves the ETags for blobs in an Azure Storage container, optionally
#' filtered by a filename prefix.
#'
#' @param prefix A character string specifying an optional filename prefix to
#'   filter blobs by. If `NULL` or whitespace, all blobs are listed.
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return A named list of ETag values, where names are blob paths. Returns an
#'   empty list if no matching blobs are found.
#'
#' @noRd
azure_list_etags <- function(
  prefix = NULL,
  bucket,
  endpoint,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  if (stringr::str_trim(prefix) == "") {
    prefix <- NULL
  }
  container <- AzureStor::storage_endpoint(
    endpoint,
    key = auth_key,
    token = auth_token,
    sas = auth_sas
  ) |>
    AzureStor::storage_container(name = bucket)
  files <- container |>
    AzureStor::list_storage_files() |>
    tibble::as_tibble()
  if (nrow(files) > 0) {
    files <- dplyr::filter(files, !isdir)
    if (!is.null(prefix)) {
      files <- files |>
        dplyr::filter(stringr::str_starts(fs::path_file(name), prefix))
    }
    if (nrow(files) > 0) {
      files$name |>
        purrr::map(\(x) AzureStor::get_storage_properties(container, x)$etag) |>
        stats::setNames(files$name)
    } else {
      list()
    }
  } else {
    list()
  }
}

#' Download a blob from Azure Storage
#'
#' Downloads a blob from an Azure Storage container to a local file.
#' Creates intermediate directories if they do not already exist.
#'
#' @param file A character string specifying the local file path to download to.
#' @param key A character string specifying the blob name (path within the
#'   container).
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param version Not currently implemented.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return Called for its side effects; returns the result of
#'   [AzureStor::storage_download()] invisibly.
#'
#' @noRd
# TODO: implement "version" optional argument
azure_download <- function(
  file,
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  fs::path_dir(file) |>
    fs::dir_create()
  AzureStor::storage_endpoint(
    endpoint,
    key = auth_key,
    token = auth_token,
    sas = auth_sas
  ) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_download(src = key, dest = file, overwrite = TRUE)
}

#' Delete a blob from Azure Storage
#'
#' Deletes a blob from an Azure Storage container without prompting for
#' confirmation.
#'
#' @param key A character string specifying the blob name (path within the
#'   container).
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param version Not currently implemented.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return Invisibly returns `NULL`. Called for its side effects.
#'
#' @noRd
# TODO: implement "version" optional argument
azure_delete <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  AzureStor::storage_endpoint(
    endpoint,
    key = auth_key,
    token = auth_token,
    sas = auth_sas
  ) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::delete_storage_file(file = key, confirm = FALSE)
  invisible()
}

#' Upload a file to Azure Storage
#'
#' Uploads a local file to an Azure Storage container.
#'
#' @param file A character string specifying the local file path to upload.
#' @param key A character string specifying the destination blob name (path
#'   within the container).
#' @param bucket A character string specifying the storage container name.
#' @param endpoint A character string specifying the Azure Storage endpoint URL.
#' @param metadata A named list of metadata to attach to the blob. Not
#'   currently implemented.
#' @param auth_key A character string specifying the Azure Storage account key.
#'   Passed to [AzureStor::storage_endpoint()].
#' @param auth_token An Azure token object for authentication. Defaults to
#'   [azure_auth_token()].
#' @param auth_sas A character string specifying a shared access signature (SAS)
#'   token. Passed to [AzureStor::storage_endpoint()].
#'
#' @return Called for its side effects; returns the result of
#'   [AzureStor::storage_upload()] invisibly.
#'
#' @noRd
# TODO: implement "metadata" optional argument
azure_upload <- function(
  file,
  key,
  bucket,
  endpoint,
  metadata = list(),
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  AzureStor::storage_endpoint(
    endpoint,
    key = auth_key,
    token = auth_token,
    sas = auth_sas
  ) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_upload(src = file, dest = key)
}

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
#'
#' @noRd
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
# nocov end
