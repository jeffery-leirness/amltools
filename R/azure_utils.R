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
        app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                   silent = TRUE)
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
        AzureRMR::get_azure_token(resource,
                                  tenant = tenant,
                                  app = app,
                                  auth_type = auth_type)
      }
    }
  )
}

azure_upload_targets <- function(key, path) {
  if (fs::is_dir(path)) {
    stop("This CAS repository does not support directory outputs.")
  }
  AzureStor::upload_to_url(path,
                           dest = paste(Sys.getenv("TARGETS_ENDPOINT"),
                                        Sys.getenv("TARGETS_CONTAINER"),
                                        targets::tar_path_store(), key, sep = "/"),
                           token = Sys.getenv("TARGETS_AUTH_TOKEN"))
}

azure_download_targets <- function(key, path) {
  AzureStor::download_from_url(paste(Sys.getenv("TARGETS_ENDPOINT"),
                                     Sys.getenv("TARGETS_CONTAINER"),
                                     targets::tar_path_store(), key, sep = "/"),
                               dest = path,
                               token = Sys.getenv("TARGETS_AUTH_TOKEN"),
                               overwrite = TRUE)
}

azure_exists_targets <- function(key) {
  AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                              token = Sys.getenv("TARGETS_AUTH_TOKEN")) |>
    AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
    AzureStor::storage_file_exists(fs::path(targets::tar_path_store(), key))
}
