# mount_path <- fs::path("~", "blobfuse_mount") |>
#   fs::path_expand()
# config_path <- fs::path("config.yaml")
# cache_path <- fs::path("/mnt/blobfuse_cache")

# Install blobfuse2
# sudo wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb
# sudo dpkg -i packages-microsoft-prod.deb
# sudo apt-get update
# sudo apt-get install blobfuse2

#' Get an Azure Storage account key
#'
#' Retrieves the primary access key for an Azure Storage account using the
#' Azure CLI (`az storage account keys list`). Requires that the Azure CLI is
#' installed and authenticated.
#'
#' @param resource_group A character string specifying the Azure resource group
#'   name containing the storage account.
#' @param account_name A character string specifying the Azure Storage account
#'   name.
#'
#' @return A character string containing the primary storage account access key.
#'
#' @noRd
get_azure_key <- function(
  resource_group = "nccos-rg-hmt-prod-e2",
  account_name = "nccosmlmsestor1"
) {
  stdout <- processx::run(
    "az",
    args = c(
      "storage",
      "account",
      "keys",
      "list",
      "--resource-group",
      resource_group,
      "--account-name",
      account_name
    )
  )$stdout |>
    jsonlite::fromJSON()
  dplyr::filter(stdout, keyName == "key1") |>
    dplyr::pull(value)
}

#' Mount Azure Blob Storage using blobfuse2
#'
#' Mounts an Azure Blob Storage container to a local directory using
#' [blobfuse2](https://github.com/Azure/azure-storage-fuse). If the storage
#' is already mounted, a message is displayed and the function exits early.
#' Requires that `blobfuse2` and the Azure CLI are installed and authenticated.
#'
#' @param mount_path A `fs_path` object or character string specifying the
#'   local directory to mount the storage to.
#' @param cache_path A `fs_path` object or character string specifying the
#'   local directory blobfuse2 uses as a cache.
#' @param config_path A `fs_path` object or character string specifying the
#'   path to the blobfuse2 configuration YAML file.
#' @param resource_group A character string specifying the Azure resource group
#'   name containing the storage account.
#' @param account_name A character string specifying the Azure Storage account
#'   name.
#'
#' @return Called for its side effects; returns invisibly. Prints a message
#'   indicating whether the mount was successful or already active.
#'
#' @noRd
mount_blob_storage <- function(
  mount_path = fs::path("/mnt/blobfuse_mount"),
  cache_path = fs::path("/mnt/blobfuse_cache"),
  config_path = fs::path("config.yaml"),
  resource_group = "nccos-rg-hmt-prod-e2",
  account_name = "nccosmlmsestor1"
  # keyring = "azure_keys",
  # service = "blobfuse_key",
  # username = "azure_storage"
) {
  if (!fs::dir_exists(mount_path)) {
    is_mounted <- FALSE
    fs::dir_create(mount_path)
    processx::run("sudo", args = c("mkdir", mount_path))
    processx::run("sudo", args = c("chown", "azureuser", mount_path))
  } else {
    check_mount <- paste("mountpoint -q", mount_path)
    is_mounted <- processx::run(
      "mountpoint",
      args = c("-q", mount_path),
      error_on_status = FALSE
    )$status ==
      0
  }
  if (!is_mounted) {
    message("Mounting Azure Blob Storage...")

    # # 1. Prompt for the main password
    # main_pass <- askpass::askpass("Enter Keyring Main Password:")

    # # 2. Access the file backend and unlock it using the main password
    # kb <- keyring::backend_file$new()
    # kb$keyring_unlock(keyring, password = main_pass)

    # # 3. Retrieve the Azure Storage key from the unlocked keyring
    # azure_key <- kb$get(service, username = username, keyring = keyring)

    # 4. Create the cache directory if it doens't already exist
    if (!fs::dir_exists(cache_path)) {
      processx::run("sudo", args = c("mkdir", cache_path))
      processx::run("sudo", args = c("chown", "azureuser", cache_path))
    }

    # 5. Inject to the environment and mount
    # Sys.setenv(AZURE_STORAGE_ACCESS_KEY = azure_key)
    Sys.setenv(
      AZURE_STORAGE_ACCESS_KEY = get_azure_key(resource_group, account_name)
    )
    processx::run(
      "blobfuse2",
      args = c(
        "mount",
        mount_path,
        "--config-file",
        config_path,
        "--tmp-path",
        cache_path
      )
    )

    # 6. Lock the keyring and clean up
    # kb$keyring_lock("azure_keys")
    Sys.unsetenv("AZURE_STORAGE_ACCESS_KEY")
    # rm(azure_key, main_pass)

    message("Mount successful.")
  } else {
    message("Azure Blob Storage is already mounted.")
  }
}

#' Set up symlinks for targets pipeline storage paths
#'
#' Creates symbolic links in a stable temporary directory to provide consistent
#' absolute paths for [targets](https://docs.ropensci.org/targets/) pipeline
#' inputs and outputs. This prevents targets from being flagged as outdated
#' when absolute file paths change across compute nodes.
#'
#' @return A named list of resolved directory paths, where mounted paths have
#'   been replaced with their corresponding symlink paths.
#'
#' @noRd
set_targets_symlinks <- function() {
  # 6. Set up directory paths using symlinks for accessing data and saving outputs
  #   - This is necessary to ensure targets are not flagged as outdated due to changes in absolute file paths across compute nodes
  opt <- list(
    dir_workspace = fs::path(
      mount_path,
      "UI",
      "leirness-data",
      "sampling-images-annotation"
    ),
    dir_input = fs::path(
      mount_path,
      "UI",
      "NCCOS-SEA-Branch-MDBC-Project",
      "data",
      "MGM",
      "PHM",
      "outputs",
      "predictors"
    ),
    dir_output = fs::path("output") |>
      fs::path_abs()
  )
  dir_link <- fs::path("/tmp/static_mount")
  if (fs::dir_exists(dir_link)) {
    fs::dir_delete(dir_link)
  }
  fs::dir_create(dir_link)
  if (fs::dir_exists(dir_link)) {
    opt <- purrr::imodify(opt, \(x, name) {
      if (!is.na(x) && is.character(x) && length(x) == 1 && fs::dir_exists(x)) {
        fs::link_create(x, new_path = fs::path(dir_link, name))
        fs::path(dir_link, name)
      } else {
        x
      }
    })
  }
}
