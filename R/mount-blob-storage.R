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
#' @export
get_azure_key <- function(resource_group, account_name) {
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
#' @param container_name A character string specifying the Azure Storage container
#'   name.
#'
#' @return Called for its side effects; returns invisibly. Prints a message
#'   indicating whether the mount was successful or already active.
#'
#' @export
mount_blob_storage <- function(
  mount_path = fs::path("/mnt/blobfuse_mount", container_name),
  cache_path = fs::path("/mnt/blobfuse_cache", container_name),
  config_path = "config.yaml",
  container_name
) {
  if (!fs::dir_exists(mount_path)) {
    is_mounted <- FALSE
    if (!fs::dir_exists(fs::path_dir(mount_path))) {
      processx::run("sudo", args = c("mkdir", fs::path_dir(mount_path)))
      processx::run(
        "sudo",
        args = c("chown", "azureuser", fs::path_dir(mount_path))
      )
    }
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
    if (!fs::dir_exists(cache_path)) {
      if (!fs::dir_exists(fs::path_dir(cache_path))) {
        processx::run("sudo", args = c("mkdir", fs::path_dir(cache_path)))
        processx::run(
          "sudo",
          args = c("chown", "azureuser", fs::path_dir(cache_path))
        )
      }
      processx::run("sudo", args = c("mkdir", cache_path))
      processx::run("sudo", args = c("chown", "azureuser", cache_path))
    }
    processx::run(
      "blobfuse2",
      args = c(
        "mount",
        mount_path,
        "--config-file",
        config_path,
        "--container-name",
        container_name,
        "--tmp-path",
        cache_path
      )
    )
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
