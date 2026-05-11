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
  mount_path
}

#' Set up symlink for Azure storage paths
#'
#' Creates symbolic links in a stable directory to provide consistent
#' absolute paths for [targets](https://docs.ropensci.org/targets/) pipeline
#' inputs and outputs. This prevents targets from being flagged as outdated
#' when absolute file paths change across compute nodes.
#'
#' @param source_paths A named list or named character vector of source
#'   directory paths to link. Names are used as symlink names in `link_dir`.
#' @param link_dir A path for the directory where symlinks should be created.
#'
#' @return A named list of resolved directory paths, where mounted paths have
#'   been replaced with their corresponding symlink paths.
#'
#' @export
set_symlink <- function(
  source_paths,
  link_dir = fs::path("/tmp/static_mount")
) {
  if (!is.list(source_paths) && !is.character(source_paths)) {
    stop("`source_paths` must be a named list or named character vector.")
  }

  source_paths <- as.list(source_paths)
  if (is.null(names(source_paths)) || any(names(source_paths) == "")) {
    stop("`source_paths` must be named. Names are used as symlink names.")
  }

  fs::dir_create(link_dir)

  if (fs::dir_exists(link_dir)) {
    source_paths <- purrr::imodify(source_paths, \(x, name) {
      if (!is.na(x) && is.character(x) && length(x) == 1 && fs::dir_exists(x)) {
        new_path <- fs::path(link_dir, name)
        if (fs::link_exists(new_path)) {
          fs::link_delete(new_path)
        }
        if (!fs::link_exists(new_path)) {
          fs::link_create(x, new_path = new_path)
          new_path
        } else {
          x
        }
      } else {
        x
      }
    })
  }

  source_paths
}
