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
#' [blobfuse2](https://github.com/Azure/azure-storage-fuse) with deterministic
#' zero-latency cache timeout configurations. Disabling attribute, entry, and
#' file page caching forces blobfuse2 to query Azure Storage REST APIs
#' directly, guaranteeing real-time target status parity across cluster nodes.
#' If the storage is already mounted, a message is displayed and the function
#' exits early. Requires that `blobfuse2` and the Azure CLI are installed and
#' authenticated.
#'
#' @param account_name A character string specifying the Azure Storage account
#'   name.
#' @param container_name A character string specifying the Azure Storage container
#'   name.
#' @param mount_point A `fs_path` object or character string specifying the
#'   local directory to mount the storage to. If not provided, defaults to
#'   `~/.azureml-blobstore/<container_name>`.
#' @param attr_timeout Cache timeout in seconds for file attributes and directory entries. Defaults to `0L`.
#' @param file_cache_timeout Cache timeout in seconds for local file contents. Defaults to `0L`.
#' @param disable_kernel_cache Logical. Whether to disable Linux kernel page caching. Defaults to `TRUE`.
#'
#' @return Called for its side effects; returns invisibly the expanded `mount_path`.
#'
#' @export
mount_blob_storage <- function(
  account_name,
  container_name,
  mount_point = fs::path("~/azureml-blobstore", container_name),
  attr_timeout = 0L,
  file_cache_timeout = 0L,
  disable_kernel_cache = TRUE
) {
  mount_path <- fs::path_expand(mount_point)

  check_mount <- processx::run(
    "mountpoint",
    args = c("-q", mount_path),
    error_on_status = FALSE
  )

  if (check_mount$status == 0) {
    message("The directory '", mount_path, "' is already mounted.")
    return(invisible(mount_path))
  }

  if (fs::dir_exists(mount_path) && length(fs::dir_ls(mount_path)) > 0) {
    stop(
      "Mount point ",
      mount_path,
      " already exists but is not an empty directory. Please specify a different mount point."
    )
  }

  old_env <- Sys.getenv(
    c(
      "AZURE_STORAGE_AUTH_TYPE",
      "AZURE_STORAGE_ACCOUNT",
      "AZURE_STORAGE_ACCOUNT_CONTAINER",
      "AZURE_CONFIG_DIR"
    ),
    names = TRUE,
    unset = NA
  )
  on.exit(
    {
      restore <- old_env[!is.na(old_env)]
      if (length(restore) > 0) {
        do.call(Sys.setenv, as.list(restore))
      }
      Sys.unsetenv(names(old_env[is.na(old_env)]))
    },
    add = TRUE
  )

  Sys.setenv(
    AZURE_STORAGE_AUTH_TYPE = "azcli",
    AZURE_STORAGE_ACCOUNT = account_name,
    AZURE_STORAGE_ACCOUNT_CONTAINER = container_name,
    AZURE_CONFIG_DIR = fs::path_expand("~/.azure")
  )

  fs::dir_create(mount_path)

  fuse_args <- c(
    "-E",
    "blobfuse2",
    "mount",
    mount_path,
    "--streaming",
    "--allow-other",
    paste0("--attr-cache-timeout=", attr_timeout),
    paste0("--attr-timeout=", attr_timeout),
    paste0("--entry-timeout=", attr_timeout),
    paste0("--file-cache-timeout=", file_cache_timeout)
  )

  if (isTRUE(disable_kernel_cache)) {
    fuse_args <- c(fuse_args, "--disable-kernel-cache")
  }

  mount_res <- processx::run("sudo", args = fuse_args, error_on_status = FALSE)

  if (mount_res$status == 0) {
    message(
      "Successfully mounted container '",
      container_name,
      "' to ",
      mount_path
    )
  } else {
    warning(
      "Mount failed with exit code ",
      mount_res$status,
      ".\n",
      "Error output:\n",
      mount_res$stderr
    )
  }

  invisible(mount_path)
}


#' Unmount Azure Blob Storage
#'
#' Unmounts a previously mounted Azure Blob Storage container.
#'
#' @param mount_point The local directory where the container is mounted.
#'
#' @return Invisibly returns `TRUE` if the unmount was successful, otherwise `FALSE`.
#'
#' @export
unmount_blob_storage <- function(mount_point) {
  mount_path <- fs::path_expand(mount_point)

  check_mount <- processx::run(
    "mountpoint",
    args = c("-q", mount_path),
    error_on_status = FALSE
  )

  if (check_mount$status != 0) {
    message("The directory '", mount_path, "' is not currently mounted.")
    return(invisible(TRUE))
  }

  unmount_res <- processx::run(
    "sudo",
    args = c("umount", mount_path),
    error_on_status = FALSE
  )

  if (unmount_res$status == 0) {
    message("Successfully unmounted '", mount_path, "'")
  } else {
    warning(
      "Unmount failed with exit code ",
      unmount_res$status,
      ".\n",
      "Error output:\n",
      unmount_res$stderr
    )
  }
  invisible(unmount_res$status == 0)
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
