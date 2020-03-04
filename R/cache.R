#' In-memory cache initialized from file
#'
#' A cache in memory, but initialized from, and serializable to a file on the
#' filesystem. Anything added to the cache and not serialized will last only in
#' the current R session. The persistent file cache can be updated by invoking
#' `cacheObj$serialize()` (where `cacheObj` is the object returned by this
#' function), or a file copy can be created by passing a different path as
#' argument to the invocation.
#'
#' The implementation is based on [memoise::cache_memory()], and is fully
#' compatible as a cache function with [memoise::memoise()] (parameter `cache`).
#' It differs in the following ways:
#' - The object returned (a list) has additional keys `serialize` and `restore`,
#'   both functions accepting a path to a file.
#' - The computed hash values (as keys for the cache) remain the same across
#'   reinstallation of this package, making it possible to use a persistent
#'   cache that comes installed with the package.
#'
#' @param algo The hashing algorithm used for the cache, see
#' \code{\link[digest]{digest}} for available algorithms.
#' @param path the path for the file from which to initially load the cache, and
#'    by default also the one to restore from and serialize to when requested.
#'    If the file does not exist it will be treated as if an empty file had been
#'    provided.
#' @param compress logical, whether to use compression when serializing the cache.
#' @return
#' An object (in the form of a list) with the same interface as the objects
#' returned by [memoise::cache_memory()] etc., with functions `serialize()` and
#' `restore()` added. Both accept parameter `path`, which defaults to the value
#' of the `path` parameter above, and serializes / restores the cache to / from
#' the file designated by its value.
#' @references
#' Hadley Wickham, Jim Hester, Kirill Müller and Daniel Cook (2017).
#' memoise: Memoisation of Functions. R package version 1.1.0.
#' https://CRAN.R-project.org/package=memoise
#' @family cache methods
#' @keywords internal
#' @importFrom digest digest
#' @export
cache_serializableMemory <- function(algo = "sha512", path = NULL, compress = FALSE) {

  cache <- NULL

  cache_restore <- function(path = NULL, warnNotExists = TRUE) {
    if (missing(path) || is.null(path))
      path <- parent.env(env = environment())$path

    if (is.null(path))
      # no file path set here or when initialized, treat as reset
      cache_reset()
    else if (file.exists(path))
      # hopefully all is good - file path set and exists
      cache <<- readRDS(path)
    else {
      # file path set but doesn't exist, treat as restoring to empty file
      if (warnNotExists)
        warning("File '", path, "' to restore cache from does not exist. ",
                "Treating as empty cache, i.e., cache reset.",
                call. = FALSE)
      cache_reset()
    }
  }

  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
  }

  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
  }

  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }

  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }

  cache_drop_key <- function(key) {
    rm(list = key, envir = cache, inherits = FALSE)
  }

  cache_serialize <- function(path = NULL) {
    if (missing(path) || is.null(path))
      path <- parent.env(env = environment())$path

    if (is.null(path))
      warning("No path set or provided to which to serialize cache", call. = FALSE)
    else
      saveRDS(cache, file = path, compress = compress)
  }

  hash_objects <- function(...) {
    args <- list(...)
    obj <- args[[1]]
    obj <- lapply(obj, function(obj)
      if (is.call(obj))
        paste(format(obj), collapse = "\n")
      else
        obj
    )
    args[[1]] <- unlist(obj)
    args$algo <- algo
    do.call(digest::digest, args)
  }

  cache_restore(path, warnNotExists = FALSE)

  list(
    digest = hash_objects,
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    drop_key = cache_drop_key,
    keys = function() ls(cache),
    serialize = cache_serialize,
    restore = cache_restore
  )
}

#' Serialize a memoised function's cache to disk
#'
#' @param f the (memoised) function for which to serialize the cache to file
#' @param ... parameters to be passed on to the `serialize()` method of the cache,
#'   normally the file path if different from the one with which the cache
#'   was initialized.
#' @family cache methods
#' @importFrom memoise is.memoised
#' @export
serialize_cache <- function(f, ...) {
  if (! memoise::is.memoised(f)) stop("`f` is not a memoised function", call. = FALSE)

  env <- environment(f)
  if (exists("_cache", env, inherits = FALSE)) {
    cache <- get("_cache", env)
    if ("serialize" %in% names(cache))
      return(cache$serialize(...))
    else
      warning("function cache does not support serializing upon request", call. = FALSE)
  }
  invisible(NULL) # this is what saveRDS() returns normally
}

#' Restore a memoised function's cache from a file
#'
#' @param f the (memoised) function for which to restore the cache from file
#' @param ... parameters to be passed on to the `restore()` method of the cache,
#'   normally the file path if different from the one with which the cache was
#'   initialized. If a file path is provided, by default a warning is issued
#'   if it does not exist. This can be suppressed by additionally passing
#'   `warnNotExists = FALSE`.
#' @family cache methods
#' @importFrom memoise is.memoised
#' @export
restore_cache <- function(f, ...) {
  if (! memoise::is.memoised(f)) stop("`f` is not a memoised function", call. = FALSE)

  env <- environment(f)
  if (exists("_cache", env, inherits = FALSE)) {
    cache <- get("_cache", env)
    if ("restore" %in% names(cache))
      cache$restore(...)
    else
      warning("function cache does not support restoring from file upon request", call. = FALSE)
  }
}

#' Memoise a function optionally with persistent cache
#'
#' This is a thin layer over [memoise::memoise()], by applying a few defaults.
#' Specifically, by default the cache is in memory, but optionally a name for
#' a persistent cache can be given, in which case the cache is still held (and
#' grown) in memory, but is initialized from a file if it exists.
#'
#' The name of the persistent cache is turned into a file path by prefixing it
#' with the path to the `data-cache/` directory within the installed package,
#' and appending the extension `.rds`. Normally this location will not
#' be writable, i.e., the default persistent file cache will normally come with
#' installation, and stay unchanged.
#' @note
#' This function is a shortcut meant to be used only by the package's developers.
#' The described behavior when providing a persistent cache name can be achieved
#' with files located anywhere by using [memoise::memoise()] and passing
#' [cache_serializableMemory()] for the `cache` parameter.
#' @param f the function to [memoise][memoise::memoise()]
#' @param persistName character, a name for the persistent cache to load. Do not
#'    include extension or path. The corresponding file is assumed to be part
#'    of the installed package. If there is no such persistent cache file, this
#'    is ignored on startup/load.
#' @importFrom memoise memoise
#' @keywords internal
memoise <- function(f, persistName = NULL) {
  if (is.null(persistName) || nchar(persistName) == 0)
    memoise::memoise(f, cache = memoise::cache_memory(algo = "xxhash64"))
  else {
    fileCache <- file.path(system.file(package = "rphenoscape"),
                           "data-cache", paste0(persistName, ".rds"))
    memoise::memoise(f, cache = cache_serializableMemory(algo = "xxhash64",
                                                         path = fileCache))
  }
}