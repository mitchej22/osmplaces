#' SDOH place-type taxonomy
#'
#' Lists the place types that can be requested through the `tags` argument of
#' [get_places()], with the ohsome filter each one is translated to.
#'
#' @return A data frame with one row per place type.
#' @examples
#' places_taxonomy()[, c("label", "sdoh_domain")]
#' @export
places_taxonomy <- function() {
  taxonomy <- read_taxonomy()
  data.frame(
    label = names(taxonomy),
    description = vapply(taxonomy, function(x) x$description, ""),
    sdoh_domain = vapply(taxonomy, function(x) x$sdoh_domain, ""),
    geometry_types = vapply(taxonomy, function(x) paste(x$geometry_types, collapse = ", "), ""),
    aliases = vapply(taxonomy, function(x) paste(x$aliases, collapse = ", "), ""),
    ohsome_filter = vapply(taxonomy, build_ohsome_filter, ""),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

taxonomy_path <- function() {
  system.file("extdata", "taxonomy.yaml", package = "osmplaces")
}

# Read the YAML taxonomy into a named list (names are place-type labels).
read_taxonomy <- function(path = taxonomy_path()) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("Taxonomy file not found: ", path, call. = FALSE)
  }
  raw <- yaml::yaml.load(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  )
  types <- lapply(raw$place_types, normalize_place_type)
  names(types) <- vapply(types, function(x) x$label, "")
  types
}

# YAML 1.1 reads an unquoted `yes`/`no` as TRUE/FALSE; OSM tag values need the
# original words.
yaml_chr <- function(x) {
  x <- unlist(x)
  if (is.logical(x)) x <- ifelse(x, "yes", "no")
  as.character(x)
}

normalize_place_type <- function(pt) {
  specs <- lapply(pt$tag_specs, function(s) {
    values <- yaml_chr(s$value)
    if (!length(values)) {
      stop(sprintf("Taxonomy entry '%s' has a tag_spec with no values.", pt$label),
           call. = FALSE)
    }
    list(
      key = as.character(s$key),
      value = values,
      exclude = lapply(s$exclude, function(e) {
        list(key = as.character(e$key), value = yaml_chr(e$value))
      })
    )
  })

  geometry_types <- as.character(unlist(pt$geometry_types))
  if (!length(geometry_types)) geometry_types <- c("node", "way", "relation")
  if (!all(geometry_types %in% c("node", "way", "relation"))) {
    stop(sprintf("Taxonomy entry '%s' has invalid geometry_types.", pt$label),
         call. = FALSE)
  }

  list(
    label = as.character(pt$label),
    description = as.character(pt$description %||% ""),
    sdoh_domain = as.character(pt$sdoh_domain %||% ""),
    geometry_types = geometry_types,
    tag_specs = specs,
    aliases = as.character(unlist(pt$aliases)),
    notes = as.character(pt$notes %||% "")
  )
}

# Resolve `tags` ("all", labels, or aliases) to unique taxonomy labels.
resolve_tags <- function(tags, taxonomy) {
  labels <- names(taxonomy)
  if (is.null(tags) || !length(tags)) {
    stop("`tags` must be \"all\" or at least one taxonomy label.", call. = FALSE)
  }
  tags <- as.character(tags)
  if (any(tolower(trimws(tags)) == "all")) return(labels)

  norm <- function(x) tolower(gsub("[ -]+", "_", trimws(x)))
  alias_map <- unlist(lapply(unname(taxonomy), function(pt) {
    stats::setNames(rep(pt$label, length(pt$aliases)), norm(pt$aliases))
  }))
  lookup <- c(stats::setNames(labels, norm(labels)), alias_map)

  resolved <- unname(lookup[norm(tags)])
  bad <- tags[is.na(resolved)]
  if (length(bad)) {
    stop("Unknown tag(s): ", paste(shQuote(bad), collapse = ", "),
         ". See places_taxonomy() for valid labels.", call. = FALSE)
  }
  unique(resolved)
}

# Translate one taxonomy entry into an ohsome filter expression, e.g.
# (type:node or type:way) and ((shop=convenience and not (disused=yes)))
build_ohsome_filter <- function(place_type) {
  quote_tok <- function(x) {
    if (grepl("^[A-Za-z0-9_]+$", x)) return(x)
    paste0("\"", gsub("([\"\\\\])", "\\\\\\1", x), "\"")
  }
  match_expr <- function(key, values) {
    values <- vapply(values, quote_tok, "")
    if (length(values) == 1L) {
      sprintf("%s=%s", quote_tok(key), values)
    } else {
      sprintf("%s in (%s)", quote_tok(key), paste(values, collapse = ", "))
    }
  }

  spec_filters <- vapply(place_type$tag_specs, function(s) {
    expr <- match_expr(s$key, s$value)
    if (length(s$exclude)) {
      excl <- vapply(s$exclude, function(e) match_expr(e$key, e$value), "")
      expr <- sprintf("(%s and not (%s))", expr, paste(excl, collapse = " or "))
    }
    expr
  }, "")

  types <- paste(sprintf("type:%s", place_type$geometry_types), collapse = " or ")
  sprintf("(%s) and (%s)", types, paste(spec_filters, collapse = " or "))
}
