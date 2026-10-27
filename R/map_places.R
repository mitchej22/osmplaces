#' Map place counts
#'
#' Draws the counts returned by [get_places()] as a choropleth, either as an
#' interactive [leaflet][leaflet::leaflet] map (default) or a static
#' [ggplot2][ggplot2::ggplot] map.
#'
#' The interactive map has a panel of buttons, one per tag, that recolours the
#' map and legend without reloading it. Hovering a geography shows the count
#' for the selected tag; clicking it lists every non-zero tag for that
#' geography. Colours are quantile classes of the non-zero counts, with zeros
#' in light grey, so a few very busy areas do not wash out the rest of the map.
#' The geometry is drawn once, so maps with many tags stay small.
#'
#' Static maps with more than one tag are faceted and share one colour scale.
#'
#' @param x An `sf` object from `get_places(..., geometry = TRUE)`.
#' @param tag Character vector of count columns (taxonomy labels) or
#'   [calc_access()] score columns (such as `"access_grocery"`) to map. `NULL`
#'   (default) maps every tag in `x`, or only the access scores if `x` came from
#'   [calc_access()].
#' @param interactive `TRUE` (default) for a leaflet map, `FALSE` for a static
#'   ggplot2 map.
#' @param palette A ColorBrewer palette name, such as `"YlOrRd"` or `"Blues"`.
#' @param basemap Background tiles for the interactive map: `"esri"` (default,
#'   Esri light gray), `"osm"` (OpenStreetMap), `"none"` (no tiles, for
#'   networks that block tile servers), or your own tile URL template such as
#'   `"https://tiles.example.com/{z}/{x}/{y}.png"`. The built-in choices need
#'   no API key, but tile providers set their own usage terms, and some
#'   (including OpenStreetMap's) refuse requests from embedded viewers that send
#'   no referrer. CARTO basemaps now require a key, so they are not built in.
#'
#' @return A `leaflet` htmlwidget or a `ggplot` object.
#'
#' @examples
#' \dontrun{
#' res <- get_places(
#'   state = "OH", county = "Cuyahoga", year = 2019,
#'   tags = c("grocery", "pharmacy"), geography = "tract", geometry = TRUE
#' )
#' map_places(res)                       # interactive, buttons switch tags
#' map_places(res, basemap = "none")     # no background tiles
#' map_places(res, "pharmacy", interactive = FALSE)
#'
#' htmlwidgets::saveWidget(map_places(res), "places.html")
#' }
#' @export
map_places <- function(x, tag = NULL, interactive = TRUE, palette = "YlOrRd",
                       basemap = "esri") {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object. Re-run get_places() with `geometry = TRUE`.",
         call. = FALSE)
  }
  tags <- place_count_columns(x, tag)
  date <- attr(x, "date")
  access <- intersect(tags, attr(x, "access_columns"))
  notes <- access_notes(x, tags)
  if (isTRUE(interactive)) {
    map_places_leaflet(x, tags, palette, basemap, date, access, notes)
  } else {
    map_places_ggplot(x, tags, palette, date, access, notes)
  }
}

# One line per mapped column describing how a calc_access() score was made.
access_notes <- function(x, tags) {
  specs <- attr(x, "access")
  vapply(tags, function(tg) specs[[tg]]$label %||% "", character(1), USE.NAMES = FALSE)
}

utils::globalVariables("count")

place_count_columns <- function(x, tag = NULL) {
  counts <- attr(x, "tags")
  if (is.null(counts)) {
    df <- sf::st_drop_geometry(x)
    counts <- names(df)[vapply(df, is.numeric, logical(1))]
  }
  access <- intersect(attr(x, "access_columns"), names(x))
  available <- intersect(unique(c(access, counts)), names(x))

  if (is.null(tag)) {
    # After calc_access() the scores are what you want to see, so map those.
    default <- if (length(access)) access else available
    if (!length(default)) stop("No count columns found in `x`.", call. = FALSE)
    return(default)
  }

  tag <- unique(as.character(tag))
  unknown <- setdiff(tag, available)
  if (length(unknown)) {
    stop("Not a count column in `x`: ", paste(unknown, collapse = ", "),
         ". Available: ", paste(available, collapse = ", "), call. = FALSE)
  }
  tag
}

require_pkg <- function(pkg, fallback) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required ", fallback,
         ". Install it with install.packages(\"", pkg, "\").", call. = FALSE)
  }
}

# Interactive map ----------------------------------------------------------

BASEMAPS <- list(
  esri = list(
    url = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = "Tiles &copy; Esri"
  ),
  osm = list(
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = "&copy; OpenStreetMap contributors"
  )
)

add_basemap <- function(m, basemap) {
  if (is.null(basemap) || identical(basemap, "none")) return(m)
  msg <- "`basemap` must be \"esri\", \"osm\", \"none\", or a tile URL template."
  if (!is.character(basemap) || length(basemap) != 1L || is.na(basemap)) {
    stop(msg, call. = FALSE)
  }
  spec <- BASEMAPS[[basemap]]
  if (is.null(spec)) {
    if (!grepl("{z}", basemap, fixed = TRUE)) stop(msg, call. = FALSE)
    spec <- list(url = basemap, attribution = "")
  }
  leaflet::addTiles(m, urlTemplate = spec$url, attribution = spec$attribution,
                    options = leaflet::tileOptions(maxZoom = 19))
}

# Zeros and missing values get their own colours; non-zero counts are split
# into up to `n` quantile classes. Returns one fill colour per value plus the
# legend colours and labels.
count_classes <- function(vals, palette = "YlOrRd", n = 5L) {
  zero_col <- "#eeeeee"
  na_col <- "#bdbdbd"
  fill <- rep(na_col, length(vals))
  colors <- character(0)
  labels <- character(0)

  is_zero <- !is.na(vals) & vals == 0
  if (any(is_zero)) {
    fill[is_zero] <- zero_col
    colors <- zero_col
    labels <- "0"
  }

  is_pos <- !is.na(vals) & vals > 0
  if (any(is_pos)) {
    upper <- unique(as.numeric(
      stats::quantile(vals[is_pos], seq_len(n) / n, type = 1, names = FALSE)
    ))
    k <- length(upper)
    pos_at <- if (k == 1L) 0.7 else seq(0.2, 1, length.out = k)
    cols <- leaflet::colorNumeric(palette, domain = c(0, 1))(pos_at)
    cls <- cut(vals[is_pos], breaks = c(0, upper), labels = FALSE)
    fill[is_pos] <- cols[cls]
    colors <- c(colors, cols)
    labels <- c(labels, class_labels(upper, whole = all(is.na(vals) | vals == round(vals))))
  }

  if (anyNA(vals)) {
    colors <- c(colors, na_col)
    labels <- c(labels, "No data")
  }
  list(fill = fill, colors = colors, labels = labels)
}

# Legend labels for the classes whose upper bounds are `upper`. Counts get
# whole-number ranges ("1-4", "5-9"); fractional values such as access scores
# get "<= u1", "u1-u2", ... with just enough digits to tell the bounds apart.
class_labels <- function(upper, whole) {
  k <- length(upper)
  if (whole) {
    fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
    lower <- c(1, utils::head(upper, -1) + 1)
    return(ifelse(lower >= upper, fmt_n(upper),
                  paste0(fmt_n(lower), "\u2013", fmt_n(upper))))
  }
  shown <- upper
  for (digits in 2:8) {
    shown <- signif(upper, digits)
    if (!anyDuplicated(shown)) break
  }
  txt <- vapply(shown, format, character(1), big.mark = ",", trim = TRUE,
                scientific = FALSE, drop0trailing = TRUE)
  if (k == 1L) return(txt)
  c(paste0("\u2264 ", txt[[1]]), paste0(txt[-k], "\u2013", txt[-1]))
}

# Whole numbers keep every digit; fractional values (access scores) get three
# significant digits.
fmt_values <- function(v) {
  vapply(unname(v), function(z) {
    if (is.na(z)) return("n/a")
    if (z != round(z)) z <- signif(z, 3)
    format(z, big.mark = ",", trim = TRUE, scientific = FALSE)
  }, character(1))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Click popups: GEOID plus every column with a non-zero value, largest first.
places_popups <- function(ids, counts, empty = "no places") {
  m <- as.matrix(counts)
  nm <- gsub("_", " ", colnames(m), fixed = TRUE)
  vapply(seq_along(ids), function(i) {
    v <- m[i, ]
    keep <- which(!is.na(v) & v > 0)
    keep <- keep[order(-v[keep])]
    rows <- if (length(keep)) {
      paste0("<tr><td>", html_escape(nm[keep]),
             "</td><td style='text-align:right;padding-left:14px'><b>",
             fmt_values(v[keep]),
             "</b></td></tr>", collapse = "")
    } else {
      paste0("<tr><td><i>", html_escape(empty), "</i></td></tr>")
    }
    paste0("<b>GEOID ", html_escape(ids[[i]]), "</b><table>", rows, "</table>")
  }, character(1))
}

# Runs in the browser after the map renders. Draws the tag buttons and legend
# and recolours the single polygon layer when a button is clicked.
PLACES_MAP_JS <- r"--(
function(el, x, d) {
  var map = this;
  var A = function(v) { return v === null || v === undefined ? [] : (Array.isArray(v) ? v : [v]); };
  var esc = function(s) {
    return String(s).replace(/[&<>"']/g, function(c) {
      return {"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;"}[c];
    });
  };
  var fmt = function(n) {
    if (n === null || n === undefined) return "n/a";
    n = Number(n);
    return Number.isInteger(n) ? n.toLocaleString() : n.toLocaleString(undefined, {maximumSignificantDigits: 3});
  };

  var ids = A(d.ids), tags = A(d.display), totals = A(d.totals);
  var isCount = A(d.isCount), notes = A(d.notes);
  var counts = A(d.counts).map(function(v) { return A(v); });
  var fill = A(d.fill).map(function(v) { return A(v); });
  var lcol = A(d.legendColors).map(function(v) { return A(v); });
  var llab = A(d.legendLabels).map(function(v) { return A(v); });
  var layers = ids.map(function(id) { return map.layerManager.getLayer("shape", id); });
  var current = 0;

  layers.forEach(function(layer, i) {
    if (!layer) return;
    layer.bindTooltip(function() {
      return "GEOID " + esc(ids[i]) + "<br>" + esc(tags[current]) + ": <b>" + fmt(counts[current][i]) + "</b>";
    }, {sticky: true});
  });

  var legendDiv = null;
  function renderLegend() {
    var html = "<div style='font-weight:bold;margin-bottom:4px'>" + esc(tags[current]) + "</div>";
    if (notes[current]) {
      html += "<div style='color:#666;margin-bottom:4px;max-width:210px;white-space:normal'>" + esc(notes[current]) + "</div>";
    }
    lcol[current].forEach(function(c, j) {
      html += "<div><span style='display:inline-block;width:18px;height:12px;margin-right:6px;" +
              "vertical-align:middle;border:1px solid #999;background:" + c + "'></span>" +
              esc(llab[current][j]) + "</div>";
    });
    legendDiv.innerHTML = html;
  }

  function select(k) {
    current = k;
    layers.forEach(function(layer, i) {
      if (layer) layer.setStyle({fillColor: fill[k][i]});
    });
    renderLegend();
  }

  var panel = L.control({position: "topright"});
  panel.onAdd = function() {
    var div = L.DomUtil.create("div", "osmplaces-panel");
    div.style.cssText = "background:#fff;padding:8px 10px;overflow-y:auto;min-width:170px;" +
      "font:12px/1.6 sans-serif;box-shadow:0 1px 5px rgba(0,0,0,.4);border-radius:4px;";
    var setHeight = function() { div.style.maxHeight = Math.max(120, map.getSize().y - 160) + "px"; };
    setHeight();
    map.on("resize", setHeight);

    var html = "<div style='font-weight:bold;margin-bottom:2px'>" + esc(d.title || "Place type") + "</div>";
    if (d.date) html += "<div style='color:#666;margin-bottom:4px'>OSM as of " + esc(d.date) + "</div>";
    tags.forEach(function(t, k) {
      html += "<label style='display:block;cursor:pointer;white-space:nowrap'>" +
        "<input type='radio' name='osmplaces-tag' value='" + k + "'" + (k === 0 ? " checked" : "") +
        " style='margin:0 6px 0 0;vertical-align:middle'>" + esc(t) +
        (isCount[k] === false ? "" : " <span style='color:#888'>(" + fmt(totals[k]) + ")</span>") +
        "</label>";
    });
    div.innerHTML = html;
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    div.addEventListener("change", function(e) {
      if (e.target && e.target.name === "osmplaces-tag") select(parseInt(e.target.value, 10));
    });
    return div;
  };
  panel.addTo(map);

  var legend = L.control({position: "bottomright"});
  legend.onAdd = function() {
    legendDiv = L.DomUtil.create("div", "osmplaces-legend");
    legendDiv.style.cssText = "background:rgba(255,255,255,.9);padding:6px 10px;" +
      "font:12px/1.5 sans-serif;box-shadow:0 1px 5px rgba(0,0,0,.4);border-radius:4px;";
    return legendDiv;
  };
  legend.addTo(map);
  renderLegend();
}
)--"

map_places_leaflet <- function(x, tags, palette, basemap, date,
                               access = character(0), notes = rep("", length(tags))) {
  require_pkg("leaflet", "for interactive maps (or use `interactive = FALSE`)")
  require_pkg("htmlwidgets", "for interactive maps (or use `interactive = FALSE`)")

  x <- sf::st_transform(x, 4326)
  ids <- if ("geoid" %in% names(x)) as.character(x$geoid) else as.character(seq_len(nrow(x)))
  counts <- sf::st_drop_geometry(x)[tags]
  classes <- lapply(counts, count_classes, palette = palette)
  only_access <- length(access) > 0L && all(tags %in% access)

  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  m <- add_basemap(m, basemap)
  m <- leaflet::addPolygons(
    m, data = x, layerId = ids,
    fillColor = classes[[1]]$fill, fillOpacity = 0.75,
    color = "#666666", weight = 0.5, opacity = 0.8,
    popup = places_popups(ids, counts,
                          empty = if (only_access) "no access" else "no places"),
    highlightOptions = leaflet::highlightOptions(weight = 2.5, color = "#222222",
                                                 bringToFront = TRUE)
  )

  payload <- list(
    ids = I(ids),
    display = I(gsub("_", " ", tags, fixed = TRUE)),
    totals = I(unname(vapply(counts, sum, numeric(1), na.rm = TRUE))),
    isCount = I(!tags %in% access),
    notes = I(notes),
    title = if (only_access) "Accessibility" else "Place type",
    counts = unname(lapply(counts, I)),
    fill = unname(lapply(classes, function(cl) I(cl$fill))),
    legendColors = unname(lapply(classes, function(cl) I(cl$colors))),
    legendLabels = unname(lapply(classes, function(cl) I(cl$labels))),
    date = if (is.null(date)) "" else as.character(date)
  )
  htmlwidgets::onRender(m, PLACES_MAP_JS, data = payload)
}

# Static map ---------------------------------------------------------------

map_places_ggplot <- function(x, tags, palette, date,
                              access = character(0), notes = rep("", length(tags))) {
  require_pkg("ggplot2", "for static maps (or use `interactive = TRUE`)")

  long <- do.call(rbind, lapply(tags, function(tg) {
    sf::st_sf(geoid = x$geoid, tag = tg, count = x[[tg]],
              geometry = sf::st_geometry(x))
  }))
  long$tag <- factor(long$tag, levels = tags)

  only_access <- length(access) > 0L && all(tags %in% access)
  subtitle <- c(if (!is.null(date)) paste("OSM places as of", date),
                if (length(tags) == 1L && nzchar(notes)) notes)

  p <- ggplot2::ggplot(long) +
    ggplot2::geom_sf(ggplot2::aes(fill = count), color = NA) +
    ggplot2::scale_fill_distiller(palette = palette, direction = 1,
                                  na.value = "grey80",
                                  name = if (only_access) "Access" else "Count") +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = if (length(subtitle)) paste(subtitle, collapse = "\n"))
  if (length(tags) > 1L) p <- p + ggplot2::facet_wrap("tag")
  if (length(tags) == 1L) p <- p + ggplot2::labs(title = tags)
  p
}
