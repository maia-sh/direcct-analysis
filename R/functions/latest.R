# credit: Wil Doane
latest <- function (name, dir = here::here("data"), hash = FALSE) {
  pattern <- sprintf("^20[0-9\\-]+_%s", name)
  filenames <- list.files(dir, pattern, full.names = TRUE)
  result <- sort(filenames, decreasing = TRUE)[1]
  if (is.na(result))
    NULL
  else {
    if (hash)
      message(digest::sha1(readLines(result)))
    result
  }
}
