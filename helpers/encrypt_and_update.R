library(googlesheets4)

source("cypher.R")

gsheet_url <- "https://docs.google.com/spreadsheets/d/1XuC5QCF-ulwKjhnn3bq01EvfWYQWR7F15c5OVMXK4D4/edit?usp=sharing"
sheet_name <- "INESCAPtABLE"

manor <- googlesheets4::read_sheet(ss = gsheet_url, sheet = sheet_name)
manor$weight_kg <- as.numeric(manor$weight_kg)

encrypt_msgs <- function(manor) {
  manor$inspect <- vapply(1:nrow(manor), \(i) {
    msg <- manor$inspect_og[i]
    key <- manor$key_private[i]
    encode(string = msg, key = key)
  }, character(1))

  manor
}

set_dynamic_keys <- function(manor) {
  student_manor <- manor %>%
    select(
      room, item, description,
      weight_kg, length_m, emf_reading,
      inspect, key
    )

  wts <- manor$weight_kg
  wts[is.na(wts)] <- median(wts, na.rm = TRUE)
  manor$key_private[manor$item == "Fireplace"] <- round(mean(wts))

  manor$key_private[manor$item == "Wall Sconce"] <- sum(manor$emf_reading >= 5)
  manor$key_private[manor$item == "Vase"] <- round(100 * cor(manor$weight_kg, manor$length_m, use = "complete.obs"))
  manor$key_private[manor$item == "Reading Lamp"] <- round(100 * summary(lm(emf_reading ~ weight_kg + length_m + room, data = manor))$r.squared)
  manor$key_private[manor$item == "Window"] <- round(10000 * predict(lm(emf_reading ~ weight_kg + length_m, data = manor), newdata = data.frame(weight_kg = 0.5,length_m = 0.2)))

  manor$key_private[manor$item == "Coat Rack"] <- sum(is.na(student_manor))
  manor
}

# I'm dumb and lazy.. should work; who knows, run again if it doesn't
manor <- encrypt_msgs(manor)
manor <- set_dynamic_keys(manor)

manor <- encrypt_msgs(manor)
manor <- set_dynamic_keys(manor)


has_test_pieces <- !is.na(manor$inspect) & !is.na(manor$key_private)
test_i <- sample(which(has_test_pieces), size = 3)

decoded <- vapply(test_i, \(i) {
  msg <- manor$inspect[i]
  key <- manor$key_private[i]
  decode(msg, key)
}, character(1))
print(decoded)

response <- readline(prompt = "continue with overwrite? (Y): ")
if (response == "Y") {
  googlesheets4::write_sheet(manor, ss = gsheet_url, sheet = sheet_name)
}
