# Split string to vector of chars
to_chars <- function(string) {
  # Only intended for single string
  stopifnot(length(string) == 1)
  strsplit(string, NULL)[[1]]
}

# Keep i in bounds of a vec's subsettable range
normalize_index <- function(i, vec) {
  n <- length(vec)

  if (i >= 1 & i <= n) {
    return(i)
  }

  remainder <- i %% n
  if (remainder < 1) {
    return(n + remainder)
  }

  return(remainder)
}

# Caesar cypher encryption
#
# "ABCXYZ" -[encryption]-> "BCDYZA"
#
# Shift letters forward/backward in alphabet
# `encode()` takes a message and a numeric key for how far to shift
# `decode()` takes an encoded message and the key it was encoded
#            with and undoes the shift
#
# e.g. encode("EFG", 1) -> "FGH"  ;  decode("FGH", 1) -> "EFG"
# e.g. encode("EFG", -1) -> "DEF"
encode <- function(string, key) {
  if (is.na(string) | is.na(key)) {
    return(NA_character_)
  }

  alphabet <- c(
    letters, LETTERS, 1:9,
    " ", ".", "'", '"', "?", "&", "$", "#", "!", ","
  )

  # Break a string up into a vector of characters
  # (e.g. `"car"` would become `c("c", "a", "r")`)
  chars <- to_chars(string)
  shifted_chars <- vapply(chars, function(char) {
    # Return the char as is if not in alphabet
    if (!(char %in% alphabet)) {
      return(char)
    }

    # Find where char appears in alphabet
    i <- which(alphabet == char)

    # Encode the character by shifting i by the key
    new_i <- i + key

    # Make sure i is a legal position in alphabet
    # (ie wrap around if i is too small or too big)
    new_i <- normalize_index(new_i, alphabet)

    # Return the encoded character
    alphabet[new_i]
  }, character(1), USE.NAMES = FALSE)

  # Collapse characters back into a single string
  # (e.g. `c("c", "a", "r")` would become `"car"`)
  paste(shifted_chars, collapse = "")
}

# Caesar Cipher Decryption Function
decode <- function(string, key) {
  if (is.na(string) | is.na(key)) {
    return(NA_character_)
  }

  encode(string, -key)
}


msg <- "Howdy! Rocky Top will always be, home sweet home to me!"
key <- -34556789

(encrypted <- encode(msg, key))
decode(encrypted, key)
