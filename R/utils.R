rotl_is_flag <- function(x) {
  identical(length(x), 1L) && 
  rlang::is_bare_logical(x) &&
    !is.na(x)
    
}

rotl_is_string <- function(x) {
  identical(length(x), 1L) && 
  rlang::is_bare_character(x) &&
    !is.na(x)    
}

check_factory <- function(f, x, arg_name, msg) {
  if (f(x)) {
    return(TRUE)    
  }
  
  stop(
    paste0(
      "Argument ",
      sQuote(arg_name),
      msg
    ),
    call. = FALSE
  )
}

check_is_flag <- function(x) {
  arg_name <- deparse(substitute(x))
  check_factory(
    rotl_is_flag,
    x,
    arg_name,
    " is not a flag (a length one logical vector)."
  )
}

check_is_string <- function(x) {
  arg_name <- deparse(substitute(x))
  check_factory(
    rotl_is_string,
    x,
    arg_name,
    " is not a string (a length one character vector)."
  )
}
