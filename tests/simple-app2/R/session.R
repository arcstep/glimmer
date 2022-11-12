#
bucket_names <- function(session) {
  session$userData$`__VAR_BUCKET__`
}

#
bucket_get <- function(session, varName) {
  session$userData[[varname]]
}

#
bucket_set <- function(session, varName, value) {
  newBucket <- c(session$userData$`__VAR_BUCKET__`, varName) |> unique()
  session$userData$`__VAR_BUCKET__` <- newBucket
  session$userData[[varName]] <- value
}

#
bucket_remove <- function(session, varName) {
  newBucket <- session$userData$`__VAR_BUCKET__`[session$userData$`__VAR_BUCKET__` != varName]
  session$userData$`__VAR_BUCKET__` <- newBucket
  session$userData[[varName]] <- NULL
}