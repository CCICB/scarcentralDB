#' #' Retrieve Molecular Scars Associated with Tumour Development
#' #'
#' #' This function returns a list of molecular scars associated with various processes
#' #' involved in tumour development.
#' #'
#' #' @return A list of molecular scars and their associated processes.
#' #' @export
#' #'
#' #' @examples
#' #' scars <- scars_list()
#' #' print(scars)
#' scars_list <- function(){
#'   jsonlite::read_json(path = path_scars_json())
#' }
#'
#'
#' path_scarcentral <- function(type = c("excel", "json")){
#'   type <- rlang::arg_match(type)
#'
#'   if(type == "excel"){
#'     system.file("scarcentral.xlsx", package = "scarcentralDB")
#'   }
#'   else if(type == "json"){
#'     system.file("scarcentral.json", package = "scarcentralDB")
#'   }
#'   else
#'     stop(
#'       "forgot to implement for type: [", type, "]. Please make a new issue here:",
#'       " https://github.com/selkamand/scarcentralDB/issues"
#'     )
#' }
#'
#'
#' load_scars_excel <- function(){
#'  df <- openxlsx::read.xlsx(path_scarcentral(type = "excel"), sheet = "Scars")
#'
#'  # Drop rows where process is NA
#'  df <- df[!is.na(df$Process),]
#'
#'  # Add assertions
#'  assertions::assert_names_include(df, c("Process", "ScarName"))
#'  assertions::assert_no_duplicates(df, paste(df[["Process"]], df[["ScarName"]]))
#'
#'  return(df)
#' }
#'
#'
#' load_processes_excel <- function(){
#'   df <- openxlsx::read.xlsx(path_scarcentral(type = "excel"), sheet = "Processes")
#'
#'   # Drop rows where name is NA
#'   df <- df[!is.na(df$name),]
#'
#'   return(df)
#' }
#'
#' load_diseases_excel <- function(){
#'   df <- openxlsx::read.xlsx(path_scarcentral(type = "excel"), sheet = "Diseases")
#'   return(df)
#' }
#'
#'
#' scars_as_list <- function(){
#'   df_scars <- load_scars_excel()
#'   split(df_scars, df_scars$Process) |> lapply(\(x){split(x, x$ScarName)})
#' }
#'
#' scars_as_json <- function(){
#'   ls <- scars_as_list()
#'   json <- jsonlite::toJSON(ls)
#'   json <- jsonlite::prettify(json)
#'   return(json)
#' }
#'
#'
#' path_scars_json <- function(){
#'   paste0(system.file(package = "scarcentralDB"), "/scars.json")
#' }
#'
#' convert_scars_excel_to_json <- function(filepath=path_scars_json(), overwrite = FALSE){
#'   json <- scars_as_json()
#'
#'   if(!overwrite){
#'     assertions::assert_file_does_not_exist(
#'       filepath,
#'       msg = "File [{.path {filepath}}] already exists. To overwrite set {.arg overwrite = TRUE}"
#'     )
#'   }
#'
#'   cli::cli_alert_info("Writing json file to {.path {filepath}}")
#'   write(json, file = filepath)
#'
#'   return(invisible(NULL))
#' }
#'

