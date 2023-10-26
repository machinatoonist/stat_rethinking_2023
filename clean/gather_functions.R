# gather_functions <- function(folder_path) {
#     # Initialize an empty list to store the functions
#     func_list <- list()
#     
#     # Get the list of R script files in the folder
#     r_files <- list.files(folder_path, pattern = "\\.R$")
#     
#     # Loop through each R script file
#     for (file in r_files) {
#         file_path <- paste0(folder_path, "/", file)
#         
#         # Read the content of the file
#         file_content <- readLines(file_path)
#         
#         # Parse and evaluate the content to define the functions
#         eval(parse(text = file_content))
#     }
#     
#     # Get the list of all objects in the environment
#     all_objects <- ls()
#     
#     # Loop through each object and add it to the list if it's a function
#     for (obj in all_objects) {
#         if (is.function(get(obj))) {
#             func_list[[obj]] <- get(obj)
#         }
#     }
#     
#     return(func_list)
# }
# 
# # Example usage
# # Assuming you have R script files in a folder named "my_functions"
# result <- gather_functions("my_functions")
# 
# 
# # Function to gather functions from a single folder
# gather_functions <- function(folder_path) {
#     func_list <- list()
#     r_files <- list.files(folder_path, pattern = "\\.R$")
#     
#     for (file in r_files) {
#         file_path <- paste0(folder_path, "/", file)
#         file_content <- readLines(file_path)
#         eval(parse(text = file_content))
#     }
#     
#     all_objects <- ls()
#     for (obj in all_objects) {
#         if (is.function(get(obj))) {
#             func_list[[obj]] <- get(obj)
#         }
#     }
#     
#     return(func_list)
# }

gather_functions <- function(folder_path) {
    func_list <- list()
    r_files <- list.files(folder_path, pattern = "\\.R$")
    
    for (file in r_files) {
        file_path <- paste0(folder_path, "/", file)
        file_content <- readLines(file_path)
        
        # Combine lines into a single string and parse it
        combined_content <- paste(file_content, collapse = "\n")
        parsed_content <- parse(text = combined_content)
        
        # Loop through each parsed expression and evaluate only if it's a function definition
        for (expr in parsed_content) {
            if (is.function(eval(expr, envir = new.env(), enclos = parent.frame()))) {
                eval(expr)
            }
        }
    }
    
    all_objects <- ls()
    for (obj in all_objects) {
        if (is.function(get(obj))) {
            func_list[[obj]] <- get(obj)
        }
    }
    
    return(func_list)
}


# Function to gather functions from all folders in an R project
gather_all_functions <- function(root_path) {
    all_folders <- list.dirs(root_path, recursive = TRUE)
    
    # Initialize an empty list to store all functions
    all_functions <- list()
    
    # Loop through each folder and gather functions
    for (folder in all_folders) {
        folder_functions <- gather_functions(folder)
        all_functions <- c(all_functions, folder_functions)
    }
    
    return(all_functions)
}

# Example usage
# Assuming your R project root is at "my_project"
all_functions <- gather_all_functions(current_dir)

# Get current working directory
current_dir <- getwd()
root_path = current_dir

# Set working directory
setwd("/path/to/root")

find_git_root <- function(start_dir = getwd()) {
    while (!file.exists(paste0(start_dir, "/.git")) && !identical(start_dir, "/")) {
        start_dir <- dirname(start_dir)
    }
    if (identical(start_dir, "/")) {
        return(NULL)
    }
    return(start_dir)
}

# Example usage
root_dir <- find_git_root()

