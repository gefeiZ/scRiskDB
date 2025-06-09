library(reticulate)
reticulate::use_condaenv("shiny_py", required = TRUE)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(slickR)
library(shinyjs)
library(DT)
library(DBI)
library(shinythemes)
library(shinyWidgets)
organism = "org.Hs.eg.db"
library(organism, character.only = TRUE)
library(clusterProfiler)
#analysis NEED
library(uuid)  
library(future)
library(promises)
library(ggplot2)
library(dplyr)
library(progress)
options(shiny.maxRequestSize = 500*1024^2)

mysql_connector <- import("mysql.connector")

config <- fromJSON("db_config.json")

conn <- mysql_connector$connect(
  host = config$host,
  port = config$port,
  user = config$user,
  password = config$password,
  database = config$database,
  auth_plugin = "mysql_native_password"
)


cursor <- conn$cursor()
# Tab menu
cursor$execute("SELECT DISTINCT `Tissue name` FROM `Tissues`")
tissue_choices <- cursor$fetchall()
tissue_choices <- unlist(lapply(tissue_choices, function(x) x[[1]]))
tissue_choices_df <- data.frame(TissueName = tissue_choices)

cursor$execute("SELECT DISTINCT `Disease_id` FROM `Risk Gene`")
disease_choices <- cursor$fetchall()
disease_choices <- unlist(lapply(disease_choices, function(x) x[[1]]))
disease_choices_df <- data.frame(TissueName = disease_choices)


cursor$execute("SELECT DISTINCT `Tissue name` FROM `CellType_Peak`")
tissue_choices_cell <- cursor$fetchall()
tissue_choices_cell <- unlist(lapply(tissue_choices_cell, function(x) x[[1]]))
tissue_choices_cell_df <- data.frame(TissueName = tissue_choices_cell)

cursor$close()
conn$close()



