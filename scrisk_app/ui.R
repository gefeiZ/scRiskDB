
########UI PART##########
ui <- dashboardPage(
  
  skin = "purple",
  # Header
  dashboardHeader(title = "scRiskDB_Demo"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Repository", tabName = "Repository", icon = icon("database"),
                         menuSubItem("Explore SNVs", tabName = "Snp"),
                         menuSubItem("SNV to Risk Genes", tabName = "gene"),
                         menuSubItem("SNV to Risk CREs", tabName = "cre"),
                         menuSubItem("SNV to Function", tabName = "geneset"),
                         menuSubItem("Explore Risk Cells", tabName = "cell")),
                menuItem("More Analysis", tabName = "more_analysis", icon = icon("chart-bar")),
                menuItem("Help", tabName = "help", icon = icon("question-circle")),
                menuItem("Contact", tabName = "contact", icon = icon("envelope")),
                menuItem("Submission", tabName = "submission", icon = icon("upload"))
    )
  ),
  
  # Main body
  dashboardBody(
    
    #### CSS style ####
    tags$head(
      tags$style(HTML("
      .custom-title {
        font-size: 24px; 
        color: #2C3E50; 
        background-color: #ebf7fa; 
        padding: 10px; 
        text-align: center;
        border-radius: 5px; 
        box-shadow: 0 2px 5px rgba(0,0,0,0.1); 
        margin-bottom: 20px;
      }
      .custom-download-button:hover {
      background-color: #a9f5cc;  
    }
    "))
    ),
    
    
    
    tabItems(
      ############### HOME ######################
      tabItem(tabName = "home",
              h2("Welcome to scRiskDB_Demo"),
              p("A platform for displaying Human Disease-Related Tissue-Specific Resources."),
              p("scRiskDB provide the annotated,prioritized tissue-,development-specific data"),
              tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              
              #resize huamnbody png
              tags$head(
                tags$style(HTML(".slick-slide img {
                                width: auto !important;
                                height: 450px !important;}
                                 .image-container {
                                  margin-bottom: 40px; /* Add some space below the image */}"))),
              
              fluidRow(
                column(12, div(class = "image-container", slickR::slickROutput("slick_slider")))),
              
              tags$br(),
              tags$br(),
              tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              
              fluidRow(
                column(width = 6, 
                       box(title = "SNV to Risk Genes", status = "info", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                           width = 12, p("Exploring the single nucleotide variations (SNVs) annotation"),
                           actionLink("go_snp", "Go >>>"),
                           p("Exploring tissue specific disease risk genes based on SC-VAR:sce-MAGMA approach"),
                           actionLink("go_gene", "Go >>>"))),
                
                column(width = 6, box(title = "SNV to Risk CREs", status = "info", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                                      width = 12, p("Exploring the single nucleotide variations (SNVs) annotation"),
                                      actionLink("go_snp", "Go >>>"),
                                      p("Exploring tissue specific disease risk cis-regulatory elements based on SC-VAR:sce-MAGMA approach"),
                                      actionLink("go_cre", "Go >>>"))),
                
                column(width = 6, box(title = "Gene to Function", status = "info", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                                      width = 12, p("Exploring disease risk geneset related pathways"),
                                      actionLink("go_geneset", "Go >>>"))),
                
                column(width = 6, box(title = "Risk to Relevant Cells", status = "info", solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                                      width = 12, p("Exploring disease associated high-risk celltypes based on SC-VAR:sce-DRS approach"),
                                      actionLink("go_cells", "Go >>>")))
              ),
              
              tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              fluidRow(
                box(title = "Latest database update", status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, collapsed = TRUE, p("Version 2024.10")),
                box(title = "Reference", status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, collapsed = TRUE, p("Gefei Zhao, Binbin Lai
                                                            
                                                            SC-VAR: a computational tool for interpreting polygenic disease risks using single-cell epigenomic data, Briefings in Bioinformatics, 
                                                            
                                                            Volume 26, Issue 2, March 2025, bbaf123, https://doi.org/10.1093/bib/bbaf123"))
              )
      ),
      ################## Repository ################
      tabItem(tabName = "Repository", h2("Repository content goes here.")),
      ################## END #######################
      
      ######SEARCH SNPS#########
      tabItem(tabName = "Snp", 
              tags$h2(class = "custom-title", "SNV to Disease"),
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              p("    Here you can explore tissue specific disease single nucleotide variation (SNV)"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("disease", "Select Disease:", choices = disease_choices_df,selected = disease_choices_df[1]),
                  selectInput("tissue", "Select Tissue:", choices = tissue_choices_df,selected = tissue_choices_df[1]),
                  textInput("rsid", "Enter rsID:",placeholder = "e.g., rs185081027"),
                  actionButton("submit", "Search")
                ),
                mainPanel(
                  tableOutput("snp_result_table"))),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton(outputId = "snp_download_result", label = "Download Results", class = "custom-download-button"))
              ),
              tags$div(
                style = "position: absolute; bottom: 20px; right: 20px;",
                actionButton("return_home", "HOME", class = "btn btn-primary")
              )
      ),
      
      
      
      ################## SUB GENES ################
      tabItem(tabName = "gene", 
              #h2("RISK GENES"),
              tags$h2(class = "custom-title", "RISK GENES"),
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              p("    Here you can explore tissue specific disease risk genes"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
              # add  choices
              fluidRow(
                column(6,
                       selectInput(inputId = 'gene_tissue_input', label = 'Select Tissue:', 
                                   choices = tissue_choices_df, selected = tissue_choices_df[1])),
                column(6,
                       selectInput(inputId = 'gene_disease_input', label = 'Select Disease:', 
                                   choices = disease_choices_df, selected = disease_choices_df[1]))
              ),
              # results table
              fluidRow(tags$div(style = "margin: 20px;",  # 
                                DTOutput(outputId = 'result_table'))),
              
              
              br(),br(),
              
              #Download results
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton(outputId = "download_result", label = "Download Results", class = "custom-download-button"))
              ),
              
              
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              tags$hr(style = "border:none; border-top:5px dashed #caf2bb;"),
              fluidRow(
                column(12, align = "left",
                       actionButton("compare_dev_stage", "Compare Adult vs Fetal",class="btn btn-warning"))
              ),
              
              #tags$hr(style = "border:none; border-top:2px dashed #caf2bb;"),
              br(),br(),
              fluidRow(
                column(6,
                       h5("Adult-Specific Risk Genes"),
                       br(),br(),
                       DTOutput("adult_specific_table")),
                
                column(6,
                       h5("Fetal-Specific Risk Genes"),
                       br(),br(),
                       DTOutput("fetal_specific_table"))
              ),
              br(),br(),
              
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton("download_dev_comparison_gene", "Download Comparison Result (Genes)", class = "custom-download-button"))
              )
      ),
      ################## END #######################
      
      ################## RISK CREs ################
      tabItem(tabName = "cre", 
              
              tags$h2(class = "custom-title", "RISK CREs"),
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              p("    Here you can explore tissue specific disease risk CREs"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
              # add  choices
              fluidRow(
                column(6,
                       selectInput(inputId = 'cre_tissue_input', label = 'Select Tissue:', 
                                   choices = tissue_choices_df, selected = tissue_choices_df[1])),
                column(6,
                       selectInput(inputId = 'cre_disease_input', label = 'Select Disease:', 
                                   choices = disease_choices_df, selected = disease_choices_df[1]))
              ),
              # results table
              fluidRow(tags$div(style = "margin: 20px;",  # 
                                DTOutput(outputId = 'cre_result_table'))),
              
              br(),br(),
              
              #Download results
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton(outputId = "cre_download_result", label = "Download Results", class = "custom-download-button"))
              ),
              
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              tags$hr(style = "border:none; border-top:5px dashed #caf2bb;"),
              fluidRow(
                column(12, align = "left",
                       actionButton("compare_dev_stage_cre", "Compare Adult vs Fetal",class="btn btn-warning"))
              ),
              
              
              br(),br(),
              fluidRow(
                column(6,
                       h5("Adult-Specific Risk CREs"),
                       br(),br(),
                       DTOutput("adult_specific_cre_table")),
                
                column(6,
                       h5("Fetal-Specific Risk CREs"),
                       br(),br(),
                       DTOutput("fetal_specific_cre_table"))
              ),
              
              br(),br(),
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton("download_dev_comparison_cre", "Download Comparison Result (CRE)", class = "custom-download-button"))
              )
      ),
      ################## END #######################
      
      
      ################## Risk PATHWAYS ################
      tabItem(tabName = "geneset", 
              
              tags$h2(class = "custom-title", "Risk Pathways"),
              #tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              p("    Here you can explore tissue specific disease high-risk Pathways"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
              # add  choices
              fluidRow(
                column(6,
                       selectInput(inputId = 'go_tissue_input', label = 'Select Tissue:', 
                                   choices = tissue_choices_df, selected = tissue_choices_df[1])),
                column(6,
                       selectInput(inputId = 'go_disease_input', label = 'Select Disease:', 
                                   choices = disease_choices_df, selected = disease_choices_df[1]))
              ),
              
              
              
              fluidRow(
                column(6, actionButton("display_results", "Display Table", class = "btn btn-info")),
                column(6, actionButton("display_plot", "Visualization", class = "btn btn-secondary"))
              ),
              
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              #SweetAlert  Added
              useShinyjs(),  
              useSweetAlert(), 
              progressBar(id = "progress", value = 0, display_pct = TRUE), 
              
              
              
              tags$h3("Enrichment Analysis Results"),
              
              fluidRow(tags$div(style = "margin: 20px;",  
                                uiOutput("enrichment_output"))), 
              
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
              fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton(outputId = "download_enrichment", label = "Download Results", class = "custom-download-button"))
              ),
              
              #bottom back home 
              tags$div(
                style = "position: absolute; bottom: 20px; right: 20px;",
                actionButton("return_home", "HOME", class = "btn btn-primary")
              )
      ),
      ################## END #######################
      
      
      ################## Risk CELLS ################
      tabItem(tabName = "cell", 
              tags$h2(class = "custom-title", "RISK CELLS"),
              p("Here you can explore tissue specific disease risk Cell Types"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
      
              fluidRow(
                column(
                  width = 12,
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("cell_tissue_input", "Select Tissue:", 
                                  choices = tissue_choices_cell_df, selected = tissue_choices_cell_df[1]),
                      selectInput("cell_disease_input", "Select Diseases:", 
                                  choices = disease_choices_df, selected = c(disease_choices_df[1], disease_choices_df[1]), multiple = TRUE),
                      actionButton("plot_button", "Generate Plot")
                    ),
                    mainPanel(
                      plotOutput("heatmap")
                    )
                  )
                )
              ),
              
              tags$hr(style = "border:none; border-top:4px dashed #caf2bb;"),
              
          
              fluidRow(
                column(
                  width = 12,
                  box(
                    title = "Cell Type-specific Information",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    p("Select a cell type and disease to view cell type specific Trait-associated CREs, genes, and SNPs."),
                    fluidRow(  
                      column(6, uiOutput("celltype_selector_single")),
                      column(6, selectInput("cell_disease_input_single", "Select a Disease:",  
                                            choices = disease_choices_df, multiple = FALSE, selected = ""))
                    ),
                    br(), br(),
                    downloadButton("download_selected", "Download Selected Rows"),
                    br(), br(),
                    DT::dataTableOutput("cre_gene_snp_table")
                  )
                )
              ),
              
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
            
              fluidRow(
                column(
                  width = 12,
                  h4("Additional Risk Cell Analysis"),
                  tags$a("Explore disease-critical celltypes of your own data >>>", href = "#", style = "color: steelblue;")
                )
              )
      ),
      
      ################## END #######################
      
      
      ################## SUB MORE Analysis ####################
      tabItem(tabName = "more_analysis", 
              tags$h2(class = "custom-title", "Analysis Your Single-Cell Analysis"),
              p("Here you can explore high-risk celltypes of your own single cell data"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
              wellPanel(
                fileInput("file1", "Choose your .h5ad File", accept = c(".h5ad")),
                textInput("group_id", "Group By ID:", value = "celltype (Should in adata.obs)"),
                
                fluidRow(
                  column(6, 
                         selectInput("data_type", "Select Data Type:", 
                                     choices = c("Single Cell RNA-seq" = "rna", "Single Cell ATAC-seq" = "atac"),
                                     selected = "rna"))
                )
              ),
              
              # 使用 conditionalPanel 控制选择框的显示
              conditionalPanel(
                condition = "input.data_type != 'atac'", # 当选择 'RNA-seq' 时显示
                wellPanel(
                  selectInput("disease_select", "Select Disease:", choices = c("Disease1", "Disease2")),
                  selectInput("tissue_select", "Select Tissue:", choices = c("Tissue1", "Tissue2"))
                )
              ),
              
              conditionalPanel(
                condition = "input.data_type == 'atac'", # 当选择 'ATAC-seq' 时显示
                wellPanel(
                  fileInput("gs_upload", "Upload ATAC Risk Data (CRE ZSTAT)")
                )
              ),
              
              verbatimTextOutput("risk_data_status"),
              
              wellPanel(
                actionButton("submit_analysis", "Submit Analysis", 
                             class = "btn btn-info", style = "width: 100%"),
                
                tags$hr(),
                
                
                strong("Your Task ID:"),
                verbatimTextOutput("task_id_output"),
                
                textInput("task_id_input", "Enter Task ID to Check Status:", value = ""),
                
                strong("Status:"),
                verbatimTextOutput("run_status"),
                
                #downloadButton("download_task", "Download Results", class = "btn btn-success", style = "width: 100%")
                uiOutput("download_ui")
                
              )
      ),
      
      
      ################## END #######################
      
      
      
      ################HELP###########################
      tabItem(
        tabName = "help",
        box(
          title = "Help & Documentation", status = "warning", solidHeader = TRUE, width = 12,
          withMathJax(),
          div(
            includeMarkdown("www/help.md"),
            style = "max-height: 70vh; overflow-y: auto; padding-right: 10px;"
          )
        ),
        tags$div(
          style = "position: absolute; bottom: 20px; right: 20px;",
          actionButton("return_home", "HOME", class = "btn btn-primary")
        )
      ),
      ################# END ########################    
      
      
      ################## Contact ##################
      tabItem(tabName = "contact", 
              h2("Contact Us"),
              tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              fluidRow(
                column(8,
                       tags$p("If you have any questions or suggestions, please don't hesitate to reach out to us. Your input is valuable to us."),
                       tags$hr(),
                       tags$p(tags$b(icon("user"), " Contact:"), "Gefei Zhao & Binbin Lai"),
                       tags$p(tags$b(icon("envelope"), " Email:"), "laib@bjmu.edu.cn"),
                       tags$p(tags$b(icon("map-marker"), " Address:"), "Peking University Health science center, Beijing, China"),
                       tags$p(tags$b(icon("flask"), " Our lab:"), 
                              tags$a(href="https://laiblab.github.io", "https://laiblab.github.io", target="_blank"))
                )
              )),
      ################# END ########################
      
      ################## Submission ################
      tabItem(tabName = "submission", 
              h2("Submit tissue-specific risk list"),
              tags$hr(style = "border:none; border-top:5px solid #5E81AC;"),
              fileInput("file_sub", "Choose a file to upload",
                        multiple = TRUE,
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt")),
              verbatimTextOutput("file_info")
      )
      ################## END ####################### 
      
    ) #dashboard
  ) # Main body
) #ui