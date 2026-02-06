
########UI PART##########

ui <- tagList(
  
  dashboardPage(
  
  skin = "purple",

  dashboardHeader(title = "scRiskDB 数据记录"),
  
 
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Repository", tabName = "Repository", icon = icon("database"),
                         menuSubItem("Explore SNVs", tabName = "Snp"),
                         menuSubItem("SNV to Risk Genes", tabName = "gene"),
                         menuSubItem("SNV to Risk CREs", tabName = "cre"),
                         menuSubItem("Explore Risk Cells", tabName = "cell")),
                         
                menuItem("Analysis Tools", icon = icon("chart-bar"),
                         menuSubItem("SNV to Function (Pathways)", tabName = "geneset"),
                         menuSubItem("Cell Scoring", tabName = "more_analysis")),
                         
                
                menuItem("Help", tabName = "help", icon = icon("question-circle")),
                menuItem("Contact", tabName = "contact", icon = icon("envelope")),
                menuItem("Submission", tabName = "submission", icon = icon("upload")))
  ),
  
  # Main body
  dashboardBody(
    

    #### CSS style & Script ####
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


        
 
        .help-box {
          background-color: #ffffff;      
          border-top: 3px solid #605ca8;  
          border-radius: 3px;           
          box-shadow: 0 1px 3px rgba(0,0,0,0.1); 
          padding: 30px;                 
          height: 75vh;                  
          overflow-y: auto;              
          margin-bottom: 20px;           
        }
        
        

        .well .nav-header {
          font-size: 12px;          
          font-weight: 700;         
          color: #888888;            
          text-transform: uppercase; 
          letter-spacing: 1px;      
          margin-top: 25px;         
          margin-bottom: 10px;      
          padding-left: 15px;      
          border-bottom: 1px solid #eee; 
          padding-bottom: 5px;      
        }
        
        .well .nav-header:first-child {
          margin-top: 0px;
        }
        
   
        .well {
          background-color: #ffffff;     
          border: none;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
   
        .nav-pills > li.active > a, 
        .nav-pills > li.active > a:focus, 
        .nav-pills > li.active > a:hover {
          background-color: #605ca8;      
          color: white;
        }
      ")),  
      
 
      tags$script(HTML("
        function copyToClipboard(text) {
          var textArea = document.createElement('textarea');
          textArea.value = text;
          textArea.style.position = 'fixed';
          textArea.style.left = '-9999px';
          textArea.style.top = '0';
          document.body.appendChild(textArea);
          textArea.focus();
          textArea.select();
          try {
            var successful = document.execCommand('copy');
            if(successful) {
              alert('Copied: ' + text);
            } else {
              console.error('Copy failed.');
            }
          } catch (err) {
            console.error('Unable to copy', err);
          }
          document.body.removeChild(textArea);
        }
      ")), 
      
    
      tags$script(HTML("
        function jumpToOpenTargets(geneSymbol) {
       
          const apiUrl = 'https://api.platform.opentargets.org/api/v4/graphql';
          
        
          const query = `
            query {
              search(queryString: \"${geneSymbol}\", entityNames: [\"target\"], page: {index: 0, size: 1}) {
                hits {
                  id
                  entity
                }
              }
            }
          `;
  
     
          fetch(apiUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ query: query })
          })
          .then(response => response.json())
          .then(data => {
        
            const hits = data.data.search.hits;
            
            if (hits.length > 0) {
              const targetId = hits[0].id;
           
              window.open('https://platform.opentargets.org/target/' + targetId + '/associations', '_blank');
            } else {
        
              window.open('https://platform.opentargets.org/search?q=' + geneSymbol, '_blank');
            }
          })
          .catch(error => {
            console.error('Error:', error);
            window.open('https://platform.opentargets.org/search?q=' + geneSymbol, '_blank');
          });
        }
      ")) 
      
    ), 
    
    tabItems(
      ############### HOME ######################
      tabItem(tabName = "home",
              h2("Welcome to scRiskDB"),
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
                    collapsible = TRUE, collapsed = TRUE, p("Version 2026.01")),
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
                  DTOutput("snp_result_table"))),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
            fluidRow(
                tags$div(style = "margin: 20px;", 
                         downloadButton(outputId = "snp_download_result", label = "Download Results", class = "custom-download-button"))
              ),
              tags$div(
                style = "position: absolute; bottom: 20px; right: 20px;",
                actionButton("return_home_snp", "HOME", class = "btn btn-primary")
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
              fluidRow(
                column(width = 12,
                       box(
                         title = "Risk Gene List", status = "primary", solidHeader = TRUE, width = NULL,
                         DTOutput(outputId = 'result_table')
                       )
                )
              ),
              
              
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
                #column(6, actionButton("display_plot", "Visualization", class = "btn btn-secondary"))
                column(6, uiOutput("plot_button_ui"))
              ),
              
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              #SweetAlert  Added
              useShinyjs(),  
              useSweetAlert(), 
              progressBar(id = "progress", value = 0, display_pct = TRUE), 
              
              
              
              tags$h4("Enrichment Analysis Results"),
              
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
                actionButton("return_home_geneset", "HOME", class = "btn btn-primary")
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
              tags$h2(class = "custom-title", "Analyze Your Single-Cell Data"),
              p("Here you can explore high-risk celltypes of your own single cell data"),
              tags$hr(style = "border:none; border-top:5px solid #caf2bb;"),
              
          
              fluidRow(
                column(width = 12,
                       box(
                         title = "Upload & Submit", 
                         status = "info", 
                         solidHeader = TRUE, 
                         width = NULL, 
                         
                  
                         fileInput("file1", "Choose your .h5ad File", accept = c(".h5ad")),
                         textInput("group_id", "Group By ID:", value = "celltype"),
                         p(style="font-size: 12px; color: grey; margin-top: -10px; margin-bottom: 15px;", 
                           "(Note: This column name must exist in adata.obs)"),
                         
              
                         selectInput("data_type", "Select Data Type:", 
                                     choices = c("Single Cell RNA-seq" = "rna", "Single Cell ATAC-seq" = "atac"),
                                     selected = "rna"),
                         
                   
                         conditionalPanel(
                           condition = "input.data_type != 'atac'", 
                           wellPanel(
                             style = "background: #f9f9f9; border-left: 3px solid #00c0ef;",
                             selectInput("disease_select", "Select Disease:", choices = disease_choices_df, selected = disease_choices_df[1]),
                             selectInput("tissue_select", "Select Tissue:", choices = tissue_choices_df, selected = tissue_choices_df[1])
                           )
                         ),
                         
                    
                         conditionalPanel(
                           condition = "input.data_type == 'atac'", 
                           wellPanel(
                             style = "background: #f9f9f9; border-left: 3px solid #00c0ef;",
                             
                             
                             fileInput("gs_upload", "Upload Calculated Risk Data"),
                    
                             div(style = "margin-top: -15px; margin-bottom: 10px;",
                                 actionLink("jump_to_help_atac", 
                                            label = "How to prepare ATAC risk data? (Step-by-step Tutorial)", 
                                            icon = icon("question-circle"),
                                            style = "color: #d9534f; font-weight: bold; text-decoration: underline; font-size: 13px;")
                             ),
                             tags$hr(style = "margin: 10px 0;"),
                             strong("Download Template"), br(),
                      
                             downloadButton("download_template", "Download File Template (.csv)", class = "btn btn-default btn-xs", style = "margin-bottom: 10px;"),
                             
                             
                      

                           )
                         ),
                         
                         tags$br(),
                         
                  
                         div(style = "text-align: center; margin-top: 15px;",
                             actionButton("submit_analysis", "Submit Analysis", 
                                  
                                          class = "btn btn-info", 
                                      
                                          style = "width: 200px; font-weight: bold; color: white; box-shadow: 0 2px 5px rgba(0,0,0,0.15);"
                             )
                         ),
                   
                         uiOutput("submission_feedback") 
                       )
                )
              ),
              
 
              fluidRow(
                column(width = 12,
                       box(
                         title = "Check Status & Download", 
                         status = "success", 
                         solidHeader = TRUE, 
                         width = NULL,
                         
                         p("Enter your Task ID below to retrieve results (even after closing the page)."),
                         
                   
                         div(class = "input-group", style = "width: 100%; max-width: 600px; margin: 0 auto;",
                             tags$span(class = "input-group-addon", icon("search")),
                             textInput("task_id_input", label = NULL, placeholder = "Paste your Task ID here (e.g., b119c6b6...)", width = "100%")
                         ),
                         tags$br(),
                         
                    
                         uiOutput("status_card_ui"),
                         
               
                         uiOutput("download_ui_polished")
                       )
                )
              )
      ),
      
      
      ################## END #######################
      
      

      ################HELP###########################
      tabItem(
        tabName = "help",
        
        fluidRow(
  
          column(width = 12, 
                 
                 tags$h2(class = "custom-title", "User Guide & Documentation"),
                 
                 navlistPanel(
                   id = "help_nav",
                   widths = c(2, 10), 
                   well = TRUE,       
                   
         
                   "Overview",
                   tabPanel("Introduction", icon = icon("info-circle"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_intro.md")
                            )
                   ),
                   tabPanel("Data Sources", icon = icon("database"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_data.md")
                            )
                   ),
                   
     
                   "Repository Features",
               
                   tabPanel("SNV to Risk Genes", icon = icon("dna"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_gene.md")
                            )
                   ),
                   tabPanel("SNV to Risk CREs", icon = icon("project-diagram"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_cre.md")
                            )
                   ),
                   tabPanel("Pathways & Critical Cells", icon = icon("chart-pie"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_plots.md")
                            )
                   ),
                   tabPanel("Focus on SNV", icon = icon("play"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_snv.md")
                            )
                   ),
                
                   "Analysis Tools",
                 
                   tabPanel("Analyze My Data (Scoring)", icon = icon("calculator"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_analysis.md")
                            )
                   ),
                   
                
                   "Support",
                 
                   tabPanel("FAQ & Troubleshooting", icon = icon("question-circle"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_faq.md")
                            )
                   ),
                   tabPanel("Citation", icon = icon("quote-left"),
                            div(class = "help-box", 
                                includeMarkdown("www/help_citation.md")
                            )
                   )
                 ) # end navlistPanel
          ) # end column
        ), # end fluidRow
        

        tags$div(
          style = "position: fixed; bottom: 20px; right: 30px; z-index: 100;",
          actionButton("return_home_help", "HOME", class = "btn btn-primary", 
                       style = "box-shadow: 0 2px 5px rgba(0,0,0,0.2);") 
        )
      ),
      ################# END ########################

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
),

tags$div(
  style = "
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: #f8f9fa;
      text-align: center;
      padding: 5px 0;
      font-size: 10px;
      color: #6c757d;
      z-index: 1000;
    ",
  HTML('
    scRiskDB 数据记录 |
    <a href="https://beian.miit.gov.cn/" target="_blank" style="color: #6c757d; text-decoration: none;">
      京ICP备2025121216号
    </a> |
    <img src="https://www.beian.gov.cn/img/ghs.png" style="vertical-align: middle; height: 8px; margin-left: 5px;"/>
    <a href="https://beian.mps.gov.cn/#/query/webSearch?code=11010802045893" 
       rel="noreferrer" target="_blank" style="color: #6c757d; text-decoration: none; margin-left: 3px;">
      京公网安备11010802045893号
    </a>
  ')
)
)