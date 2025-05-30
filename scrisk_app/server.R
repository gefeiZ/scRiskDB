
########SERVER PART##########
server <- function(input, output,session) {
  
  
  conn <- mysql_connector$connect(
    host = config$host,
    port = config$port,
    user = config$user,
    password = config$password,
    database = config$database,
    auth_plugin = "mysql_native_password"
  )
  
  

  cursor <- conn$cursor()
  
  
  updateSelectInput(session, 'gene_tissue_input', choices = tissue_choices_df)
  updateSelectInput(session, 'gene_disease_input', choices = disease_choices_df)
  updateSelectInput(session, 'cre_tissue_input', choices = tissue_choices_df)
  updateSelectInput(session, 'cre_disease_input', choices = disease_choices_df)
  updateSelectInput(session, 'go_tissue_input', choices = tissue_choices_df)
  updateSelectInput(session, 'go_disease_input', choices = disease_choices_df)
  
  
  output$slick_slider <- renderSlickR({
    imgs <- c("www/web_homepage.png")
    
    slickR(imgs)
  })
  
  
  ### submit file
  save_path <- "/data/scriskb/user_uploads/"
  if (!dir.exists(save_path)) {
    dir.create(save_path)
  }
  
  observeEvent(input$file_sub, {
    req(input$file_sub)
    
    # 获取上传文件的信息（包括路径）
    uploaded_files <- input$file_sub
    
    for (i in 1:nrow(uploaded_files)) {
      file.copy(uploaded_files$datapath[i],
                file.path(save_path, uploaded_files$name[i]))
    }
    
    output$file_info <- renderPrint({
      cat("Uploaded files:\n")
      print(uploaded_files$name)
    })
  })
  
  
  
  ## GO bottom
  
  observeEvent(input$go_snp, {
    updateTabItems(session, "tabs", "Snp")
  })
  
  observeEvent(input$go_gene, {
    updateTabItems(session, "tabs", "gene")
  })
  
  observeEvent(input$go_cre, {
    updateTabItems(session, "tabs", "cre")
  })
  
  observeEvent(input$go_geneset, {
    updateTabItems(session, "tabs", "geneset")
  })
  
  observeEvent(input$go_cells, {
    updateTabItems(session, "tabs", "cell")
  })
  
  observeEvent(input$go_more_analysis, {
    updateTabItems(session, "tabs", "more_analysis")  
  })
  
  observeEvent(input$return_home, {
    updateTabItems(session, "tabs", "home")
  })
  
  
  
  ####################### SNV SERVER###########################
  observeEvent(input$submit, {
    selected_disease <- input$disease
    selected_tissue <- input$tissue
    rsid <- input$rsid
    
    #query <- sprintf("SELECT * FROM snp_data WHERE `Disease_id` = '%s' AND `Tissue` = '%s' AND `rsID` = '%s'",
    #                 selected_disease, selected_tissue, rsid)
    query <- sprintf("
    SELECT DISTINCT
  '%s' AS rsID,
  rg.`Tissue name` AS Tissue,
  rg.`Disease name` AS Disease_name,
  rg.`Disease_id` AS Disease_id,
  rc.`Risk CREs` AS CRE,
  rg.`Risk Genes` AS Gene,
  rg.`ZSTAT` AS Weight
FROM
  `Risk Gene` rg
LEFT JOIN `Risk CRE` rc
  ON rg.`Tissue name` = rc.`Tissue name`
  AND rg.`Disease_id` = rc.`Disease_id`
  AND rg.`Risk Genes` = rc.`Risk Genes`
WHERE
  rg.`Tissue name` = '%s'
  AND rg.`Disease_id` = '%s'
  AND (
    rg.`SNPs` LIKE '%%%%%s%%%%'
    OR rc.`SNPs` LIKE '%%%%%s%%%%'
  )",                     rsid, selected_tissue, selected_disease, rsid, rsid
    )
    
    
    
    cursor <- conn$cursor()
    cursor$execute(query)
    snp_filtered_data <- cursor$fetchall()
    
    
    if (length(snp_filtered_data) == 0) {
      output$snp_result_table <- renderTable({
        data.frame(Message = "No results found")
      })
    } else {
      snp_filtered_data_df <- data.frame(matrix(unlist(snp_filtered_data), ncol = length(snp_filtered_data[[1]]), byrow = TRUE))
      colnames(snp_filtered_data_df) <- c("rsID", "Tissue", "Disease name", "Disease ID", "CRE", "GENE", "Weight")  
      
      
      output$snp_result_table <- renderTable({
        snp_filtered_data_df
      },width = "auto")
    }
  })
  
  
  output$snp_download_result <- downloadHandler(
    filename = function() {
      paste("risk_snps_result_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_disease <- input$disease
      selected_tissue <- input$tissue
      rsid <- input$rsid
      
      
      query <- sprintf("SELECT * FROM snp_data WHERE `Disease_id` = '%s' AND `Tissue` = '%s' AND `rsID` = '%s'",
                       selected_disease, selected_tissue, rsid)
      
      
      cursor <- conn$cursor()
      cursor$execute(query)
      snp_filtered_data <- cursor$fetchall()
      snp_filtered_data_df <- data.frame(matrix(unlist(snp_filtered_data), ncol = length(snp_filtered_data[[1]]), byrow = TRUE))
      if (length(snp_filtered_data) == 0) {
        write.csv(data.frame(Message = "No results found"), file, row.names = FALSE)
      } else {
        snp_filtered_data_df <- as.data.frame(snp_filtered_data)
        colnames(snp_filtered_data_df) <- c("rsID", "Tissue", "Disease name", "Disease ID", "CRE", "GENE", "Weight") 
        write.csv(snp_filtered_data_df, file, row.names = FALSE)
      }
    }
  )
  
  onStop(function() {
    cursor$close()
    conn$close()
  })
  
  #######################END###########################  
  
  ######################## GENE SERVER ###############################
  output$result_table <- renderDT({
    selected_tissue <- input$gene_tissue_input
    selected_disease <- input$gene_disease_input
    
    
    query <- sprintf("SELECT * FROM `Risk Gene` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                     selected_tissue, selected_disease)
    
    
    cursor <- conn$cursor()
    cursor$execute(query)  
    
    
    gene_result <- cursor$fetchall()
    
    
    if (length(gene_result) == 0) {
      return(data.frame(Message = "No results found"))
    }
    
    
    if (length(gene_result) > 0 && length(gene_result[[1]]) > 0) {
      filtered_data <- data.frame(matrix(unlist(gene_result), ncol = length(gene_result[[1]]), byrow = TRUE))
      colnames(filtered_data) <- c("Tissue name","Disease ID","Disease name", "Risk Genes", "Pval", "ZSTAT", "SNPs")  
      return(filtered_data)
    } else {
      return(data.frame(Message = "Data format error"))
    }
  })
  
  
  
  
  output$download_result <- downloadHandler(
    filename = function() {
      paste("risk_genes_result_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_tissue <- input$gene_tissue_input
      selected_disease <- input$gene_disease_input
      
      
      query <- sprintf("SELECT * FROM `Risk Gene` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                       selected_tissue, selected_disease)
      
      cursor <- conn$cursor()
      cursor$execute(query)  
      gene_result <- cursor$fetchall()
      filtered_data <- data.frame(matrix(unlist(gene_result), ncol = length(gene_result[[1]]), byrow = TRUE))
      if (length(filtered_data) == 0) {
        write.csv(data.frame(Message = "No results found"), file, row.names = FALSE)
      } else {
        colnames(filtered_data) <- c("Tissue name","Disease ID", "Disease name", "Risk Genes", "Pval", "ZSTAT", "SNPs")
        write.csv(filtered_data, file, row.names = FALSE)
      }
    }
  )
  
  
  onStop(function() {
    cursor$close()
    conn$close()
  })
  
  ######################END###########################    
  
  
  observeEvent(input$compare_dev_stage, {
    selected_tissue_raw <- input$gene_tissue_input
    selected_disease <- input$gene_disease_input
    
    # 提取组织基础名（去掉括号及其中内容）
    tissue_base <- sub("\\(.*\\)", "", selected_tissue_raw)
    tissue_base <- trimws(tissue_base)  # 去掉左右空格
    
    tissue_adult <- paste0(tissue_base, "(Adult)")
    tissue_fetal <- paste0(tissue_base, "(Fetal)")
    
    # Adult 查询
    query_adult_gene <- sprintf("SELECT `Risk Genes` FROM `Risk Gene` 
                          WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                                tissue_adult, selected_disease)
    cursor <- conn$cursor()
    cursor$execute(query_adult_gene)
    adult_result_gene <- cursor$fetchall()
    adult_genes <- unique(unlist(lapply(adult_result_gene, `[[`, 1)))
    
    # Fetal 查询
    query_fetal_cre <- sprintf("SELECT `Risk Genes` FROM `Risk Gene` 
                          WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                               tissue_fetal, selected_disease)
    cursor$execute(query_fetal_cre)
    fetal_result_gene <- cursor$fetchall()
    fetal_genes <- unique(unlist(lapply(fetal_result_gene, `[[`, 1)))
    
    
    # 取差异
    adult_specific_gene <- setdiff(adult_genes, fetal_genes)
    fetal_specific_gene <- setdiff(fetal_genes, adult_genes)
    
    # 输出表格
    output$adult_specific_table <- renderDT({
      if (length(adult_specific_gene) == 0) {
        data.frame(Message = "No adult-specific genes found")
      } else {
        data.frame(`Adult-Specific Risk Genes` = adult_specific_gene)
      }
    })
    
    output$fetal_specific_table <- renderDT({
      if (length(fetal_specific_gene) == 0) {
        data.frame(Message = "No fetal-specific genes found")
      } else {
        data.frame(`Fetal-Specific Risk Genes` = fetal_specific_gene)
      }
    })
  })
  
  output$download_dev_comparison_gene <- downloadHandler(
    filename = function() {
      paste0("Dev_Comparison_GENE_", input$gene_tissue_input, "_", input$gene_disease_input, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      selected_tissue_raw <- input$gene_tissue_input
      selected_disease <- input$gene_disease_input
      
      tissue_base <- sub("\\(.*\\)", "", selected_tissue_raw)
      tissue_base <- trimws(tissue_base)
      tissue_adult <- paste0(tissue_base, "(Adult)")
      tissue_fetal <- paste0(tissue_base, "(Fetal)")
      
      # 查询 Adult CRE
      query_adult_gene <- sprintf("SELECT `Risk Genes` FROM `Risk Gene` 
                            WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                                  tissue_adult, selected_disease)
      cursor <- conn$cursor()
      cursor$execute(query_adult_gene)
      adult_result_gene <- cursor$fetchall()
      adult_genes <- unique(unlist(lapply(adult_result_gene, `[[`, 1)))
      
      
      query_fetal_gene <- sprintf("SELECT `Risk Genes` FROM `Risk Gene` 
                            WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                                  tissue_fetal, selected_disease)
      cursor$execute(query_fetal_gene)
      fetal_result_gene <- cursor$fetchall()
      fetal_genes <- unique(unlist(lapply(fetal_result_gene, `[[`, 1)))
      
      if (length(adult_result_gene) == 0 || length(fetal_result_gene) == 0) {
        write.csv(data.frame(Message = "Insufficient data for comparison."), file, row.names = FALSE)
      } else {
        adult_specific_gene <- setdiff(adult_genes, fetal_genes)
        fetal_specific_gene <- setdiff(fetal_genes, adult_genes)
        
        combined_df <- data.frame(
          `Adult-Specific Risk GENEs` = c(adult_specific_gene, rep(NA, max(0, length(fetal_specific_gene) - length(adult_specific_gene)))),
          `Fetal-Specific Risk GENEs` = c(fetal_specific_gene, rep(NA, max(0, length(adult_specific_gene) - length(fetal_specific_gene))))
        )
        
        write.csv(combined_df, file, row.names = FALSE)
      }
    }
  ) 
  
  ####################### CRE SERVER ###############################
  output$cre_result_table <- renderDT({
    selected_tissue <- input$cre_tissue_input
    selected_disease <- input$cre_disease_input
    
   
    query <- sprintf("SELECT * FROM `Risk CRE` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                     selected_tissue, selected_disease)
    
   
    cursor <- conn$cursor()
    cursor$execute(query)  
    

    cre_result <- cursor$fetchall()
    
    
   
    if (length(cre_result) == 0) {
      return(data.frame(Message = "No results found"))
    }
    

    if (length(cre_result) > 0 && length(cre_result[[1]]) > 0) {
      cre_filtered_data <- data.frame(matrix(unlist(cre_result), ncol = length(cre_result[[1]]), byrow = TRUE))
      colnames(cre_filtered_data) <- c("Tissue name","Disease ID","Disease name", "Risk CREs", "Risk Genes", "SNPs", "ZSTAT")  
      return(cre_filtered_data)
    } else {
      return(data.frame(Message = "Data format error"))
    }
  })
  
  
  

  output$cre_download_result <- downloadHandler(
    filename = function() {
      paste("risk_cres_result_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_tissue <- input$cre_tissue_input
      selected_disease <- input$cre_disease_input
      
   
      query <- sprintf("SELECT * FROM `Risk CRE` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                       selected_tissue, selected_disease)
      
      cursor <- conn$cursor()
      cursor$execute(query)  
      cre_result <- cursor$fetchall()
      cre_filtered_data <- data.frame(matrix(unlist(cre_result), ncol = length(cre_result[[1]]), byrow = TRUE))
      if (length(cre_filtered_data) == 0) {
        write.csv(data.frame(Message = "No results found"), file, row.names = FALSE)
      } else {
        colnames(cre_filtered_data) <- c("Tissue name","Disease ID","Disease name", "Risk CREs", "Risk Genes", "SNPs", "ZSTAT") 
        write.csv(cre_filtered_data, file, row.names = FALSE)
      }
    }
  )
  
 
  onStop(function() {
    cursor$close()
    conn$close()
  })
  
  ######################END###########################  
  rv <- reactiveValues()
  observeEvent(input$compare_dev_stage_cre, {
    selected_tissue_raw <- input$cre_tissue_input
    selected_disease <- input$cre_disease_input
    
    tissue_base <- sub("\\(.*\\)", "", selected_tissue_raw)
    tissue_base <- trimws(tissue_base)
    tissue_adult <- paste0(tissue_base, "(Adult)")
    tissue_fetal <- paste0(tissue_base, "(Fetal)")
    
    cursor <- conn$cursor()
    
   
    query_adult_cre <- sprintf(
      "SELECT `Risk Genes`, `Risk CREs` FROM `Risk CRE` 
     WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
      tissue_adult, selected_disease
    )
    cursor$execute(query_adult_cre)
    adult_result_cre <- cursor$fetchall()
    
    
    query_fetal_cre <- sprintf(
      "SELECT `Risk Genes`, `Risk CREs` FROM `Risk CRE` 
     WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
      tissue_fetal, selected_disease
    )
    cursor$execute(query_fetal_cre)
    fetal_result_cre <- cursor$fetchall()
    
    
    if (length(adult_result_cre) == 0 || length(fetal_result_cre) == 0) {
      output$adult_specific_cre_table <- renderDT({
        data.frame(Message = "Insufficient data for adult/fetal stage.")
      })
      output$fetal_specific_cre_table <- renderDT({
        data.frame(Message = "Insufficient data for adult/fetal stage.")
      })
      return()
    }
    
 
    extract_cre_gene_pairs <- function(result_list) {
      do.call(rbind, lapply(result_list, function(row) {
        genes <- unlist(strsplit(row[[1]], "\\s+"))
        cres <- unlist(strsplit(row[[2]], "\\s+"))
       
        len <- min(length(genes), length(cres))
        data.frame(Gene = genes[1:len], CRE = cres[1:len], stringsAsFactors = FALSE)
      }))
    }
    
    
    
    adult_df <- extract_cre_gene_pairs(adult_result_cre)
    fetal_df <- extract_cre_gene_pairs(fetal_result_cre)
    
   
    adult_specific_cre <- setdiff(adult_df$CRE, fetal_df$CRE)
    fetal_specific_cre <- setdiff(fetal_df$CRE, adult_df$CRE)
    
   
    adult_specific_df <- adult_df[adult_df$CRE %in% adult_specific_cre, ]
    fetal_specific_df <- fetal_df[fetal_df$CRE %in% fetal_specific_cre, ]
    
    output$adult_specific_cre_table <- renderDT({
      if (nrow(adult_specific_df) == 0) {
        data.frame(Message = "No adult-specific CREs found.")
      } else {
        adult_specific_df
      }
    })
    
    output$fetal_specific_cre_table <- renderDT({
      if (nrow(fetal_specific_df) == 0) {
        data.frame(Message = "No fetal-specific CREs found.")
      } else {
        fetal_specific_df
      }
    })
    
    
    rv$adult_specific_cre <- adult_specific_df
    rv$fetal_specific_cre <- fetal_specific_df
  })
  
  
  
  
  output$download_dev_comparison_cre <- downloadHandler(
    filename = function() {
      paste0("Dev_Comparison_CRE_", input$cre_tissue_input, "_", input$cre_disease_input, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      adult_specific_df <- rv$adult_specific_cre
      fetal_specific_df <- rv$fetal_specific_cre
      
      if (is.null(adult_specific_df) || is.null(fetal_specific_df)) {
        write.csv(data.frame(Message = "No data available for download."), file, row.names = FALSE)
      } else {
        max_len <- max(nrow(adult_specific_df), nrow(fetal_specific_df))
        

        adult_df_padded <- rbind(
          adult_specific_df,
          data.frame(Gene = rep(NA, max_len - nrow(adult_specific_df)),
                     CRE = rep(NA, max_len - nrow(adult_specific_df)))
        )
        
        fetal_df_padded <- rbind(
          fetal_specific_df,
          data.frame(Gene = rep(NA, max_len - nrow(fetal_specific_df)),
                     CRE = rep(NA, max_len - nrow(fetal_specific_df)))
        )
        

        output_df <- data.frame(
          `Adult Gene` = adult_df_padded$Gene,
          `Adult CRE` = adult_df_padded$CRE,
          `Fetal Gene` = fetal_df_padded$Gene,
          `Fetal CRE` = fetal_df_padded$CRE,
          stringsAsFactors = FALSE
        )
        
        write.csv(output_df, file, row.names = FALSE)
      }
    }
  )
  
  
  ####################### CELL SERVER ###############
  
  observeEvent(input$plot_button, {
    output$heatmap <- renderPlot({
      req(input$cell_tissue_input, input$cell_disease_input)
      
      query <- paste0("SELECT * FROM `Risk cell` WHERE `Tissue name` = '", input$cell_tissue_input, 
                      "' AND Disease_id IN ('", paste(input$cell_disease_input, collapse = "','"), "')")
      
      cursor <- conn$cursor()
      cursor$execute(query)
      cell_data <- cursor$fetchall()
      
      if (length(cell_data) == 0) {
        return(
          ggplot() +
            theme_void() +
            geom_text(aes(0, 0, label = "No data found for selected tissue and disease.
                          Please add other Diseases!"),
                      size = 6, color = "grey") +
            xlim(-1, 1) + ylim(-1, 1)
        )
      }
      
      cell_data <- do.call(rbind, lapply(cell_data, function(x) {
        data.frame(
          Tissue_name = x[[1]],
          Disease_id = x[[2]],
          Cell_type = x[[3]],
          assoc_mcp = as.numeric(x[[4]]),
          stringsAsFactors = FALSE
        )
      }))
      
      # 加上这个判断
      if (nrow(cell_data) == 0) {
        return(
          ggplot() +
            theme_void() +
            geom_text(aes(0, 0, label = "No data found for selected tissue and disease.
                          Please add other Diseases!"),
                      size = 6, color = "grey") +
            xlim(-1, 1) + ylim(-1, 1)
        )
      }
      
      cell_data$assoc_mcp <- as.numeric(cell_data$assoc_mcp)
      
      
      #cell_data$assoc_mcp[is.na(cell_data$assoc_mcp)] <- 1
      
      
      cell_data$logPval <- -log10(cell_data$assoc_mcp)
      
      
      cell_data$mark <- ifelse(cell_data$assoc_mcp < 0.05, "*", "")
      
      
      ggplot(cell_data, aes(x = Cell_type, y = Disease_id, fill = logPval)) +
        geom_tile(color = "gray", width = 1, height = 1) + 
        scale_fill_gradient2(low = "white", mid = "orange", high = "red", midpoint = 2, name = "-log10 P") +  
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
              axis.title.y = element_blank(),  
              axis.title.x = element_blank(),
              panel.grid = element_blank()) +
        labs(y = "", x = "GROUP") +
        coord_fixed() +  
        
        
        geom_tile(data = filter(cell_data, assoc_mcp < 0.05), color = "black", size = 1, width = 1, height = 1) +
        
        
        geom_text(aes(label = mark), color = "black", size = 6)
    })
  })
  
  
  output$celltype_selector_single <- renderUI({
    req(input$cell_tissue_input)
    
    query <- paste0("SELECT DISTINCT cell_type FROM CellType_Peak WHERE `Tissue name` = '", input$cell_tissue_input, "'")
    cursor <- conn$cursor()
    cursor$execute(query)
    cell_types <- unlist(lapply(cursor$fetchall(), function(x) x[[1]]))
    
    selectInput("celltype_input_single", "Select Cell Type:",
                choices = cell_types, selected = "")
  })
  
  
  
  cre_data_reactive <- reactive({
    
    if (is.null(input$cell_tissue_input) ||
        is.null(input$celltype_input_single) ||
        is.null(input$cell_disease_input_single) ||
        input$celltype_input_single == "" ||
        input$cell_disease_input_single == ""){
      return(NULL)
    }
    req(input$cell_tissue_input, input$celltype_input_single, input$cell_disease_input_single)
    
    # Step 1: 获取组织+细胞类型 对应的 CRE
    query_cre <- paste0("SELECT CRE FROM CellType_Peak WHERE `Tissue name` = '", input$cell_tissue_input, 
                        "' AND cell_type = '", input$celltype_input_single, "'")
    cursor <- conn$cursor()
    cursor$execute(query_cre)
    cre_list <- unlist(lapply(cursor$fetchall(), function(x) x[[1]]))
    
    
    if (length(cre_list) == 0) {
      return(data.frame(Message = "The CellType Specific Peak is not Trait-relevant CRE,Try Another."))
    }
    
    
    
    # Step 2: 在 Risk CRE 表中查找这些 CRE 对应的 disease-specific 行
    cre_in_clause <- paste0("'", paste(cre_list, collapse = "','"), "'")
    query_risk <- paste0(
      "SELECT `Risk CREs`, `Risk Genes`, `SNPs`,`Disease_id` FROM `Risk CRE` ",
      "WHERE `Tissue name` = '", input$cell_tissue_input, "' ",
      "AND `Disease_id` = '", input$cell_disease_input_single, "' ",
      "AND `Risk CREs` IN (", cre_in_clause, ")"
    )
    cursor$execute(query_risk)
    results <- cursor$fetchall()
    
    
    if (length(results) == 0) {
      return(data.frame(Message = "The CellType Specific Peak is not Trait-relevant CRE,Try Another."))
    } else {
      output$no_data_warning <- renderUI({ NULL }) 
      df <- as.data.frame(do.call(rbind, results))
      colnames(df) <- c("Risk CRE", "Gene", "SNPs", "Disease") 
      return(df)
    }
    
  })
  
  
  output$cre_gene_snp_table <- DT::renderDataTable({
    df <- cre_data_reactive()
    req(df)
    DT::datatable(df, selection = "multiple", options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$download_selected <- downloadHandler(
    filename = function() {
      
      fname <- paste0("Cell2CRE_gene_snp_", input$cell_tissue_input, "_", 
                      input$celltype_input_single, "_", input$cell_disease_input_single, ".csv")
      gsub("[^A-Za-z0-9_.-]", "_", fname)
    },
    content = function(file) {
      df <- cre_data_reactive()
      req(df)
      
     
      df_clean <- as.data.frame(lapply(df, function(x) if (is.list(x)) unlist(x) else x),
                                stringsAsFactors = FALSE)
      
      write.csv(df_clean, file, row.names = FALSE)
    }
  )
  
  ######################END###########################
  
  #######################PATHWAY SERVER###########################
  observeEvent(input$display_results, {
    output$enrichment_output <- renderUI({
      DTOutput("enrichment_results")  
    })
  })
  
  observeEvent(input$display_plot, {
    output$enrichment_output <- renderUI({
      plotOutput("enrichment_plot")  
    })
  })
  
  observeEvent(c(input$go_tissue_input, input$go_disease_input), {
    updateProgressBar(session = session, id = "progress", value = 0)
  })
  
  
  enrichment_result <- reactive({
    req(input$go_tissue_input, input$go_disease_input)  
    
    
    selected_tissue <- input$go_tissue_input
    selected_disease <- input$go_disease_input
    
    query <- sprintf("SELECT `Risk Genes` FROM `Risk Gene` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
                     selected_tissue, selected_disease)
    
    cursor <- conn$cursor()
    cursor$execute(query)
    gene_result <- cursor$fetchall()
    
    updateProgressBar(session = session, id = "progress", value = 30)  
    
    if (length(gene_result) == 0) {
      print("No genes found in database.")
      updateProgressBar(session = session, id = "progress", value = 100)  
      return(NULL)
    }
    
    
    filtered_data <- data.frame(matrix(unlist(gene_result), ncol = length(gene_result[[1]]), byrow = TRUE))
    colnames(filtered_data) <- c("Risk Genes")
    gene_list <- unique(unlist(strsplit(as.character(filtered_data$`Risk Genes`), split = ",")))
    gene_list <- gene_list[!is.na(gene_list) & gene_list != ""]
    gene_list <- as.character(gene_list)
    gene_list<-na.omit(gene_list)
    gene_list = sort(gene_list, decreasing = TRUE)
    #updateProgressBar(session = session, id = "progress", value = 50)  
    
    if (length(gene_list) == 0) {
      print("Gene list is empty after processing.")
      updateProgressBar(session = session, id = "progress", value = 100)  
      return(NULL)
    }
    
    
    enrichment_result <- tryCatch({
      enrichGO(gene = gene_list, OrgDb = org.Hs.eg.db, keyType = "SYMBOL", ont = "BP",pvalueCutoff = 0.05, qvalueCutoff = 0.10,readable = T)
    }, error = function(e) {
      print(paste("enrichGO failed:", e$message))
      return(NULL)
    })
    
    updateProgressBar(session = session, id = "progress", value = 80)  
    
    if (is.null(enrichment_result) || is.null(enrichment_result@result) || nrow(enrichment_result@result) == 0) {
      print("No enrichment results found.")
      updateProgressBar(session = session, id = "progress", value = 100)  
      return(NULL)
    }
    
    
    updateProgressBar(session = session, id = "progress", value = 100) 
    return(enrichment_result)
  })
  
  
  output$enrichment_results <- renderDT({
    res <- enrichment_result()
    if (is.null(res) || is.null(res@result) || nrow(res@result) == 0) {
      return(data.frame(Message = "No significant enrichment found"))
    }
    
    return(res@result)  
  })
  
  output$enrichment_plot <- renderPlot({
    res <- enrichment_result()
    if (is.null(res) || is.null(res@result) || nrow(res@result) == 0) {
      output$enrichment_output <- renderUI({
        tags$p("No significant results available for visualization.")
      })
      return(NULL)
    }
    #emapplot(res)
    goplot(res, showCategory = 10)
  })
  
  # Downloads
  output$download_enrichment <- downloadHandler(
    filename = function() {
      paste0("GO_Enrichment_", input$go_tissue_input, "_", input$go_disease_input, ".csv")
    },
    content = function(file) {
      res <- enrichment_result()
      if (is.null(res)) {
        write.csv(data.frame(Message = "No significant enrichment found"), file, row.names = FALSE)
      } else {
        write.csv(res@result, file, row.names = FALSE)
      }
    }
  )
  
  ######################END###########################
  
  
  #######################SC_VAR SERVER###########################
  
  plan(multisession)
  
  

  task_list <- reactiveVal(list())
  current_task_id <- reactiveVal(NULL) 
  observeEvent(input$submit_analysis, {
    
    req(input$file1)  
    
 
    task_id <- UUIDgenerate()
    current_task_id(task_id)
    

    task_dir <- file.path("/data/scriskb/tasks", task_id)
    dir.create(task_dir, recursive = TRUE)
  
    file_path <- file.path(task_dir, input$file1$name)
    file.copy(input$file1$datapath, file_path)
    gs_path <- file.path(task_dir, "risk_file.txt")
    
    
    if (input$data_type == "rna") {
 
      query <- sprintf(
        "SELECT `Risk Genes`, `ZSTAT` FROM `Risk Gene` WHERE `Tissue name` = '%s' AND `Disease_id` = '%s'",
        selected_tissue, selected_disease
      )
      cursor <- conn$cursor()
      cursor$execute(query)
      gs_data <- cursor$fetchall()
      
   
      columns <- c("Genes", "ZSTAT")  
      gs_data <- as.data.frame(do.call(rbind, gs_data), stringsAsFactors = FALSE)
      colnames(gs_data) <- columns
      write.table(gs_data, gs_path, sep = "\t", row.names = FALSE, quote = FALSE)
    } else if (input$data_type == "atac") {
      req(input$gs_upload)  
      file.copy(input$gs_upload$datapath, gs_path)
    }
    
    
    task_info <- list(
      id = task_id,
      file_path = file_path,
      group_by=input$group_id,
      data_type = input$data_type,
      tissue = input$analysis_tissue_input,
      disease = input$analysis_disease_input,
      email = input$email,
      status = "Scoring"
    )
    
    
    tasks <- task_list()
    tasks[[task_id]] <- task_info
    task_list(tasks)
    group_by_quoted <- shQuote(input$group_id)
  
    future({
      system2("/home/shiny/miniconda3/envs/shiny_py/bin/python",
              args = c("scripts/process_data.py",
                       "--file", file_path,
                       "--risk_data", gs_path,
                       "--group_by",group_by_quoted,
                       "--output", file.path(task_dir)),
              wait = TRUE)
    }) %...>% (function(result) {
    
      tasks <- task_list()
      result_files <- list.files(task_dir, pattern = "result.txt$", full.names = TRUE)
      if (length(result_files) > 0 && all(file.exists(result_files))) {
        tasks[[task_id]]$status <- "Completed"
      } else {
        tasks[[task_id]]$status <- "Failed"
      }
      
      task_list(tasks)
    }) %...!% (function(e) {
    
      tasks <- task_list()
      tasks[[task_id]]$status <- "Failed"
      task_list(tasks)
    })
    
    output$task_id_output <- renderText(sprintf("Job ID: %s", task_id))
  })
  
  # **修改任务状态查询**
  output$run_status <- renderText({
    current_id <- current_task_id()
    if (is.null(current_id) || current_id == "") {
      current_id <- "" 
    }
    
    task_id <- ifelse(nchar(input$task_id_input) > 0, input$task_id_input, current_id)
    
    if (task_id == "") {
      return("No Task ID found, please enter a valid Task ID.")
    }
    
   
    tasks <- task_list()
    if (!is.null(tasks[[task_id]])) {
      return(sprintf("Task %s status: %s", task_id, tasks[[task_id]]$status))
    } else {
      return("Task not found.")
    }
  })
  
  

  output$download_ui <- renderUI({
    task_id <- input$task_id_input
    tasks <- task_list()
    
    if (!is.null(tasks[[task_id]]) && tasks[[task_id]]$status == "Completed") {
      downloadButton("download_task", "Download Results", class = "btn btn-success", style = "width: 100%")
    } else {
      disabled_download <- tags$button("Results (Waiting...)", class = "btn btn-secondary", disabled = TRUE, style = "width: 100%")
      return(disabled_download)
    }
  })
  

  output$download_task <- downloadHandler(
    filename = function() {
      paste0(input$task_id_input, "_results.tar.gz")
    },
    content = function(file) {
      task_id <- input$task_id_input
      task_path <- file.path("tasks", task_id)
      
  
      tasks <- task_list()
      if (is.null(tasks[[task_id]]) || tasks[[task_id]]$status != "Completed") {
        stop("Task is not completed yet. Please wait.")
      }
      
      if (dir.exists(task_path)) {
        tar_file <- file.path(tempdir(), paste0(task_id, "_results.tar.gz"))
        system(sprintf("tar -czf %s -C %s .", tar_file, task_path))  
        file.copy(tar_file, file)
      } else {
        stop("Task results not found!")
      }
    }
  )
  
  ######################END###########################
  
  onStop(function() {
    cursor$close()
    conn$close()
    
  })
}
