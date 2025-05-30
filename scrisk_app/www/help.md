# ❓ Help & User Guide

Welcome to **scRiskdb**. This guide explains how to use the app, what data is included, and how to cite.

------------------------------------------------------------------------

### 🔍 How to Use scRiskdb

-   Annotation,Pathway analysis and Cell types risk association are processed via `SC-VAR`, `clusterProfiler`,`scDRS`

------------------------------------------------------------------------

### SNV Annotation

> #### **`Focus on SNV`**

1.  Select a tissue and disease from the tab.(e.g `Adrenal_Glands(Fetal)`,`I9_ANGINA`)
2.  Enter an `rsID` (e.g., `rs1000000851`) and click **Search**.
3.  View the results, including CRE annotations, gene annotations, Disease Information, and SNP weight (Z_score) under the specific selection.
4.  Click **Download Results** to downloads.

### SNV to Gene

1.  Select a tissue and disease from the drop-down menus.(e.g `Adipose_Omentum(Adult)`,`AB1_BACTINF_NOS`)
2.  View the results, including the disease associated risk genes, the SNP annotations and gene weight (Z_score) under the specific selection.

You can also

-   Enter a `Gene symbol name` (e.g., `CRP`) in **Search** box to check the specific gene you are interested in.

-   Click **Download Results** to downloads.

    -   ***Special Function***

        Click `Compare Adult vs Fetal` to explore the risk genes difference between different development stage.

### SNV to cis-Regulatory Elements (CREs)

1.  Select a tissue and disease from the dropdown menus.(e.g `Adipose_Omentum(Adult)`,`AB1_ERYSIPELAS`)
2.  View the results, including the disease associated risk CREs, the gene that regulated by the CRE as well as the SNP annotations under the specific selection.

You can also

-   Enter a Gene name (e.g., `RP1L1`) in `Search` box to check the gene you are interested in.

-   Enter a region (e.g., `chr1-120000-1300000`) in `Search` box to check the CRE you are interested in.

-   Click **Download Results** to downloads.

    -   ***Special Function***

        Click `Compare Adult vs Fetal` to explore the regulatory difference between different development stage.

### SNV to Function

> #### **`Pathways`**

1.  Select a disease and tissue from the drop-down menus.
2.  You can choose either the `Display Table` bottom or `Visualisation` bottom to view the GO results.

> #### **`Aggregate SNV information to trait-associated Cells`**

1.  Select a tissue with any diseases you interested in the tab.

2.  Click **Generate Plot**.

3.  View the heat map, the significant disease association cell types are bold labelled.

    -   ***Special Function*** 

        You can broswer the cell type specific Trait-relevant CREs.
        
        eg: Select a tissue and disease from the tab.(e.g `Left_Ventircle(Adult)`,`Fibroblast`,`I9_AF`)
        
        Output:
        
        CRE:chr10-103448764-103449065
        
        GENE:SH3PXD2A
        
        SNPs:rs555347613, rs114681462, rs148371916
        
        The gene SH3PXD2A has been linked to AF through GWAS by former papers.

------------------------------------------------------------------------

### **Analyze your single cell Data**

1.  Choose `.h5ad` file from your device.

2.  Specify the single cell data type (e.g `single cell RNA` or `single cell ATAC`)

    #### **`For RNA Data`**

-   Select a disease and tissue from the tabs, then the system will automatically pool risk data from the database to calculate the disease risk score.

    #### **`For ATAC Data`**

-   You should create your own risk CREs file and upload to server to do the calculation.( you can follow the SC_VAR usage to create the risk file of your own.)

    #### **`For Multiome Data`**

-   You can select one of the Omics data to use.

3.  Click **Submit Analysis**

4.  **Download Results** when it's ready.

------------------------------------------------------------------------

### 📊 Data Sources

#### **`GWAS`**`: FINNGEN R11`

We selected 318 traits with clear ICD-11 codes, summarized as follows:

| ICD11-Code | Num | Description                                                                   |
|---------------------|-------------------|--------------------------------|
| M13        | 42  | Diseases of the musculoskeletal system and connective tissue                  |
| K11        | 41  | Diseases of the digestive system                                              |
| I9         | 36  | Diseases of the circulatory system                                            |
| N14        | 28  | Diseases of the genitourinary system                                          |
| O15        | 25  | Pregnancy, childbirth and the puerperium                                      |
| J10        | 23  | Diseases of the respiratory system                                            |
| H7         | 23  | Diseases of the eye and adnexa                                                |
| G6         | 13  | Diseases of the nervous system                                                |
| F5         | 16  | Mental and behavioural disorders                                              |
| E4         | 14  | Endocrine, nutritional and metabolic diseases                                 |
| L12        | 11  | Diseases of the skin and subcutaneous tissue                                  |
| H8         | 10  | Diseases of the ear and mastoid process                                       |
| AB1        | 10  | Certain infectious and parasitic diseases                                     |
| C3         | 8   | Neoplasms, from cancer register                                               |
| ST19       | 6   | Injury, poisoning and certain other consequences of external causes           |
| CD2        | 6   | Neoplasms from hospital discharges                                            |
| D3         | 4   | Diseases of the blood and blood-forming organs and immune mechanism disorders |
| Z21        | 1   | Factors influencing health status and contact with health services            |
| R18        | 1   | Symptoms, signs and abnormal clinical and laboratory findings                 |

#### **`Single-Cell Atlas Data`**`:`

We included single-cell data with atlas level, summarized as follows:

-   **sci-ATAC-seq data of human adult tissues**, GEO: GSE184462, GEO: GSE160472, dbGaP: phs001961, dbGaP: phs002204

-   **sci-ATAC-seq data of human fetal tissues**, dbGaP: phs002003

------------------------------------------------------------------------

#### 📌 Citation

-   Gefei Zhao, Binbin Lai.*SC-VAR: a computational tool for interpreting polygenic disease risks using single-cell epigenomic data*, Briefings in Bioinformatics,2025.
-   Zhang, Hou, et al. *Polygenic enrichment distinguishes disease associations of individual cells in single-cell RNA-seq data*, Nature Genetics, 2022.
-   Yu G, Wang L, Han Y and He Q. *clusterProfiler: an R package for comparing biological themes among gene clusters.* OMICS: A Journal of Integrative Biology, 2012.

------------------------------------------------------------------------
