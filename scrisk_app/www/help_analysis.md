## 🧮 Analyze Your Single-Cell Data

Upload and score your own datasets (scRNA-seq or scATAC-seq) against our disease risk models.

### Upload & Submit

1.  **Upload File**: Choose your `.h5ad` file (Max 10GB).
2.  **Configuration**:
    -   **RNA**: Select Disease & Tissue from our database.

    -   **ATAC**: Download our template, create your own risk file, and upload it.

        ![](images/ana1.png){width="608"}
3.  **Submit**: Click the **`Submit Analysis`** button.
    -   You will receive a unique **Job ID** (e.g., `b119c6b6...`). **Please copy this ID.**

### Retrieve Results

![](images/ana2.png){width="469"}

-   **Task Persistence**: You can close the window and come back later!
-   **Check Status**:
    1.  Paste your **Job ID** into the **"Enter Task ID"** box.
    2.  If the task is `Completed`, a green **`Download Results`** button will appear.
-   **Please note results not downloaded within 24 hours will be cleared by the system.**

------------------------------------------------------------------------

<details>

<summary><strong>Click to view detailed ATAC Data Formatting & Workflow</strong></summary>

<br>

### ⚠️ How to prepare ATAC risk data? (Step-by-step Tutorial)

The workflow is identical to the **SC-VAR** pipeline. You can follow the complete code tutorial here:

> [**🔗 SC-VAR scATAC Pipeline for scRiskDB (GitHub Notebook)**](https://github.com/gefeiZ/scRiskDB/blob/main/Usage/scATAC_RiskFile_pipline.ipynb)

#### 1. Required Input Data Formats

***A. GWAS Summary Statistics***

A text file containing SNP association data. Space or tab-delimited.

**Required Columns:** `chr`, `pos`, `rsids`, `pval`

***B. Peak-to-Gene Links (scATAC Data)***

A file defining the co-accessibility networks (which peaks regulate which genes). Tab-delimited.

**Required Columns:** `gene`, `chr`, `start`, `end`

#### 2. Required Reference Files

You must ensure the reference genome versions match your data (hg19 or hg38).

-   **Gencode Annotation:** Use **v43** for hg19 / **v44** for hg38.
    -   File: `gencode.vxx.annotation.gff3.gz`
-   **MAGMA Gene Annotation:**
    -   Original MAGMA gene body definition file (`magma0.genes.annot`).
-   **1000 Genomes Panel:**
    -   PLINK binary files (`.bed`/`.bim`/`.fam`) for LD calculation (e.g., `g1000_eur`).

#### 3. Processing Workflow

The SC-VAR pipeline consists of **five key steps** to generate the final risk annotation file:

**Step 1: Load Data**

Import GWAS statistics (`scv.read_gwas`). Import Peak-Gene connections (`scv.get_p2g_conn` or `scv.load_peak_data`).

**Step 2: Map SNPs to Peaks**

Identify GWAS SNPs that fall within the open chromatin peaks (`scv.snp_peak`).

**Step 3: Map Peaks to Genes**

Correlate peaks with gene genomic coordinates using the Gencode GFF3 file (`scv.gene_corr`).

**Step 4: Build Annotation**

Combine the overlap matrix, gene correlations, and MAGMA annotations to construct the final **Peak-SNP-Gene annotation file** (`scv.annotate`). **Output Filename Example:** `Schizophrenia.scemagma.genes.annot`

**Step 5: Get risk File**

When get sce-MAGMA gene results, Then use (`generate_atac_risk_file`) function this will give you the final candidate score risk file.

</details>

<br>

------------------------------------------------------------------------

#### 📂 Required Output Format

After running the pipeline, you need to generate a **`.csv`** （tab split）or **`.txt`** file to upload to scRiskDB.

-   **Header:** Required.
-   **Columns:**
    1.  `CRE_ID`: Genomic coordinates (format: `chr:start-end` or `chr-start-end`).
    2.  `ZSTAT`: The calculated association score (Z-score).

**Example File Content can be download at the Cell Scoring page.**
