# 🧬 Risk Genes Module

This module enables the exploration of tissue-specific disease candidate risk genes identified by **SC-VAR** framework.

------------------------------------------------------------------------

## 📋 How to Use

1.  **Select Context:** Choose a **Tissue** (e.g., *Brain*) and a **Disease** (e.g., *Schizophrenia*) from the ***dropdown*** menus.
2.  **Browse Results:** The table displays candidate risk genes sorted by `ZSTAT` (Association Weight).And you can search interested genes by their gene name in the ***Search Box***.
3.  **Analyze & Validate:** Use the validation links described below to gather more causal evidence.

> **Interface Guide:**
>
> ![](gene_page1.png){width="100%"}

#### **What's More?**

4.  **Developmental Comparison:** Click the **"Compare Adult vs Fetal"** button.
    -   This generates two side-by-side tables showing genes specific to the **Adult** stage versus the **Fetal** stage, helping you identify developmental-specific risk drivers.
    -   Click **"Download Results"** to export the full gene list for your own analysis.

------------------------------------------------------------------------

## 🌟 Functional & Causal Validation

We provide two powerful external integration to help users move from **"Predicted Candidate Gene"** to **"Validated Target"**:

### **1. Open Targets Platform (Therapeutic Evidence)**

-   **How to access:** Click on the **Blue Gene Name** (e.g., **`GRIN2A`** ) in the results table.

-   **Usage:** Directly navigates to the **Associations** page for that gene.

-   **Why use it? (Example: GRIN2A in Schizophrenia)**

    -   **Assess Druggability (ChEMBL):** Check the **ChEMBL** column to see if there are known drugs or clinical compounds targeting this gene (e.g., GRIN2A is a known target for NMDA receptor modulators).

    -   **Verify Genetic Evidence:** The **GWAS** and **Gene Burden** columns provide orthogonal genetic support, confirming the gene's association across independent cohorts.

    -   **Literature Mining:** The **Europe PMC** column aggregates text-mining evidence, helping you quickly find relevant publications linking the gene to the disease.

        ![](open_target1.png){width="541"}

### 2. NCI LDexpress (eQTL & LD Analysis)

-   **How to access:** Click the **`Check LD`** button in the **Validation** column.
-   **How to use:**
    -   First, click the **Copy Icon(1)** to automatically copy the top 10 risk SNVs associated with this gene.
    -   Then, click the **Link(2)** to open LDexpress. Paste the SNVs into the input box.
-   **eQTL Validation:** It searches if your risk variants (or variants in Linkage Disequilibrium with them) are associated with gene expression changes in **GTEx v8** tissues.
    -   **Mechanism:** LDexpress helps determine if a non-coding variant functions by regulating the expression levels of nearby genes.
    -   **LD Calculation:** LDexpress calculates $R^2$ and $D'$ statistics based on 1000 Genomes Project populations to find proxy variants.
