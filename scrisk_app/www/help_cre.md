## 🔗 Risk cis-Regulatory Elements (CREs)

This module links non-coding GWAS variants to functional regulatory elements (Enhancers/Promoters).

### 📋 General Usage

1.  **Select Context:** Choose a **Tissue** and **Disease** from the dropdown menus.

2.  **Filter & Search:** Use the Search box to find specific Genes or genomic regions.

3.  **Download:** Click **"Download Results"** to save the annotated CRE list for downstream analysis.

    ![](images/截屏2026-02-05%2020.28.51.png){width="574"}

4.  **Developmental Comparison:** Click the **"Compare Adult vs Fetal"** button.

    -   This generates two side-by-side tables showing CREs specific to the **Adult** stage versus the **Fetal** stage（The same as gene page).
    -   Click **"Download Results"** to export the full gene list for your own analysis.

    ![](images/截屏2026-02-05%2021.03.10.png){width="531"}

------------------------------------------------------------------------

### 🌟 Key Feature 1: Cell-Type Specificity

-   **How to use:** Check the **`Linked Cell Types`** column in the results table.

-   **What it tells you:** We explicitly label which specific cell types (e.g., *Astrocyte*, *Excitatory Neuron*) show chromatin accessibility at this CRE region.

-   **Why it matters:** This helps you determine if the disease risk is driven by a specific cellular context, rather than a tissue effect.

    ![](images/cre_k1.png){width="626"}

### 🌟 Key Feature 2: Physical Validation (Hi-C Tutorial)

To prove that a CRE actually regulates a distant gene, we need physical evidence that they touch each other in 3D space. We integrate the **3D Genome Browser** for this purpose.

Follow this **Step-by-Step Guide** to visualize the interaction:

#### Step 1: Get the Coordinates

-   Find the **Validation** column in the result table.

-   Click the **Copy Icon**.

-   *Note:* The copied coordinates are automatically **expanded by ±100kb** (e.g., `chr1:182,100,000-182,400,000`) to ensure you can see the surrounding chromatin loops and TAD structures.

    ![](images/cre_k2.png){width="641"}

#### Step 2: Select the Dataset

-   Click the **`3D View`** button to open the 3D Genome Browser.

-   **Configure the browser** based on your query:

    1.  **Species:** Select `Human`.

    2.  **Organ:** Select the tissue you are interested in (e.g., `Brain`).

    3.  **Cell Type:** Select the cell type that matches the **`Linked Cell Types`** column (e.g., `Astrocyte`).

    4.  Click **`+ Add`** to load the dataset.

> **Visual Guide: Dataset Selection**
>
> ![](images/cre_3d1.png){width="508"}
>
> (Make sure to match the Cell Type with our "Linked Cell Types" recommendation)

#### Step 3: Visualize & Interpret

-   Paste the coordinates you copied in Step 1 into the search bar at the top.

> **Visual Guide: Interpretation**
>
> ![](images/截屏2026-02-05%2020.27.29.png){width="297"}
>
> *(Example: A clear chromatin interaction loop in Cerebellum Astrocytes)*

**1. The "Red Triangle" (Heatmap)** **:**

The map shows interaction frequency. The x-axis is the genomic coordinate. The intense red pixels indicate regions that physically touch each other. **What to look for:** Look for a **dark red dot or cluster** off the diagonal line that aligns with your CRE and the target Gene. This "Spark" confirms they are interacting in the nucleus.

**2. TADs (Topologically Associating Domains)** **:** You will see large, triangular blocks of red signal along the diagonal. These are TADs—structural "neighborhoods" of the genome.

***Interpretation:*** DNA sequences within the same TAD interact frequently, while interactions across TAD boundaries are insulated (blocked).

***Functionality Rule:*** An Enhancer (CRE) and its Target Gene usually reside **within the same TAD**. If your CRE and Gene are in different TADs (separated by a white boundary), the regulatory link is less likely.

**3. Chromatin Loops (The Arcs)** **:** Look at the bottom track showing purple/blue arcs.

***Interpretation:*** These arcs represent high-confidence loops (e.g., Enhancer-Promoter loops).

***Functionality Rule:*** If you see an arc where **one end anchors at your CRE** and the **other end anchors at the Gene Promoter**, this is the strongest evidence of functional regulation.

**4. Summary: Is this CRE functional?**

| Evidence Level        | Observation in Browser                                                                                        |
|:---------------|:-------------------------------------------------------|
| **High Confidence**   | A clear **Loop (Arc)** connects the CRE to the Gene Promoter; both are inside the same **TAD**.               |
| **Medium Confidence** | No distinct Loop, but they are in the same **TAD** with high interaction frequency (red signal) between them. |
| **Low Confidence**    | The CRE and Gene are in different TADs (separated by a boundary) or show no interaction signal (white).       |
