# scRiskDB

scRiskDB (available at www.scriskdb.cn ) is a freely accessible, registration-free resource designed to bridge the gap between trait-associated genetic variants and their functional impacts at single-cell resolution. 

By integrating single-cell multi-omics data—including scRNA-seq, scATAC-seq, and multiome datasets—with GWAS summary statistics, scRiskDB enables researchers to systematically annotate single-nucleotide variants (SNVs), identify candidate risk genes and cis-regulatory elements (CREs), explore trait-relevant pathways, and compare disease-associated regulatory landscapes across tissues and developmental stages.


![Fig1_abstract](https://github.com/user-attachments/assets/86233386-b193-41f3-b561-ad686be8f614)


## 🚀 Local Deployment (Run scRiskDB on your own machine)

To ensure stability and allow users to run customized analyses locally, we provide a complete local deployment guide.

### 1. Download the Database
We have provided some lightweight MySQL database dump containing the complete data for our featured case studies (e.g., Schizophrenia) on Zenodo: 
👉 **[https://zenodo.org/records/18765434]**

### 2. Import the Database to Local MySQL
Install MySQL on your local machine, create a new database, and import the downloaded `.sql` file:
```bash
mysql -u root -p -e "CREATE DATABASE scriskdb;"
mysql -u root -p scriskdb < scRiskDB_example.sql
```
### 3. Configure the Shiny App
Clone this repository to your local machine

### 4.Rename the config_template.R 
Rename the file to config.R and update it with your local MySQL credentials:
```bash
DB_USER <- "root"          # Your local MySQL username
DB_PASSWORD <- "your_password"    # Your local MySQL password
```

### 5. Run the Application
Open R or RStudio and run the Shiny app:
