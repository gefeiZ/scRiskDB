## ❓ Frequently Asked Questions

**Q: What should I do if I cannot find my rsID in the SNV search?**\
**A:** Make sure the rsID is correctly formatted (e.g., `rs123456`). If no result appears, the SNP may not be present in the selected tissue or disease context. And do not click Downloads bottom when there is no results,It will give you an error.

**Q: Can I search for a gene instead of an rsID?**\
**A:** Yes. In the SNV-to-Gene and SNV-to-CRE modules, you can search by gene symbol (e.g., `TP53`) to explore disease-associated regulatory mechanisms linked to that gene.

**Q: What does the ZSTAT/Weight represent in the results?**\
**A:** The ZSATA or the Weight is the Z score calculated based on P-values reflects the association strength of the SNP, gene or CRE with the selected disease under the specified tissue context.

**Q: What if I lose my Task ID?** A: The Task ID is unique to each submission. If lost, you may need to resubmit the analysis.

**Q: My ATAC upload failed.**\
**A:** Ensure your `.csv` risk file follows the template format exactly (Columns: `CRE_ID`, `ZSTAT`).And your single cell data may not exceed 10 GB due to the limitations of the web server.

**Q: Why do some tissues have no results for a specific disease?**\
**A:** Risk association depends on the presence of trait-relevant SNPs or active regulatory elements in that specific tissue. If no signal is found, the table will be empty.
