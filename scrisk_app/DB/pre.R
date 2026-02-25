#packages
library(Seurat)
library(Signac)
library(GenomicRanges)
library(GenomeInfoDb)
library(EnsDb.Hsapiens.v86)
library(ggplot2)
library(patchwork)
library(SummarizedExperiment)
library(rtracklayer)
library(SingleCellExperiment)
library(cicero)

#####DATA LOADER
file_metadata_pairs <- list(
  list(file = '66_sample.fragments.bed.gz', 
       metadata = '66_selected_metadata.csv'),
  list(file = '71_sample.fragments.bed.gz',
       metadata = '71_selected_metadata.csv'),
  list(file = '69_sample.fragments.bed.gz',
       metadata = '69_selected_metadata.csv'),
  list(file = '64_sample.fragments.bed.gz',
       metadata = '64_selected_metadata.csv'),
  list(file = '36_sample.fragments.bed.gz',
       metadata = '36_selected_metadata.csv'),
  list(file = '6_sample.fragments.bed.gz',
       metadata = '6_selected_metadata.csv')
)




#FUNCTION
create_and_combine_genomic_ranges <- function(file_metadata_pairs) {
  gr_list <- list()
  
  i <- 1
  for (pair in file_metadata_pairs) {
    metadata_path <- pair$metadata
    file_path <- pair$file
    
    total_counts <- CountFragments(file_path)
    cutoff <- 1000
    barcodes <- total_counts[total_counts$frequency_count > cutoff, ]$CB
    frags <- CreateFragmentObject(path = file_path, cells = barcodes)
    peaks <- CallPeaks(frags, macs2.path="~/Miniconda/envs/PeakCalling_analysis/bin/macs2")
    gr <- makeGRangesFromDataFrame(peaks)
    gr_list[[i]] <- gr
    i <- i + 1
  }
  
  combined_gr <- GenomicRanges::reduce(do.call(c, gr_list))
  
  return(combined_gr)
}

####RUN 1
combined_gr <- create_and_combine_genomic_ranges(file_metadata_pairs)

peakwidths <- width(combined_gr)
combined_gr <- combined_gr[peakwidths  < 10000 & peakwidths > 20]
combined_gr

combined_peak <- combined_gr[!grepl("_", seqnames(combined_gr)), ]


#RUN 2

create_and_merge_seurat_objects <- function(file_metadata_pairs) {
  seurat_objects <- list()
  i <- 1
  for (pair in file_metadata_pairs) {
    file_path <- pair$file
    metadata_path <- pair$metadata
    
    total_counts <- CountFragments(file_path)
    cutoff <- 1000
    barcodes <- total_counts[total_counts$frequency_count > cutoff, ]$CB
    frags <- CreateFragmentObject(path = file_path, cells = barcodes)
    counts <- FeatureMatrix(fragments = frags, features = combined_peak, cells = barcodes)
    metadata <- read.csv(file = metadata_path, header = TRUE, row.names = 'cellname')
    
    chrom_assay <- CreateChromatinAssay(counts = counts, sep = c(":", "-"), fragments = file_path, min.cells = 0, min.features = 200)
    seurat_object <- CreateSeuratObject(counts = chrom_assay, assay = "peaks", meta.data = metadata)
    seurat_object <- subset(seurat_object, subset = cell.type != "NA")
    seurat_object$dataset <- basename(metadata_path)
    
    seurat_objects[[i]] <- seurat_object
    i <- i + 1
  }
  
  combined_seurat_object <- merge(x = seurat_objects[[1]], y = seurat_objects[-1])
  
  return(combined_seurat_object)
}



combined <- create_and_merge_seurat_objects(file_metadata_pairs)
combined_peak.keep <- seqnames(granges(combined)) %in% standardChromosomes(granges(combined))
combined <- combined[as.vector(combined_peak.keep), ]
combined$topcelltype=sub("\\(.*|\\d.*", "", combined$cell.type)
cell_counts = table(combined$topcelltype)
keep_cell_types = names(cell_counts[cell_counts >= 10])
combined=subset(combined, subset = topcelltype %in% keep_cell_types)
saveRDS(combined, paste0('merge_peak_signac',Sys.Date(),'.rds'))
expression_matrix <- as.matrix(GetAssayData(combined, slot = "counts"))



cells <- combined@meta.data[,1:4]
genes <- combined@assays$peaks@counts@Dimnames[[1]]
gene_df <- data.frame(ids = genes, row.names = genes)

input_cds <-  suppressWarnings(new_cell_data_set(expression_matrix ,
                                                 cell_metadata = cells,
                                                 gene_metadata = gene_df))

rm(expression_matrix)


input_cds <- monocle3::detect_genes(input_cds)

input_cds <- detect_genes(input_cds)
input_cds <- estimate_size_factors(input_cds)
input_cds <- preprocess_cds(input_cds, method = "LSI")
input_cds <- reduce_dimension(input_cds, reduction_method = 'UMAP', 
                              preprocess_method = "LSI")
#pause
umap_coords <- reducedDims(input_cds)$UMAP
cicero_cds <- make_cicero_cds(input_cds, reduced_coordinates = umap_coords)

conns <- run_cicero(cicero_cds,'hg38.chrom.sizes', sample_num = 100)
gene_anno <- rtracklayer::readGFF("Homo_sapiens.GRCh38.109.gtf")
# rename some columns to match requirements
gene_anno$chromosome <- paste0("chr", gene_anno$seqid)
gene_anno$gene <- gene_anno$gene_id
gene_anno$transcript <- gene_anno$transcript_id
gene_anno$symbol <- gene_anno$gene_name
pos <- subset(gene_anno, strand == "+")
pos <- pos[order(pos$start),] 
pos <- pos[!duplicated(pos$transcript),] 
pos$end <- pos$start + 1000 

neg <- subset(gene_anno, strand == "-")
neg <- neg[order(neg$start, decreasing = TRUE),] 
neg <- neg[!duplicated(neg$transcript),] 
neg$start <- neg$end - 1000

gene_annotation_sub <- rbind(pos, neg)

gene_annotation_sub <- gene_annotation_sub[,c("chromosome", "start", "end", "symbol")]

names(gene_annotation_sub)[4] <- "gene"

input_cds <- annotate_cds_by_site(input_cds, gene_annotation_sub)

p2g<-fData(input_cds)
p2g<-data.frame(p2g)
write.table(p2g,file="fdata_p2g.csv",sep="\t",quote=F)


#save h5ad file
library(SeuratData)
library(SeuratDisk)
SaveH5Seurat(data, filename = "output.h5Seurat")
Convert("output.h5Seurat", dest = "h5ad")
