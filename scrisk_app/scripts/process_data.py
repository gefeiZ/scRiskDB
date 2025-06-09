import argparse
import pandas as pd
import numpy as np
import scanpy as sc
import scdrs
import sc_var
from sc_var import method as scv
import os



def load_risk_data(risk_data_path):
    risk_df = pd.read_csv(risk_data_path, sep='\t', skiprows=1)  
    risk_df.columns = ['gene', 'ZSTAT']  
    return risk_df

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--file", required=True, help="Path to .h5ad file")
    parser.add_argument("--risk_data", required=True, help="Path to risk data file")
    parser.add_argument("--output", required=True, help="Output file path")
    parser.add_argument("--group_by", required=True, help="Group lable for Stat")
    
    args = parser.parse_args()

   
    df_risk = load_risk_data(args.risk_data)

  
    adata = sc.read_h5ad(args.file)
    #sc.pp.normalize_total(adata, target_sum=1e4)
    #sc.pp.log1p(adata)
    #sc.pp.scale(adata,zero_center=False)

    scdrs.preprocess(adata, n_mean_bin=30, n_var_bin=30)

    gene_list = df_risk["gene"].tolist()
    gene_weights = df_risk["ZSTAT"].tolist()
    gene_weights = np.array(gene_weights, dtype=np.float64)
    dict_df_score={}
    dict_df_score["scedrs_score"] = scdrs.score_cell(
        data=adata,
        gene_list=gene_list,
        gene_weight=gene_weights,
        ctrl_match_key="mean_var",
        n_ctrl=1000,
        weight_opt="uniform",
        return_ctrl_raw_score=False,
        return_ctrl_norm_score=True,
        verbose=False,
    )
    # 保存结果
    df_stat_results = scv.stat_analysis(adata, dict_df_score["scedrs_score"],group_cols=[args.group_by])
    for key, df in df_stat_results.items():
      df.to_csv(os.path.join(args.output, key + "result.txt"), sep="\t")

