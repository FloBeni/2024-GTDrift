


import pandas as pd
import sys

print(sys.argv)

path_input = sys.argv[1]

sra_list=sys.argv[2]
sra_list=sra_list.split(",")

dt = pd.read_table(path_input , comment="#")
column_names = dt.columns.tolist()


for sra in sra_list:
	print(sra)
	columns_to_remove = ['gene_id','gene_name','seq_id','major_intron','start','end','strand','seqname','splice5','splice3','type','sum_n1','sum_n2','sum_n3','splice_variant_rate','attributes','nonsplice_variant_rate','intron_class','median_fpkm','mean_fpkm', 'exon_coverage']
	columns_to_remove = columns_to_remove + [i for i in column_names if ("exon_coverage_" in i and "exon_coverage_"+sra != i ) or ("fpkm_" in i and "fpkm_"+sra != i)]
	columns_to_remove = columns_to_remove + [i for i in column_names if ("n1_" in i and "n1_"+sra != i ) or ("n2_" in i and "n2_"+sra != i) or ("n3_" in i and "n3_"+sra != i)]
	df = dt.drop(columns=[i for i in columns_to_remove if i in column_names])
	df.columns = df.columns.str.replace('n1_', 'ns_')
	df.columns = df.columns.str.replace('n2_', 'na_')
	df.columns = df.columns.str.replace('n3_', 'nu_')
	print(sys.argv[3])
	df.to_csv(sys.argv[3] + sra + sys.argv[4], index=False, sep="\t",compression='gzip')

