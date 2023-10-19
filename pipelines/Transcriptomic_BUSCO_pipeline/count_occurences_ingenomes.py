
import pandas as pd
import sys
from collections import Counter
from Bio import SeqIO

print(sys.argv)
gc_table_path = sys.argv[1]
genome_path = sys.argv[2]

def count_dinucleotides(genome_path):
    counts = Counter()
    for record in SeqIO.parse(genome_path, "fasta"):
        seq = str(record.seq).upper()
        for i in range(len(seq) - 1):
            counts[seq[i:i + 2]] += 1
            counts[seq[i]] += 1
        counts[seq[i+1]] += 1
    df = pd.DataFrame.from_dict(counts, orient='index', columns=['Freq'])
    df.index.name = 'genome_character'
    return df

df = count_dinucleotides(genome_path)
print(df)

df.to_csv( gc_table_path , index = True, sep = "\t")