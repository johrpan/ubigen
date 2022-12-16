from io import StringIO
import json
import pandas
import requests

# Genes of interest (in this case, genes involved in glycolysis according to
# the KEGG pathways database [KEGG:hsa00010+M00001]).
genes = [
    "ENSG00000111640",
    "ENSG00000111669",
    "ENSG00000149925",
    "ENSG00000074800",
    "ENSG00000105220",
    "ENSG00000067225",
    "ENSG00000102144",
    "ENSG00000141959",
    "ENSG00000156515",
    "ENSG00000171314",
    "ENSG00000067057",
    "ENSG00000111674",
    "ENSG00000159322",
    "ENSG00000152556",
    "ENSG00000109107",
    "ENSG00000159399",
    "ENSG00000108515",
    "ENSG00000160883",
    "ENSG00000226784",
    "ENSG00000188316",
    "ENSG00000106633",
    "ENSG00000136872",
    "ENSG00000156510",
    "ENSG00000143627",
    "ENSG00000170950",
]

# Get a summary on the ubiquity of the above gene set:

response_summary = requests.post(
    "https://ubigen.uni-rostock.de/api/summary",
    headers={"Content-Type": "text/plain"},
    data=" ".join(genes),
)

summary = json.loads(response_summary.content)
median_percentile = summary["median_percentile"]
median_percentile_rounded = round(median_percentile, 3)
estimated_change = summary["change"]
p_value = summary["p_value"]

# Example information: Median percentile of the genes and p-value for difference
# in scores in comparison with other genes.

print(f"Median percentile: {median_percentile_rounded * 100}%")
print(f"Estimated score difference: {estimated_change} (p = {p_value})")

# Retrieve detailed information on all parameters and the ranking of the gene
# set defined above:

response_ranking = requests.post(
    "https://ubigen.uni-rostock.de/api/ranking",
    headers={"Content-Type": "text/plain"},
    data=" ".join(genes),
)

# Parse the data using pandas. This gives lots of opportunity for further
# analyses.

data = pandas.read_csv(StringIO(response_ranking.text))

# Example analysis: Recompute the mean percentile using pandas.

median_percentile_new = data["percentile"].median()
median_percentile_new_rounded = round(median_percentile_new, 3)

print(f"Recomputed median percentile: {median_percentile_new_rounded * 100}%")
