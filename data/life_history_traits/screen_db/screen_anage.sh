species=$1

query="${species//_/ }"

echo "AnAge"

search=""
for term in $query; do
search="${search}_${term}"
done

text_EOL=$(curl -s "https://genomics.senescence.info/species/entry.php?species=${search:1}"  | html2text | tr '\n' ' '  )

paste <(echo "${species}") <(echo "AnAge ") <(echo "$text_EOL" | grep -oE ".{0,0}Maximum longevity.{0,20}" | grep -oE "(.*?years)"  | head -n 1) <(echo "$species") >> lifespan.tab
paste <(echo "${species}") <(echo "AnAge ") <(echo "$text_EOL" | grep -oE ".{0,0}Adult weight.{0,20}" | grep -oE "(.*?g)"  | head -n 1) <(echo "$species") >> weight.tab
