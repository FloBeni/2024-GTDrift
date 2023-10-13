species=$1

query="${species//_/ }"

echo "fishbase"



search=""
for term in $query; do
search="${search}-${term}"
done

text_EOL=$(curl -s "https://fishbase.mnhn.fr/summary/${search:1}.html"  | html2text | tr '\n' ' ' )

paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}Max length.{0,20}" | grep -oE "(.*?cm|mm|m)"  | head -n 1) <(echo "$species") >> length.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}Max length.{0,100}" | grep -oE "(.*?female)"  | head -n 1) <(echo "$species") >> length.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}max. published weight.{0,20}" | grep -oP ".*?(?: g| kg| t)" | head -n 1) <(echo "$species") >>  weight.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}max. reported age:.{0,30}" | grep -oE "(.*?years)" | head -n 1) <(echo "$species") >> lifespan.tab
    
