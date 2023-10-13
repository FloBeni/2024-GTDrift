species=$1

query="${species//_/ }"

echo "ADW"


search=""
for term in $query; do
search="$search%20$term"
done  

text_EOL=$(curl -s "https://animaldiversity.org/accounts/${search:3}/"  | html2text | tr '\n' ' ' )

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") <(echo "$species") >> length.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}length.*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )" | grep -oP ".*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") <(echo "$species") >> length.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}Length.*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )" | grep -oP ".*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") <(echo "$species") >> lifespan.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}lifespan.*?(?:year |month |day |years |months |days |years\\.|months\\.|days\\.|years\\,|months\\,|days\\,|\\. )" | grep -oP ".*?(?:year|month|day)")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") <(echo "$species") >> weight.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}mass.*?(?:kg | g |kg\\.| g\\.|kg,| g,|\\. )" | grep -oP ".*?(?:kg| g)")"


