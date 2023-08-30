
printf 'length\n' > length.tab
printf 'lifespan\n' > lifespan.tab
printf 'mass\n' > weight.tab

list=$(cat $1)
for species in $list; do
echo "${species}"
query="${species//_/ }"

echo "$query"


search=""
for term in $query; do
search="$search%20$term"
done  

text_EOL=$(curl -s "https://animaldiversity.org/accounts/${search:3}/"  | html2text | tr '\n' ' ' )
echo "https://animaldiversity.org/accounts/${search:3}/"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") >> length.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}length.*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )" | grep -oP ".*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") >> length.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}Length.*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )" | grep -oP ".*?(?:cm |mm | m |cm\\.|mm\\.| m\\.|cm,|mm,| m,|\\. )")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") >> lifespan.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}lifespan.*?(?:year |month |day |years |months |days |years\\.|months\\.|days\\.|years\\,|months\\,|days\\,|\\. )" | grep -oP ".*?(?:year|month|day)")"

while read -r line; do
    paste <(echo "${species}") <(echo "ADW") <(echo "$line") >> weight.tab
done <<< "$(echo "$text_EOL" | grep -oP ".{0,0}mass.*?(?:kg | g |kg\\.| g\\.|kg,| g,|\\. )" | grep -oP ".*?(?:kg| g)")"



url="https://eol.org/search?utf8=%E2%9C%93&q=${search:3}"
html=$(curl -s "$url")
link=$(echo "$html" | grep -oP 'href="\K[^"]+' |  grep '/pages/' | head -n 1)

echo "Link: $link"
newurl="https://eol.org/$link"

text_EOL=$(curl -s $newurl  | html2text | tr '\n' ' ' )

paste <(echo "${species}") <(echo "EOL ") <(echo "$text_EOL" | grep -oP ".{0,0}can grow to.*?(?:m.|m )" | grep -oP ".*?(?:m.|m )" | head -n 1) >> length.tab
paste <(echo "${species}") <(echo "EOL ") <(echo "$text_EOL" | grep -oP ".{0,17}to live for.*?(?:months|years)" | grep -oP ".*?(?:months|years)" | head -n 1) >> lifespan.tab

paste <(echo "${species}") <(echo "EOL ") <(echo "$text_EOL" | grep -oP ".{0,0}life span.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}life span.*?\*" | grep -oP ".{0,30}?(?: years | g | mm | cm\^3 | cm | tons | t )") >> lifespan.tab

while read -r line; do
    paste <(echo "${species}") <(echo "EOL") <(echo "$line") >> weight.tab
done <<< "$(paste <(echo "$text_EOL" | grep -oP ".{0,10}mass.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}mass.*?\*" | grep -oP ".{0,30}?(?: years | g | kg | mm | cm\^3 | m | cm | tons | t )"))"


while read -r line; do
    paste <(echo "${species}") <(echo "EOL") <(echo "$line") >> length.tab
done <<< "$(paste <(echo "$text_EOL" | grep -oP ".{0,15}length.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}length.*?\*" | grep -oP ".{0,30}?(?: years | g | mm | cm\^3 | m | cm | tons | t )"))"




search=""
for term in $query; do
search="${search}_${term}"
done

text_EOL=$(curl -s "https://genomics.senescence.info/species/entry.php?species=${search:1}"  | html2text | tr '\n' ' '  )

paste <(echo "${species}") <(echo "AnAge ") <(echo "$text_EOL" | grep -oE ".{0,0}Maximum longevity.{0,20}" | grep -oE "(.*?years)"  | head -n 1) >> lifespan.tab
paste <(echo "${species}") <(echo "AnAge ") <(echo "$text_EOL" | grep -oE ".{0,0}Adult weight.{0,20}" | grep -oE "(.*?g)"  | head -n 1) >> weight.tab




search=""
echo "Googling: $query"
for term in $query; do
search="${search}-${term}"
done

text_EOL=$(curl -s "https://fishbase.mnhn.fr/summary/${search:1}.html"  | html2text | tr '\n' ' ' )

paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}Max length.{0,20}" | grep -oE "(.*?cm|mm|m)"  | head -n 1) >> length.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}Max length.{0,100}" | grep -oE "(.*?female)"  | head -n 1) >> length.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}max. published weight.{0,20}" | grep -oP ".*?(?: g| kg| t)" | head -n 1) >>  weight.tab
paste <(echo "${species}") <(echo "fishbase ") <(echo "$text_EOL" | grep -oE ".{0,0}max. reported age:.{0,30}" | grep -oE "(.*?years)" | head -n 1) >> lifespan.tab
    

done
