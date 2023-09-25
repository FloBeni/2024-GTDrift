species=$1

query="${species//_/ }"

echo "EOL"

search=""
for term in $query; do
search="$search%20$term"
done  

IFS=$' \t\n'

sp1=$(echo "$query" | cut -d ' ' -f1)

url="https://eol.org/search?utf8=%E2%9C%93&q=${search:3}"
html=$(curl -s "$url")

listspecies=$(echo "$html" | grep -oP 'href="\K[^"]+|search-title..\K[^"]+(?=<\/div>)' | awk '{ FS="\n"; RS="" }{for(i=1;i<=NF;i++) if(index($i, "pages")) { link=link $(i+1) "\n"}} END{print link}')
listlink=$(echo "$html" | grep -oP 'href="\K[^"]+|search-title..\K[^"]+(?=<\/div>)' | awk '{ FS="\n"; RS="" }{for(i=1;i<=NF;i++) if(index($i, "pages")) { link=link $(i) "\n"}} END{print link}')

IFS=$'\n' # Set the Internal Field Separator to newline to preserve line breaks

for linktotal in $(paste <(echo "$listlink") <(echo "$listspecies"))
do
link=$(echo $linktotal | awk '{print $1}')
#echo "Link: $link"
species_title=${linktotal//$link}
species_title=${species_title//"	"}
#echo $species_title

newurl="https://eol.org/$link"

text_EOL=$(curl -s $newurl  | html2text | tr '\n' ' ' )

IFS=$' \t\n'

paste <(echo "${species}") <(echo "EOL") <(echo "$text_EOL" | grep -oP ".{0,0}can grow to.*?(?:m.|m )" | grep -oP ".*?(?:m.|m )" | head -n 1) <(echo "$species_title") >> length.tab
paste <(echo "${species}") <(echo "EOL") <(echo "$text_EOL" | grep -oP ".{0,17}to live for.*?(?:months|years)" | grep -oP ".*?(?:months|years)" | head -n 1) <(echo "$species_title") >> lifespan.tab

paste <(echo "${species}") <(echo "EOL") <(echo "$text_EOL" | grep -oP ".{0,0}life span.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}life span.*?\*" | grep -oP ".{0,30}?(?: years | g | mm | cm\^3 | cm | tons | t )") <(echo "$species_title") >> lifespan.tab

while read -r line; do
    paste <(echo "${species}") <(echo "EOL") <(echo "$line") <(echo "$species_title") >> weight.tab
done <<< "$(paste <(echo "$text_EOL" | grep -oP ".{0,10}mass.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}mass.*?\*" | grep -oP ".{0,30}?(?: years | g | kg | mm | cm\^3 | m | cm | tons | t )"))"


while read -r line; do
    paste <(echo "${species}") <(echo "EOL") <(echo "$line") <(echo "$species_title") >> length.tab
done <<< "$(paste <(echo "$text_EOL" | grep -oP ".{0,15}length.*? ") <(echo "$text_EOL" | grep -oP ".{0,0}length.*?\*" | grep -oP ".{0,30}?(?: years | g | mm | cm\^3 | m | cm | tons | t )"))"

done
IFS=$' \t\n'
