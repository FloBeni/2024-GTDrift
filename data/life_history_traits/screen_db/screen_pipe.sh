
#printf 'length\n' > length.tab
#printf 'lifespan\n' > lifespan.tab
#printf 'mass\n' > weight.tab

list=$(cat $1)
for species in $list; do
echo "${species}"

./screen_eol.sh ${species}
#./screen_adw.sh ${species}
#./screen_anage.sh ${species}
./screen_fishbase.sh ${species}

done
