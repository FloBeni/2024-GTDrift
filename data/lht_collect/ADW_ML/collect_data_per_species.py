from transformers import AutoModelForQuestionAnswering, AutoTokenizer, pipeline
import requests
from bs4 import BeautifulSoup
from word2number import w2n
import os
import re

def convert_to_w2n(text_to_conv):
    s = text_to_conv.split(" ")
    text=""
    for i in s:
        try:
            text += " " + str(w2n.word_to_num(i))
        except:
            text += " " + i
    return(text)


# model_name = "deepset/roberta-base-squad2"
model_name = "deepset/tinyroberta-squad2"
# model_name = "deepset/roberta-large-squad2-hp"

# a) Get predictions
nlp = pipeline('question-answering', model=model_name, tokenizer=model_name)

vec = []
myfile = open('list_species.tab', 'r')
list_species = []
with myfile as file:
    for line in file:
        list_species.append(line.strip())

os.makedirs(""+model_name, exist_ok=True)

# path_length_file = ''+model_name+'/length.tab'
# path_weight_file = ''+model_name+'/weight.tab'
# path_lifespan_file = ''+model_name+'/lifespan.tab'

length_file = open(path_length_file, 'w')
weight_file = open(path_weight_file, 'w')
lifespan_file = open(path_lifespan_file, 'w')

species = "Vanessa_cardui"

pattern = r'(?<=[^a-z])(kg|g|mg|tons|t|hours|inches|month|centimeters|grams|year|meters|meter|day|months|years|days|m|cm|mm|µm|μm)(?=[^a-z])'
patternnumeric = r'([0-9])'

for species in list_species:
    print(species)
    url = "https://animaldiversity.org/accounts/"+ species + "/"


    response = requests.get(url)
    html_code = response.content

    soup = BeautifulSoup(html_code, 'html.parser')

    physical_description_tag = soup.find('h3', id='physical_description')

    if physical_description_tag:
        text = ''
        next_element = physical_description_tag.next_sibling

        while next_element and next_element.name != 'section':
            if next_element.name:
                text += ' ' + next_element.get_text(separator='').replace('\n', ' ')
            next_element = next_element.next_sibling


        text = re.sub(r'Range wingspan.*? in', '', text)
        text = re.sub(r'Range wingspan.*? ft', '', text)
        # print(text.strip())
        context = text
        print("Mass")
        for i in range(0,10):
            if len(context) > 3:
                QA_input = {
                    'question': 'what is the body mass?',
                    'context': context
                }
                res = nlp(QA_input, top_k=100)
                # res = [i for i in res if any(c.isalpha() for c in i["answer"])]
                res = [i for i in res if re.findall(pattern, i["answer"]+';') and re.findall(patternnumeric, convert_to_w2n(i["answer"])+';')]
                # res = [i for i in res if len(i["answer"]) > 3]
                if res != []:
                    res=res[0]
                    start_pos = res['start']
                    end_pos = res['end']

                    context = context[:start_pos] + context[end_pos:]
                    print(res)
                    # weight_file.write(species + "\t" + str(res["score"]) + "\t" +  res["answer"] + '\n')
                    with open(path_weight_file, "a") as file:
                        file.write(species + "\tweight\t" + str(res["score"]) + "\t" + convert_to_w2n(res["answer"]) + '\n')


        context = text
        print("Length")
        for i in range(0,10):
            if len(context) > 3:
                QA_input = {
                    'question': 'what is the body length?',
                    'context': context
                }
                res = nlp(QA_input, top_k=100)
                # res = [i for i in res if any(c.isalpha() for c in i["answer"])]
                res = [i for i in res if re.findall(pattern, i["answer"]+';') and re.findall(patternnumeric, convert_to_w2n(i["answer"])+';')]
                if res != []:
                    res=res[0]
                    start_pos = res['start']
                    end_pos = res['end']

                    context = context[:start_pos] + context[end_pos:]
                    print(res)
                    # length_file.write(species + "\t" + str(res["score"]) + "\t" +  res["answer"] + '\n')

                    with open(path_length_file, "a") as file:
                        file.write(species + "\tlength\t" + str(res["score"]) + "\t" + convert_to_w2n(res["answer"]) + '\n')



    physical_description_tag = soup.find('h3', id='lifespan_longevity')

    if physical_description_tag:
        text = ''
        next_element = physical_description_tag.next_sibling

        while next_element and next_element.name != 'section':
            if next_element.name:
                text += ' ' + next_element.get_text(separator='').replace('\n', ' ')
            next_element = next_element.next_sibling


        context = text
        print("Lifespan")
        for i in range(0,10):
            if len(context) > 3:
                QA_input = {
                    'question': 'what is the longevity?',
                    'context': context
                }
                res = nlp(QA_input, top_k=100)
                res = [i for i in res if re.findall(pattern, i["answer"]+';') and re.findall(patternnumeric, convert_to_w2n(i["answer"])+';')]
                # res = [i for i in res if any(c.isalpha() for c in i["answer"]) and ]
                if res != []:
                    res = res[0]
                    start_pos = res['start']
                    end_pos = res['end']

                    context = context[:start_pos] + context[end_pos:]
                    print(res)

                    with open(path_lifespan_file, "a") as file:
                        file.write(species + "\tlifespan\t" + str(res["score"]) + "\t" + convert_to_w2n(res["answer"]) + '\n')


    physical_description_tag = soup.find('h3', id='reproduction')

    if physical_description_tag:
        text = ''
        next_element = physical_description_tag.next_sibling

        while next_element and next_element.name != 'section':
            if next_element.name:
                text += ' ' + next_element.get_text(separator='').replace('\n', ' ')
            next_element = next_element.next_sibling


        context = text
        print("Lifespan")
        for i in range(0,10):
            if len(context) > 3:
                QA_input = {
                    'question': 'what is the longevity?',
                    'context': context
                }
                res = nlp(QA_input, top_k=100)
                res = [i for i in res if re.findall(pattern, i["answer"]+';') and re.findall(patternnumeric, convert_to_w2n(i["answer"])+';')]
                # res = [i for i in res if any(c.isalpha() for c in i["answer"]) and ]
                if res != []:
                    res = res[0]
                    start_pos = res['start']
                    end_pos = res['end']

                    context = context[:start_pos] + context[end_pos:]
                    print(res)

                    with open(path_lifespan_file, "a") as file:
                        file.write(species + "\treproduction\t" + str(res["score"]) + "\t" + convert_to_w2n(res["answer"]) + '\n')
