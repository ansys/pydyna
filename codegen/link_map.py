# This script requires ollama mistral model
# Download and install https://ollama.com/download, pull mistral model with `ollama pull mistral`

import json
import random
import ollama
from pydantic import BaseModel

dyna_structure_string = """LS-DYNA KEYWORD STRUCTURE
LS-DYNA input files consist of text blocks called keywords. Every keyword begins with an asterisk followed by the keyword name, for example *PART or *MAT_ELASTIC. The lines that follow contain values called fields. These fields are positional: their meaning is determined strictly by their position in the line and not by names written in the file. The LS-DYNA manual defines the meaning of each position.

FIELDS
Fields are comma-separated values placed after a keyword. Common field types include:
PID – Part ID
MID – Material ID
SECID – Section ID
NID – Node ID
NSID – Node Set ID
SID – Generic Set ID (used by *SET_PART, *SET_ELEMENT, *SET_SHELL, *SET_SEGMENT, etc.)
EID – Element ID
LCID – Load Curve ID
These field names never appear in the input file itself. They are implicit and must be interpreted based on their index in the line.

REFERENCES BETWEEN KEYWORDS
Many fields reference other keywords. A field referring to an ID links to the keyword that defines that ID. For example:
PID refers to an ID defined by a *PART keyword.
MID refers to a material defined by MAT_.
SECID refers to a section defined by SECTION_.
NSID refers to a set defined by *SET_NODE.
SID may refer to *SET_PART, *SET_ELEMENT, *SET_SHELL, or *SET_SEGMENT.
References are how the LS-DYNA input deck forms a connected model: parts reference materials and sections, sets group entities, and loads and constraints reference sets or nodes.

SIGN RULES FOR IDs
In many LS-DYNA keywords the sign of an ID changes the interpretation:
ID > 0 means a normal reference to a set or entity.
ID < 0 means the absolute value points to a single element or triggers an alternate behavior.
ID = 0 sometimes means “apply to the entire model” or “ignore reference.”
The exact behavior varies by keyword, but these rules appear repeatedly throughout the LS-DYNA language.

MINIMAL EXAMPLE
*MAT_ELASTIC
11, 210000.0, 0.3
This defines a material with MID = 11.
*SECTION_SHELL
1, 0.1
This defines a shell section with SECID = 1.
*PART
5, 1, 11
car_door
This defines a part with PID = 5.
Its SECID = 1 references the *SECTION_SHELL definition.
Its MID = 11 references the *MAT_ELASTIC definition.

SET EXAMPLE
*SET_PART
100
5, 6, 7
SID = 100 defines the set ID.
The values 5, 6, and 7 reference PIDs of parts defined in *PART keywords.

WHAT AN LLM MUST LEARN

Every keyword begins with *KEYWORD.
Fields are positional and their meaning depends on the keyword specification.
Many fields reference other keywords by ID.
Sets group IDs and use SID or NSID.
Sign rules modify how references work.
The LS-DYNA model is a graph where keywords connect through shared IDs."""

instruction_keywords = """Instructions:
The task is to find out to with field y in a ls-dyna keyword B the field x of a ls-dyna keyword A is referencing to.
Before you start with the task understand how keywords and field are structured in LS-DYNA KEYWORD STRUCTURE.

Later we provide help texts. They contain information of several keywords A. From this help texts you should figure out which keywords B are most likely referenced by the field x.

Select the potential keywords from the list of Available keywords.

Return a list containing the keywords which are most likely referenced in the format required.
"""

instruction_reference = """Instructions:
The task is to find out to with field y in a ls-dyna keyword B the field x of a ls-dyna keyword A is referencing to.
Before you start with the task understand how keywords and field are structured in LS-DYNA KEYWORD STRUCTURE.

Later we provide help texts. They contain information of several keywords A. From this help texts you should figure out which fields y of keywords B are most likely referenced by the field x.

For each keyword B we provide the possible fields. Pick the one that is most likely referenced.

Return a list containing the keywords and their corresponding fields which are likely referenced in the format required."""

class KeywordList(BaseModel):
    keywords: list[str]


class Reference(BaseModel):
    keywords: list[str]
    fields: list[str]
    conditions: list[str]


def get_available_keywords(kwd_json: dict) -> list[str]:
    """Extract all available keyword names from kwd.json."""
    return sorted(kwd_json.keys())


def get_fields_for_keyword(kwd_json: dict, keyword: str) -> list[str]:
    """Extract all field names for a given keyword."""
    fields = set()
    if keyword in kwd_json:
        for card in kwd_json[keyword]:
            for field in card['fields']:
                if 'name' in field:
                    fields.add(field['name'])
    return sorted(fields)


def sample_help_texts(help_strings: list[str], sample_size: int = 5) -> list[str]:
    random.seed(123)
    random_indexes = random.sample(range(len(help_strings)), min(sample_size, len(help_strings)))
    sampled_help = [help_strings[i] for i in random_indexes]
    return sampled_help

def extract_relevant_keywords(sampled_help: list[str], available_keywords: list[str]) -> Reference:
    
    # Step 1: Identify referenced keywords

    prompt_keywords = f""" Available keywords:
    {'\n'.join(available_keywords)}"""

    prompt_helptexts = f"""
    {'\n'.join(sampled_help)}"""

    promt = '\n'.join([instruction_keywords, dyna_structure_string, prompt_keywords, prompt_helptexts])
    print(promt)
    response_keywords = ollama.chat(
        model='deepseek-r1',
        messages=[{'role': 'user', 'content': promt}],
        format=KeywordList.model_json_schema(),
    )
    
    keyword_result = KeywordList.model_validate_json(response_keywords['message']['content'])
    print(f"Identified keywords: {keyword_result.keywords}")
    
    if not keyword_result.keywords:
        return []
    else:
        return keyword_result.keywords

def extract_relevant_fields(sampled_help: list[str], keyword_result: KeywordList, kwd_json: dict) -> Reference:
    feasible_fields = []
    for keyword in keyword_result.keywords:

        available_fields = get_fields_for_keyword(kwd_json, keyword)
        fields = f"""available fields for {keyword}: {','.join(available_fields)}"""
        feasible_fields.append(fields)

    prompt_fields = f""" feasible field candidates for each keyword:
    {'\n'.join(feasible_fields)}"""
    
    prompt_helptexts = f""" help texts:
    {'\n'.join(sampled_help)}"""

    promt = '\n'.join([instruction_reference, dyna_structure_string, prompt_fields, prompt_helptexts])
    print(promt)    
    response_fields = ollama.chat(
        model='deepseek-r1',
        messages=[{'role': 'user', 'content': promt}],
        format=Reference.model_json_schema(),
    )
    print(response_fields['message']['content'])
    field_result = Reference.model_validate_json(response_fields['message']['content'])

    return field_result

def get_links(filepath: str) -> dict:
    links = {}
    for key in kwd_json.keys():
        for card in kwd_json[key]:
            for field in card['fields']:
                if 'link' in field:
                    link = field['link']
                    help = field['help']

                    if link not in links:
                        links[link] = {
                            'description': [help]
                        }
                    else:
                        links[link]['description'].append(help)
                    #elif help not in links[link]['description']:
                    #    links[link]['description'].append(help)
                    #else:
                    #    continue
    return links




with open('codegen/kwd.json', 'r') as file:
    kwd_json = json.load(file)
links = get_links('codegen/kwd.json')
available_keywords = get_available_keywords(kwd_json)
print(len(available_keywords))
for index in [0]: #range(len(links)):
    help_texts = links[list(links.keys())[index]]['description']
    sampled_help = sample_help_texts(help_texts, sample_size=5)
    print(sampled_help)
    keyword_result = extract_relevant_keywords(sampled_help, available_keywords)
    if not keyword_result:
        print(f"No keywords found for link ID {list(links.keys())[index]}")
        continue
    field_result = extract_relevant_fields(sampled_help, keyword_result, kwd_json)
    field_result.write_json(f'codegen/output/reference_fields_{list(links.keys())[index]}.json', indent=4)
    print(f"Link ID: {list(links.keys())[index]}")
    print(f"Referenced Keywords: {keyword_result.keywords}")
    print(f"Referenced Fields: {field_result.fields}")
    print("=" * 120)