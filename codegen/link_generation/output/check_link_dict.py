import json
import pandas as pd

pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)


""""  "-3328": [
    {
      "source_keyword": "AIRBAG_HYBRID",
      "source_field": "A23",
      "referenced_keywords": [
        "PART",
        "SET_PART"
      ],
      "confidence": 0.95,
      "condition": {
        "field": "LCA23",
        "value_map": {
          "-1": "SET_PART",
          "NE.-1": "PART"
        },
        "confidence": 0.95
      }
    },
    {
      "source_keyword": "AIRBAG_HYBRID_ID",
      "source_field": "A23",
      "referenced_keywords": [
        "PART",
        "SET_PART"
      ],
      "confidence": 0.95,
      "condition": {
        "field": "LCA23",
        "value_map": {
          "-1": "SET_PART",
          "NE.-1": "PART"
        },
        "confidence": 0.95
      }
    }
  ],"""
def flatten_dict(data: dict) -> dict:
    flat_dict = {
        "link_id": [],
        "source_keyword": [],
        "source_field": [],
        "referenced_keywords": [],
        "confidence": [],
        "condition_field": [],
        "condition_value_map": [],
        "condition_confidence": []
    }

    for link_id, link_entries in data.items():
        for entry in link_entries:
            flat_dict["link_id"].append(link_id)
            flat_dict["source_keyword"].append(entry.get("source_keyword"))
            flat_dict["source_field"].append(entry.get("source_field"))
            flat_dict["referenced_keywords"].append(str(entry.get("referenced_keywords")))
            if "confidence" in entry:
                flat_dict["confidence"].append(entry.get("confidence"))
            else:
                flat_dict["confidence"].append(None)
            if "condition" in entry:
                condition = entry.get("condition", {})
                flat_dict["condition_field"].append(condition.get("field"))
                flat_dict["condition_value_map"].append(str(condition.get("value_map")))
                flat_dict["condition_confidence"].append(condition.get("confidence"))
            else:
                flat_dict["condition_field"].append(None)
                flat_dict["condition_value_map"].append(None)
                flat_dict["condition_confidence"].append(None)

    return flat_dict


with open('agent_reference_revised_manual.json') as f:
    data = json.load(f)

flat_data = flatten_dict(data)
link_df = pd.DataFrame(flat_data)

#link_df = link_df.drop(columns=['source_keyword', 'confidence', 'condition_confidence', 'condition_field', 'source_field'], inplace=False)


print(link_df.dtypes)
print(link_df.nunique())

# Find link_ids with more than one unique referenced_keywords or condition_value_map
grouped = link_df.groupby('link_id').agg({
    'referenced_keywords': pd.Series.nunique,
    'condition_value_map': pd.Series.nunique
})

# Filter for link_ids with >1 unique value in either column
inconsistent_link_ids = grouped[(grouped['referenced_keywords'] > 1) | (grouped['condition_value_map'] > 1)].index

# Show all rows for these inconsistent link_ids
inconsistent_links = link_df[link_df['link_id'].isin(inconsistent_link_ids)]

print(inconsistent_link_ids)


# handle inconsistent referenced keywords
for id in inconsistent_link_ids:
    inconsistent_refs = inconsistent_links[inconsistent_links['link_id']  == id]['referenced_keywords'].unique()
    for inconsistent_ref in inconsistent_refs:
        print(inconsistent_ref)
        print(inconsistent_links[(inconsistent_links['link_id'] == id) & 
                            (inconsistent_links['referenced_keywords'] == inconsistent_ref)][['link_id', 'source_keyword', 'source_field']])
        print('\n')
    print('*' *80, '\n')

# which links have the same link_id but different condition_value_maps
#print(link_df.columns)
#inconsistent_links = link_df[link_df.duplicated(subset=['link_id', 'condition_value_map'], keep=False)]
#print(inconsistent_links['link_id'].unique())
#print(inconsistent_links[inconsistent_links['link_id'] == '-1'])