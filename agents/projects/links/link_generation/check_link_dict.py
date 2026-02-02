# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import json
import pandas as pd

pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)

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


print(link_df.dtypes)
print(link_df.nunique())

# Find link_ids with more than one unique referenced_keywords or condition_value_map
grouped = link_df.groupby('link_id').agg({
    'referenced_keywords': pd.Series.nunique,
    'condition_value_map': pd.Series.nunique
})


inconsistent_link_ids = grouped[(grouped['referenced_keywords'] > 1) | (grouped['condition_value_map'] > 1)].index


inconsistent_links = link_df[link_df['link_id'].isin(inconsistent_link_ids)]

print(inconsistent_link_ids)


# handle / print inconsistent referenced keywords
for id in inconsistent_link_ids:
    inconsistent_refs = inconsistent_links[inconsistent_links['link_id']  == id]['referenced_keywords'].unique()
    for inconsistent_ref in inconsistent_refs:
        print(inconsistent_ref)
        print(inconsistent_links[(inconsistent_links['link_id'] == id) & 
                            (inconsistent_links['referenced_keywords'] == inconsistent_ref)][['link_id', 'source_keyword', 'source_field']])
        print('\n')
    print('*' *80, '\n')