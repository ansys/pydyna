"""Extract link type details from agent_reference_revised_manual.json for implementation."""
import json
import sys
from collections import defaultdict

def analyze_link_type(data, link_id):
    """Analyze a specific link type from the JSON."""
    entries = data.get(str(link_id), [])
    
    if not entries:
        print(f"Link type {link_id} not found in JSON")
        return
    
    # Gather statistics
    source_keywords = defaultdict(list)
    target_keywords = set()
    confidences = []
    
    for entry in entries:
        src = entry.get('source_keyword', 'unknown')
        src_field = entry.get('source_field', 'unknown')
        source_keywords[src].append(src_field)
        
        for target in entry.get('referenced_keywords', []):
            target_keywords.add(target)
        
        if 'confidence' in entry:
            confidences.append(entry['confidence'])
    
    # Print summary
    print(f"\n{'='*80}")
    print(f"LINK TYPE {link_id} ANALYSIS")
    print(f"{'='*80}")
    
    print(f"\nTotal references: {len(entries)}")
    print(f"Source keywords: {len(source_keywords)}")
    print(f"Target keywords: {sorted(target_keywords)}")
    print(f"Average confidence: {sum(confidences)/len(confidences):.3f}" if confidences else "N/A")
    
    # Primary target
    if target_keywords:
        primary = entries[0].get('referenced_keywords', ['?'])[0]
        print(f"Primary target: {primary}")
    
    # Show top source keywords
    print(f"\nTop 10 source keywords:")
    sorted_sources = sorted(source_keywords.items(), key=lambda x: len(x[1]), reverse=True)
    for kw, fields in sorted_sources[:10]:
        print(f"  {kw:50s} : {len(fields):3d} fields")
    
    # Show sample fields
    print(f"\nSample field names (first 20):")
    all_fields = set()
    for fields in source_keywords.values():
        all_fields.update(fields)
    
    for i, field in enumerate(sorted(all_fields)[:20], 1):
        print(f"  {i:2d}. {field}")
    
    # Show a few complete entries
    print(f"\nSample entries (first 3):")
    for i, entry in enumerate(entries[:3], 1):
        print(f"\n  {i}. {entry.get('source_keyword', '?')}.{entry.get('source_field', '?')}")
        print(f"     → {entry.get('referenced_keywords', [])}")
        print(f"     Confidence: {entry.get('confidence', 'N/A')}")
    
    # Check for patterns in field names
    print(f"\nField name patterns:")
    patterns = defaultdict(int)
    for fields in source_keywords.values():
        for field in fields:
            if 'ID' in field.upper():
                patterns['contains_ID'] += 1
            if 'SID' in field.upper():
                patterns['contains_SID'] += 1
            if 'PID' in field.upper():
                patterns['contains_PID'] += 1
            if field.upper().startswith('LC'):
                patterns['starts_with_LC'] += 1
    
    for pattern, count in sorted(patterns.items(), key=lambda x: x[1], reverse=True):
        print(f"  {pattern:20s}: {count:4d} occurrences")


def generate_codegen_snippet(data, link_id):
    """Generate code snippet for implementing this link type."""
    entries = data.get(str(link_id), [])
    if not entries:
        return
    
    targets = set()
    for entry in entries:
        targets.update(entry.get('referenced_keywords', []))
    
    primary_target = entries[0].get('referenced_keywords', ['UNKNOWN'])[0]
    
    print(f"\n{'='*80}")
    print(f"CODEGEN SNIPPET FOR LINK TYPE {link_id}")
    print(f"{'='*80}")
    
    # Suggest LinkType enum entry
    enum_name = primary_target.replace('*', '').replace('-', '_')
    print(f"\n# Add to LinkType enum in keyword_base.py:")
    print(f"{enum_name} = {link_id}")
    print(f'""\"Reference to a {primary_target} keyword.\"\"\"')
    
    # Suggest LinkIdentity constant
    print(f"\n# Add to LinkIdentity class in class_generator.py:")
    print(f"{enum_name} = {link_id}")
    
    # Suggest handler function
    func_name = f"_add_{enum_name.lower()}_link_data"
    print(f"\n# Add handler function in class_generator.py:")
    print(f"def {func_name}(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):")
    print(f'    ""\"Add link data for {primary_target} keywords.\"\"\"')
    
    if len(targets) == 1:
        # Simple link
        print(f"    link_data_dict = {{")
        print(f'        "classname": "{primary_target.replace("_", "")}",')
        print(f'        "modulename": "path.to.{primary_target.lower()}",  # TODO: verify')
        print(f'        "keyword_type": "{primary_target.split("_")[0]}",')
        subtype = "_".join(primary_target.split("_")[1:]) if "_" in primary_target else primary_target
        print(f'        "keyword_subtype": "{subtype}",')
        print(f'        "fields": _convert_link_fields_to_template_format(link_fields),')
        
        # Guess ID field name
        id_field = 'id'
        if 'CURVE' in primary_target:
            id_field = 'lcid'
        elif 'TABLE' in primary_target:
            id_field = 'tbid'
        elif 'FUNCTION' in primary_target:
            id_field = 'fid'
        elif 'CHAMBER' in primary_target or 'VENT' in primary_target:
            id_field = 'id'
        
        print(f'        "linkid": "{id_field}",  # TODO: verify')
        print(f'        "link_type_name": "{enum_name}",')
        print(f"    }}")
        print(f"    link_data.append(link_data_dict)")
    else:
        # Polymorphic
        print(f"    # Polymorphic link - targets: {targets}")
        print(f"    # TODO: Implement multi-target resolution")
    
    # Show where to add in _get_links
    print(f"\n# Add to _get_links() dict:")
    print(f"LinkIdentity.{enum_name}: [],")
    
    # Show where to add in _add_links
    print(f"\n# Add to _add_links() elif chain:")
    print(f"elif link_type == LinkIdentity.{enum_name}:")
    print(f"    {func_name}(link_data, link_fields)")
    print(f"    link_count += len(link_fields)")


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print("Usage: python extract_link_info.py <link_type_id> [link_type_id...]")
        print("\nExample: python extract_link_info.py 92 84 120")
        print("\nAnalyze specific link types from agent_reference_revised_manual.json")
        sys.exit(1)
    
    with open('agent_reference_revised_manual.json') as f:
        data = json.load(f)
    
    for link_id_str in sys.argv[1:]:
        try:
            link_id = int(link_id_str)
            analyze_link_type(data, link_id)
            generate_codegen_snippet(data, link_id)
        except ValueError:
            print(f"Invalid link type ID: {link_id_str}")


if __name__ == '__main__':
    main()
