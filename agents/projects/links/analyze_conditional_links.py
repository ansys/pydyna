"""Analyze conditional link patterns from agent_reference_revised_manual.json."""
import json
from collections import defaultdict

with open('agent_reference_revised_manual.json') as f:
    data = json.load(f)

conditional_entries = data.get('-1', [])

print("=" * 90)
print("CONDITIONAL LINKS ANALYSIS")
print("=" * 90)
print(f"\nTotal conditional link entries: {len(conditional_entries)}")

# Group by control field
by_control_field = defaultdict(list)
for entry in conditional_entries:
    cond = entry.get('condition', {})
    field = cond.get('field', 'unknown')
    by_control_field[field].append(entry)

print(f"\nControl fields used: {len(by_control_field)}")
for field, entries in sorted(by_control_field.items(), key=lambda x: len(x[1]), reverse=True):
    print(f"  {field:20s}: {len(entries):4d} entries")

# Analyze the main control field (SIDTYP)
if 'SIDTYP' in by_control_field:
    sidtyp_entries = by_control_field['SIDTYP']
    
    print(f"\n{'='*90}")
    print(f"SIDTYP CONTROL FIELD (Primary Conditional Pattern)")
    print(f"{'='*90}")
    print(f"\nTotal entries: {len(sidtyp_entries)}")
    
    # Collect unique value maps
    value_maps = defaultdict(int)
    target_sets = defaultdict(set)
    
    for entry in sidtyp_entries:
        cond = entry.get('condition', {})
        vmap = cond.get('value_map', {})
        
        # Create a hashable representation
        vmap_str = str(sorted(vmap.items()))
        value_maps[vmap_str] += 1
        
        # Track what targets appear for each value
        for value, target in vmap.items():
            target_sets[value].add(target)
    
    print(f"\nUnique value map patterns: {len(value_maps)}")
    print("\nTop 5 value map patterns:")
    for i, (vmap_str, count) in enumerate(sorted(value_maps.items(), 
                                                  key=lambda x: x[1], 
                                                  reverse=True)[:5], 1):
        print(f"\n{i}. Used {count} times:")
        # Parse back the pattern
        vmap_items = eval(vmap_str)  # Safe here since we created it
        for value, target in vmap_items:
            print(f"   {value:10s} → {target}")
    
    print(f"\n\nSIDTYP value interpretation (consolidated):")
    for value in sorted(target_sets.keys(), key=lambda x: (x != '0', x)):
        targets = sorted(target_sets[value])
        if len(targets) == 1:
            print(f"  {value:10s} → {targets[0]}")
        else:
            print(f"  {value:10s} → {' or '.join(targets)}")
    
    # Show sample source keywords using SIDTYP
    print(f"\n\nSample keywords using SIDTYP control:")
    source_kws = set()
    for entry in sidtyp_entries[:20]:
        source_kws.add(entry.get('source_keyword', 'unknown'))
    
    for kw in sorted(source_kws)[:10]:
        print(f"  - {kw}")

# Analyze other control fields
print(f"\n{'='*90}")
print(f"OTHER CONTROL FIELD PATTERNS")
print(f"{'='*90}")

for field in sorted(by_control_field.keys()):
    if field == 'SIDTYP' or field == 'unknown':
        continue
    
    entries = by_control_field[field]
    print(f"\n{field} ({len(entries)} entries):")
    
    # Get sample value map
    if entries:
        sample = entries[0]
        cond = sample.get('condition', {})
        vmap = cond.get('value_map', {})
        
        print(f"  Sample source: {sample.get('source_keyword', '?')}.{sample.get('source_field', '?')}")
        print(f"  Value map:")
        for value, target in sorted(vmap.items())[:5]:
            print(f"    {value:10s} → {target}")
        
        if len(vmap) > 5:
            print(f"    ... ({len(vmap) - 5} more values)")

# Implementation recommendation
print(f"\n{'='*90}")
print(f"IMPLEMENTATION RECOMMENDATIONS")
print(f"{'='*90}")

print("""
1. Create ConditionalLink metadata class:
   ```python
   @dataclass
   class ConditionalLink:
       control_field: str
       value_map: Dict[str, LinkType]
       default: Optional[LinkType] = None
   ```

2. Extend _link_fields to support ConditionalLink:
   ```python
   _link_fields = {
       "sid": ConditionalLink(
           control_field="sidtyp",
           value_map={
               "0": LinkType.SET_SEGMENT,
               "1": LinkType.SET_SHELL,
               "2": LinkType.SET_PART,
               "3": LinkType.PART,
               "4": LinkType.SET_NODE,
               # ... etc
           }
       ),
   }
   ```

3. Update property generation in templates to check for conditional:
   - If simple link: generate normal property
   - If conditional: generate property that evaluates control field

4. Extend Deck.get_link() to handle conditional resolution

5. Generate conditional links from agent_reference_revised_manual.json
   during codegen process
""")

print(f"\nMost common SIDTYP value map (for reference):")
print("""
SIDTYP Value Map (Standard Pattern):
  0  → SET_SEGMENT       (segment set)
  1  → SET_SHELL         (shell element set)  
  2  → SET_PART          (part set)
  3  → PART              (single part)
  4  → SET_NODE          (node set)
  5  → ALL or PART       (include all or part)
  6  → SET_PART          (exempted part set)
  7  → SET_PART_TREE     (part tree branch)
""")
