**Auto-Manifest: Manual-Driven Overrides**

Purpose: summarize how the LS-DYNA manuals should drive `manifest.json` overrides for codegen, provide exemplar keywords, and outline a lightweight extraction methodology and roadmap toward machine-assisted manifest generation.

**High-Level Plan**:
- **Map signals:** Detect series, conditional, duplicate, union, subtype, grouping and shared-field patterns in manuals and map them to codegen handlers.
- **Prototype extraction:** Parse a handful of exemplar keyword sections and emit manifest fragments.
- **Validate:** Regenerate auto classes for exemplars and run focused tests.
- **Scale:** Extend parsing rules and add lightweight NLP to increase coverage.

**Manual-Derived Signals (what to look for)**
- **Conditional cards:** Phrases like "if", "when", "only if", "for TYPE==..." → map to `conditional-card` with trigger field.
- **Series / variable-length:** Phrases like "repeat i=1..N", "length given by NIP/LMC/n" → map to `series-card` (control field = counter).
- **Table / curve rows:** "enter one row per", "pairs (x,y)" → `table-card` / `table-card-group`.
- **Duplicate blocks:** "duplicate of", repeated parameter blocks for axes/layers → `duplicate`/`skip-card` or `table-card-group` with offsets.
- **Union / alternatives:** "either A or B", "alternative form" → union-aware fields (shared-field / override-field).
- **Subtypes / families:** Suffixes (FINITE, MOVING, ORTHO) or material variants → `type: "multiple"` with `generations`.
- **Card sets & grouping:** Blocks labeled A–G, or repeated header+body blocks → `card-set`, `add-option`.
- **Shared/title cards & negative indices:** Title/ID option cards that apply across cards use negative indices in manifest; search for "same title applies".

**Representative Keywords & Expected Overrides**
- `SECTION_SHELL`: `reorder-card`, `series-card`, `conditional-card`, `table-card`, `card-set` — manuals show integration/count-driven repetition and material/formulation-conditioned cards.
- `SECTION_SOLID`: `conditional-card`, `table-card`, `series-card`, `override-field` — integration and LMC modes drive variable cards.
- `SECTION_TSHELL`: `series-card`, `conditional-card` — angle/integration series tied to counters.
- `CONTROL_IMPLICIT_EIGENVALUE`: `conditional-card`, `insert-card` — solver-mode-dependent cards.
- `DEFINE_TABLE` / `DEFINE_CURVE`: `table-card`, `series-card` — arbitrary-length table/curve rows.
- `MAT_077_O` / `MAT_077_H` (Ogden/hyperelastic): `series-card`, `override-field` — parameter counts driven by order `n`.
- `MAT_124`: `table-card` — material constants tables.
- `MAT_295`: `shared-field`, `duplicate`, `table-card-group` — union properties and anisotropic branches.
- `DEFINE_CONTACT_VOLUME` / `CONTACT_AUTOMATIC_SINGLE_SURFACE`: `conditional-card`, `add-option`, `skip-card`, `insert-card` — contact families have many option-driven sub-cards.
- `RIGIDWALL_*`: `type: "multiple"` generations per subtype.

**Lightweight `docx.d` Parsing Methodology**
1. Load `docx.d` structure and extract nodes: `headings`, `paragraphs`, `tables`, `styles`.
2. Identify keyword sections by heading patterns; normalize to manifest keys (strip stars, uppercase, replace punctuation with `_`).
3. For each section:
   - Extract contiguous tables immediately after headings as candidate card lines.
   - Scan nearby paragraphs for regex patterns indicating conditions, counters, repeats, or "either/or" language.
   - Detect card labels (A/B/C or Card 1/2) to form `card-set` or grouped blocks.
4. Translate signals to handler stanzas following handler order: `reorder-card` → `table-card` → `override-field` → `replace-card` → `insert-card` → `series-card` → `add-option` → `card-set` → `conditional-card` → `rename-property` → `skip-card` → `table-card-group` → `external-card-implementation` → `shared-field` → `override-subkeyword`.
5. Emit manifest fragment per keyword with references to source manual (file, heading, paragraph/table ids) for traceability.

**Validation Strategy**
- Regenerate auto classes for edited keywords using the codegen entry point and run focused tests: `test_series_card.py`, `test_duplicate_card_group.py`, `test_define_table_linking.py`, `test_contact_options.py`.
- Run linter/pre-commit: `pre-commit run --all-files`.
- Diff generated classes against baseline to detect regressions.

**Roadmap & Phases**
- Phase 1 (Pilot): Implement parser for Vol I Keyword and Vol II Materials for 6–8 exemplar keywords; emit manifest fragments and validate.
- Phase 2 (Scale): Expand parser to contact/manual families, add alias normalization and negative-index handling, implement robust field-name matching to kwd.json.
- Phase 3 (NLP): Add rule-based NLP to detect conditional phrases and union language; add confidence scoring and human-in-the-loop review UI.
- Phase 4 (Automation): Increase coverage and move to continuous manifest generation with selective manual vetting; integrate into codegen CI checks.

**Risks & Mitigations**
- Ambiguous manual wording → flag low-confidence extractions for manual review.
- Mis-mapping legacy names → maintain alias map and require human confirmation when no exact kwd.json match.
- Condition-to-card misassignment → validate that condition fields exist in kwd.json and run dry codegen to detect index errors.

**Next Steps**
- Prototype: parse `SECTION_SHELL` and `SECTION_BEAM` doc sections, emit manifest fragments, and run example codegen for quick validation.
- Track progress in the project TODO (use `manage_todo_list` entries already created).

---
File: `agents/projects/auto-manifest.md`
