# Auto-keyword class generator system

## What it is
The PyDyna auto-keyword class generator system generates python classes for
dyna keywords based on specifications in kwd.json, manifest.json, and additional-cards.json
It is implemented in `codegen/generate.py` and has a command line interface.

## To use
It is recommended to use a virtual environment

- Install dependencies:
``pip install .[codegen]``

- To run the code generation system for all classes:
``python codegen/generate.py -o /path/to/keyword_classes``

- To run the code generation system for a single keyword, e.g. SECTION_SHELL:
``python codegen/generate.py -k SECTION_SHELL``

- To remove all the generated code:
``python codegen/generate.py -c``

## How it works
The class generator uses Jinja templates to generate three distinct things:
- Python classes
- Import machinery
- keyword to type mapping

The python classes are what users of PyDyna interact with directly. The import machinery produces
`auto_keywords.py`, which contains a list of import statements that import classes from the
Python files where they are defined. The keyword to type mapping produces a dictionary mapping the
keyword name witht he python class that defines it.

The primary specification for keywords is found in `kwd.json`. It contains basic definitions
for most keywords, including their cards and fields (which are defined by offset, name, default
value, option, width, and helpstring). `kwd.json` must not be modified by hand, it is
produced by machine in a process that is external to PyDyna.

While this specification is expansive, providing definitions for thousands of keywords, not all
information pertaining to a keyword can be found there. In addition, there are some errors in that
specification. Due to this, `manifest.json` and `additional-cards.json` contain that information
that either supplements or corrects the information in `kwd.json`.

Corrections include:
    - fixing the order of cards ("reorder-card")
    - skipping an unnecessary card ("skip-card")
    - changing the name of a subkeyword ("override-subkeyword")
    - changing the definition of a field ("override-field")
    - replacing a card ("replace-card")
    - inserting a card ("insert-card")
    - changing the name of a python property ("rename-property")

Supplements include:
    - adding aliases - see `Appendix A - Aliasing`
    - cards that repeat, handled as a two dimensional table ("table-card")
    - A field represented as a one-dimensional array that repeat across cards ("variable-card")
    - A set of adjacent cards with their own specification. These may repeat ("card-set")
    - A card that is only active under a condition ("conditional-card")
    - A set of adjacent cards that repeat, handled as a two dimensional table ("table-card-group")
    - Adding option cards ("add-option")
    - A field shared across multiple cards with only one meaning ("shared-field")


## Appendix A

### Aliasing

In some cases, two keywords are defined in exactly the same way and have the same meaning. This is called
an alias. Examples of this are `MAT_058` and `MAT_LAMINATED_COMPOSITE_FABRIC`. In such cases, the class
generator will generate two classes, but one of the classes will alias the behavior of the other, the only
difference being the name of the keyword. In the case of `MAT_058` and its alias, both keywords are defined
in `kwd.json`, so one of them will be ignored by the code generator. It is possible for only one of the two
keywords to be defined in `kwd.json`, such as is the case for `SET_NODE` and `SET_NODE_LIST`. In that case,
the class generator will produce the same effect, except that it does not need to ignore anything in
`kwd.json`.

