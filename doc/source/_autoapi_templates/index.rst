API reference
=============

This section provides descriptions of PyDYNA subpackages, submodules, classes,
methods, and attributes. Use the search feature or click links to view API documentation.

.. toctree::
   :titlesonly:
   :maxdepth: 2

   {% for page in pages %}
   {% if (page.top_level_object or page.name.split('.') | length == 3) and page.display %}
   {{ page.include_path }}
   {% endif %}
   {% endfor %}
