API reference
=============

This section describes {{ project_name }} endpoints, their capabilities, and how
to interact with them programmatically.

.. toctree::
   :titlesonly:
   :maxdepth: 3

   {% for page in pages %}
   {% set length = autoapi_depth | int %}
   {% if (page.top_level_object or page.name.split('.') | length == length) and page.display %}
   <span class="nf nf-md-package"></span> {{ page.name }}<{{ page.include_path }}>
   {% endif %}
   {% endfor %}
   _autosummary/index