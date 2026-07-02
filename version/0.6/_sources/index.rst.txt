PyDYNA documentation  |version|
===============================

.. include:: ../../README.rst
   :start-after: .. readme_start

.. jinja:: main_toctree

    .. toctree::
       :hidden:

       getting-started/index
       user-guide/index
       {% if build_api %}
       autoapi/index
       {% endif %}
       {% if build_examples %}
       examples/index
       {% endif %}
       contributing
       changelog
