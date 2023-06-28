PyDyna documentation  |version|
===============================

.. include:: ../../README.rst
.. include:: ../../../docker/pre/README.rst


.. jinja:: main_toctree

    .. toctree::
       :hidden:

       getting-started/index
       user-guide/index
       {% if build_api %}
       autoapi/index
       {% endif %}
       contributing
       {% if build_examples %}
       examples/index
       {% endif %}
