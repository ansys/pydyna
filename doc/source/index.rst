PyDyna documentation  |version|
===============================

.. include:: ../../README.rst


.. jinja:: main_toctree

    .. toctree::
       :hidden:

       Resources/Getting_Started
       Resources/User_Guide
       {% if build_api %}
       autoapi/index
       {% endif %}
       Resources/Contributing
       {% if build_examples %}
       examples/index
       {% endif %}
