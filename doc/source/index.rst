PyDYNA documentation  |version|
===============================

.. note::
   The ``ansys.dyna.core.solvers`` module and `ansys.dyna.core.pre` are deprecated and no longer supported. 
   Please use the `ansys.dyna.core.keywords` module for all keywords and the `ansys.dyna.core.run` module for running solvers.

.. grid:: 1 2 3 3
   :gutter: 1 2 3 3
   :padding: 1 2 3 3

   .. grid-item-card:: :fa:`power-off` Getting started
      :link: getting-started
      :link-type: doc

      Learn how to install and use PyDYNA, including how to set up the
      environment and run the solver.

   .. grid-item-card:: :fa:`list-alt` User guide
      :link: user-guide/index
      :link-type: doc


      Understand how to use PyDYNA's features and capabilities,.

   .. jinja:: main_toctree

      {% if build_api %}
      .. grid-item-card:: :fa:`wrench` API reference
         :link: api/index
         :link-type: doc

         Explore the API reference documentation for PyDYNA, including
         detailed descriptions of classes, methods, and functions.

      {% endif %}
      
      {% if build_examples %}
      
      .. grid-item-card:: :fa:`clone` Examples
         :link: examples/index
         :link-type: doc

         Browse a collection of examples that demonstrate how to use PyDYNA
         for various applications.
      {% endif %}

   .. grid-item-card:: :fa:`user-group` Contribute
      :link: contributing
      :link-type: doc

      Find out how to contribute to the PyDYNA project, including how to
      report issues and submit code changes.

   .. grid-item-card:: :fa:`clipboard-list` Changelog
      :link: changelog
      :link-type: doc

      View the latest changes and updates to PyDYNA, including new features
      and bug fixes.



.. jinja:: main_toctree

    .. toctree::
       :hidden:
       :maxdepth: 3

       getting-started
       user-guide/index
       {% if build_api %}
       api/index
       {% endif %}
       {% if build_examples %}
       examples/index
       {% endif %}
       contributing
       changelog
