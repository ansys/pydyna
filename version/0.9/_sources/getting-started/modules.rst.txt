Modules
=======
PyDYNA consists of four modules, ``ansys.dyna.core.pre``, ``ansys.dyna.core.solver``,
``ansys.dyna.core.run``, and ``ansys.dyna.core.keywords``.
learn about the different modules in PyDYNA.

.. grid:: 2 2 3 3
   :gutter: 3 3 4 4
   
   .. grid-item-card:: PyDYNA keywords
      :link: modules/keywords
      :link-type: doc
      
      Interact with LS-DYNA keywords using the ``keywords`` module.

   .. grid-item-card:: PyDYNA run
        :link: modules/run
        :link-type: doc

        Run LS-DYNA using the ``run`` module.

   .. grid-item-card:: PyDYNA server
        :link: modules/server
        :link-type: doc
    
        Run the LS-DYNA server locally or in a Docker container.

.. toctree::
    :hidden:
    :maxdepth: 2

    modules/keywords
    modules/run
    modules/server
