





:class:`EmOutput`
=================


.. py:class:: em_output.EmOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mats`
            - Get or set the Level of matrix assembly output to the screen:
          * - :py:attr:`~matf`
            - Get or set the Level of matrix assembly output to the messag file:
          * - :py:attr:`~sols`
            - Get or set the Level of solver output on the screen:
          * - :py:attr:`~solf`
            - Get or set the Level of solver output to the messag file:
          * - :py:attr:`~mesh`
            - Get or set the Controls the output of the mesh data to the d3hsp file
          * - :py:attr:`~mem`
            - Get or set the Controls the output of information about the memory used by the EM solve to the messag file:
          * - :py:attr:`~timing`
            - Get or set the Controls the output of information about the time spent in the different parts of the EM solver to the messag file


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from em_output import EmOutput

Property detail
---------------

.. py:property:: mats
   :type: int


   
   Get or set the Level of matrix assembly output to the screen:
   EQ.0: No output
   EQ.1: Basic assembly steps
   EQ.2: Basic assembly steps+percentage completed+final statistics
   EQ.3: Basic assembly steps+percentage completed+statistics at each percentage of completion
















   ..
       !! processed by numpydoc !!

.. py:property:: matf
   :type: int


   
   Get or set the Level of matrix assembly output to the messag file:
   EQ.0: No output
   EQ.1: Basic assembly steps
   EQ.2: Basic assembly steps+percentage completed+final statistics
   EQ.3: Basic assembly steps+percentage completed+statistics at each percentage of completion
















   ..
       !! processed by numpydoc !!

.. py:property:: sols
   :type: int


   
   Get or set the Level of solver output on the screen:
   EQ.0: No output
   EQ.1: Global information at each FEM iteration
   EQ.2: Detailed information at each FEM iteration
















   ..
       !! processed by numpydoc !!

.. py:property:: solf
   :type: int


   
   Get or set the Level of solver output to the messag file:
   EQ.0: No output
   EQ.1: Global information at each FEM iteration
   EQ.2: Detailed information at each FEM iteration
















   ..
       !! processed by numpydoc !!

.. py:property:: mesh
   :type: int


   
   Get or set the Controls the output of the mesh data to the d3hsp file
   EQ.0: No mesh output
   EQ.1: Mesh info is written to the d3hsp file
















   ..
       !! processed by numpydoc !!

.. py:property:: mem
   :type: int


   
   Get or set the Controls the output of information about the memory used by the EM solve to the messag file:
   EQ. 0 : no memory information written.
   EQ .1  memory information written
















   ..
       !! processed by numpydoc !!

.. py:property:: timing
   :type: int


   
   Get or set the Controls the output of information about the time spent in the different parts of the EM solver to the messag file
   EQ. 0 : no timing information written.
   EQ. 1 : timing information written
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'OUTPUT'






