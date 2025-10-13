





:class:`InitialStrainShellSet`
==============================


.. py:class:: initial_strain_shell_set.InitialStrainShellSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRAIN_SHELL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStrainShellSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the shell element set ID.
          * - :py:attr:`~large`
            - Get or set the Large format flag:
          * - :py:attr:`~ilocal`
            - Get or set the Flag for coordinate system of strain components:
          * - :py:attr:`~epsxx`
            - Get or set the Define the xx strain component at inner integration (global cartesian system).
          * - :py:attr:`~epsyy`
            - Get or set the Define the yy strain component at inner integration (global cartesian system).
          * - :py:attr:`~epszz`
            - Get or set the Define the zz strain component at inner integration (global cartesian system).
          * - :py:attr:`~epsxy`
            - Get or set the Define the xy strain component at inner integration (global cartesian system).
          * - :py:attr:`~epsyz`
            - Get or set the Define the yz strain component at inner integration (global cartesian system).
          * - :py:attr:`~epszx`
            - Get or set the Define the zx strain component at inner integration (global cartesian system).
          * - :py:attr:`~t`
            - Get or set the Parametric coordinate of through thickness integration point between -1and 1 inclusive.


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

    from initial_strain_shell_set import InitialStrainShellSet

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the shell element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Large format flag:
   EQ.0:   off
   EQ.1 : on.Each strain field is twice as long for higher precision.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilocal
   :type: int


   
   Get or set the Flag for coordinate system of strain components:
   EQ.0:   global,
   EQ.1 : local(not supported).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsxx
   :type: float


   
   Get or set the Define the xx strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsyy
   :type: float


   
   Get or set the Define the yy strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: epszz
   :type: float


   
   Get or set the Define the zz strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsxy
   :type: float


   
   Get or set the Define the xy strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsyz
   :type: float


   
   Get or set the Define the yz strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: epszx
   :type: float


   
   Get or set the Define the zx strain component at inner integration (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the Parametric coordinate of through thickness integration point between -1and 1 inclusive.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRAIN_SHELL_SET'






