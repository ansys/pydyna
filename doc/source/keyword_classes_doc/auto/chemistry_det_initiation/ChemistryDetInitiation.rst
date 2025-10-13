





:class:`ChemistryDetInitiation`
===============================


.. py:class:: chemistry_det_initiation.ChemistryDetInitiation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_DET_INITIATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryDetInitiation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this one-dimensional detonation computation.
          * - :py:attr:`~compid`
            - Get or set the Chemical composition identifier of composition to use.
          * - :py:attr:`~nmesh`
            - Get or set the Number of equal-width elements in the one-dimensional domain.
          * - :py:attr:`~dlen`
            - Get or set the Length of the one-dimensional domain.
          * - :py:attr:`~cfl`
            - Get or set the Time-step limiting factor.
          * - :py:attr:`~tlimit`
            - Get or set the Time limit for the simulation
          * - :py:attr:`~xyzd`
            - Get or set the Position of the detonation front in the DETDIR direction.
          * - :py:attr:`~detdir`
            - Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
          * - :py:attr:`~file`
            - Get or set the Name of the LSDA file in which to write the one-dimensional solution.


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

    from chemistry_det_initiation import ChemistryDetInitiation

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this one-dimensional detonation computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: compid
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of composition to use.
















   ..
       !! processed by numpydoc !!

.. py:property:: nmesh
   :type: Optional[int]


   
   Get or set the Number of equal-width elements in the one-dimensional domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlen
   :type: Optional[float]


   
   Get or set the Length of the one-dimensional domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: Optional[float]


   
   Get or set the Time-step limiting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlimit
   :type: Optional[float]


   
   Get or set the Time limit for the simulation
















   ..
       !! processed by numpydoc !!

.. py:property:: xyzd
   :type: Optional[float]


   
   Get or set the Position of the detonation front in the DETDIR direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: detdir
   :type: Optional[int]


   
   Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
















   ..
       !! processed by numpydoc !!

.. py:property:: file
   :type: Optional[str]


   
   Get or set the Name of the LSDA file in which to write the one-dimensional solution.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'DET_INITIATION'






