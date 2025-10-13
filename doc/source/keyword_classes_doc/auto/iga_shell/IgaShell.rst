





:class:`IgaShell`
=================


.. py:class:: iga_shell.IgaShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Isogeometric shell (patch) ID, see Remark 1 and Remark 2. A unique number must be chosen.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~nisr`
            - Get or set the Interpolation elements in the local r-direction, see Remark 3.
          * - :py:attr:`~niss`
            - Get or set the Interpolation elements in the local s-direction, see Remark 3.
          * - :py:attr:`~idfne`
            - Get or set the Element ID of first IGA-Element (knot span) within this isogeometric shell (patch) definition.


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

    from iga_shell import IgaShell

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Isogeometric shell (patch) ID, see Remark 1 and Remark 2. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nisr
   :type: float


   
   Get or set the Interpolation elements in the local r-direction, see Remark 3.
   LT.0.: ABS(NISR) is the average edge length of the interpolation elements in the local r - direction.
   EQ.0. : The number of interpolation elements per isogeometric element
   is equal to the polynomial degree in the local r - direction.
   GT.0. : NINT(NISR) is the number of interpolation elements per isogeometric element in the local r - direction..
















   ..
       !! processed by numpydoc !!

.. py:property:: niss
   :type: float


   
   Get or set the Interpolation elements in the local s-direction, see Remark 3.
   LT.0.: ABS(NISS) is the average edge length of the interpolation elements in the local s - direction.
   EQ.0. : The number of interpolation elements per isogeometric element
   is equal to the polynomial degree in the local s - direction.
   GT.0. : NINT(NISS) is the number of interpolation elements per
   isogeometric element in the local s - direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: idfne
   :type: int


   
   Get or set the Element ID of first IGA-Element (knot span) within this isogeometric shell (patch) definition.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'SHELL'






