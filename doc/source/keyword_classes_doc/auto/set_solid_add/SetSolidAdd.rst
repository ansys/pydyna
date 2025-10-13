





:class:`SetSolidAdd`
====================


.. py:class:: set_solid_add.SetSolidAdd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SOLID_ADD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSolidAdd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Solid element set ID. All shell sets should have a unique set ID.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~dsid1`
            - Get or set the First solid set ID.
          * - :py:attr:`~dsid2`
            - Get or set the Second solid set ID.
          * - :py:attr:`~dsid3`
            - Get or set the Third solid set ID.
          * - :py:attr:`~dsid4`
            - Get or set the Fourth solid set ID.
          * - :py:attr:`~dsid5`
            - Get or set the Fifth solid set ID.
          * - :py:attr:`~dsid6`
            - Get or set the Sixth solid set ID.
          * - :py:attr:`~dsid7`
            - Get or set the Seventh solid set ID.
          * - :py:attr:`~dsid8`
            - Get or set the Eighth solid set ID.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from set_solid_add import SetSolidAdd

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Solid element set ID. All shell sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid1
   :type: Optional[int]


   
   Get or set the First solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid2
   :type: Optional[int]


   
   Get or set the Second solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid3
   :type: Optional[int]


   
   Get or set the Third solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid4
   :type: Optional[int]


   
   Get or set the Fourth solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid5
   :type: Optional[int]


   
   Get or set the Fifth solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid6
   :type: Optional[int]


   
   Get or set the Sixth solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid7
   :type: Optional[int]


   
   Get or set the Seventh solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid8
   :type: Optional[int]


   
   Get or set the Eighth solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SOLID_ADD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





