





:class:`BoundaryAcousticImpedanceComplex`
=========================================


.. py:class:: boundary_acoustic_impedance_complex.BoundaryAcousticImpedanceComplex(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_IMPEDANCE_COMPLEX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticImpedanceComplex

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID of an acoustic surface.
          * - :py:attr:`~zr`
            - Get or set the Real part of the boundary impedance Zr.
          * - :py:attr:`~zi`
            - Get or set the Imaginary part of the boundary impedance Zi.
          * - :py:attr:`~lcidr`
            - Get or set the Frequency dependence of Zr.
          * - :py:attr:`~lcidi`
            - Get or set the Frequency dependence of Zi.


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

    from boundary_acoustic_impedance_complex import BoundaryAcousticImpedanceComplex

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID of an acoustic surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: zr
   :type: float


   
   Get or set the Real part of the boundary impedance Zr.
















   ..
       !! processed by numpydoc !!

.. py:property:: zi
   :type: float


   
   Get or set the Imaginary part of the boundary impedance Zi.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidr
   :type: int


   
   Get or set the Frequency dependence of Zr.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidi
   :type: int


   
   Get or set the Frequency dependence of Zi.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_IMPEDANCE_COMPLEX'






