





:class:`LoadSuperplasticForming`
================================


.. py:class:: load_superplastic_forming.LoadSuperplasticForming(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SUPERPLASTIC_FORMING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSuperplasticForming

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcp1`
            - Get or set the Load curve number for Phase I pressure loading, see *DEFINE_CURVE.
          * - :py:attr:`~csp1`
            - Get or set the Contact surface number to determine completion of Phase 1.
          * - :py:attr:`~ncp1`
            - Get or set the Percent of nodes in contact to terminate Phase I, see *CONTACT_OPTION.
          * - :py:attr:`~lcp2`
            - Get or set the Load curve number for Phase II pressure loading (reverse), see *DEFINE_ CURVE.
          * - :py:attr:`~csp2`
            - Get or set the Contact surface number to determine completion of Phase II, see *CONTACT_OPTION.
          * - :py:attr:`~ncp2`
            - Get or set the Percent of nodes in contact to terminate Phase II.
          * - :py:attr:`~erate`
            - Get or set the Desired strain rate. This is the time derivative of the logarithmic strain.
          * - :py:attr:`~scmin`
            - Get or set the Minimum allowable value for load curve scale factor. To maintain a constant strain rate the pressure curve is scaled. In the case of a snap through buckling the pressure may be removed completely. By putting a value here the pressure will continue to act but at a value given by this scale factor multiplying the pressure curve.
          * - :py:attr:`~scmax`
            - Get or set the Maximum allowable value for load curve scale factor. Generally, it is a good idea to put a value here to keep the pressure from going to unreasonable values after full contact has been attained. When full contact is achieved the strain rates will approach zero and pressure will go to infinity unless it is limited or the calculation terminates.
          * - :py:attr:`~ncyl`
            - Get or set the Number of cycles for monotonic pressure after reversal.


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

    from load_superplastic_forming import LoadSuperplasticForming

Property detail
---------------

.. py:property:: lcp1
   :type: Optional[int]


   
   Get or set the Load curve number for Phase I pressure loading, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: csp1
   :type: Optional[int]


   
   Get or set the Contact surface number to determine completion of Phase 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncp1
   :type: Optional[float]


   
   Get or set the Percent of nodes in contact to terminate Phase I, see *CONTACT_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcp2
   :type: Optional[int]


   
   Get or set the Load curve number for Phase II pressure loading (reverse), see *DEFINE_ CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: csp2
   :type: Optional[int]


   
   Get or set the Contact surface number to determine completion of Phase II, see *CONTACT_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncp2
   :type: Optional[float]


   
   Get or set the Percent of nodes in contact to terminate Phase II.
















   ..
       !! processed by numpydoc !!

.. py:property:: erate
   :type: Optional[float]


   
   Get or set the Desired strain rate. This is the time derivative of the logarithmic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: scmin
   :type: Optional[float]


   
   Get or set the Minimum allowable value for load curve scale factor. To maintain a constant strain rate the pressure curve is scaled. In the case of a snap through buckling the pressure may be removed completely. By putting a value here the pressure will continue to act but at a value given by this scale factor multiplying the pressure curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: scmax
   :type: Optional[float]


   
   Get or set the Maximum allowable value for load curve scale factor. Generally, it is a good idea to put a value here to keep the pressure from going to unreasonable values after full contact has been attained. When full contact is achieved the strain rates will approach zero and pressure will go to infinity unless it is limited or the calculation terminates.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyl
   :type: int


   
   Get or set the Number of cycles for monotonic pressure after reversal.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SUPERPLASTIC_FORMING'






