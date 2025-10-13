





:class:`ElementBeamSource`
==========================


.. py:class:: element_beam_source.ElementBeamSource(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEAM_SOURCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBeamSource

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bsid`
            - Get or set the Beam Source ID.  A unique number has to be used.
          * - :py:attr:`~bsnid`
            - Get or set the Source node ID.
          * - :py:attr:`~bseid`
            - Get or set the Source element ID.
          * - :py:attr:`~nele`
            - Get or set the Number of elements to be pulled out.
          * - :py:attr:`~lfed`
            - Get or set the Beam element fed length.
          * - :py:attr:`~fpull`
            - Get or set the Pull-out force.
          * - :py:attr:`~lmin`
            - Get or set the Minimum beam element length, see notes below.


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

    from element_beam_source import ElementBeamSource

Property detail
---------------

.. py:property:: bsid
   :type: int


   
   Get or set the Beam Source ID.  A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsnid
   :type: int


   
   Get or set the Source node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: bseid
   :type: int


   
   Get or set the Source element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nele
   :type: int


   
   Get or set the Number of elements to be pulled out.
















   ..
       !! processed by numpydoc !!

.. py:property:: lfed
   :type: float


   
   Get or set the Beam element fed length.
















   ..
       !! processed by numpydoc !!

.. py:property:: fpull
   :type: float


   
   Get or set the Pull-out force.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmin
   :type: float


   
   Get or set the Minimum beam element length, see notes below.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEAM_SOURCE'






