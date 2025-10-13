





:class:`MatS07`
===============


.. py:class:: mat_s07.MatS07(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_S07 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatS07

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~k0`
            - Get or set the K0, short time stiffness.
          * - :py:attr:`~ki`
            - Get or set the K-infinity, long time stiffness.
          * - :py:attr:`~beta`
            - Get or set the Decay parameter.
          * - :py:attr:`~tc`
            - Get or set the Cut off time. After this time a constant force/moment is transmitted.
          * - :py:attr:`~fc`
            - Get or set the Force/moment after cutoff time
          * - :py:attr:`~copt`
            - Get or set the Time implementation option:
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

    from mat_s07 import MatS07

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: k0
   :type: Optional[float]


   
   Get or set the K0, short time stiffness.
















   ..
       !! processed by numpydoc !!

.. py:property:: ki
   :type: Optional[float]


   
   Get or set the K-infinity, long time stiffness.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Decay parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: float


   
   Get or set the Cut off time. After this time a constant force/moment is transmitted.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Force/moment after cutoff time
















   ..
       !! processed by numpydoc !!

.. py:property:: copt
   :type: Optional[float]


   
   Get or set the Time implementation option:
   EQ.0.0: incremental time change (default),
   NE.0: continuous time change.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'S07'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





