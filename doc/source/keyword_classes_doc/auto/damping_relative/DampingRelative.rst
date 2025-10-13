





:class:`DampingRelative`
========================


.. py:class:: damping_relative.DampingRelative(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DAMPING_RELATIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DampingRelative

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cdamp`
            - Get or set the Fraction of critical damping.
          * - :py:attr:`~freq`
            - Get or set the Frequency at which CDAMP is to apply (cycles per unit time, e.g., Hz if time unit is seconds).
          * - :py:attr:`~pidrb`
            - Get or set the Part ID of rigid body, see *PART. Motion relative to this rigid body will be damped.
          * - :py:attr:`~psid`
            - Get or set the Part set ID. The requested damping is applied only to the parts in the set.
          * - :py:attr:`~dv2`
            - Get or set the Optional constant for velocity-squared term
          * - :py:attr:`~lcid`
            - Get or set the ID of curve that defines fraction of critical damping vs. time. CDAMP will be ignored if LCID is non-zero.


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

    from damping_relative import DampingRelative

Property detail
---------------

.. py:property:: cdamp
   :type: float


   
   Get or set the Fraction of critical damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: freq
   :type: float


   
   Get or set the Frequency at which CDAMP is to apply (cycles per unit time, e.g., Hz if time unit is seconds).
















   ..
       !! processed by numpydoc !!

.. py:property:: pidrb
   :type: int


   
   Get or set the Part ID of rigid body, see *PART. Motion relative to this rigid body will be damped.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID. The requested damping is applied only to the parts in the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: dv2
   :type: float


   
   Get or set the Optional constant for velocity-squared term
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the ID of curve that defines fraction of critical damping vs. time. CDAMP will be ignored if LCID is non-zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DAMPING'


.. py:attribute:: subkeyword
   :value: 'RELATIVE'






