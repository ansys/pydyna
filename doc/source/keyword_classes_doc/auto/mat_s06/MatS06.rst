





:class:`MatS06`
===============


.. py:class:: mat_s06.MatS06(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_S06 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatS06

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~lcdl`
            - Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for loading;
          * - :py:attr:`~lcdu`
            - Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for unloading
          * - :py:attr:`~beta`
            - Get or set the Hardening parameter:
          * - :py:attr:`~tyi`
            - Get or set the Initial yield force in tension ( > 0).
          * - :py:attr:`~cyi`
            - Get or set the Initial yield force in compression ( < 0).
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

    from mat_s06 import MatS06

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdl
   :type: Optional[int]


   
   Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for loading;
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdu
   :type: Optional[int]


   
   Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for unloading
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Hardening parameter:
   EQ.0.0: tensile and compressive yield with strain softening (negative or zero slope allowed in the force versus disp. load curves),
   NE.0.0: kinematic hardening without strain softening,
   EQ.1.0: isotropic hardening without strain softening.
















   ..
       !! processed by numpydoc !!

.. py:property:: tyi
   :type: Optional[float]


   
   Get or set the Initial yield force in tension ( > 0).
















   ..
       !! processed by numpydoc !!

.. py:property:: cyi
   :type: Optional[float]


   
   Get or set the Initial yield force in compression ( < 0).
















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
   :value: 'S06'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





