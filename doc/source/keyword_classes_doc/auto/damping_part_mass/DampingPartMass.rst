





:class:`DampingPartMass`
========================


.. py:class:: damping_part_mass.DampingPartMass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DAMPING_PART_MASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DampingPartMass

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID which specifies system damping for parts.
          * - :py:attr:`~sf`
            - Get or set the Scale factor for load curve. This allows a simple modification of the load curve values.
          * - :py:attr:`~flag`
            - Get or set the Set this flag to unity if the global components of the damping forces require separate scale factors.
          * - :py:attr:`~stx`
            - Get or set the Scale factor on global x translational damping forces.
          * - :py:attr:`~sty`
            - Get or set the Scale factor on global y translational damping forces.
          * - :py:attr:`~stz`
            - Get or set the Scale factor on global z translational damping forces.
          * - :py:attr:`~srx`
            - Get or set the Scale factor on global x rotational damping moments.
          * - :py:attr:`~sry`
            - Get or set the Scale factor on global y rotational damping moments.
          * - :py:attr:`~srz`
            - Get or set the Scale factor on global z rotational damping moments.


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

    from damping_part_mass import DampingPartMass

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID which specifies system damping for parts.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor for load curve. This allows a simple modification of the load curve values.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: int


   
   Get or set the Set this flag to unity if the global components of the damping forces require separate scale factors.
















   ..
       !! processed by numpydoc !!

.. py:property:: stx
   :type: float


   
   Get or set the Scale factor on global x translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: sty
   :type: float


   
   Get or set the Scale factor on global y translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: stz
   :type: float


   
   Get or set the Scale factor on global z translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: srx
   :type: float


   
   Get or set the Scale factor on global x rotational damping moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: sry
   :type: float


   
   Get or set the Scale factor on global y rotational damping moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: srz
   :type: float


   
   Get or set the Scale factor on global z rotational damping moments.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DAMPING'


.. py:attribute:: subkeyword
   :value: 'PART_MASS'






