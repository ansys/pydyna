





:class:`BoundarySphNonReflecting`
=================================


.. py:class:: boundary_sph_non_reflecting.BoundarySphNonReflecting(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SPH_NON_REFLECTING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySphNonReflecting

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vtx`
            - Get or set the x-coordinate of tail of a normal vector originating on the wall
          * - :py:attr:`~vty`
            - Get or set the y-coordinate of tail.
          * - :py:attr:`~vtz`
            - Get or set the z-coordinate of tail.
          * - :py:attr:`~vhx`
            - Get or set the x-coordinate of head.
          * - :py:attr:`~vhy`
            - Get or set the y-coordinate of head.
          * - :py:attr:`~vhz`
            - Get or set the z-coordinate of head.


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

    from boundary_sph_non_reflecting import BoundarySphNonReflecting

Property detail
---------------

.. py:property:: vtx
   :type: Optional[float]


   
   Get or set the x-coordinate of tail of a normal vector originating on the wall
   (tail) and terminating in the body (head); that is, the vector points
   from the non-reflecting boundary plane to the body.
















   ..
       !! processed by numpydoc !!

.. py:property:: vty
   :type: Optional[float]


   
   Get or set the y-coordinate of tail.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtz
   :type: Optional[float]


   
   Get or set the z-coordinate of tail.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhx
   :type: Optional[float]


   
   Get or set the x-coordinate of head.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhy
   :type: Optional[float]


   
   Get or set the y-coordinate of head.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhz
   :type: Optional[float]


   
   Get or set the z-coordinate of head.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SPH_NON_REFLECTING'






