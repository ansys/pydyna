





:class:`DatabaseProfile`
========================


.. py:class:: database_profile.DatabaseProfile(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_PROFILE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseProfile

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Interval time.
          * - :py:attr:`~sid`
            - Get or set the Set ID
          * - :py:attr:`~stype`
            - Get or set the Set type:
          * - :py:attr:`~data`
            - Get or set the Data type:
          * - :py:attr:`~dir`
            - Get or set the Direction:
          * - :py:attr:`~updloc`
            - Get or set the Flag to update the set location
          * - :py:attr:`~mmg`
            - Get or set the Multi-Material ALE group id. See Remark 2.


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

    from database_profile import DatabaseProfile

Property detail
---------------

.. py:property:: dt
   :type: Optional[int]


   
   Get or set the Interval time.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type:
   EQ.1:   Node Set,
   EQ.2:   Solid Set,
   EQ.3:   Shell Set,
   EQ.4:   Segment Set,
   EQ.5:   Beam Set.
   EQ.6:   tshell set
















   ..
       !! processed by numpydoc !!

.. py:property:: data
   :type: int


   
   Get or set the Data type:
   EQ.1:   x-velocity,
   EQ.2:   y-velocity,
   EQ.3:   z-velocity,
   EQ.4:   velocity magnitude,
   EQ.5:   x-acceleration,
   EQ.6:   y-acceleration,
   EQ.7:   z-acceleration,
   EQ.8:   acceleration magnitude,
   EQ.9:   pressure,
   EQ.10:  xx-stress,
   EQ.11:  yy-stress,
   EQ.12:  zz-stress,
   EQ.13:  xy-stress,
   EQ.14:  yz-stress,
   EQ.15:  zx-stress,
   EQ.16:  temperature,
   EQ.17:  volume fraction,
   EQ.18:  kinetic energy,
   EQ.19:  internal energy,
   EQ.20:  density.
   EQ.21:  xx-strain,
   EQ.22:  yy-strain,
   EQ.23:  zz-strain,
   EQ.24:  xy-strain,
   EQ.25:  yz-strain,
   EQ.26:  zx-strain.
   EQ.27:  effective plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction:
   EQ.1:   x-direction,
   EQ.2:   y-direction,
   EQ.3:   z-direction,
   EQ.4:   Curvilinear (relative distances between elements of set ID are added up in the order defined by the set)
















   ..
       !! processed by numpydoc !!

.. py:property:: updloc
   :type: int


   
   Get or set the Flag to update the set location
   EQ.0:   Only the initial position of set ID is considered
   EQ.1:   The positions of the elements composing the set are updated each DT
















   ..
       !! processed by numpydoc !!

.. py:property:: mmg
   :type: Optional[int]


   
   Get or set the Multi-Material ALE group id. See Remark 2.
   GT.0:   Multi-Material ALE group id
   LT.0:   |MMG| is the id of a *SET_MULTI-MATERIAL_GROUP_LIST that can list several Multi-Material ALE group ids.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'PROFILE'






