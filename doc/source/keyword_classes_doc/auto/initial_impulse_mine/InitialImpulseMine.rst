





:class:`InitialImpulseMine`
===========================


.. py:class:: initial_impulse_mine.InitialImpulseMine(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_IMPULSE_MINE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialImpulseMine

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID
          * - :py:attr:`~mtnt`
            - Get or set the Equivalent mass of TNT
          * - :py:attr:`~rhos`
            - Get or set the Density of overburden soil
          * - :py:attr:`~depth`
            - Get or set the Burial depth from the ground surface to the center of the mine. This value must be a positive
          * - :py:attr:`~area`
            - Get or set the Cross sectional area of the mine
          * - :py:attr:`~scale`
            - Get or set the Scale factor for the impulse
          * - :py:attr:`~unit`
            - Get or set the Unit system. This must match the units used by finite element model.
          * - :py:attr:`~x`
            - Get or set the x- coordinates of mine center.
          * - :py:attr:`~y`
            - Get or set the y-coordinates of mine center.
          * - :py:attr:`~z`
            - Get or set the z- coordinates of mine center.
          * - :py:attr:`~nidmc`
            - Get or set the Optional node ID representing the mine center (see *NODE). If defined then X, Y and Z are ignored
          * - :py:attr:`~gvid`
            - Get or set the Vector ID representing the vertically upward direction, i.e., normal to the ground surface
          * - :py:attr:`~tbirth`
            - Get or set the Birth time. Impulse is activated at this time
          * - :py:attr:`~psid`
            - Get or set the Part set ID identifying the parts affected by the mine.  See *SET_‌PART.  If the segment set defined by SSID includes segments of more than one part, PSID may be used to load only segments of identified parts. Otherwise, if PSID is set to zero, the part affected by the mine defaults to the part comprised by the nodes of the segment set
          * - :py:attr:`~search`
            - Get or set the Limit the search depth into the structure. Initial nodal velocity is distributed from the segment to a depth equal to the SEARCH value. The


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

    from initial_impulse_mine import InitialImpulseMine

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: mtnt
   :type: float


   
   Get or set the Equivalent mass of TNT
















   ..
       !! processed by numpydoc !!

.. py:property:: rhos
   :type: float


   
   Get or set the Density of overburden soil
















   ..
       !! processed by numpydoc !!

.. py:property:: depth
   :type: float


   
   Get or set the Burial depth from the ground surface to the center of the mine. This value must be a positive
















   ..
       !! processed by numpydoc !!

.. py:property:: area
   :type: float


   
   Get or set the Cross sectional area of the mine
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the Scale factor for the impulse
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit system. This must match the units used by finite element model.
   EQ.1: inch, dozen slugs (i.e., lbf-s^2/in), second, psi (default)
   EQ.2: meter, kilogram, second, Pascal
   EQ.3: centimeter, gram, microsecond, megabar
   EQ.4: millimeter, kilogram, millisecond, GPa
   EQ.5: millimeter, metric ton, second, MPa
   EQ.6: millimeter, gram, millisecond, MPa
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x- coordinates of mine center.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-coordinates of mine center.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z- coordinates of mine center.
















   ..
       !! processed by numpydoc !!

.. py:property:: nidmc
   :type: int


   
   Get or set the Optional node ID representing the mine center (see *NODE). If defined then X, Y and Z are ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: gvid
   :type: Optional[int]


   
   Get or set the Vector ID representing the vertically upward direction, i.e., normal to the ground surface
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time. Impulse is activated at this time
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID identifying the parts affected by the mine.  See *SET_‌PART.  If the segment set defined by SSID includes segments of more than one part, PSID may be used to load only segments of identified parts. Otherwise, if PSID is set to zero, the part affected by the mine defaults to the part comprised by the nodes of the segment set
















   ..
       !! processed by numpydoc !!

.. py:property:: search
   :type: float


   
   Get or set the Limit the search depth into the structure. Initial nodal velocity is distributed from the segment to a depth equal to the SEARCH value. The
   value must be positive. If set to zero the search depth is unlimited and
   extends through the part(s) identified by PSID
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'IMPULSE_MINE'






