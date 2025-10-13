





:class:`DefinePblastAirgeo`
===========================


.. py:class:: define_pblast_airgeo.DefinePblastAirgeo(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_PBLAST_AIRGEO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefinePblastAirgeo

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~gid`
            - Get or set the ID of a GEOMETRY defining initial air particle domain.
          * - :py:attr:`~gtype1`
            - Get or set the Geometry type
          * - :py:attr:`~gtype2`
            - Get or set the Geometry type
          * - :py:attr:`~xa`
            - Get or set the (XA, YA, ZA) defines a vector of the x-axis.
          * - :py:attr:`~ya`
            - Get or set the (XA, YA, ZA) defines a vector of the x-axis.
          * - :py:attr:`~za`
            - Get or set the (XA, YA, ZA) defines a vector of the x-axis.
          * - :py:attr:`~xb`
            - Get or set the (XB, YB, ZB) defines a vector of the y-axis.
          * - :py:attr:`~yb`
            - Get or set the (XB, YB, ZB) defines a vector of the y-axis.
          * - :py:attr:`~zb`
            - Get or set the (XB, YB, ZB) defines a vector of the y-axis.
          * - :py:attr:`~x0`
            - Get or set the Center coordinates of air domain.
          * - :py:attr:`~y0`
            - Get or set the Center coordinates of air domain.
          * - :py:attr:`~z0`
            - Get or set the Center coordinates of air domain.
          * - :py:attr:`~g1`
            - Get or set the Dimension value depending on GTYPE.
          * - :py:attr:`~g2`
            - Get or set the Dimension value depending on GTYPE.
          * - :py:attr:`~g3`
            - Get or set the Dimension value depending on GTYPE.
          * - :py:attr:`~xc`
            - Get or set the Center coordinates of domain excluded from the air domain.
          * - :py:attr:`~yc`
            - Get or set the Center coordinates of domain excluded from the air domain.
          * - :py:attr:`~zc`
            - Get or set the Center coordinates of domain excluded from the air domain.
          * - :py:attr:`~g4`
            - Get or set the See definition of G1, G2, G3.
          * - :py:attr:`~g5`
            - Get or set the See definition of G1, G2, G3.
          * - :py:attr:`~g6`
            - Get or set the See definition of G1, G2, G3.
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

    from define_pblast_airgeo import DefinePblastAirgeo

Property detail
---------------

.. py:property:: gid
   :type: int


   
   Get or set the ID of a GEOMETRY defining initial air particle domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: gtype1
   :type: int


   
   Get or set the Geometry type
   EQ.1: box
   EQ.2: sphere
   EQ.3: cylinder
   EQ.4: ellipsoid
   EQ.5: hemisphere (see Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: gtype2
   :type: int


   
   Get or set the Geometry type
   EQ.1: box
   EQ.2: sphere
   EQ.3: cylinder
   EQ.4: ellipsoid
   EQ.5: hemisphere (see Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: xa
   :type: float


   
   Get or set the (XA, YA, ZA) defines a vector of the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: ya
   :type: float


   
   Get or set the (XA, YA, ZA) defines a vector of the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: za
   :type: float


   
   Get or set the (XA, YA, ZA) defines a vector of the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xb
   :type: float


   
   Get or set the (XB, YB, ZB) defines a vector of the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: yb
   :type: float


   
   Get or set the (XB, YB, ZB) defines a vector of the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: zb
   :type: float


   
   Get or set the (XB, YB, ZB) defines a vector of the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: float


   
   Get or set the Center coordinates of air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: float


   
   Get or set the Center coordinates of air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: float


   
   Get or set the Center coordinates of air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: g1
   :type: float


   
   Get or set the Dimension value depending on GTYPE.
   GTYPE.EQ.1: length of x edge
   GTYPE.EQ.2: Radius of sphere
   GTYPE.EQ.3: Radius of cross section
   GTYPE.EQ.4: length of x-axes
   GTYPE.EQ.5: Radius of hemisphere.
















   ..
       !! processed by numpydoc !!

.. py:property:: g2
   :type: float


   
   Get or set the Dimension value depending on GTYPE.
   GTYPE.EQ.1: length of y edge
   GTYPE.EQ.3: length of cylinder
   GTYPE.EQ.4: length of y-axes.
















   ..
       !! processed by numpydoc !!

.. py:property:: g3
   :type: float


   
   Get or set the Dimension value depending on GTYPE.
   GTYPE.EQ.1: length of z edge
   GTYPE.EQ.4: length of z-axes.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the Center coordinates of domain excluded from the air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the Center coordinates of domain excluded from the air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the Center coordinates of domain excluded from the air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: g4
   :type: float


   
   Get or set the See definition of G1, G2, G3.
















   ..
       !! processed by numpydoc !!

.. py:property:: g5
   :type: float


   
   Get or set the See definition of G1, G2, G3.
















   ..
       !! processed by numpydoc !!

.. py:property:: g6
   :type: float


   
   Get or set the See definition of G1, G2, G3.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'PBLAST_AIRGEO'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





