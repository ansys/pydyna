





:class:`MatAddPzelectric`
=========================


.. py:class:: mat_add_pzelectric.MatAddPzelectric(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_PZELECTRIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddPzelectric

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID for which the piezoelectric properties apply
          * - :py:attr:`~dtype`
            - Get or set the Type of piezoelectric property definition (see remarks below)
          * - :py:attr:`~gpt`
            - Get or set the Number of Gauss points used for integration:
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
          * - :py:attr:`~dxx`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~dyy`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~dzz`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~dxy`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~dxz`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~dyz`
            - Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
          * - :py:attr:`~px11`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~px22`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~px33`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~px12`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~px13`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~px23`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~py11`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~py22`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~py33`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~py12`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~py23`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz11`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz22`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz33`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz12`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz13`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~pz23`
            - Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2
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

    from mat_add_pzelectric import MatAddPzelectric

Property detail
---------------

.. py:property:: mid
   :type: Optional[str]


   
   Get or set the Material ID for which the piezoelectric properties apply
















   ..
       !! processed by numpydoc !!

.. py:property:: dtype
   :type: str


   
   Get or set the Type of piezoelectric property definition (see remarks below)
   EQ.S:   stress based definition
   EQ.E : strain based definition
















   ..
       !! processed by numpydoc !!

.. py:property:: gpt
   :type: int


   
   Get or set the Number of Gauss points used for integration:
   EQ.0: Default value 8.full integration
   EQ.1:   reduced integration
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: int


   
   Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
   EQ.0.0: locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
   EQ.1.0 : locally orthotropic with material axes determined by a point in space and the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
















   ..
       !! processed by numpydoc !!

.. py:property:: dxx
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: dyy
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: dzz
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: dxy
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: dxz
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: dyz
   :type: Optional[float]


   
   Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
















   ..
       !! processed by numpydoc !!

.. py:property:: px11
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: px22
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: px33
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: px12
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: px13
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: px23
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: py11
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: py22
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: py33
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: py12
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: py23
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz11
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz22
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz33
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz12
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz13
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pz23
   :type: Optional[float]


   
   Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2
















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
   :value: 'ADD_PZELECTRIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





