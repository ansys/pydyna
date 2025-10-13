





:class:`MatResultantAnisotropic`
================================


.. py:class:: mat_resultant_anisotropic.MatResultantAnisotropic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_RESULTANT_ANISOTROPIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatResultantAnisotropic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e11p`
            - Get or set the E11p, for in plane behavior.
          * - :py:attr:`~e22p`
            - Get or set the E11p, for in plane behavior.
          * - :py:attr:`~v12p`
            - Get or set the V12p, for in plane behavior.
          * - :py:attr:`~v21p`
            - Get or set the V21p, for in plane behavior.
          * - :py:attr:`~g12p`
            - Get or set the G12p, for in plane behavior.
          * - :py:attr:`~g23p`
            - Get or set the G23p, for in plane behavior.
          * - :py:attr:`~g31p`
            - Get or set the G31p, for in plane behavior.
          * - :py:attr:`~e11b`
            - Get or set the E11B, for in plane behavior.
          * - :py:attr:`~e22b`
            - Get or set the E11B, for in plane behavior.
          * - :py:attr:`~v12b`
            - Get or set the V12B, for in plane behavior.
          * - :py:attr:`~v21b`
            - Get or set the V21B, for in plane behavior.
          * - :py:attr:`~g12b`
            - Get or set the G12B, for in plane behavior.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~ln11`
            - Get or set the Yield curve ID for N11.
          * - :py:attr:`~ln22`
            - Get or set the Yield curve ID for N22.
          * - :py:attr:`~ln12`
            - Get or set the Yield curve ID for N12.
          * - :py:attr:`~lq1`
            - Get or set the Yield curve ID for Q1.
          * - :py:attr:`~lq2`
            - Get or set the Yield curve ID for Q2.
          * - :py:attr:`~lm11`
            - Get or set the Yield curve ID for M11.
          * - :py:attr:`~lm22`
            - Get or set the Yield curve ID for M22.
          * - :py:attr:`~lm12`
            - Get or set the Yield curve ID for M12.
          * - :py:attr:`~a1`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~a2`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~a3`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~v1`
            - Get or set the Define components of vector a for AOPT=3.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector a for AOPT=3.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector a for AOPT=3.
          * - :py:attr:`~d1`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~d2`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~d3`
            - Get or set the Define components of vector a for AOPT=2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overidden on the element card. see *ELEMENT_SHELL_BETA.
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

    from mat_resultant_anisotropic import MatResultantAnisotropic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e11p
   :type: Optional[float]


   
   Get or set the E11p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22p
   :type: Optional[float]


   
   Get or set the E11p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: v12p
   :type: Optional[float]


   
   Get or set the V12p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: v21p
   :type: Optional[float]


   
   Get or set the V21p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12p
   :type: Optional[float]


   
   Get or set the G12p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23p
   :type: Optional[float]


   
   Get or set the G23p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: g31p
   :type: Optional[float]


   
   Get or set the G31p, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: e11b
   :type: Optional[float]


   
   Get or set the E11B, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22b
   :type: Optional[float]


   
   Get or set the E11B, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: v12b
   :type: Optional[float]


   
   Get or set the V12B, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: v21b
   :type: Optional[float]


   
   Get or set the V21B, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12b
   :type: Optional[float]


   
   Get or set the G12B, for in plane behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: ln11
   :type: Optional[float]


   
   Get or set the Yield curve ID for N11.
















   ..
       !! processed by numpydoc !!

.. py:property:: ln22
   :type: Optional[float]


   
   Get or set the Yield curve ID for N22.
















   ..
       !! processed by numpydoc !!

.. py:property:: ln12
   :type: Optional[float]


   
   Get or set the Yield curve ID for N12.
















   ..
       !! processed by numpydoc !!

.. py:property:: lq1
   :type: Optional[float]


   
   Get or set the Yield curve ID for Q1.
















   ..
       !! processed by numpydoc !!

.. py:property:: lq2
   :type: Optional[float]


   
   Get or set the Yield curve ID for Q2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lm11
   :type: Optional[float]


   
   Get or set the Yield curve ID for M11.
















   ..
       !! processed by numpydoc !!

.. py:property:: lm22
   :type: Optional[float]


   
   Get or set the Yield curve ID for M22.
















   ..
       !! processed by numpydoc !!

.. py:property:: lm12
   :type: Optional[float]


   
   Get or set the Yield curve ID for M12.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overidden on the element card. see *ELEMENT_SHELL_BETA.
















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
   :value: 'RESULTANT_ANISOTROPIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





