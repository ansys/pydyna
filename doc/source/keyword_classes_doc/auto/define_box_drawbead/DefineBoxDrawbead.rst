





:class:`DefineBoxDrawbead`
==========================


.. py:class:: define_box_drawbead.DefineBoxDrawbead(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BOX_DRAWBEAD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBoxDrawbead

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~boxid`
            - Get or set the Box ID. Define unique numbers.
          * - :py:attr:`~pid`
            - Get or set the Part ID of blank.
          * - :py:attr:`~sid`
            - Get or set the set ID defining along the drawbead.
          * - :py:attr:`~idir`
            - Get or set the Direction of tooling movement:
          * - :py:attr:`~stype`
            - Get or set the Set type:
          * - :py:attr:`~radius`
            - Get or set the The radius of the tube, which is centered around the draw bead.  Elements of part ID, PID, that lie within the tube will be included in the contact.    If the radius is not defined, a rectangular box is used instead.  This option is recommended for curved draw beads and for draw beads that are not aligned with the global axes.
          * - :py:attr:`~cid`
            - Get or set the Optional coordinate system ID. This optional is only available for the tubular drawbead
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

    from define_box_drawbead import DefineBoxDrawbead

Property detail
---------------

.. py:property:: boxid
   :type: int


   
   Get or set the Box ID. Define unique numbers.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Part ID of blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: int


   
   Get or set the set ID defining along the drawbead.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: int


   
   Get or set the Direction of tooling movement:
   EQ.1: tooling moves in x-direction (default),
   EQ.2: tooling moves in y-direction,
   EQ.3: tooling moves in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type:
   EQ.2:  part set ID,
   EQ.3:  part ID,
   EQ.4:  node set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the The radius of the tube, which is centered around the draw bead.  Elements of part ID, PID, that lie within the tube will be included in the contact.    If the radius is not defined, a rectangular box is used instead.  This option is recommended for curved draw beads and for draw beads that are not aligned with the global axes.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Optional coordinate system ID. This optional is only available for the tubular drawbead
















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
   :value: 'BOX_DRAWBEAD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





