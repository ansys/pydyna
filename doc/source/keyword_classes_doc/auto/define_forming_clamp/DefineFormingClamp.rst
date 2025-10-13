





:class:`DefineFormingClamp`
===========================


.. py:class:: define_forming_clamp.DefineFormingClamp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FORMING_CLAMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFormingClamp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~clp1`
            - Get or set the Part ID of a moving rigid body clamp, defined by *PART and *MAT_020 (*MAT_RIGID).
          * - :py:attr:`~clp2`
            - Get or set the Part ID of a fixed rigid body clamp, defined by *PART and *MAT_020.  This is sometimes called “net pad.
          * - :py:attr:`~vid`
            - Get or set the Define CLP1 moving direction:
          * - :py:attr:`~gap`
            - Get or set the Final desired distance between CLP1 and CLP2 at the end of clamping.
          * - :py:attr:`~at`
            - Get or set the Begin time for CLP1’s move.
          * - :py:attr:`~dt`
            - Get or set the Duration of CLP1’s move.
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

    from define_forming_clamp import DefineFormingClamp

Property detail
---------------

.. py:property:: clp1
   :type: Optional[int]


   
   Get or set the Part ID of a moving rigid body clamp, defined by *PART and *MAT_020 (*MAT_RIGID).
















   ..
       !! processed by numpydoc !!

.. py:property:: clp2
   :type: Optional[int]


   
   Get or set the Part ID of a fixed rigid body clamp, defined by *PART and *MAT_020.  This is sometimes called “net pad.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Define CLP1 moving direction:
   GT.0:   Vector ID from *DEFINE_VECTOR, specifying the moving direction of CLP1
   LT.0:   Absolute value is a node ID, whose normal vector will be used to define the moving direction of CLP1.
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: float


   
   Get or set the Final desired distance between CLP1 and CLP2 at the end of clamping.
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Begin time for CLP1’s move.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Duration of CLP1’s move.
















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
   :value: 'FORMING_CLAMP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





