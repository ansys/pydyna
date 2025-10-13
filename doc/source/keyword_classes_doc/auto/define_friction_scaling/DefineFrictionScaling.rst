





:class:`DefineFrictionScaling`
==============================


.. py:class:: define_friction_scaling.DefineFrictionScaling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FRICTION_SCALING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFrictionScaling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fsid`
            - Get or set the Friction scaling ID number.  Each friction scaling definition should have a unique ID which is used for output messages only.
          * - :py:attr:`~cid`
            - Get or set the Contact ID.  Optional input to limit friction scaling to one contact interface with this ID number.
          * - :py:attr:`~psid`
            - Get or set the Part set ID.  Optional input to limit friction scaling to parts in the set
          * - :py:attr:`~scale1`
            - Get or set the Friction scale factor for the inner surface of shell segments
          * - :py:attr:`~scaleo`
            - Get or set the Friction scale factor for the outer surface of shell segments
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

    from define_friction_scaling import DefineFrictionScaling

Property detail
---------------

.. py:property:: fsid
   :type: Optional[int]


   
   Get or set the Friction scaling ID number.  Each friction scaling definition should have a unique ID which is used for output messages only.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Contact ID.  Optional input to limit friction scaling to one contact interface with this ID number.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID.  Optional input to limit friction scaling to parts in the set
















   ..
       !! processed by numpydoc !!

.. py:property:: scale1
   :type: float


   
   Get or set the Friction scale factor for the inner surface of shell segments
















   ..
       !! processed by numpydoc !!

.. py:property:: scaleo
   :type: float


   
   Get or set the Friction scale factor for the outer surface of shell segments
















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
   :value: 'FRICTION_SCALING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





