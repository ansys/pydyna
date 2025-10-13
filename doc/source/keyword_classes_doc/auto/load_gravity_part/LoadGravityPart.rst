





:class:`LoadGravityPart`
========================


.. py:class:: load_gravity_part.LoadGravityPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_GRAVITY_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadGravityPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for application of gravity load
          * - :py:attr:`~dof`
            - Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
          * - :py:attr:`~lc`
            - Get or set the Load curve defining factor vs. time (or zero if STGA, STGR are defined)
          * - :py:attr:`~accel`
            - Get or set the Acceleration (will be multiplied by factor from curve
          * - :py:attr:`~lcdr`
            - Get or set the Load curve defining factor vs. time during dynamic relaxation
          * - :py:attr:`~stga`
            - Get or set the Construction stage at which part is added (optional)
          * - :py:attr:`~stgr`
            - Get or set the Construction stage at which part is removed (optional)


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

    from load_gravity_part import LoadGravityPart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for application of gravity load
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: Optional[int]


   
   Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[int]


   
   Get or set the Load curve defining factor vs. time (or zero if STGA, STGR are defined)
















   ..
       !! processed by numpydoc !!

.. py:property:: accel
   :type: float


   
   Get or set the Acceleration (will be multiplied by factor from curve
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdr
   :type: Optional[int]


   
   Get or set the Load curve defining factor vs. time during dynamic relaxation
















   ..
       !! processed by numpydoc !!

.. py:property:: stga
   :type: int


   
   Get or set the Construction stage at which part is added (optional)
















   ..
       !! processed by numpydoc !!

.. py:property:: stgr
   :type: int


   
   Get or set the Construction stage at which part is removed (optional)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'GRAVITY_PART'






