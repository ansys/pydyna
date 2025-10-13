





:class:`MatDamperNonlinearViscous`
==================================


.. py:class:: mat_damper_nonlinear_viscous.MatDamperNonlinearViscous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DAMPER_NONLINEAR_VISCOUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDamperNonlinearViscous

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~lcdr`
            - Get or set the Load curve identification describing force versus rate-of-displacement relationship or a moment versus rate-of-rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point (0,0).
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

    from mat_damper_nonlinear_viscous import MatDamperNonlinearViscous

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdr
   :type: Optional[int]


   
   Get or set the Load curve identification describing force versus rate-of-displacement relationship or a moment versus rate-of-rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point (0,0).
















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
   :value: 'DAMPER_NONLINEAR_VISCOUS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





