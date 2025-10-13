





:class:`IcfdDefineWaveDamping`
==============================


.. py:class:: icfd_define_wave_damping.IcfdDefineWaveDamping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DEFINE_WAVE_DAMPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDefineWaveDamping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Point ID defining the start of the damping layer.
          * - :py:attr:`~nid`
            - Get or set the Normal ID defined using ICFD_DEFINE_POINT and pointing to the outgoing direction of the damping layer.
          * - :py:attr:`~l`
            - Get or set the Length of damping layer. If no is value specified, the damping layer will have a length corresponding to five element lengths
          * - :py:attr:`~f1`
            - Get or set the Linear and quadratic damping factor terms.
          * - :py:attr:`~f2`
            - Get or set the Linear and quadratic damping factor terms.
          * - :py:attr:`~n`
            - Get or set the Damping term factor.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID acting as temporal scale factor on damping term.


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

    from icfd_define_wave_damping import IcfdDefineWaveDamping

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Point ID defining the start of the damping layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Normal ID defined using ICFD_DEFINE_POINT and pointing to the outgoing direction of the damping layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Length of damping layer. If no is value specified, the damping layer will have a length corresponding to five element lengths
















   ..
       !! processed by numpydoc !!

.. py:property:: f1
   :type: Optional[float]


   
   Get or set the Linear and quadratic damping factor terms.
















   ..
       !! processed by numpydoc !!

.. py:property:: f2
   :type: Optional[float]


   
   Get or set the Linear and quadratic damping factor terms.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[int]


   
   Get or set the Damping term factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID acting as temporal scale factor on damping term.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DEFINE_WAVE_DAMPING'






