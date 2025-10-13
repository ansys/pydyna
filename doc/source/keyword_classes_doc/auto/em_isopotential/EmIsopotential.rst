





:class:`EmIsopotential`
=======================


.. py:class:: em_isopotential.EmIsopotential(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_ISOPOTENTIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmIsopotential

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~isoid`
            - Get or set the ID of the Isopotential.
          * - :py:attr:`~settype`
            - Get or set the Set type:
          * - :py:attr:`~setid`
            - Get or set the Set ID.
          * - :py:attr:`~rdltype`
            - Get or set the Used for the application: composite Tshell battery, with *EM_RANDLES_TSHELL.Selects which layers of the underlying EM mesh is included in the isopotential:


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

    from em_isopotential import EmIsopotential

Property detail
---------------

.. py:property:: isoid
   :type: Optional[int]


   
   Get or set the ID of the Isopotential.
















   ..
       !! processed by numpydoc !!

.. py:property:: settype
   :type: int


   
   Get or set the Set type:
   EQ.1:Segment Set.
   EQ.2:Node Set.
   EQ.3:Fluid surface part. See *ICFD_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdltype
   :type: int


   
   Get or set the Used for the application: composite Tshell battery, with *EM_RANDLES_TSHELL.Selects which layers of the underlying EM mesh is included in the isopotential:
   EQ.0: Default. No specific treatment.
   EQ.1: Current Collector Positive.
   EQ.2: Positive Electrode.
   EQ.3: Separator.
   EQ.4: Negative Electrode.
   EQ.5: Current Collector Negative.
   The layers functions are defined in *EM_MAT_001.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'ISOPOTENTIAL'






