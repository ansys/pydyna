





:class:`DefineSphDeCoupling`
============================


.. py:class:: define_sph_de_coupling.DefineSphDeCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_DE_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphDeCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Definition ID. This must be a unique number..
          * - :py:attr:`~heading`
            - Get or set the Definition descriptor. It is suggested that unique descriptions be       used.
          * - :py:attr:`~sphid`
            - Get or set the SPH part or part set ID.
          * - :py:attr:`~desid`
            - Get or set the DES part or part set ID
          * - :py:attr:`~sphtyp`
            - Get or set the SPH part type:
          * - :py:attr:`~destyp`
            - Get or set the DES part type:
          * - :py:attr:`~pfact`
            - Get or set the Penalty scale factor.
          * - :py:attr:`~dfact`
            - Get or set the Penalty scale factor for contact damping coefficient.
          * - :py:attr:`~sphbox`
            - Get or set the BOX ID for SPH parts, See Remarks.
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

    from define_sph_de_coupling import DefineSphDeCoupling

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Definition ID. This must be a unique number..
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Definition descriptor. It is suggested that unique descriptions be       used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sphid
   :type: Optional[int]


   
   Get or set the SPH part or part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: desid
   :type: Optional[int]


   
   Get or set the DES part or part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sphtyp
   :type: int


   
   Get or set the SPH part type:
   EQ.0: Part set ID,
   EQ.1: Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: destyp
   :type: int


   
   Get or set the DES part type:
   EQ.0: Part set ID,
   EQ.1: Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfact
   :type: float


   
   Get or set the Penalty scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfact
   :type: float


   
   Get or set the Penalty scale factor for contact damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: sphbox
   :type: Optional[int]


   
   Get or set the BOX ID for SPH parts, See Remarks.
















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
   :value: 'SPH_DE_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





