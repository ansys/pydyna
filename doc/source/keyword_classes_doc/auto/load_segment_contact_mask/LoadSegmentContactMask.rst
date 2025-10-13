





:class:`LoadSegmentContactMask`
===============================


.. py:class:: load_segment_contact_mask.LoadSegmentContactMask(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_CONTACT_MASK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentContactMask

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~lsid`
            - Get or set the Load set ID to mask, which must match a *LOAD_SEGMENT_SET.       See Remark 2.
          * - :py:attr:`~p1`
            - Get or set the Lower pressure limit. When the surface pressure due to contact is
          * - :py:attr:`~p2`
            - Get or set the Upper pressure limit. When the surface pressure due to contact is
          * - :py:attr:`~cid1`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid2`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid3`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid4`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid5`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid6`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid7`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
          * - :py:attr:`~cid8`
            - Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only


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

    from load_segment_contact_mask import LoadSegmentContactMask

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: lsid
   :type: Optional[int]


   
   Get or set the Load set ID to mask, which must match a *LOAD_SEGMENT_SET.       See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Lower pressure limit. When the surface pressure due to contact is
   below P1, no masking is done and the full load defined in *LOAD_SEGMENT_SET is applied.
   For pressures between P1 and P2 see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Upper pressure limit. When the surface pressure due to contact is
   above P2, no load is applied due to the *LOAD_SEGMENT_SET. For pressures between P1 and P2 see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid1
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid2
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid3
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid4
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid5
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid6
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid7
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid8
   :type: Optional[int]


   
   Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
   SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
   For TIEBREAK contacts, pressure is masked until the tie fails. Once     the tie fails, the full pressure will be applied for the remainder of the
   simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
   contact surface areas, are used to compute the contact pressure at      each node to determine any masking effect.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_CONTACT_MASK'






