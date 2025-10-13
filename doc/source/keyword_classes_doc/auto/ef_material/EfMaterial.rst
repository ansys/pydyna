





:class:`EfMaterial`
===================


.. py:class:: ef_material.EfMaterial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EF_MATERIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EfMaterial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nmat`
            - Get or set the Specifies the material ID, of the exchange factor material.
          * - :py:attr:`~name`
            - Get or set the Specifies the material’s name.  This parameter is used only to make the output file easier to read.
          * - :py:attr:`~mtyp`
            - Get or set the Specifies if and how emission occurs:
          * - :py:attr:`~exe`
            - Get or set the Specifies the x component of emission for a type 1 material
          * - :py:attr:`~eye`
            - Get or set the Specifies the y component of emission for a type 1 material
          * - :py:attr:`~eze`
            - Get or set the Specifies the z component of emission for a type 1 material
          * - :py:attr:`~rhos`
            - Get or set the Specifies the specular reflectance
          * - :py:attr:`~rhod`
            - Get or set the Specifies the diffuse reflectance
          * - :py:attr:`~taus`
            - Get or set the Specifies the specular transmittance.
          * - :py:attr:`~taud`
            - Get or set the Specifies the diffuse transmittance.
          * - :py:attr:`~rdiffr`
            - Get or set the LS-DYNA simulates diffuse reflection according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFR.
          * - :py:attr:`~rdifft`
            - Get or set the LS-DYNA simulates diffuse transmittance according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFT.


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

    from ef_material import EfMaterial

Property detail
---------------

.. py:property:: nmat
   :type: Optional[int]


   
   Get or set the Specifies the material ID, of the exchange factor material.
















   ..
       !! processed by numpydoc !!

.. py:property:: name
   :type: Optional[str]


   
   Get or set the Specifies the material’s name.  This parameter is used only to make the output file easier to read.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtyp
   :type: Optional[int]


   
   Get or set the Specifies if and how emission occurs:
   EQ. - 2:        There is to be no emission and F_ij = 1 is written to the output file for this surface.
   EQ. - 1 : There is to be no emission and F_ij = 0 is written to the output file for this surface.
   EQ.0 : Emission is to be distributed in θ according to :ε(θ) = cos ^ r(θ)
   EQ.1 : Beam emission is to occur in the direction {E_X,E_Y,E_Z}
   EQ.2 : This specifies that emission according to user specified function.
















   ..
       !! processed by numpydoc !!

.. py:property:: exe
   :type: Optional[float]


   
   Get or set the Specifies the x component of emission for a type 1 material
















   ..
       !! processed by numpydoc !!

.. py:property:: eye
   :type: Optional[float]


   
   Get or set the Specifies the y component of emission for a type 1 material
















   ..
       !! processed by numpydoc !!

.. py:property:: eze
   :type: Optional[float]


   
   Get or set the Specifies the z component of emission for a type 1 material
















   ..
       !! processed by numpydoc !!

.. py:property:: rhos
   :type: Optional[float]


   
   Get or set the Specifies the specular reflectance
















   ..
       !! processed by numpydoc !!

.. py:property:: rhod
   :type: Optional[float]


   
   Get or set the Specifies the diffuse reflectance
















   ..
       !! processed by numpydoc !!

.. py:property:: taus
   :type: Optional[float]


   
   Get or set the Specifies the specular transmittance.
















   ..
       !! processed by numpydoc !!

.. py:property:: taud
   :type: Optional[float]


   
   Get or set the Specifies the diffuse transmittance.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdiffr
   :type: float


   
   Get or set the LS-DYNA simulates diffuse reflection according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFR.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdifft
   :type: float


   
   Get or set the LS-DYNA simulates diffuse transmittance according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFT.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EF'


.. py:attribute:: subkeyword
   :value: 'MATERIAL'






