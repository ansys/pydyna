class ExampleClass:
    """The summary line for a class docstring should fit on one line.

    Attributes should be documented inline with the attribute's
    declaration.

    Properties created with the ``@property`` decorator should be
    documented in the property's getter method.

    Parameters
    ----------
    param1 : str
        Description of ``param1``.
    param2 : list of str
        Description of ``param2``. Multiple lines are supported.
    param3 : int, optional
        Description of ``param3``.

    Examples
    --------
    An example of how to initialize this class should be given.

    >>> from ansys.product import library
    >>> example = library.ExampleClass('mystr', ['apple', 'orange'], 3)

    """

    def __init__(self, param1, param2, param3=0):
        self.attr1 = param1
        self.attr2 = param2
        self.attr3 = param3  #: Doc comment *inline* with attribute

        #: list of str: Doc comment *before* attribute, with type specified
        self.attr4 = ["attr4"]
        self.attr5 = None
        self._value = "readwrite_property"

    @property
    def readonly_property(self) -> str:
        """Properties should be documented in their getter method.

        Note that you do not need to provide a return here, but if you
        are not, provide a return type using typing.

        Examples
        --------
        >>> from ansys.product import library
        >>> example = library.ExampleClass('mystr', ['apple', 'orange'], 3)
        >>> example.readonly_property
        "readonly_property"

        """
        return "readonly_property"

    @property
    def readwrite_property(self) -> str:
        """Properties with both a getter and setter.

        These should only be documented in their getter method.

        If the setter method contains notable behavior, it should be
        mentioned here.

        Examples
        --------
        >>> from ansys.product import library
        >>> example = library.ExampleClass('mystr', ['apple', 'orange'], 3)
        >>> example.readwrite_property
        "readwrite_property"

        >>> example.readwrite_property = 'hello world'
        >>> example.readwrite_property
        'hello world'

        """
        return self._value

    @readwrite_property.setter
    def readwrite_property(self, value):
        self._value = value

    def example_method(self, param1, param2):
        """Class methods are similar to regular functions.

        Parameters
        ----------
        param1 : str
            The first parameter.
        param2 : str
            The second parameter.

        Returns
        -------
        bool
            True if successful, False otherwise.

        Notes
        -----
        Do not include the `self` parameter in the ``Parameters`` section.

        Examples
        --------
        >>> from ansys.product import library
        >>> example = library.ExampleClass('mystr', ['apple', 'orange'], 3)
        >>> example.example_method('foo', 'bar')
        True

        """
        return True

    def __special__(self):
        """By default special members with docstrings are not included.

        Special members are any methods or attributes that start with and
        end with a double underscore.

        """
        pass

    def __special_without_docstring__(self):
        pass  # do not need docstrings

    def _private(self):
        """By default private members are not included.

        Private members are any methods or attributes that start with an
        underscore and are *not* special. By default they are not included
        in the output.

        """
        pass

    def _private_without_docstring(self):
        pass


class Complex:
    """Custom implementation of a complex number.

    Parameters
    ----------
    real : float
        Real component of the complex number.
    imag : float, optional
        Imaginary component of the complex number.

    Examples
    --------
    >>> from ansys.product import library
    >>> my_num = library.Complex(real=1, imag=-1.0)
    >>> my_num
    (1.0 + 1.0j)

    """

    def __init__(self, real, imag=0.0):
        self._real = float(real)
        self._imag = float(imag)

    @property
    def real(self):
        """Real component of this complex number.

        Examples
        --------
        >>> from ansys.product import library
        >>> my_num = library.Complex(real=1, imag=-1.0)
        >>> my_num.real
        1.0

        """
        return self._real

    @real.setter
    def real(self, real):
        self._real = float(real)

    @property
    def imag(self):
        """Real component of this complex number.

        Examples
        --------
        >>> from ansys.product import library
        >>> my_num = library.Complex(real=1, imag=-1.0)
        >>> my_num.imag
        -1.0

        Set the imaginary component

        >>> my_num.imag = 2.0
        >>> my_num.imag
        2.0

        """
        return self._imag

    @imag.setter
    def imag(self, imag):
        self._imag = float(imag)

    def __add__(self, other):
        return Complex(self._real + other.real, self._imag + other.imag)

    def __sub__(self, other):
        return Complex(self._real - other.real, self._imag - other.imag)

    def __mul__(self, other):
        return Complex((self._real * other.real) - (self._imag * other.imag),
            (self._imag * other.real) + (self._real * other.imag))

    def __truediv__(self, other):
        r = (other.real**2 + other.imag**2)
        return Complex((self._real*other.real - self._imag*other.imag)/r,
            (self._imag*other.real + self._real*other.imag)/r)

    @property
    def abs(self):
        """Absolute value of this number

        Examples
        --------
        >>> from ansys.product import library
        >>> my_num = library.Complex(real=1, imag=1.0)
        >>> my_num.abs
        """
        return (self._real**2 + self._imag**2)**0.5

    def __repr__(self):
        """Be sure your class has a representational string"""
        if self._imag < 0:
            return f'({self._real} - {abs(self._imag)}j)'
        return f'({self._real} + {self._imag}j)'
