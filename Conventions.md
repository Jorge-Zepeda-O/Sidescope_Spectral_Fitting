## CLASS CONVENTIONS
### Organization
- Properties
	- Constant Variables
	- Static Variables (Methods)
		- Variable use
		- Variable mutation
	- Dynamic Variables
- Methods
	- Dynamic
		- Constructor
		- General Functions
		- Visualization Functions
	- Static

### Variable Naming
- **`CONSTANT`**  
A standalone constant variable, callable by any instance of this class

- **`GROUP_VAR`**  
A grouped constant variable, callable by any instance of this class

- **`S_multiword`**  
A static variable, which persists between instances of this class

- **`S_CamelCase`**  
A static function, which can be called by any instance of this class

- **`Capitalized`**  
An object/class declaration

- **`CamelCase`**  
A dynamic method of this class

- **`variable`**  
A standalone local variable, restricted to each instance of this class

- **`multiword`**  
A local variable with multiple shortened words to describe its functionality

- **`group_var`**  
A grouped local variable, related to other local variables in `group`

---------------------------------------------------------------------------------------------------
## FUNCTION CONVENTIONS
### Docstring Organization
```
function [outputs] = FunctionName(inputs, optional inputs)
% Function summary that could theoretically go on for a
% while, but ultimately describes what the function does
%	---------------------------------------------------
%	Argument Definitions:
%	> essential inputs:	(Type) 	Description
%
%	~ optional inputs	(Type) 	This is a really
%		long description that spans multiple lines
%				(~) 	Another Description
%
%	~ optional inputs	(Type)	Note that if one
%		description spans multiple lines, there is
% 		to be a space in between them.  This also
%		includes defaults!
%
%	- - - - - - - - - - - - - - - - - - - - - - - - - -
% 	Implicit Parameters:
%	>> implicit inputs	(Type) 	Not multi-line
%	>> implicit inputs	(Type) 	so no spaces
%
%	---------------------------------------------------
%	Outputs:
%	< output	(Type) 	Description
%	~ output	(Type)	A rarity!
```

Note that when a dynamic function calls on `this`, it is not described in the docstring, as it is described already in the parent class

### Argument Definitions
- **`>`**  
Indicates an essential input argument

- **`>>`**  
Indicates an input argument that is implicit from an essential input (usually the essential input is a struct or class)

- **`~`**  
Indicates an argument is optional and has a default value (if an input) or is ignored (if an output)

- **`<`**  
Indicates an output argument

### Common Section Titles
- **`Argument Defaults`**  
The section that handles optional arguments

- **`Argument Passing`**  
The section that holds logic for passing parameters into a class

- **`Initialize`**  
The section that allocates memory to various variables

- **`Create [X]`**  
The section that holds the logic for creating an important variable `X`

- **`[X] Update`**  
The section that handles updating parameters corresponding to `X`

- **`Null Output`**  
The section that clears the outputs if none are required

---------------------------------------------------------------------------------------------------
## VARIABLE CONVENTIONS
### Singletons
- **`(~)`**  
(0 x 0) | Single empty, used when the argument is optional to describe default behavior

- **`(#)`**  
(1 x 1) | Single value

- **`(bool)`**  
(1 x 1) | Single boolean value

- **`('chr')`**  
(1 x 1) | Single character value (e.g.: `'c', 'h', 'a', 'r'`)

- **`("str")`**  
(1 x 1) | Single string value (could be multiple characters like `"Hello World"`)

- **`(Obj)`**  
(1 x 1) | Single object of class `Obj`


### Tuples 
- **`(x, y)`**  
(1 x 2) | Tuple of related values `x`, and `y`, which are singletons

- **`(-, +)`**  
(1 x 2) | Tuple consisting of a minimum `-` and maximum `+` (usually of a range)

- **`(x_1, ... x_K)`**  
(1 x K) | Tuple of related values `x_1`, ..., `x_K`

### Vectors
- **`[#]`**  
(N x 1) | Vector of unrelated values - meanings listed in description

- **`[x]`**  
(N x 1) | Vector of related values corresponding to `x`

- **`['chr']`**  
(N x 1) | Vector of characters - (any character array)

- **`["str"]`**  
(N x 1) | Vector of string values

- **`[Obj]`**  
(N x 1) | Vector of related objects of class `Obj`

- **`[-:+]`**  
(1 x M) Vector corresponding to the range between minimum `-` and maximum `+`

### Vector Arrays
- **`[x, y]`**  
(N x 2) | Array of related values corresponding to `x` and `y`

- **`[x_1, ..., x_K]`**  
(N x K) | Array of related values corresponding to `x_1`, ..., `x_K`


### Matrices
- **`[[x, y]]`**  
(N x M) | Matrix of related values, where `x` is (N x 1) and `y` is (M x 1)

- **`[[[x, y, z]]]`**  
(N x M x L) | Three-dimensional matrix of related values `x`, `y`, and `z`

- **`[...[x_1, ..., x_K]...]`**  
(N1 x ... x NK) | Multi-dimensional matrix of related values `x_1`, ..., `x_K`


### Cells 
- **`{x}`**  
(N x 1) | Cell of related values corresponding to `x`

- **`{x_1, ... x_K}`**  
(N x K) | Cell of related values corresponding to `x_1`, ..., `x_K`

- **`{Obj}`**  
(N x 1) | cell of related objects