{%
 % Example nested parameter
 %}

% global variables


function add(a:int, b:int):int
	result a+b
end add

function mul(a:int, b:int):int
	result a*b
end mul


% main program
put add(mul(add(1, add(2, 3)), 1-2+3*40), mul(add(1, add(2, 3)), 1-2+3*40))



