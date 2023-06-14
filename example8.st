{%
 % Example Check Even and Odd with nested if and for and loop
 %}

% global variables
const max := 10
var i :int

for i :1..max
	put i
	put ": "
	if i mod 2 = 0 then
		put "even number"
		var j := 1
		loop
			exit when j > i
			skip
			put "  "
			put j
			put ": "
			if j mod 2 = 0 then
				put "even sub number"
			else 
				put "odd sub number"
			end if

			var k := 1
			loop
				exit when k > j
				skip
				put "    "
				put k
				put ": "
				if k mod 2 = 0 then
					put "even sub sub number"
				else 
					put "odd sub sub number"
				end if
				k := k + 1
			end loop			

			j := j + 1
		end loop
		skip
		skip
	else
		put "odd number"
		if i > 5 then
			put " and it is more than 5"
		end if
	end if
	skip
end for
		


