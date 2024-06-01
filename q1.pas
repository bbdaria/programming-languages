program PascalTriangle;

function getElement(row,col: integer) : integer;
var
    sum: integer;
begin
    if (row = 1) or (col = 1) or (col = row) then 
        getElement := 1
    else
        begin
            sum := getElement(row - 1, col - 1) + getElement(row - 1, col);
            getElement := sum;
        end;
end; 


var n: integer;
var line: array[1..15] of integer;
var
    i,j : integer;

begin

for i:= 1 to 15 do
    line[i] := 0;

ReadLn(n);
for i := 1 to n do
    begin
        for j:= 1 to i do
            begin
                line[j] := getElement(i,j)
            end;
        for j:=1 to i-1 do
            begin
                write(line[j]);
                write(' ')
            end;
        WriteLn(line[i])
    end
end.
