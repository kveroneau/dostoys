unit utils;

interface

function AsStr(value: integer): string;

implementation

function AsStr(value: integer): string;
var
  r: string;
begin
  Str(value,r);
  AsStr:=r;
end;

end.
