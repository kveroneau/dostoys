unit cgalib;

interface

uses graph;

procedure DrawTitle;

var
  AppTitle: string[40];

implementation

procedure DrawTitle;
begin
  Rectangle(20,20,GetMaxX-20,40);
  OutTextXY(30,27,AppTitle);
end;

begin
  AppTitle:='CGA System';
end.
