unit qvfs;

interface

type
  PVFSNode = ^TVFSNode;
  TVFSNode = packed record
    fname: string[20];
    typ: byte;
    wh: string[20];
    data: string[80];
  end;

procedure OpenQVFS(VFSFile: string);
procedure CloseQVFS;

procedure SetNode(node: PVFSNode; AFname, AWh, AData: string);
procedure AddNode(node: PVFSNode);
function FindNode(AFname, AWh: string): PVFSNode;

implementation

var
  vfs: File of TVFSNode;
  node: TVFSNode;

procedure SetNode(node: PVFSNode; AFname, AWh, AData: string);
begin
  with node^ do
  begin
    if AFname <> '' then
      fname:=AFname;
    if AWh <> '' then
      wh:=AWh;
    if AData <> '' then
      data:=AData;
  end;
end;

procedure AddNode(node: PVFSNode);
begin
  Seek(vfs, FileSize(vfs));
  Write(vfs, node^);
end;

function FindNode(AFname, AWh: string): PVFSNode;
begin
  Seek(vfs,1);
  repeat
    Read(vfs, node);
    if node.fname = AFname then
    begin
      if (AWh <> '') and (node.wh = AWh) then
        FindNode:=@node
      else if Awh = '' then
        FindNode:=@node;
  until eof(vfs);
  FindNode:=Nil;
end;

procedure InitVFS;
begin
  Rewrite(vfs);
  SetNode(@node, 'QVFS*','HDR','Quick Virtual File System Header');
  node.typ:=0;
  Write(vfs, node);
  SetNode(@node, 'FSRoot','','File System Root');
  Write(vfs, node);
end;

procedure OpenQVFS(VFSFile: string);
begin
  Assign(vfs, VFSFile);
  {$I-}
  Reset(vfs);
  {$I+}
  if IOResult <> 0 then
    InitVFS;
end;

procedure CloseQVFS;
begin
  Close(vfs);
end;


end.
