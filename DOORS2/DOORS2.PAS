program doors2;

uses graph, leakchk, registry, syscfg, bootsys, actkeys, tasksys, crt, coder;

begin
  if SystemConfig^.BootGUI then
    InitGUI;
  if SystemConfig^.DramaMode then
    Delay(1000);
  InitActKeys;
  if SystemConfig^.DramaMode then
    Delay(1000);
  InitTasks;
  if SystemConfig^.DramaMode then
    Delay(1000);
  StartTask('CODER');
  if SystemConfig^.DramaMode then
    Delay(1000);
  ProcessTasks;
  if SysState.gui_mode then
    CloseGraph;
  Dispose(SystemConfig);
  CloseRegistry;
end.