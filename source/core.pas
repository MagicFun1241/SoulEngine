unit core;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, php4delphi, zendAPI, phpAPI, PHPTypes,
  regGui, guiComponents, guiForms, guiProperties, dsUtils,
  Vcl.Dialogs, ExeMod,

  {$IFDEF ADD_CHROMIUM}
  guiChromium,
  {$ENDIF}
  uPhpEvents;

var
  myPHPEngine: TPHPEngine;
  mypsvPHP: TpsvPHP;

function getPsvPHP(): TpsvPHP;
function getPhpEngine(): TPHPEngine;
procedure core_Init(aPHPEngine: TPHPEngine = nil; aPsvPHP: TpsvPHP = nil);

implementation

function getPsvPHP(): TpsvPHP;
begin
  Result := mypsvPHP;
end;

function getPhpEngine(): TPHPEngine;
begin
  Result := myPHPEngine;
end;

procedure core_Init(aPHPEngine: TPHPEngine = nil; aPsvPHP: TpsvPHP = nil);
begin
  regGui.registerGui();

  if aPHPEngine = nil then
    myPHPEngine := TPHPEngine.Create(Application)
  else
    myPHPEngine := aPHPEngine;

  if aPsvPHP = nil then
    mypsvPHP := TpsvPHP.Create(Application)
  else
    mypsvPHP := aPsvPHP;

  myPHPEngine.AddFunction('core_get_phpEngine', @getPhpEngine);
  myPHPEngine.AddFunction('core_get_psvPHP', @getPsvPHP);

  InitializeEventSystem(myPHPEngine);
  InitializeGuiComponents(myPHPEngine);
  InitializeGuiForms(myPHPEngine);
  InitializeGuiProperties(myPHPEngine);
  {$IFDEF ADD_CHROMIUM}
  InitializeGuiChromium(myPHPEngine);
  {$ENDIF}
  InitializeDsUtils(myPHPEngine);

  myPHPEngine.StartupEngine;
end;


end.


