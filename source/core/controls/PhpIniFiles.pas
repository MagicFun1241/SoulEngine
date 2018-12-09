unit PhpIniFiles;

interface

uses IniFiles;

type
  TPHPIniFile = class(TObject)
  private
    Ini: TIniFile;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure UpdateFile;
    //procedure ReadString();
  end;

implementation

constructor TPHPIniFile.Create(FileName: string);
begin
  inherited;
  Ini := TIniFile.Create(FileName);
end;

destructor TPHPIniFile.Destroy;
begin
  Ini.Free;
  inherited;
end;

procedure TPHPIniFile.UpdateFile;
begin
  Ini.UpdateFile;
end;

end.
